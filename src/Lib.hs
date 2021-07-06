{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad ()
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Aeson.Types (FromJSON (parseJSON), withObject, (.:))
import qualified Data.ByteString.Char8 as B
import Data.Foldable (for_)
import Data.List.Split (splitOn)
import Data.Maybe ()
import GHC.Generics (Generic)
import Network.HTTP.Conduit
  ( Request (requestHeaders),
    parseRequest,
  )
import Network.HTTP.Simple
  ( getResponseBody,
    httpBS,
    httpJSON,
    httpJSONEither,
    parseRequest,
  )
import System.Directory (createDirectoryIfMissing)
import System.Exit (die)
import System.IO (IOMode (WriteMode), hPutStr, openFile)

data DockerResponse = DockerResponse
  { token :: String,
    accessToken :: String,
    expiresIn :: Int,
    issuedAt :: String
  }
  deriving (Generic, Show)

instance FromJSON DockerResponse where
  parseJSON = withObject "DockerResponse" $ \v ->
    DockerResponse
      <$> v .: "token"
      <*> v .: "access_token"
      <*> v .: "expires_in"
      <*> v .: "issued_at"

data Config = Config
  { mediaType :: String,
    size :: Int,
    digestC :: String
  }
  deriving (Generic, Show)

data Layer = Layer
  { mediaType :: String,
    size :: Int,
    digestL :: String
  }
  deriving (Generic, Show)

data PullResponse = PullResponse
  { schemaVersion :: Int,
    mediaType :: String,
    config :: Config,
    layers :: [Layer]
  }
  deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v ->
    Config
      <$> v .: "mediaType"
      <*> v .: "size"
      <*> v .: "digest"

instance FromJSON Layer where
  parseJSON = withObject "Layer" $ \v ->
    Layer
      <$> v .: "mediaType"
      <*> v .: "size"
      <*> v .: "digest"

instance FromJSON PullResponse where
  parseJSON = withObject "PullResponse" $ \v ->
    PullResponse
      <$> v .: "schemaVersion"
      <*> v .: "mediaType"
      <*> v .: "config"
      <*> v .: "layers"

getToken :: (MonadIO m, MonadThrow m) => String -> m String
getToken repo = do
  let requestURL = "http://auth.docker.io/token?service=registry.docker.io&scope=repository:" ++ repo ++ ":pull"
  request <- parseRequest requestURL
  response <- httpJSONEither request
  result <- case getResponseBody response of
    Left exception -> do
      let errMsg = "Error: " ++ show exception
      liftIO $ die errMsg
    Right value -> pure value
  return $ token (result :: DockerResponse)

getManifest :: (MonadIO m, MonadThrow m) => String -> m PullResponse
getManifest name =
  let [pullName, pullTag] = case splitOn ":" name of
        [x, y] -> [x, y]
        [x] -> [x, "latest"]
        _ -> []
      image = "library/" ++ pullName
      registry = "https://registry-1.docker.io/v2/" ++ image ++ "/manifests/" ++ pullTag
   in do
        token <- getToken image
        initRequest <- parseRequest registry
        let newRequest = initRequest {requestHeaders = [("Authorization", B.pack $ "Bearer " ++ token), ("Accept", "application/vnd.docker.distribution.manifest.v2+json")]}
        response <- httpJSON newRequest
        let result = getResponseBody response
        pure result

getConfig :: (MonadIO m, MonadThrow m) => String -> m String
getConfig name = do
  response <- getManifest name
  return $ (digestC . config) response

getDigests :: (MonadIO m, MonadThrow m) => String -> m [String]
getDigests name = do
  response <- getManifest name
  return $ (map digestL . layers) response

getConfigAndDigests :: (MonadIO m, MonadThrow m) => String -> m (String, [String])
getConfigAndDigests name = do
  response <- getManifest name
  return $ digestC . config &&& map digestL . layers $ response

createLayers :: String -> IO ()
createLayers name = do
  let imageDir = "./images/"
      layerDir = "./images/" <> name <> "/"
  mapM_ (createDirectoryIfMissing True) [imageDir, layerDir]
  configAndDigests <- getConfigAndDigests name
  for_ (snd configAndDigests) \digest ->
    do
      let image = "library/" ++ name
          newLayer = layerDir ++ digest ++ ".tar"
          pull = "https://registry-1.docker.io/v2/" ++ image ++ "/blobs/" ++ digest
      initRequest <- parseRequest pull
      token <- getToken image
      let newRequest = initRequest {requestHeaders = [("Authorization", B.pack $ "Bearer " ++ token), ("Accept", "application/vnd.docker.distribution.manifest.v2+json")]}
      response <- httpBS newRequest
      let contents = B.unpack $ getResponseBody response
      handle <- openFile newLayer WriteMode
      hPutStr handle contents