# Inspector

Inspector is a little tool/library to pull layers of an Image from the Docker library.

## Usage

- `createLayers "<IMAGE_WITHOUT_TAG>"`
will create a folder in the current working directory and will pull the layers and store them in a tar archive.

- `getDigests "<IMAGE_WITH/WITHOUT TAG>"`
will fetch the name of the digests.

## Note

This is just a toy project I wrote to get good at Haskell and is not at all production ready. If you like the idea, feel free to submit a PR!

## References/Credits

**[containers/skopeo](https://github.com/containers/skopeo)**

**[docker drag](https://github.com/NotGlop/docker-drag)**

**Thanks to Morrow#0001 on the Functional Programming Discord for helping me understand the type errors.**
