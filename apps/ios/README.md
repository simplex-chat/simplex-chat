# Setup for iOS

## Prerequisites

- Prepare folders:

  ```sh
  mkdir -p ./apps/ios/Libraries/mac ./apps/ios/Libraries/ios ./apps/ios/Libraries/sim
  ```

## Update binaries

1. Extract binaries to `./apps/ios/Libraries/mac`.

2. Prepare binaries:

    ```sh
    chmod +w ./apps/ios/Libraries/mac/*
    cp ./apps/ios/Libraries/mac/* ./apps/ios/Libraries/ios
    cp ./apps/ios/Libraries/mac/* ./apps/ios/Libraries/sim
    for f in ./apps/ios/Libraries/ios/*; do mac2ios $f; done | wc -l
    for f in ./apps/ios/Libraries/sim/*; do mac2ios -s $f; done | wc -l
    ```

3. Put binaries into `./apps/ios/Libraries`.

    ```sh
    cp ./apps/ios/Libraries/sim/* ./apps/ios/Libraries
    ```

    or:

    ```sh
    cp ./apps/ios/Libraries/ios/* ./apps/ios/Libraries
    ```
