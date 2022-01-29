# Setup for iOS

## Prerequisites

- `mac2ios` executable and in PATH:

  https://github.com/zw3rk/mobile-core-tools

- Folders:

  ```sh
  mkdir -p ./apps/ios/Libraries/mac ./apps/ios/Libraries/ios ./apps/ios/Libraries/sim
  ```

## Update binaries

1. Extract binaries to `./apps/ios/Libraries/mac`.

2. Prepare binaries for iOS and for Xcode simulator:

    ```sh
    chmod +w ./apps/ios/Libraries/mac/*
    cp ./apps/ios/Libraries/mac/* ./apps/ios/Libraries/ios
    cp ./apps/ios/Libraries/mac/* ./apps/ios/Libraries/sim
    for f in ./apps/ios/Libraries/ios/*; do mac2ios $f; done | wc -l
    for f in ./apps/ios/Libraries/sim/*; do mac2ios -s $f; done | wc -l
    ```

3. Put binaries into `./apps/ios/Libraries`.

    For simulator:

    ```sh
    cp ./apps/ios/Libraries/sim/* ./apps/ios/Libraries
    ```

    For iOS:

    ```sh
    cp ./apps/ios/Libraries/ios/* ./apps/ios/Libraries
    ```
