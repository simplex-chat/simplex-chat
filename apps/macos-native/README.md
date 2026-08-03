# SimpleX Native for macOS

This is the native SwiftUI/AppKit macOS frontend. It links to the existing desktop SimpleX core and opens the existing desktop database at `~/.local/share/simplex`.

It does not use Compose, change the core protocol, or introduce a second message format.

Build the app after the desktop core libraries have been staged:

```sh
./build-app.sh
```

The resulting application is `/private/tmp/simplex-native-build/SimpleX.app`. Set
`SIMPLEX_NATIVE_OUTPUT_DIR` to stage it elsewhere. Only one SimpleX frontend may
open the desktop database at a time.
