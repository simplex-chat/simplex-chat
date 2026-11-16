#!/bin/bash

# Runner images from 20261013 add Command Line Tools whose default SDK is macOS 27, which Xcode 26.6's linker cannot read
# ("tapi error: malformed file"); GHC's links reach it despite DEVELOPER_DIR, so the default goes back to macOS 26.5.

set -euo pipefail

sdks=/Library/Developer/CommandLineTools/SDKs
if [ -L "$sdks/MacOSX.sdk" ] && [ -d "$sdks/MacOSX26.5.sdk" ]; then
  sudo ln -sfn MacOSX26.5.sdk "$sdks/MacOSX.sdk"
fi
ls -l "$sdks"
