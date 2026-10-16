#!/bin/bash

# Installs a pinned AXe release the way its Homebrew formula (cameroncooke/homebrew-axe) does, rather than the tap's
# latest version, because it runs in the job that later holds the Claude token. Puts axe on the job's PATH.

set -euo pipefail

version=1.8.0
sha256=87eb09cd49dfc74fa8f7f5a46a03dfe2f14518b01d819f6a4094eab0d344ea7f
dir=$RUNNER_TEMP/axe
mkdir -p "$dir/release"
curl --proto '=https' --tlsv1.2 -sSfL -o "$dir/axe.tar.gz" "https://github.com/cameroncooke/AXe/releases/download/v$version/AXe-macOS-homebrew-v$version.tar.gz"
echo "$sha256  $dir/axe.tar.gz" | shasum -a 256 -c
tar -xzf "$dir/axe.tar.gz" -C "$dir/release"
axe=$(find "$dir/release" -type f -name axe -print -quit)
for framework in "$(dirname "$axe")"/Frameworks/*.framework; do
  codesign --force --sign - --timestamp=none "$framework"
done
codesign --force --sign - --timestamp=none "$axe"
"$axe" --version
dirname "$axe" >> "$GITHUB_PATH"
