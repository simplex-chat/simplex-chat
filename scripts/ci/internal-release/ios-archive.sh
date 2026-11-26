#!/bin/bash

set -euo pipefail

out=$1
art=${2:-}
core=$out/pkg-ios-aarch64-swift-json.zip
team=5NN7GUYB6T
nix=/nix/var/nix/profiles/default
work=$(mktemp -d)
mkdir -p "$out" ~/Downloads

if [ ! -f "$core" ]; then
  curl --proto '=https' --tlsv1.2 -fsSL -o "$work/nix-install" https://releases.nixos.org/nix/nix-2.34.6/install
  echo "bf6d12da4aeaae38ab509dc736df648597bcdee051046ea76d3d53d5b18fa54a  $work/nix-install" | shasum -a 256 -c
  cat > "$work/nix.conf" << EOF
experimental-features = nix-command flakes
max-jobs = auto
extra-substituters = https://cache.zw3rk.com https://cache.iog.io
extra-trusted-public-keys = loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
cores = 0
EOF
  sh "$work/nix-install" --daemon --yes --no-channel-add --nix-extra-conf-file "$work/nix.conf"
  NIX_SSL_CERT_FILE=$nix/etc/ssl/certs/ca-bundle.crt "$nix/bin/nix" build -L --out-link "$work/core" \
    ".#hydraJobs.aarch64-darwin.aarch64-darwin-ios:lib:simplex-chat"
  cp "$work/core/pkg-ios-aarch64-swift-json.zip" "$core"
fi

cp "$core" ~/Downloads/
scripts/ci/ios-sim/prepare-libs.sh
scripts/ios/update-pbxproj.sh

if [ -n "$art" ]; then
  printf 'SIMPLEX_ASSETS_DIR = %s\nSWIFT_ACTIVE_COMPILATION_CONDITIONS = $(inherited) SIMPLEX_ASSETS\n' "$PWD/$art" > apps/ios/Local.xcconfig
  scripts/ios/copy-assets.sh "$PWD/$art"
else
  echo "::warning::no simplex-chat-art checkout: the archive has the default icons"
fi

archive=$work/SimpleX.xcarchive
xcodebuild archive -quiet -project apps/ios/SimpleX.xcodeproj -scheme "SimpleX (iOS)" -configuration Release \
  -destination generic/platform=iOS -archivePath "$archive" \
  -onlyUsePackageVersionsFromResolvedFile -skipPackagePluginValidation -skipMacroValidation CODE_SIGNING_ALLOWED=NO

sign() {
  sed "s/\$(AppIdentifierPrefix)/$team./" "$2" > "$work/entitlements.plist"
  for code in "$1"/Frameworks/*.framework "$1"/Frameworks/*.dylib; do
    if [ -e "$code" ]; then
      codesign --force --sign - --timestamp=none "$code"
    fi
  done
  codesign --force --sign - --timestamp=none --entitlements "$work/entitlements.plist" "$1"
}
app=$archive/Products/Applications/SimpleX.app
sign "$app/PlugIns/SimpleX NSE.appex" "apps/ios/SimpleX NSE/SimpleX NSE.entitlements"
sign "$app/PlugIns/SimpleX SE.appex" "apps/ios/SimpleX SE/SimpleX SE.entitlements"
sign "$app" "apps/ios/SimpleX (iOS).entitlements"
codesign --verify --deep --strict "$app"
plutil -replace ApplicationProperties.Team -string "$team" "$archive/Info.plist"
ditto -c -k --keepParent "$archive" "$out/SimpleX.xcarchive.zip"
