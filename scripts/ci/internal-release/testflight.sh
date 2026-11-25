#!/bin/bash

set -euo pipefail

if [ -z "${IOS_DISTRIBUTION_CERTIFICATE:-}" ] || [ -z "${APP_STORE_CONNECT_KEY:-}" ]; then
  echo "::notice title=TestFlight::IOS_DISTRIBUTION_CERTIFICATE or APP_STORE_CONNECT_KEY is not set: the archive is not uploaded"
  exit 0
fi

archive=$1
team=5NN7GUYB6T
work=$(mktemp -d)
keychain=$work/distribution.keychain-db
profiles="$HOME/Library/Developer/Xcode/UserData/Provisioning Profiles"
security create-keychain -p "" "$keychain"
trap 'security delete-keychain "$keychain"; rm -rf "$work"' EXIT

security set-keychain-settings "$keychain"
security unlock-keychain -p "" "$keychain"
base64 --decode <<< "$IOS_DISTRIBUTION_CERTIFICATE" > "$work/distribution.p12"
security import "$work/distribution.p12" -k "$keychain" -P "$IOS_DISTRIBUTION_CERTIFICATE_PASSWORD" -T /usr/bin/codesign
security set-key-partition-list -S apple-tool:,apple: -s -k "" "$keychain" > /dev/null
security list-keychains -d user -s "$keychain" $(security list-keychains -d user | xargs)

install_profile() {
  base64 --decode <<< "$1" > "$work/profile.mobileprovision"
  security cms -D -i "$work/profile.mobileprovision" -o "$work/profile.plist"
  uuid=$(plutil -extract UUID raw "$work/profile.plist")
  mkdir -p "$profiles"
  mv "$work/profile.mobileprovision" "$profiles/$uuid.mobileprovision"
  echo "$uuid"
}

app=$(install_profile "$IOS_PROFILE_APP")
nse=$(install_profile "$IOS_PROFILE_NSE")
se=$(install_profile "$IOS_PROFILE_SE")
cat > "$work/ExportOptions.plist" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>method</key>
  <string>app-store-connect</string>
  <key>destination</key>
  <string>upload</string>
  <key>testFlightInternalTestingOnly</key>
  <true/>
  <key>manageAppVersionAndBuildNumber</key>
  <false/>
  <key>teamID</key>
  <string>$team</string>
  <key>signingStyle</key>
  <string>manual</string>
  <key>signingCertificate</key>
  <string>Apple Distribution</string>
  <key>provisioningProfiles</key>
  <dict>
    <key>chat.simplex.app</key>
    <string>$app</string>
    <key>chat.simplex.app.SimpleX-NSE</key>
    <string>$nse</string>
    <key>chat.simplex.app.SimpleX-SE</key>
    <string>$se</string>
  </dict>
</dict>
</plist>
EOF

printf '%s' "$APP_STORE_CONNECT_KEY" > "$work/AuthKey.p8"
xcodebuild -exportArchive \
  -archivePath "$archive" \
  -exportPath "$work/export" \
  -exportOptionsPlist "$work/ExportOptions.plist" \
  -authenticationKeyPath "$work/AuthKey.p8" \
  -authenticationKeyID "$APP_STORE_CONNECT_KEY_ID" \
  -authenticationKeyIssuerID "$APP_STORE_CONNECT_ISSUER_ID"
build=$(plutil -extract CFBundleVersion raw "$archive/Products/Applications/SimpleX.app/Info.plist")
echo "::notice title=TestFlight::build $build is uploaded for internal testing"
