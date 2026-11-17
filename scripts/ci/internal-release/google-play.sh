#!/bin/bash

set -euo pipefail
shopt -s inherit_errexit

if [ -z "${ANDROID_UPLOAD_KEYSTORE:-}" ] || [ -z "${GOOGLE_PLAY_SERVICE_ACCOUNT:-}" ]; then
  echo "::notice title=Google Play::ANDROID_UPLOAD_KEYSTORE or GOOGLE_PLAY_SERVICE_ACCOUNT is not set: the bundle is not uploaded"
  exit 0
fi

bundle=$1
package=chat.simplex.app
edits=https://androidpublisher.googleapis.com/androidpublisher/v3/applications/$package/edits
uploads=https://androidpublisher.googleapis.com/upload/androidpublisher/v3/applications/$package/edits
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

base64 --decode <<< "$ANDROID_UPLOAD_KEYSTORE" > "$work/upload.keystore"
"$JAVA_HOME/bin/jarsigner" -keystore "$work/upload.keystore" \
  -storepass:env ANDROID_UPLOAD_KEYSTORE_PASSWORD -keypass:env ANDROID_UPLOAD_KEY_PASSWORD \
  "$bundle" "$ANDROID_UPLOAD_KEY_ALIAS"

base64url() {
  openssl base64 -A | tr '+/' '-_' | tr -d '='
}

jq -r .private_key <<< "$GOOGLE_PLAY_SERVICE_ACCOUNT" > "$work/service-account.pem"
header=$(printf '{"alg":"RS256","typ":"JWT"}' | base64url)
claims=$(jq -c --argjson now "$(date +%s)" \
  '{iss: .client_email, scope: "https://www.googleapis.com/auth/androidpublisher", aud: .token_uri, iat: $now, exp: ($now + 600)}' \
  <<< "$GOOGLE_PLAY_SERVICE_ACCOUNT" | base64url)
signature=$(printf '%s.%s' "$header" "$claims" | openssl dgst -sha256 -sign "$work/service-account.pem" | base64url)
token=$(curl -sS --fail-with-body "$(jq -r .token_uri <<< "$GOOGLE_PLAY_SERVICE_ACCOUNT")" \
  --data-urlencode grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer \
  --data-urlencode "assertion=$header.$claims.$signature" | jq -r .access_token)
echo "::add-mask::$token"

play() {
  curl -sS --fail-with-body -H "Authorization: Bearer $token" "$@"
}

edit=$(play -X POST "$edits" | jq -r .id)
code=$(play -X POST -H "Content-Type: application/octet-stream" --data-binary "@$bundle" "$uploads/$edit/bundles?uploadType=media" | jq -r .versionCode)
jq -n --arg code "$code" '{track: "internal", releases: [{versionCodes: [$code], status: "completed"}]}' \
  | play -X PUT -H "Content-Type: application/json" --data-binary @- "$edits/$edit/tracks/internal" > /dev/null
play -X POST "$edits/$edit:commit" > /dev/null
echo "::notice title=Google Play::version code $code is released to internal testing"
