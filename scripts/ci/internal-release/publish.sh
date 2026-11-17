#!/bin/bash

set -euo pipefail
shopt -s inherit_errexit

desktop=$1
android=$2
release=$3
repository=$GITHUB_REPOSITORY-test
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

mkdir -p "$release"
cp "$desktop"/simplex-desktop-* "$release"

if [ -n "${ANDROID_TEST_KEYSTORE:-}" ]; then
  base64 --decode <<< "$ANDROID_TEST_KEYSTORE" > "$work/test.keystore"
  apksigner=$(ls "$ANDROID_HOME"/build-tools/*/apksigner | sort -V | tail -n 1)
  sign_apk() {
    "$apksigner" sign --alignment-preserved \
      --ks "$work/test.keystore" --ks-pass env:ANDROID_TEST_KEYSTORE_PASSWORD \
      --ks-key-alias "$ANDROID_TEST_KEY_ALIAS" --key-pass env:ANDROID_TEST_KEY_PASSWORD \
      --out "$release/$2" "$android/$1"
  }
  sign_apk simplex-aarch64.apk simplex.apk
  sign_apk simplex-armv7a.apk simplex-armv7a.apk
  "$apksigner" verify --print-certs "$release/simplex.apk" | grep SHA-256 >> "$GITHUB_STEP_SUMMARY"
else
  echo "::notice title=Android test APKs::ANDROID_TEST_KEYSTORE is not set: the release has no APKs"
fi

numbers=$(scripts/ci/internal-release/build-numbers.sh show HEAD | jq --arg commit "$GITHUB_SHA" '. + {commit: $commit}')
tag=$(jq -r '"\(.desktop.version_name)-\(.desktop.version_code)"' <<< "$numbers")
notes=$(jq -r --arg url "$GITHUB_SERVER_URL/$GITHUB_REPOSITORY/commit/$GITHUB_SHA" '
  "Internal build of \($url)\n\n" +
  "Android \(.android.version_name) (\(.android.version_code)), " +
  "desktop \(.desktop.version_name) (\(.desktop.version_code)), " +
  "iOS \(.ios.version_name) (\(.ios.build))"' <<< "$numbers")
{
  echo "### $tag"
  echo
  echo "$notes"
  echo
  printf -- '- `%s`\n' $(ls "$release")
  echo
  echo '```json'
  echo "$numbers"
  echo '```'
} >> "$GITHUB_STEP_SUMMARY"

if [ -z "${SIMPLEX_CHAT_TEST_TOKEN:-}" ]; then
  echo "::notice title=simplex-chat-test::SIMPLEX_CHAT_TEST_TOKEN is not set: $tag is not published"
  exit 0
fi

export GH_TOKEN=$SIMPLEX_CHAT_TEST_TOKEN
current=$(gh api "repos/$repository/contents/release.json" --jq .sha 2> /dev/null || true)
commit=$(gh api -X PUT "repos/$repository/contents/release.json" \
  -f message="$tag" -f content="$(base64 -w 0 <<< "$numbers")" ${current:+-f sha="$current"} --jq .commit.sha)
gh release create "$tag" "$release"/* --repo "$repository" --target "$commit" --title "$tag" --notes "$notes"
echo "::notice title=simplex-chat-test::$tag is published"
