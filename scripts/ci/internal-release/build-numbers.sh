#!/bin/bash

set -euo pipefail
shopt -s inherit_errexit

properties=apps/multiplatform/gradle.properties
pbxproj=apps/ios/SimpleX.xcodeproj/project.pbxproj

one() {
  if [ -z "$2" ] || [ "$(wc -l <<< "$2")" -ne 1 ]; then
    echo "::error::expected one value of $1, found: ${2:-none}" >&2
    return 1
  fi
  echo "$2"
}

property() {
  one "$2" "$(sed -n "s/^${2//./\\.}=//p" <<< "$1")"
}

setting() {
  one "$2" "$(sed -n "s/^[[:space:]]*$2 = \(.*\);\$/\1/p" <<< "$1" | sort -u)"
}

show() {
  local gradle xcode android_name android_code desktop_name desktop_code ios_name ios_build
  gradle=$(git show "$1:$properties")
  xcode=$(git show "$1:$pbxproj")
  android_name=$(property "$gradle" android.version_name)
  android_code=$(property "$gradle" android.version_code)
  desktop_name=$(property "$gradle" desktop.version_name)
  desktop_code=$(property "$gradle" desktop.version_code)
  ios_name=$(setting "$xcode" MARKETING_VERSION)
  ios_build=$(setting "$xcode" CURRENT_PROJECT_VERSION)
  jq -n \
    --arg android_name "$android_name" \
    --argjson android_code "$android_code" \
    --arg desktop_name "$desktop_name" \
    --argjson desktop_code "$desktop_code" \
    --arg ios_name "$ios_name" \
    --argjson ios_build "$ios_build" \
    '{
      android: {version_name: $android_name, version_code: $android_code},
      desktop: {version_name: $desktop_name, version_code: $desktop_code},
      ios: {version_name: $ios_name, build: $ios_build}
    }'
}

raised() {
  local before after
  before=$(show "$1")
  after=$(show "$2")
  jq -n -r --argjson before "$before" --argjson after "$after" '
    def names: [.android.version_name, .desktop.version_name, .ios.version_name];
    def numbers: [.android.version_code, .desktop.version_code, .ios.build];
    if ($after | names) != ($before | names) then "names"
    elif [($after | numbers), ($before | numbers)] | transpose | all(.[0] > .[1]) then "raised"
    else "numbers"
    end'
}

case "${1:-}" in
  show)
    show "$2"
    ;;
  raised)
    result=$(raised "$2" "$3")
    case $result in
      raised)
        echo "release=true"
        ;;
      names)
        echo "::notice title=No internal release::the version names changed" >&2
        echo "release=false"
        ;;
      numbers)
        echo "::notice title=No internal release::not every build number is raised" >&2
        echo "release=false"
        ;;
    esac
    ;;
  bump)
    last=$(git log -1 --format=%H -G 'version_code=|CURRENT_PROJECT_VERSION = ' -- "$properties" "$pbxproj")
    if [ "$last" = "$(git rev-parse HEAD)" ]; then
      echo "::notice title=No bump::no commits after the build numbers changed in $last" >&2
      exit 0
    fi
    numbers=$(show HEAD)
    message=$(jq -r '"\(.android.version_name): android \(.android.version_code + 1), desktop \(.desktop.version_code + 1), ios \(.ios.build + 1)"' <<< "$numbers")
    perl -pi -e 's/^((?:android|desktop)\.version_code=)(\d+)$/$1 . ($2 + 1)/e' "$properties"
    perl -pi -e 's/(CURRENT_PROJECT_VERSION = )(\d+);/$1 . ($2 + 1) . ";"/e' "$pbxproj"
    git commit -q -m "$message" -- "$properties" "$pbxproj"
    result=$(raised HEAD^ HEAD)
    if [ "$result" != raised ]; then
      echo "::error::$message does not raise every build number" >&2
      exit 1
    fi
    echo "bumped=true"
    ;;
  *)
    echo "usage: build-numbers.sh show <commit> | raised <before> <after> | bump" >&2
    exit 1
    ;;
esac
