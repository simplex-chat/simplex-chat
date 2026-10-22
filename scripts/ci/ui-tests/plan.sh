#!/bin/bash

# Reads the ui-tests workflow's inputs and writes to GITHUB_OUTPUT which platforms run, and the desktop and Android
# shards as matrices.
# Usage: PLATFORM=desktop|android|both TESTS='<classes or patterns>' scripts/ci/ui-tests/plan.sh
# Without TESTS, the desktop tests run in one shard per test package; a test package no shard names fails the plan, so
# a new package can't go untested. The Android tests run in android_shards shards of AndroidJUnitRunner's own sharding
# (-e numShards -e shardIndex). With TESTS (comma- or space-separated), one job per platform runs them: as Gradle
# --tests filters on desktop, with Class#method written Class.method, and as `am instrument -e class` on Android.

set -euo pipefail

out=${GITHUB_OUTPUT:-/dev/null}
ui=apps/multiplatform/common/src
roots=("$ui/uiTestShared/kotlin/chat/simplex/app/ui" "$ui/desktopTest/kotlin/chat/simplex/app/ui")
android_shards=4
shards='[
  {"shard": "chat", "tests": "chat.simplex.app.ui.chat.*"},
  {"shard": "groups", "tests": "chat.simplex.app.ui.groups.*"},
  {"shard": "main", "tests": "chat.simplex.app.ui.main.*"},
  {"shard": "settings-desktop", "tests": "chat.simplex.app.ui.settings.* chat.simplex.app.ui.desktop.* chat.simplex.app.ui.harness.*"}
]'

case "${PLATFORM:-}" in
  desktop) desktop=true android=false ;;
  android) desktop=false android=true ;;
  both) desktop=true android=true ;;
  *)
    echo "::error::platform must be desktop, android or both, not \"${PLATFORM:-}\""
    exit 1
    ;;
esac

allowed='^[A-Za-z0-9_.*#, ]*$'
if ! [[ "${TESTS:-}" =~ $allowed ]]; then
  echo "::error::tests may have only class names, patterns (* . # _), commas and spaces"
  exit 1
fi
read -r -a tests <<< "${TESTS//,/ }"

if [ "${#tests[@]}" -eq 0 ]; then
  covered=$(jq -r '[.[].tests | split(" ")[] | sub("^chat\\.simplex\\.app\\.ui\\."; "") | sub("\\.\\*$"; "")] | join(" ")' <<< "$shards")
  missing=$(find "${roots[@]}" -name '*Test.kt' | sed 's|.*/chat/simplex/app/ui/||' | cut -d/ -f1 | sort -u | while read -r pkg; do
    case " $covered " in
      *" $pkg "*) ;;
      *) echo "chat.simplex.app.ui.$pkg" ;;
    esac
  done)
  if [ -n "$missing" ]; then
    echo "::error::no desktop shard runs the tests in $(paste -sd ' ' <<< "$missing") (add a shard in scripts/ci/ui-tests/plan.sh)"
    exit 1
  fi
  matrix=$(jq -c . <<< "$shards")
  android_matrix=$(jq -cn --argjson n "$android_shards" '[range($n) | {shard: "\(. + 1)-of-\($n)", args: "-e numShards \($n) -e shardIndex \(.)"}]')
else
  if [ "$android" = true ] && [[ "${tests[*]}" == *"*"* ]]; then
    echo "::error::Android runs whole class names (or Class#method), not patterns with *"
    exit 1
  fi
  desktop_tests="${tests[*]}"
  matrix=$(jq -cn --arg tests "${desktop_tests//#/.}" '[{shard: "selected", tests: $tests}]')
  android_classes=$(IFS=,; echo "${tests[*]}")
  android_matrix=$(jq -cn --arg classes "$android_classes" '[{shard: "selected", args: "-e class \($classes)"}]')
fi

{
  echo "desktop=$desktop"
  echo "android=$android"
  echo "desktop-shards=$matrix"
  echo "android-shards=$android_matrix"
} | tee -a "$out"
