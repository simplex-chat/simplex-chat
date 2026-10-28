#!/bin/bash

set -euo pipefail

if [ $# -lt 2 ]; then
  echo "usage: scripts/ci/app-tests/select-tests.sh <platform dir> <base commit> [<tests input>]" >&2
  exit 2
fi
platform_dir=${1%/}
base=$2
input=${3:-}
platform=$(basename "$platform_dir")
name_pattern='^[a-z0-9][a-z0-9-]*$'

tests=""
for file in "$platform_dir"/tests/*.sh; do
  [ -f "$file" ] || continue
  name=$(basename "$file" .sh)
  if ! [[ "$name" =~ $name_pattern ]]; then
    echo "error: $file: a test's name has only lowercase letters, digits and -, so no run could select this one" >&2
    exit 2
  fi
  tests="$tests $name"
done

# the input's words and the covers patterns are split on spaces, never expanded as file names
set -f

keyword=""
words=0
for word in $input; do
  words=$((words + 1))
  case "$word" in
    all | none) keyword=$word ;;
    *)
      if ! [[ "$word" =~ $name_pattern ]]; then
        echo "error: \"$word\" is not a test name, all or none" >&2
        exit 2
      fi
      ;;
  esac
done
if [ -n "$keyword" ] && [ "$words" -gt 1 ]; then
  echo "error: $keyword can't be combined with other words: \"$input\"" >&2
  exit 2
fi

selected=""
select_test() {
  echo "$1: $2" >&2
  selected="$selected$1
"
}

runs_every_test() {
  case "$1" in
    scripts/ci/app-tests/*) return 0 ;;
    "$platform_dir"/tests/*) return 1 ;;
    "$platform_dir"/*) ;;
    *) return 1 ;;
  esac
  case "$(basename "$1")" in
    *.md | claude-*) return 1 ;;
  esac
}

covered_file() {
  local pattern changed_file
  for pattern in $(sed -n 's/^# covers://p' "$platform_dir/tests/$1.sh"); do
    while IFS= read -r changed_file; do
      if [ -n "$changed_file" ] && [[ $changed_file == $pattern ]]; then
        echo "$changed_file"
        return 0
      fi
    done <<< "$changed"
  done
  return 1
}

if [ "$keyword" = all ]; then
  for name in $tests; do
    select_test "$name" "all tests asked for"
  done
elif [ -z "$keyword" ] && [ "$words" -gt 0 ]; then
  for name in $input; do
    if [ -f "$platform_dir/tests/$name.sh" ]; then
      select_test "$name" "named in the tests input"
    else
      # noted, not refused: one input can name the tests of several platforms
      echo "$name: no test on $platform" >&2
    fi
  done
elif [ -z "$keyword" ]; then
  changed=$(git diff --name-only --diff-filter=d "$base" HEAD)
  shared=""
  while IFS= read -r changed_file; do
    if [ -n "$changed_file" ] && runs_every_test "$changed_file"; then
      shared=$changed_file
      break
    fi
  done <<< "$changed"
  for name in $tests; do
    if [ -n "$shared" ]; then
      select_test "$name" "$shared changed, which every $platform test runs on"
    elif grep -qxF "$platform_dir/tests/$name.sh" <<< "$changed"; then
      select_test "$name" "changed on this branch"
    elif covered=$(covered_file "$name"); then
      select_test "$name" "covers $covered"
    fi
  done
fi

if [ -n "$selected" ]; then
  printf '%s' "$selected" | sort -u
fi
