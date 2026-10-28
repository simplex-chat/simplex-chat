#!/bin/bash

set -euo pipefail

platform_dir=$1
input=${TESTS:-}
case "$input" in
  *[![:space:]]*)
    # select-tests.sh doesn't read the base when the input names the tests
    base=HEAD
    ;;
  *)
    upstream=$(gh api "repos/$GITHUB_REPOSITORY" --jq '.parent.owner.login // .owner.login')
    base=$(gh api "repos/$GITHUB_REPOSITORY/compare/$upstream:master...$GITHUB_SHA" --jq .merge_base_commit.sha)
    git fetch -q --depth=1 origin "$base"
    ;;
esac
tests=$("$(dirname "$0")/select-tests.sh" "$platform_dir" "$base" "$input" | tr '\n' ' ')
tests=${tests% }
echo "tests=$tests" | tee -a "$GITHUB_OUTPUT"
echo "- Feature tests: ${tests:-none}" >> "$GITHUB_STEP_SUMMARY"
