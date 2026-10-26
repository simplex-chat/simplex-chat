#!/bin/bash

# The plan job's choice of feature tests: select-tests.sh's names joined by spaces as the step output `tests`, and a line
# in the job summary. With an empty TESTS input, the changes are those since the run's commit's merge base with the
# upstream repo's master (the fork's parent, or the repo itself), found with the compare API.
# Usage: scripts/ci/choose-feature-tests.sh <platform dir>, from the repo root, with TESTS, GH_TOKEN, GITHUB_REPOSITORY,
# GITHUB_SHA, GITHUB_OUTPUT and GITHUB_STEP_SUMMARY set

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
tests=$(scripts/ci/select-tests.sh "$platform_dir" "$base" "$input" | tr '\n' ' ')
tests=${tests% }
echo "tests=$tests" | tee -a "$GITHUB_OUTPUT"
echo "- Feature tests: ${tests:-none}" >> "$GITHUB_STEP_SUMMARY"
