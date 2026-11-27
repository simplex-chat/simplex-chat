#!/bin/bash

# Finds the newest run of this branch, of the pull request's head repository or else of this repository, that uploaded
# an unexpired artifact with this name, and writes its id as run-id to GITHUB_OUTPUT. Needs GH_TOKEN with actions: read.
# Usage: scripts/ci/ios-sim/earlier-run.sh <artifact name>
# Artifacts of other branches and repositories are not used: their workflows could upload anything under this name.

set -euo pipefail

name=$1
branch=${GITHUB_HEAD_REF:-$GITHUB_REF_NAME}
head_repository=$(jq ".pull_request.head.repo.id // $GITHUB_REPOSITORY_ID" "$GITHUB_EVENT_PATH")
run_id=$(gh api "repos/$GITHUB_REPOSITORY/actions/artifacts?name=$name&per_page=100" | jq -r --arg branch "$branch" --argjson head_repository "$head_repository" '
  [.artifacts[]
    | select((.expired | not) and .workflow_run.head_branch == $branch and .workflow_run.head_repository_id == $head_repository)]
  | sort_by(.created_at) | last | .workflow_run.id // empty')
if [ -n "$run_id" ]; then
  echo "$name: from run $run_id"
  echo "run-id=$run_id" >> "$GITHUB_OUTPUT"
else
  echo "$name: not built yet"
fi
