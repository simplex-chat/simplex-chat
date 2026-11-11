#!/bin/bash

# Finds the newest run of this branch in this repository that uploaded an unexpired artifact with this name,
# and writes its id as run-id to GITHUB_OUTPUT. Needs GH_TOKEN with actions: read.
# Usage: scripts/ci/desktop-linux/earlier-run.sh <artifact name>
# Artifacts from forks and other branches are not used: their workflows could upload anything under this name.

set -euo pipefail

name=$1
run_id=$(gh api "repos/$GITHUB_REPOSITORY/actions/artifacts?name=$name&per_page=100" | jq -r --arg branch "$GITHUB_REF_NAME" '
  [.artifacts[]
    | select((.expired | not) and .workflow_run.head_branch == $branch and .workflow_run.head_repository_id == .workflow_run.repository_id)]
  | sort_by(.created_at) | last | .workflow_run.id // empty')
if [ -n "$run_id" ]; then
  echo "$name: from run $run_id"
  echo "run-id=$run_id" >> "$GITHUB_OUTPUT"
else
  echo "$name: not built yet"
fi
