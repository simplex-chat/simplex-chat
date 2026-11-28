#!/bin/bash

set -euo pipefail
shopt -s inherit_errexit

commit=HEAD
if [ -z "$(git diff --name-only HEAD^ HEAD -- . ':!apps/multiplatform/gradle.properties' ':!apps/ios/SimpleX.xcodeproj/project.pbxproj')" ] &&
  [ "$(gh api "repos/$GITHUB_REPOSITORY/actions/workflows/build.yml/runs?head_sha=$(git rev-parse HEAD)&event=push" --jq .total_count)" = 0 ]; then
  commit=HEAD^
fi
sha=$(git rev-parse "$commit")
for _ in $(seq 180); do
  read -r status conclusion url < <(gh api "repos/$GITHUB_REPOSITORY/actions/workflows/build.yml/runs?head_sha=$sha&event=push" \
    --jq '.workflow_runs[0] | "\(.status) \(.conclusion) \(.html_url)"')
  [ "$status" = completed ] || [ "$status" = null ] && break
  sleep 60
done
if [ "$conclusion" != success ]; then
  echo "::error title=No internal release::build.yml of $sha: ${status} ${conclusion} ${url}"
  exit 1
fi
echo "build.yml passed on $sha: $url"
