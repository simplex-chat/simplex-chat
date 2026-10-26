#!/bin/bash

# Runs feature tests (<platform dir>/tests/<test>.sh, chosen by select-tests.sh) on the app that the smoke test left
# running, in the given order, each in its own process after restarting the app at the chat list in the reachable layout
# (ui layout reachable). A test's screens and checks.txt go to <out dir>/<test>, and the job summary gets a section per
# test: passed, failed or skipped. A test whose header says "# needs: backend" is skipped when BACKEND_DIR is empty.
# Usage: scripts/ci/feature-tests.sh <platform dir> <out dir> <test>...
# Exits 1 if any test failed. Runs under macOS's bash 3.2 on the iOS runner.

set -euo pipefail

if [ $# -lt 3 ]; then
  echo "usage: scripts/ci/feature-tests.sh <platform dir> <out dir> <test>..."
  exit 2
fi
here=$(dirname "$0")
platform_dir=${1%/}
out_dir=$2
shift 2
ui=$platform_dir/ui
export BACKEND_DIR=${BACKEND_DIR:-}

for name in "$@"; do
  if ! [[ "$name" =~ ^[a-z0-9][a-z0-9-]*$ ]]; then
    echo "error: \"$name\" is not a test name"
    exit 2
  fi
  if [ ! -f "$platform_dir/tests/$name.sh" ]; then
    echo "error: no test $platform_dir/tests/$name.sh"
    exit 2
  fi
done

needs_backend() {
  sed -n 's/^# needs://p' "$1" | grep -qw backend
}

failed=0
for name in "$@"; do
  file=$platform_dir/tests/$name.sh
  export UI_OUT=$out_dir/$name
  mkdir -p "$UI_OUT"
  # sourced per test: its checks file is the one in UI_OUT
  . "$here/checks.sh"
  : > "$checks"
  echo "::group::feature test $name"
  if needs_backend "$file" && [ -z "$BACKEND_DIR" ]; then
    note "skipped: needs the test backend, which this run didn't start"
    result=skipped
  else
    status=0
    if "$ui" layout reachable; then
      bash "$file" || status=$?
    else
      check "the app restarted at the chat list in the reachable layout" false
    fi
    if [ "$status" -ne 0 ]; then
      note "the test exited with status $status"
    fi
    if [ "$status" -ne 0 ] || grep -q '^FAIL' "$checks"; then
      result=failed
      failed=$((failed + 1))
      "$ui" shot final || true
    else
      result=passed
      [ -s "$checks" ] || note "the test made no checks"
    fi
  fi
  echo "::endgroup::"
  report_checks "Feature test $name: $result"
done

if [ "$failed" -gt 0 ]; then
  echo "error: $failed of $# feature tests failed"
  exit 1
fi
echo "feature tests passed: $*"
