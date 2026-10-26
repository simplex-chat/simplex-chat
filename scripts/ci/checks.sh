#!/bin/bash

# The checks of a scripted UI test, shared by the platforms: check adds a PASS or FAIL line to checks.txt in UI_OUT and
# goes on, fail ends the test, report_checks prints the lines and adds them to the job summary.
# Sourced by each platform's lib.sh, with UI_OUT set. Runs under macOS's bash 3.2 on the iOS runner.

checks=$UI_OUT/checks.txt

report_checks() {
  [ -s "$checks" ] || return 0
  echo "::group::checks"
  cat "$checks"
  echo "::endgroup::"
  if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
    {
      echo "### Smoke test checks"
      echo
      sed 's/^/- /' "$checks"
    } >> "$GITHUB_STEP_SUMMARY"
  fi
}

fail() {
  echo "error: $*"
  exit 1
}

check() {
  local what=$1
  shift
  if "$@"; then
    echo "PASS $what" | tee -a "$checks"
  else
    echo "FAIL $what" | tee -a "$checks"
    echo "::error::$what"
  fi
}
