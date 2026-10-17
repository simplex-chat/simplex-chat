#!/usr/bin/env bash
set -euo pipefail
if [[ $# -lt 3 || $# -gt 4 ]]; then
  echo 'usage: run.sh WINE WINESERVER PROBE [ACTUAL_ARCHIVE]' >&2
  exit 2
fi
wine=$1
wineserver=$2
probe=$3
task_dir=$(mktemp -d "${TMPDIR:-/tmp}/windows-fs-check.XXXXXXXX")
export WINEPREFIX="$task_dir/prefix"
export WINEDEBUG=-all
cleanup() {
  timeout --kill-after=1s 5s "$wineserver" -k >"$task_dir/cleanup.log" 2>&1 || true
}
trap cleanup EXIT
mkdir "$task_dir/files"
cd "$task_dir/files"
printf '\001\000' >short.bin
long=.
for _ in 1 2 3 4 5 6 7 8; do
  long="$long/abcdefghijklmnopqrstuvwxyz0123456789abcdef"
done
mkdir -p "$long"
printf '\001\000' >"$long/long.bin"
# Same archive path structure and 320-character suffix as the observed input;
# all files are owned by this check, never created in the Nix store.
unit=ghc-internal-9.1400.0-f522b3d4457177f9b9536b39408687579503475cdae062955fa3cdbd236ac23e
archive="nix/store/00000000000000000000000000000000-ghc-internal-lib-ghc-internal-x86_64-w64-mingw32-9.1400.0/store/host/x86_64-unknown-mingw32/lib/$unit/libHS$unit.a"
[[ ${#archive} == 319 && ${#long} -gt 260 ]]
mkdir -p "$(dirname "$archive")"
printf '\001\000' >"$archive"
args=(--strict short.bin "$long/long.bin" "$archive")
if [[ $# == 4 ]]; then
  if [[ -f "$4" ]]; then
    args+=("$4")
  else
    echo 'optional actual archive unavailable; using owned archive-shaped fixture'
  fi
fi
# Startup and the complete real-API probe share one finite execution window.
printf 'RUNNER before-Wine UTC=%s; target emits PROBE_MAIN_STARTED after startup\n' "$(date -u +%FT%TZ)"
SECONDS=0
set +e
timeout --kill-after=2s 20s "$wine" "$probe" "${args[@]}"
status=$?
set -e
printf 'RUNNER Wine-exit=%s elapsedSeconds=%s\n' "$status" "$SECONDS"
if [[ "$status" == 124 || "$status" == 137 ]]; then
  echo 'ENVIRONMENT/RUNTIME WINDOW EXHAUSTED: use PROBE_MAIN_STARTED to distinguish startup from probe progress'
fi
exit "$status"
