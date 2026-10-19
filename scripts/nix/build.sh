#!/usr/bin/env sh
set -eu

# Use this checkout and its lock file for every target, including cross builds.
repo_dir=$(CDPATH='' cd -- "$(dirname -- "$0")/../.." && pwd)

usage() {
  cat <<'EOF'
Usage: scripts/nix/build.sh [--list] TARGET [nix build options...]

Targets: all, native, android, windows, linux-static, ios, darwin-libs
NIX_SYSTEM selects the build host (defaults to this machine's Nix system).
NIX_OUT_DIR selects the output links (defaults to result-nix in this checkout).
NIX_BUILD_TIMEOUT bounds each derivation in seconds (defaults to 7200).
--list prints the selected Hydra attributes without evaluating or building them.
EOF
}

list_only=false
if [ "${1:-}" = "--list" ]; then
  list_only=true
  shift
fi
case "${1:-}" in
  ''|-h|--help) usage; exit 0 ;;
esac
target=$1
shift

system=${NIX_SYSTEM:-}
if [ -z "$system" ]; then
  case "$(uname -s):$(uname -m)" in
    Linux:x86_64) system=x86_64-linux ;;
    Linux:aarch64|Linux:arm64) system=aarch64-linux ;;
    Darwin:x86_64) system=x86_64-darwin ;;
    Darwin:arm64|Darwin:aarch64) system=aarch64-darwin ;;
    *) echo 'Unsupported build host; set NIX_SYSTEM explicitly.' >&2; exit 1 ;;
  esac
fi
case "$system" in
  x86_64-linux|aarch64-linux|x86_64-darwin|aarch64-darwin) ;;
  *) printf 'Unsupported NIX_SYSTEM: %s\n' "$system" >&2; exit 1 ;;
esac

packages=
add_native() {
  packages="$packages lib:simplex-chat exe:simplex-chat"
}
add_android() {
  packages="$packages aarch64-android:lib:simplex-chat aarch64-android:lib:support armv7a-android:lib:simplex-chat armv7a-android:lib:support"
}
add_windows() {
  packages="$packages x86_64-windows:exe:simplex-chat x86_64-windows:lib:simplex-chat"
}
add_linux_static() {
  packages="$packages x86_64-linux-static:exe:simplex-chat i686-linux-static:exe:simplex-chat aarch64-linux-static:exe:simplex-chat"
}
add_darwin_libs() {
  packages="$packages $system-ios:lib:simplex-chat $system:lib:simplex-chat"
}

case "$target:$system" in
  all:x86_64-linux) add_native; add_android; add_windows; add_linux_static ;;
  all:aarch64-linux|native:*) add_native ;;
  all:*-darwin) add_native; add_darwin_libs ;;
  android:x86_64-linux) add_android ;;
  windows:x86_64-linux) add_windows ;;
  linux-static:x86_64-linux) add_linux_static ;;
  ios:*-darwin) packages="$system-ios:lib:simplex-chat" ;;
  darwin-libs:*-darwin) add_darwin_libs ;;
  *) printf 'Target %s is not available on build host %s.\n' "$target" "$system" >&2; exit 1 ;;
esac

out_dir=${NIX_OUT_DIR:-$repo_dir/result-nix}
for package in $packages; do
  attribute="hydraJobs.$system.$package"
  if "$list_only"; then
    printf '%s\n' "$attribute"
  else
    mkdir -p "$out_dir/$system"
    crash_since=
    if [ "$system" = x86_64-darwin ] && [ "${GITHUB_ACTIONS:-}" = true ]; then
      crash_since=$(date +%s) || crash_since=
    fi
    if nix build --print-build-logs --timeout "${NIX_BUILD_TIMEOUT:-7200}" \
      --out-link "$out_dir/$system/$package" \
      "$repo_dir#$attribute" "$@"; then
      :
    else
      status=$?
      if [ -n "$crash_since" ] && command -v python3 >/dev/null 2>&1; then
        # ReportCrash can provide a compiler fault's registers and stack even
        # when the failure is outside the separate ghc-boot Setup diagnostic.
        python3 "$repo_dir/scripts/nix/collect-darwin-ghc-crash.py" \
          --since "$crash_since" || printf '%s\n' 'Darwin crash-report collection failed.' >&2
        if [ "$attribute" = hydraJobs.x86_64-darwin.x86_64-darwin-ios:lib:simplex-chat ]; then
          # This separate, bounded replay records an observed compiler SIGBUS.
          # Always return the saved canonical build status below.
          python3 "$repo_dir/scripts/nix/trace-intel-ghc.py" \
            --repo "$repo_dir" --attribute "$attribute" --log "$repo_dir/nix-build.log" || \
            printf '%s\n' 'Intel GHC signal diagnostic failed.' >&2
        fi
      fi
      exit "$status"
    fi
  fi
done
