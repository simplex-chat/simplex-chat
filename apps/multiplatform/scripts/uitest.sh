#!/bin/sh
# Usage: scripts/uitest.sh [gradle args]   e.g. --tests '*ChatListMenuTest', -PuiForks=2, -PuiForkEvery=N
# Runs the desktop UI tests (common/src/uiTestShared + desktopTest/.../ui) in a private network namespace with local servers.
# Needs cabal/GHC for the CLI peers and test servers, and the desktop native libs (./gradlew :desktop:cmakeBuildAndCopy).
# Results: common/build/reports/tests/desktopUiTest/pass1; run directories (failure screenshots) in build/uitest/run-*.
set -eu
self=$(readlink -f "$0")
cd "$(dirname "$self")/.."
home="$PWD/.gradle/uitest-home"
kotlin_in_process="-Pkotlin.compiler.execution.strategy=in-process"

if [ "${1:-}" = "--inside" ]; then
  shift
  run="$1"; shift
  [ "$$" -eq 1 ] || { echo "uitest: --inside runs only as PID 1 of the runner's PID namespace" >&2; exit 1; }
  trap 'kill -TERM -1 2>/dev/null; exit 143' TERM
  SIMPLEX_UITEST_RUNNER=1 ./gradlew -g "$home" --offline --no-daemon "$kotlin_in_process" :common:desktopUiTest -PuiRunDir="$run" -PuiPass=1 "$@"
  exit $?
fi

avail_mb=$(awk '/^MemAvailable:/ {print int($2 / 1024)}' /proc/meminfo)
[ "$avail_mb" -ge 3000 ] || { echo "uitest: only $avail_mb MB available, need 3000; free memory first" >&2; exit 1; }
mkdir -p build/uitest
exec 9>build/uitest/runner.lock
flock -n 9 || { echo "another uitest run holds build/uitest/runner.lock" >&2; exit 1; }
run="$PWD/build/uitest/run-$(date +%Y%m%d-%H%M%S)"
mkdir -p "$run/jvmtmp" "$run/cache"
ls -1dt build/uitest/run-* 2>/dev/null | tail -n +7 | xargs -r rm -rf

repo=$(cd ../.. && pwd)
(cd "$repo" && cabal build -O0 -f uitest --builddir=dist-uitest exe:simplex-chat exe:uitest-servers)
cli=$(cd "$repo" && cabal list-bin -O0 -f uitest --builddir=dist-uitest exe:simplex-chat)
servers=$(cd "$repo" && cabal list-bin -O0 -f uitest --builddir=dist-uitest exe:uitest-servers)

peer_template() {
  t="$run/peer-template.tmp"
  mkdir -p "$t/tmp"
  (cd "$repo" && HOME="$t" TMPDIR="$t/tmp" "$cli" -d "$t/simplex_v1" --maintenance -e /v -t 0 >"$t/out.txt" 2>&1) || true
  if [ -f "$t/simplex_v1_chat.db" ] && grep -q "no active user in maintenance mode" "$t/out.txt"; then
    rm -rf "$t/tmp" "$t/out.txt" && mv "$t" "$run/peer-template"
  else
    echo "peer template not created; peers create their databases on first start" >&2
  fi
}
peer_template &
template_pid=$!

[ -d common/src/commonMain/cpp/desktop/libs/linux-x86_64 ] || { echo "uitest: native libs missing in common/src/commonMain/cpp/desktop/libs/linux-x86_64; build the desktop app once (./gradlew :desktop:cmakeBuildAndCopy)" >&2; exit 1; }
prepare_key=$({
  find . -path ./build -prune -o -path ./.gradle -prune -o -path '*/build' -prune -o \( -name '*.gradle.kts' -o -name gradle.properties -o -name '*.versions.toml' \) -type f -print
  find common/src/commonMain/cpp -path '*/libs' -prune -o -type f -print
} | LC_ALL=C sort | xargs sha256sum | sha256sum; ls -l --time-style=full-iso common/src/commonMain/cpp/desktop/libs/linux-x86_64 | grep -v libapp-lib.so)
prepare_key=$(printf '%s' "$prepare_key" | sha256sum | cut -d' ' -f1)
lib=common/src/commonMain/cpp/desktop/libs/linux-x86_64/libapp-lib.so
if [ "$prepare_key" = "$(cat build/uitest/prepare.key 2>/dev/null)" ] && [ -f "$lib" ]; then
  echo "uitest: build files and native libs unchanged, skipping the outside Gradle step"
else
  rm -f build/uitest/prepare.key
  ./gradlew -g "$home" --no-daemon "$kotlin_in_process" :desktop:cmakeBuildAndCopy :common:desktopTestClasses :common:uitestPrepare
  ./gradlew -g "$home" --stop >/dev/null 2>&1 || true
  echo "$prepare_key" > build/uitest/prepare.key
fi

cat > build/uitest/env.properties <<ENV
libAppLib=$PWD/$lib
runDir=$run
repoRoot=$repo
cli=$cli
servers=$servers
ENV
wait "$template_pid"

case " $* " in
  *" -PuiForks="*) ;;
  *)
    forks=$(( (avail_mb - 2500) / 1000 ))
    [ "$forks" -ge 1 ] || forks=1
    [ "$forks" -le 4 ] || forks=4
    echo "uitest: $avail_mb MB available, running $forks test JVMs in parallel (override with -PuiForks=N)"
    set -- "$@" -PuiForks=$forks ;;
esac

inside_pid=
trap '[ -z "$inside_pid" ] || kill -KILL "$inside_pid"' INT TERM
unshare -rn --pid --fork --kill-child --mount-proc sh -c '
  ip link set lo up && ip link set lo multicast on && ip route add 224.0.0.0/4 dev lo src 127.0.0.1 &&
  exec "$0" --inside "$@"' "$self" "$run" "$@" &
inside_pid=$!
wait "$inside_pid"
