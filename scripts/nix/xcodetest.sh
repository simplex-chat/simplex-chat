#!/bin/bash
# Link the rolled-up Haskell core and run the SimpleX (iOS) XCTest scheme once.
# Stub archives are not a substitute: chat_migrate_init_key must be the real
# function. testLaunch runs once; the UI-configuration matrix is hours of the
# same case.
#
# SwiftPM compiles each Package.swift under /usr/bin/sandbox-exec. nixbld
# cannot apply that profile (sandbox_apply: EPERM). The one-argument
# IDEPackageSupportDisable* defaults skip it. Xcode 26 rejects the two-word
# YES form.
#
# aarch64-darwin requires the xcode-home system feature so the nixbld passwd
# home is writable for build-for-testing. The UI suite then runs as console
# user ci through /private/var/run/xcodetest-as-ci.sock. nixbld's own
# simulator is not the one on the display, and booting a second one is not
# attempted. A host without that socket fails this derivation.
set -euo pipefail

src=${1:?source tree}
out=${2:?output}
ios_lib=${3:?ios library output}
mac2ios=${4:?mac2ios}
ci_runner=${5:?ci runner}

work=$(mktemp -d)
cp -a "$src/." "$work/"
chmod -R u+w "$work"
cd "$work"

python3 "apps/ios/SimpleX Snapshots/check-previews.py"

developer=${DEVELOPER_DIR:-/Applications/Xcode.app/Contents/Developer}
export DEVELOPER_DIR=$developer
# xcodebuild lives in Xcode.app. xcrun is the /usr/bin trampoline, and a Nix
# builder's PATH does not include /usr/bin. Nix tools stay after the host paths.
export PATH="$developer/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH"
# xcodebuild and SwiftPM follow HOME. CoreSimulator follows the passwd entry.
export HOME=$work/home
export CFFIXED_USER_HOME=$HOME
mkdir -p "$HOME/Library/Developer/Xcode" "$HOME/Library/Caches" "$HOME/Library/Logs"

if [ ! -x "$developer/usr/bin/xcodebuild" ]; then
  echo "xcodebuild is not available at $developer/usr/bin/xcodebuild" >&2
  exit 1
fi
if [ ! -x "$mac2ios" ]; then
  echo "mac2ios is not available at $mac2ios" >&2
  exit 1
fi

case $(uname -m) in
  arm64|aarch64) archs=arm64 ;;
  x86_64) archs=x86_64 ;;
  *) echo "Unsupported Xcode host architecture: $(uname -m)" >&2; exit 1 ;;
esac
export ARCH=$archs

shopt -s nullglob
zips=("$ios_lib"/*.zip)
if [ "${#zips[@]}" -ne 1 ]; then
  echo "expected one ios library zip in $ios_lib, found ${#zips[@]}" >&2
  exit 1
fi
stage=$work/ios-lib
mkdir -p "$stage"
unzip -q "${zips[0]}" -d "$stage"
libs=("$stage"/*.a)
if [ "${#libs[@]}" -eq 0 ]; then
  echo "ios library zip contains no static archives" >&2
  exit 1
fi

mkdir -p apps/ios/Libraries apps/ios/Libraries/ios apps/ios/Libraries/sim
# The zip is already retargeted to device iOS. The simulator copies need
# platform 7. Do not mac2ios the device copies again.
cp -a "${libs[@]}" apps/ios/Libraries/ios/
cp -a "${libs[@]}" apps/ios/Libraries/sim/
for f in apps/ios/Libraries/sim/*.a; do
  chmod u+w "$f"
  "$mac2ios" -s "$f"
done
# File references live in Libraries/. The simulator link reads those paths.
# Libraries/sim is the -lcrypto search path. Both are the simulator archives.
cp -a apps/ios/Libraries/sim/*.a apps/ios/Libraries/
bash scripts/ios/update-pbxproj.sh

derived=$work/DerivedData
# GitHub Actions links stub archives and has no libcrypto. This build links
# the nix openssl archive, so only this invocation passes -lcrypto.
xcodebuild build-for-testing \
  -project apps/ios/SimpleX.xcodeproj \
  -scheme "SimpleX (iOS)" \
  -configuration Debug \
  -destination "generic/platform=iOS Simulator" \
  -derivedDataPath "$derived" \
  -onlyUsePackageVersionsFromResolvedFile \
  -skipPackagePluginValidation \
  -skipMacroValidation \
  -IDEPackageSupportDisableManifestSandbox=1 \
  -IDEPackageSupportDisablePluginExecutionSandbox=1 \
  ARCHS="$archs" \
  CODE_SIGNING_ALLOWED=NO \
  SIMPLEX_EXTRA_LDFLAGS=-lcrypto

mkdir -p "$out"
mkdir -p "$out/xctest"
find "$derived" -name '*.xctest' -type d -exec cp -R {} "$out/xctest/" \;
find "$out/xctest" -name '*.xctest' -type d | tee "$out/built.txt"
if [ ! -s "$out/built.txt" ]; then
  echo "xcodebuild produced no XCTest bundles" >&2
  exit 1
fi

# nixbld can see only its own shutdown simulators. The booted phone is in
# ci's Aqua session. sudo from this builder is EPERM (setuid is refused by
# the Nix seatbelt), so the store script runs through the root helper.
helper_sock=/private/var/run/xcodetest-as-ci.sock
if [ ! -S "$helper_sock" ]; then
  echo "xcodetest-as-ci is not available. The iOS suite did not run." >&2
  exit 1
fi
# The build directory is mode 0700. ci has to read the project and write
# test results under DerivedData.
chmod a+x "${NIX_BUILD_TOP:-$work}" "$(dirname "$work")" "$work"
chmod -R a+rX "$work"
chmod -R a+rwX "$derived"
# ci sets TMPDIR to $work/tmp. The build tree is not writable by the console
# user, so create that directory before the helper runs.
mkdir -p "$work/tmp"
chmod 1777 "$work/tmp"

set +e
python3 - "$ci_runner" "$work" "$derived" "$work/scripts/nix/adhoc-group-task.entitlements" <<'PY'
import json, socket, sys
runner, work, derived, ent = sys.argv[1:5]
sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
sock.settimeout(1600)
sock.connect("/private/var/run/xcodetest-as-ci.sock")
payload = json.dumps({"script": runner, "args": [work, derived, ent]}).encode() + b"\n"
sock.sendall(payload)
sock.shutdown(socket.SHUT_WR)
buf = b""
while True:
    chunk = sock.recv(1 << 20)
    if not chunk:
        break
    buf += chunk
text = buf.decode("utf-8", "replace")
line, _, rest = text.partition("\n")
sys.stdout.write(rest)
if not line.startswith("EXIT "):
    sys.stderr.write("xcodetest-as-ci returned %r\n" % line)
    sys.exit(1)
sys.exit(int(line.split()[1]))
PY
ec=$?
set -e
if [ -f "$derived/xcodetest-run.log" ]; then
  cp "$derived/xcodetest-run.log" "$out/xcodebuild.log" || true
fi
if [ "$ec" -ne 0 ]; then
  echo "XCODEBUILD_EXIT=$ec" >&2
  exit "$ec"
fi
echo "ran as ci" > "$out/simulator-tests.txt"
