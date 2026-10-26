#!/bin/bash

set -euo pipefail

derived=$1
out=$2
mkdir -p "$out"

opts=(
  -project apps/ios/SimpleX.xcodeproj
  -scheme "SimpleX (iOS)"
  -configuration Debug
  -derivedDataPath "$derived"
  -onlyUsePackageVersionsFromResolvedFile
  -skipPackagePluginValidation
  -skipMacroValidation
)
settings=(
  ARCHS=arm64
  CODE_SIGN_STYLE=Manual
  CODE_SIGN_IDENTITY=-
  DEVELOPMENT_TEAM=
  PROVISIONING_PROFILE_SPECIFIER=
)

xcodebuild build-for-testing -quiet "${opts[@]}" -destination "id=$SIM_UDID" -only-testing:"SimpleX Snapshots" "${settings[@]}"

for size in large extra-extra-extra-large accessibility-medium; do
  xcrun simctl ui "$SIM_UDID" content_size "$size"
  TEST_RUNNER_SNAPSHOT_DIR="$out" TEST_RUNNER_SNAPSHOT_SIZE="$size" xcodebuild test-without-building "${opts[@]}" -destination "id=$SIM_UDID" -only-testing:"SimpleX Snapshots" "${settings[@]}"
done
xcrun simctl ui "$SIM_UDID" content_size large
xcrun simctl shutdown "$SIM_UDID" || true

runtime=com.apple.CoreSimulator.SimRuntime.iOS-$(xcrun --sdk iphonesimulator --show-sdk-version | tr . -)
margins="$out/margins"
mkdir -p "$margins"
echo "$SIM_UDID" > "$margins/ci-iphone-udid.txt"
xcrun simctl list devicetypes -j > "$margins/devicetypes.json"
xcrun simctl list runtimes -j > "$margins/runtimes.json"

python3 - "$margins/devicetypes.json" > "$margins/iphone-types.tsv" <<'EOF'
import json, sys
for t in json.load(open(sys.argv[1]))["devicetypes"]:
    if t.get("productFamily") == "iPhone":
        print(f'{t["identifier"]}\t{t["name"]}\t{t["bundlePath"]}')
EOF

: > "$margins/created.tsv"
while IFS=$'\t' read -r -u 3 type name bundle; do
  if udid=$(xcrun simctl create "$name" "$type" "$runtime" 2>>"$margins/create-errors.txt"); then
    printf '%s\t%s\t%s\t%s\n' "$udid" "$type" "$name" "$bundle" >> "$margins/created.tsv"
  else
    printf '%s\t%s\n' "$type" "$name" >> "$margins/unsupported.tsv"
  fi
done 3< "$margins/iphone-types.tsv"

python3 - "$margins/created.tsv" > "$margins/plan.tsv" <<'EOF'
import plistlib, sys
rows = [l.rstrip("\n").split("\t") for l in open(sys.argv[1]) if l.strip()]
groups, order = {}, []
for udid, ident, name, bundle in rows:
    try:
        p = plistlib.load(open(bundle + "/Contents/Resources/profile.plist", "rb"))
        s = float(p["mainScreenScale"])
        key = (float(p["mainScreenWidth"]) / s, float(p["mainScreenHeight"]) / s, s)
    except Exception:
        key = ("?", ident, 0.0)
    if key not in groups:
        groups[key] = []
        order.append(key)
    groups[key].append((udid, ident, name))
reps = []
for key in order:
    members = groups[key]
    udid, ident, name = members[-1]
    reps.append((key, udid, ident, name))
    shared = ", ".join(m[2] for m in members)
    for m in members[:-1]:
        print(f"dup\t{m[0]}\t{m[1]}\t{m[2]}\t{key[0]}\t{key[1]}\t{key[2]}\t{name}")
sized = [r for r in reps if isinstance(r[0][0], float)]
small = min(sized, key=lambda r: (r[0][0], -r[0][2], -reps.index(r))) if sized else None
for r in ([small] if small else []) + [r for r in reps if r is not small]:
    key, udid, ident, name = r
    shared = ", ".join(m[2] for m in groups[key])
    print(f"{'small' if r is small else 'rep'}\t{udid}\t{ident}\t{name}\t{key[0]}\t{key[1]}\t{key[2]}\t{shared}")
EOF
cat "$margins/plan.tsv"

budget=$((SECONDS + 2100))
while IFS=$'\t' read -r -u 3 kind udid type name w h scale shared; do
  if [ "$kind" = dup ]; then
    xcrun simctl delete "$udid" || true
    continue
  fi
  if [ "$kind" = rep ] && [ "$SECONDS" -gt "$budget" ]; then
    echo "time budget spent, margins not measured on $name"
    printf '%s\t%s\n' "$type" "$name" >> "$margins/skipped.tsv"
    xcrun simctl delete "$udid" || true
    continue
  fi
  echo "margins on $name ($w x $h @$scale)"
  if ! xcrun simctl boot "$udid"; then
    echo "boot failed on $name"
    continue
  fi
  xcrun simctl bootstatus "$udid" -b || true
  TEST_RUNNER_SNAPSHOT_DIR="$margins" xcodebuild test-without-building "${opts[@]}" -destination "id=$udid" -only-testing:"SimpleX Snapshots/SnapshotTests/testListMargins" "${settings[@]}" > "$margins/xcodebuild-$udid.log" 2>&1 || echo "margins test failed on $name"
  if [ "$kind" = small ]; then
    for size in large extra-extra-extra-large accessibility-medium; do
      xcrun simctl ui "$udid" content_size "$size"
      TEST_RUNNER_SNAPSHOT_DIR="$out/small" TEST_RUNNER_SNAPSHOT_SIZE="$size" TEST_RUNNER_SNAPSHOT_SPACING_ONLY=1 xcodebuild test-without-building "${opts[@]}" -destination "id=$udid" -only-testing:"SimpleX Snapshots/SnapshotTests/testChatScenes" "${settings[@]}" > "$out/small-$size.log" 2>&1 || echo "small device renders failed at $size"
    done
  fi
  xcrun simctl shutdown "$udid" || true
  xcrun simctl delete "$udid" || true
done 3< "$margins/plan.tsv"

ls -l "$out"
