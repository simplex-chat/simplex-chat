#!/bin/sh
# Copy the selected Wine iserv scripts, changing only diagnostic channels.
set -eu

real_ghc=$1
destination=$2
needle='WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag'
# Wine parses these left to right. Explicit channel flags follow the global
# class exclusions, so exception and module traces remain enabled.
replacement="$needle,+seh,+loaddll"

paths=$(grep -oE '/nix/store/[0-9abcdfghijklmnpqrsvwxyz]{32}-iserv-wrapper/bin/iserv-wrapper' "$real_ghc" || true)
count=$(printf '%s\n' "$paths" | grep -c . || true)
if [ "$count" -ne 1 ]; then
  echo 'ERROR: expected one original Wine iserv path in the selected GHC wrapper' >&2
  exit 1
fi
original=$paths
mkdir -p "$destination"
for suffix in '' '-prof'; do
  source="$original$suffix"
  target="$destination/iserv-wrapper$suffix"
  if [ ! -x "$source" ]; then
    echo "ERROR: missing selected Wine iserv script: $source" >&2
    exit 1
  fi
  count=$(grep -oF "$needle" "$source" | wc -l)
  if [ "$count" -ne 1 ]; then
    echo "ERROR: expected one pinned WINEDEBUG assignment in $source" >&2
    exit 1
  fi
  sed "s/$needle/$replacement/g" "$source" > "$target"
  # Refuse any transformation beyond the one debug value.
  if ! sed "s/$replacement/$needle/g" "$target" | cmp -s "$source" -; then
    echo "ERROR: unexpected change to the selected Wine iserv script: $source" >&2
    exit 1
  fi
  chmod 700 "$target"
done
printf 'Windows TH diagnostic: %s; WINEDEBUG=%s\n' "$original" "${replacement#WINEDEBUG=}" >&2
printf '%s\n' "$destination/iserv-wrapper"
