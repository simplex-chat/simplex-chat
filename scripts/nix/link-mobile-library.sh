#!/usr/bin/env bash
# Relink a Cabal v2 store unit before haskell.nix prunes its composed store.
set -euo pipefail

if (( $# < 6 )); then
  echo "usage: $0 GHC GHC_PKG PACKAGE_DB PACKAGE KIND OUTPUT_DIR [LINK_OPTION ...]" >&2
  exit 2
fi
ghc=$1
ghc_pkg=$2
package_db=$3
package=$4
kind=$5
output_dir=$6
shift 6
mkdir -p "$output_dir/dist"
output_dir=$(cd "$output_dir" && pwd)

read -r -a units <<< "$("$ghc_pkg" --package-db="$package_db" field "$package" id --simple-output)"
if (( ${#units[@]} != 1 )); then
  echo "expected one $package unit in $package_db, got: ${units[*]}" >&2
  exit 1
fi
unit=${units[0]}
read -r -a libraries <<< "$("$ghc_pkg" --package-db="$package_db" field --ipid "$unit" hs-libraries --simple-output)"
read -r -a library_dirs <<< "$("$ghc_pkg" --package-db="$package_db" field --ipid "$unit" library-dirs --simple-output)"
if (( ${#libraries[@]} != 1 )); then
  echo "expected one Haskell archive for $unit, got: ${libraries[*]}" >&2
  exit 1
fi
archive=""
for directory in "${library_dirs[@]}"; do
  if [[ -f "$directory/lib${libraries[0]}.a" ]]; then
    archive="$directory/lib${libraries[0]}.a"
    break
  fi
done
if [[ -z "$archive" ]]; then
  echo "cannot find the vanilla Haskell archive for $unit" >&2
  exit 1
fi

# A source input makes GHC perform the final link even with only -package-id.
# It must not implicitly import the build machine's Prelude on cross targets.
cat > "$output_dir/MobileBundle.hs" <<'EOF'
{-# LANGUAGE NoImplicitPrelude #-}
module MobileBundle () where
EOF
cd "$output_dir"
common=(-v0 -no-hs-main -hide-all-packages -package-db "$package_db")
case "$kind" in
  ios)
    # Xcode's update-pbxproj.sh consumes both the plain and rolled archives.
    plain=$(basename "$archive")
    cp "$archive" "dist/$plain"
    version=$("$ghc" --numeric-version)
    "$ghc" "${common[@]}" -staticlib -threaded -package-id "$unit" \
      "$@" MobileBundle.hs -o "dist/${plain%.a}-ghc$version.a"
    ;;
  android | windows)
    if [[ "$kind" == android ]]; then
      output=libsimplex.so
    else
      output=libsimplex.dll
    fi
    "$ghc" "${common[@]}" -shared -threaded -flink-rts -package-id "$unit" \
      "$@" MobileBundle.hs -o "$output"
    ;;
  android-support)
    # libsimplex supplies the RTS/base symbols; do not bundle a second RTS.
    "$ghc" "${common[@]}" -shared -fno-link-rts -no-auto-link-packages \
      "$@" "$archive" MobileBundle.hs -o libsupport.so
    ;;
  *)
    echo "unknown mobile library kind: $kind" >&2
    exit 2
    ;;
esac
