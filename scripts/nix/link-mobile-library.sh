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

# Native iOS builds reuse the compiler's boot libraries, with ABI-identical
# replacements for mobile runtime settings and the ARM instruction baseline.
# Work in a private database: a composed store can contain symlinks into Nix.
if [[ -n "${MOBILE_LIBRARY_PACKAGE_DBS:-}" ]]; then
  original_db=$package_db
  package_db="$output_dir/package.conf.d"
  mkdir -p "$package_db"
  copy_registration() {
    local conf=$1 root=$2 contents
    contents=$(<"$conf")
    # pkgroot is relative to the database, not the registration file's source.
    printf '%s\n' "${contents//\$\{pkgroot\}/$root}" > "$package_db/$(basename "$conf")"
  }
  original_root=$(cd "$original_db/.." && pwd)
  for conf in "$original_db"/*.conf; do
    [[ -f "$conf" ]] || continue
    copy_registration "$conf" "$original_root"
  done
  IFS=: read -r -a replacement_dbs <<< "$MOBILE_LIBRARY_PACKAGE_DBS"
  for replacement_db in "${replacement_dbs[@]}"; do
    replacement_root=$(cd "$replacement_db/.." && pwd)
    found=false
    for conf in "$replacement_db"/*.conf; do
      [[ -f "$conf" ]] || continue
      found=true
      replacement_unit=$(basename "${conf%.conf}")
      if ! old_abi=$("$ghc_pkg" --package-db="$original_db" field --ipid "$replacement_unit" abi --simple-output 2>/dev/null); then
        # Native boot packages can live only in the compiler's global DB.
        old_abi=$("$ghc_pkg" --global field --ipid "$replacement_unit" abi --simple-output)
      fi
      if ! new_abi=$("$ghc_pkg" --package-db="$replacement_db" field --ipid "$replacement_unit" abi --simple-output 2>/dev/null); then
        echo "cannot read mobile replacement ABI for $replacement_unit in $replacement_db" >&2
        exit 1
      fi
      if [[ -z "$old_abi" || "$old_abi" != "$new_abi" ]]; then
        echo "mobile replacement changes the ABI of $replacement_unit: $old_abi -> $new_abi" >&2
        exit 1
      fi
      copy_registration "$conf" "$replacement_root"
    done
    if ! "$found"; then
      echo "no mobile replacement registrations in $replacement_db" >&2
      exit 1
    fi
  done
  "$ghc_pkg" --package-db="$package_db" recache
fi

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
      # GHC roots this ghc-internal initializer for executables, but not DLLs.
      # Keep its real archive member when linking the static Haskell RTS.
      set -- -optl-Wl,-u,init_ghc_hs_iface "$@"
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
