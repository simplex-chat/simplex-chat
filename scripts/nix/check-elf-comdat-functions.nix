{ pkgs, compiler, patch }:
assert pkgs.stdenv.hostPlatform.isLinux && pkgs.stdenv.hostPlatform.isx86_64;
let
  source = pkgs.applyPatches {
    name = "ghc914-rts-single-function-comdat";
    src = compiler.configured-src + "/rts";
    patches = [ patch ];
  };
in pkgs.runCommandCC "ghc914-elf-comdat-functions-check" {} ''
  set -euo pipefail
  mkdir -p "$out"
  includes=()
  for package in rts rts-headers rts-fs libffi-clib; do
    ${compiler}/bin/ghc-pkg field "$package" include-dirs --simple-output \
      > "$out/$package-include-dirs.txt"
    for dir in $(cat "$out/$package-include-dirs.txt"); do
      includes+=("-I$dir")
    done
  done
  # Use one consistent set of private headers; installed copies also carry
  # pragma-once headers which must not be included as distinct source files.
  # The upstream RTS uses GNU C extensions. The fixture itself is assembly.
  for variant in baseline patched; do
    if [ "$variant" = baseline ]; then tree=${compiler.configured-src}/rts; else tree=${source}; fi
    mkdir -p "$out/$variant-objects"
    for file in linker/Elf.c Linker.c; do
      object=$(basename "$file" .c)
      $CC -std=gnu11 -Werror=vla -pedantic -Wall -Wextra -O2 -fPIC \
        -mno-avx -mno-avx2 -DCOMPILING_RTS -DINTERNAL_LIBFFI -DFS_NAMESPACE=rts \
        -iquote "$tree" -iquote "$tree/linker" \
        "''${includes[@]}" -I"$tree" \
        -c "$tree/$file" -o "$out/$variant-objects/$object.o" \
        2>&1 | tee "$out/$variant-$object-compile.log"
    done
  done
  cp ${./elf-comdat/Probe.hs} Probe.hs
  for variant in baseline patched; do
    ${compiler}/bin/ghc -ignore-dot-ghci -package ghci -j1 -O0 Probe.hs \
      "$out/$variant-objects/Elf.o" "$out/$variant-objects/Linker.o" \
      -o "$out/$variant" -optl-Wl,-Map="$out/$variant.map"
    if grep -E '\.a\((Elf|Linker)\.o\)' "$out/$variant.map"; then
      echo 'Original RTS linker object was unexpectedly extracted' >&2
      exit 1
    fi
  done
  for variant in a b; do
    if [ "$variant" = a ]; then value=17; addend=100; else value=29; addend=200; fi
    common=(-DPROBE_VALUE=$value -DPROBE_READER=reader_$variant -DPROBE_ADDEND=$addend -DPROBE_EXTRA=extra_$variant)
    for kind in comdat zero-size zero-local-label zero-extra-function zero-extra-object zero-extra-global zero-extra-weak partial strong plain-group different-signature weak; do
      flags=(-DGROUP_SIGNATURE=comdat_function)
      case "$kind" in
        zero-size) flags+=(-DZERO_SIZE_FUNCTION) ;;
        zero-local-label) flags+=(-DZERO_SIZE_FUNCTION -DEXTRA_LOCAL_LABEL) ;;
        zero-extra-function) flags+=(-DZERO_SIZE_FUNCTION -DEXTRA_FUNCTION) ;;
        zero-extra-object) flags+=(-DZERO_SIZE_FUNCTION -DEXTRA_OBJECT) ;;
        zero-extra-global) flags+=(-DZERO_SIZE_FUNCTION -DEXTRA_GLOBAL) ;;
        zero-extra-weak) flags+=(-DZERO_SIZE_FUNCTION -DEXTRA_WEAK) ;;
        partial) flags+=(-DPARTIAL_FUNCTION) ;;
        strong) flags+=(-DNO_GROUP) ;;
        plain-group) flags+=(-DPLAIN_GROUP) ;;
        different-signature) flags=(-DGROUP_SIGNATURE=other_signature -DOTHER_SIGNATURE) ;;
        weak) flags+=(-DNO_GROUP -DWEAK_FUNCTION) ;;
      esac
      $CC -c "''${common[@]}" "''${flags[@]}" ${./elf-comdat/fixture.S} \
        -o "$out/$kind-$variant.o"
    done
    $AR rc "$out/comdat-$variant.a" "$out/comdat-$variant.o"
  done
  $CC -c -DNO_GROUP -DGROUP_SIGNATURE=comdat_function \
    -DPROBE_VALUE=41 -DPROBE_READER=reader_strong -DPROBE_ADDEND=300 \
    ${./elf-comdat/fixture.S} -o "$out/strong-third.o"
  expect_failure() {
    name=$1; shift
    status=0
    timeout 15s "$@" > "$out/$name.log" 2>&1 || status=$?
    if [ "$status" -ne 1 ]; then
      echo "Expected ordinary failure for $name, got status $status" >&2
      exit 1
    fi
    grep -F 'duplicate definition for symbol' "$out/$name.log"
    grep -F 'comdat_function' "$out/$name.log"
  }
  expect_failure baseline-comdat "$out/baseline" "$out/comdat-a.o" "$out/comdat-b.o"
  expect_failure baseline-zero-size "$out/baseline" "$out/zero-size-a.o" "$out/zero-size-b.o"
  timeout 15s "$out/patched" "$out/comdat-a.o" "$out/comdat-b.o" | tee "$out/comdat.txt"
  grep -Fx '[17,117,217]' "$out/comdat.txt"
  timeout 15s "$out/patched" "$out/comdat-b.o" "$out/comdat-a.o" | tee "$out/comdat-reversed.txt"
  grep -Fx '[29,129,229]' "$out/comdat-reversed.txt"
  for kind in zero-size zero-local-label zero-mixed; do
    if [ "$kind" = zero-mixed ]; then first=zero-size; second=comdat; else first=$kind; second=$kind; fi
    timeout 15s "$out/patched" "$out/$first-a.o" "$out/$second-b.o" | tee "$out/$kind.txt"
    grep -Fx '[17,117,217]' "$out/$kind.txt"
    timeout 15s "$out/patched" "$out/$second-b.o" "$out/$first-a.o" | tee "$out/$kind-reversed.txt"
    grep -Fx '[29,129,229]' "$out/$kind-reversed.txt"
  done
  for executable in baseline patched; do
    timeout 15s "$out/$executable" "$out/weak-a.o" "$out/weak-b.o" | tee "$out/$executable-weak.txt"
    grep -Fx '[17,117,217]' "$out/$executable-weak.txt"
  done
  for kind in strong plain-group partial zero-extra-function zero-extra-object zero-extra-global zero-extra-weak; do
    expect_failure "$kind" "$out/patched" "$out/$kind-a.o" "$out/$kind-b.o"
  done
  expect_failure different-signature "$out/patched" "$out/different-signature-a.o" "$out/comdat-b.o"
  expect_failure mixed-comdat-first "$out/patched" "$out/comdat-a.o" "$out/strong-b.o"
  expect_failure mixed-strong-first "$out/patched" "$out/strong-a.o" "$out/comdat-b.o"
  expect_failure archive-owner-change "$out/patched" --archive-conflict \
    "$out/comdat-a.a" "$out/comdat-b.a" "$out/strong-third.o"
  printf '%s\n' 'PASS: scoped COMDAT coalescing preserves relocations and strong conflicts' \
    | tee "$out/result.txt"
''
