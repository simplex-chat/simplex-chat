{ pkgs, compiler }:
assert pkgs.stdenv.hostPlatform.isLinux && pkgs.stdenv.hostPlatform.isx86_64;
assert pkgs.stdenv.buildPlatform.config == pkgs.stdenv.hostPlatform.config;
pkgs.runCommandCC "ghc914-elf-section-alignment-check" {
  nativeBuildInputs = [ pkgs.python3 ];
} ''
  set -euo pipefail
  mkdir -p "$out"
  $CC -ffreestanding -std=c11 -Werror=vla -pedantic -Wall -Wextra -O2 \
    -c ${./elf-alignment/probe.c} -o "$out/original.o"
  python3 ${./elf-alignment/repack.py} \
    "$out/original.o" "$out/packed.o" "$out/elf-layout.json"
  # A valid ET_REL section's file offset need not match its memory alignment.
  # Both paths must resolve the same bytes at a 64-byte-aligned address.
  for variant in original packed; do
    ${compiler}/bin/ghc -ignore-dot-ghci -package ghci ${./elf-alignment/Probe.hs} \
      -e "probeObject \"$out/$variant.o\"" | tee "$out/$variant.txt"
    grep -Fx '(0,1,0)' "$out/$variant.txt"
  done
  printf '%s\n' 'PASS: original and packed ELF sections honor 64-byte alignment' \
    | tee "$out/result.txt"
''
