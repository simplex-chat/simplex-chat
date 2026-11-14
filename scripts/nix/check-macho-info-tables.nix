{ pkgs, compiler }:
assert pkgs.stdenv.hostPlatform.isDarwin && pkgs.stdenv.hostPlatform.isx86_64;
assert pkgs.stdenv.buildPlatform.config == pkgs.stdenv.hostPlatform.config;
pkgs.runCommandCC "ghc914-macho-info-table-check" {
  nativeBuildInputs = [ pkgs.python3 ];
} ''
  set -euo pipefail
  mkdir -p "$out"
  ${compiler}/bin/ghc -O -c ${./macho-info-tables/InfoTables.hs} \
    -odir "$out" -hidir "$out" -o "$out/InfoTables.o"
  # Undefined Haskell runtime references are permitted in this layout fixture.
  # This checks the real compiler's object and real LLD's table placement;
  # application/runtime qualification remains in the existing build goals.
  ${pkgs.llvmPackages_21.lld}/bin/ld64.lld -dylib -arch x86_64 \
    -platform_version macos ${pkgs.stdenv.hostPlatform.darwinMinVersion} ${pkgs.stdenv.hostPlatform.darwinMinVersion} \
    -undefined dynamic_lookup -dead_strip -no_fixup_chains \
    "$out/InfoTables.o" -o "$out/InfoTables.dylib"
  python3 ${./macho-info-tables/check.py} \
    "$out/InfoTables.o" "$out/InfoTables.dylib" | tee "$out/result.json"
''
