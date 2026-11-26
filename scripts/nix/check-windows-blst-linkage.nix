{ pkgs }:
assert pkgs.stdenv.hostPlatform.isLinux && pkgs.stdenv.hostPlatform.isx86_64;
let
  cross = pkgs.pkgsCross.mingwW64;
  # Use the same pinned repository and preplanning patch as the project.
  sources = import ./source-repository-patches.nix {
    pkgs = pkgs.pkgsBuildBuild;
    cabalProject = builtins.readFile ../../cabal.project;
    sha256map = import ./sha256map.nix;
    patchesByUrl."https://github.com/simplex-chat/simplexmq.git" =
      [ ./simplexmq-blst-windows-extern.patch ];
  };
  patched = builtins.head (builtins.attrValues sources);
  original = patched.src;
in cross.stdenv.mkDerivation {
  name = "windows-blst-linkage-check";
  dontUnpack = true;
  dontConfigure = true;
  strictDeps = true;
  nativeBuildInputs = [ pkgs.python3 ];
  buildInputs = [ cross.windows.mcfgthreads ];
  buildPhase = ''
    (
      set -euo pipefail
      # These are the actual BLST translation units and Cabal portability flag.
      $CC -O2 -D__BLST_PORTABLE__ -c ${original}/cbits/blst/src/server.c -o server.o
      $CC -O2 -g0 -D__BLST_PORTABLE__ -c ${original}/cbits/blst/build/assembly.S -o original.o
      $CC -O2 -g0 -D__BLST_PORTABLE__ -c ${patched}/cbits/blst/build/assembly.S -o patched.o
      $CC -std=c11 -Werror=vla -pedantic -Wall -Wextra \
        -c ${./blst-coff/Probe.c} -o probe.o
      for variant in original patched; do
        $CC -static probe.o server.o "$variant.o" \
          -Wl,-Map,"$variant.map" -o "$variant.exe"
      done
      ${pkgs.python3}/bin/python3 ${./blst-coff/check.py} \
        server.o original.o patched.o original.exe patched.exe > result.json
      cat result.json
    )
  '';
  installPhase = ''
    mkdir -p "$out"
    cp result.json original.map patched.map "$out/"
  '';
}
