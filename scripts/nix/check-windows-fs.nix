{ pkgs, actualArchiveFile ? null }:
assert pkgs.stdenv.hostPlatform.isLinux && pkgs.stdenv.hostPlatform.isx86_64;
assert pkgs.stdenv.buildPlatform.config == pkgs.stdenv.hostPlatform.config;
let
  cross = pkgs.pkgsCross.mingwW64;
  # These are the same raw locked GHC source and Wine selection used by HN's
  # Windows runtime route. No Haskell compiler/project is built for this probe.
  source = pkgs.haskell-nix.sources.ghc914-sh;
  wine = pkgs.pkgsBuildBuild.wine64Packages.minimal;
  payload = cross.stdenv.mkDerivation {
    name = "ghc914-windows-fs-probe";
    dontUnpack = true;
    dontConfigure = true;
    strictDeps = true;
    buildInputs = [ cross.windows.mcfgthreads ];
    buildPhase = ''
      # Keep strict options local: standard fixup hooks do not use nounset.
      (
        set -euo pipefail
        echo '3a2926cd85ed7379700fe62485998a7fe26a9ba09824c370aa5b74e3d5508e5e  ${source}/rts-fs/fs.c' | sha256sum -c -
        $CC -std=c11 -Werror=vla -pedantic -Wall -Wextra -O1 -static \
          -DFS_NAMESPACE=rts -I${source}/rts-fs \
          ${./windows-fs/Probe.c} ${source}/rts-fs/fs.c -o probe.exe
      )
    '';
    installPhase = ''
      mkdir -p "$out/bin"
      cp probe.exe "$out/bin/windows-fs-probe.exe"
    '';
  };
in pkgs.runCommand "ghc914-windows-fs-check" {
  nativeBuildInputs = [ pkgs.coreutils ];
  # Prepare this exact C payload and Wine under the normal input budget before
  # applying the short runtime-check limit. This adds no Haskell project.
  passthru.inputs = pkgs.linkFarm "ghc914-windows-fs-inputs" [
    { name = "payload"; path = payload; }
    { name = "wine"; path = wine; }
  ];
} ''
  set -euo pipefail
  mkdir -p "$out"
  ${pkgs.bash}/bin/bash ${./windows-fs/run.sh} \
    ${wine}/bin/wine ${wine}/bin/wineserver ${payload}/bin/windows-fs-probe.exe \
    ${pkgs.lib.optionalString (actualArchiveFile != null) (pkgs.lib.escapeShellArg actualArchiveFile)} \
    2>&1 | tee "$out/result.log"
''
