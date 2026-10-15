{ haskellNix }:
final: prev:
let
  lib = prev.lib;
  active = prev.stdenv.hostPlatform.isAndroid
    && prev.stdenv.hostPlatform.isAarch32
    && prev.stdenv.buildPlatform.isLinux;
  # Keep this outside the native compiler package sets. Only Android ARM32
  # Template Haskell and test wrappers need the guest personality fallback.
  emulator = import ./android-arm32-qemu.nix {
    inherit haskellNix;
    pkgs = prev.pkgsBuildBuild;
  };
  exes = prev.haskell-nix.iserv-proxy-exes.ghc914-sh;
  mkTemplateHaskell = interpreterExes:
    { inherit (prev.haskell-nix)
        emulatorSystemFeatures emulatorNativeBuilderPackages; }
    // import (haskellNix + "/overlays/linux-cross.nix") {
      inherit (prev.stdenv) hostPlatform buildPlatform;
      inherit (prev) stdenv lib gmp;
      inherit (prev.pkgsBuildBuild)
        writeShellScriptBin symlinkJoin runCommand makeWrapper;
      inherit (prev.haskell-nix) haskellLib;
      qemu = emulator;
      inherit (interpreterExes)
        iserv-proxy iserv-proxy-interpreter iserv-proxy-interpreter-prof;
    };
in lib.optionalAttrs active {
  haskell-nix = prev.haskell-nix // {
    templateHaskell = prev.haskell-nix.templateHaskell // {
      ghc914-sh = mkTemplateHaskell exes // {
        evalWith = builtins.mapAttrs (_: mkTemplateHaskell) exes.evalWith;
      };
    };
  };
}
