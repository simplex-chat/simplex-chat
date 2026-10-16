{ haskellNix }:
final: prev:
let
  lib = prev.lib;
  androidArm = prev.stdenv.hostPlatform.isAndroid
    && (prev.stdenv.hostPlatform.isAarch32 || prev.stdenv.hostPlatform.isAarch64);
  androidArm32 = prev.stdenv.hostPlatform.isAndroid
    && prev.stdenv.hostPlatform.isAarch32;
  supportedEmulator = androidArm32
    && prev.stdenv.buildPlatform.isLinux
    && lib.versionAtLeast prev.pkgsBuildBuild.qemu.version "9.1"
    && lib.versionOlder prev.pkgsBuildBuild.qemu.version "12";
  # Keep this outside the native compiler package sets. Only Android ARM32
  # Template Haskell and test wrappers need the guest personality fallback.
  emulator = import ./android-arm32-qemu.nix {
    inherit haskellNix;
    pkgs = prev.pkgsBuildBuild;
  };
  exes = prev.haskell-nix.iserv-proxy-exes.ghc914-sh;
  mkTemplateHaskell = interpreterExes:
    { inherit (final.haskell-nix)
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
in lib.optionalAttrs androidArm {
  haskell-nix = prev.haskell-nix // {
    # Static-library symbol corrections do not depend on the emulator version.
    cabalProject' = projectModule: prev.haskell-nix.cabalProject' (
      [ ./android-iserv-project.nix ]
      ++ (if builtins.isList projectModule then projectModule else [ projectModule ])
    );
  } // lib.optionalAttrs supportedEmulator {
    # Use the patched ARM32 emulator on local Linux builders too. This is the
    # supported haskell.nix opt-out from its native-builder scheduling marker;
    # other target package sets keep their existing restrictions.
    emulatorSystemFeatures = [];
    templateHaskell = prev.haskell-nix.templateHaskell // {
      ghc914-sh = mkTemplateHaskell exes // {
        evalWith = builtins.mapAttrs (_: mkTemplateHaskell) exes.evalWith;
      };
    };
  };
}
