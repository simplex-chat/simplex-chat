{ config, lib, pkgs, ... }:
lib.mkIf (
  pkgs.stdenv.hostPlatform.isAndroid
  && pkgs.stdenv.hostPlatform.isAarch32
  && config.compiler-nix-name == "ghc914-sh"
  && config.name == "iserv-proxy"
) {
  # Patch before Cabal planning: v2 hashes must describe the same source it
  # later packages. Bionic's real static libc routines must win at startup.
  src = lib.mkForce (pkgs.pkgsBuildBuild.applyPatches {
    name = "android-arm32-iserv-proxy-source";
    src = pkgs.haskell-nix.sources.iserv-proxy;
    patches = [
      ./android-iserv-static-libc.patch
      # The API26 project declares GNU libiconv for ghc-internal. Its real
      # definitions need explicit registration in the static interpreter.
      ./android-iserv-gnu-iconv.patch
    ];
  });
}
