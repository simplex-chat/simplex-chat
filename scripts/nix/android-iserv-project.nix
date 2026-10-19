{ config, lib, pkgs, ... }:
let
  arm32 = pkgs.stdenv.hostPlatform.isAarch32;
  arm64 = pkgs.stdenv.hostPlatform.isAarch64;
in
lib.mkIf (
  pkgs.stdenv.hostPlatform.isAndroid
  && (arm32 || arm64)
  && config.compiler-nix-name == "ghc914-sh"
  && config.name == "iserv-proxy"
) {
  # Patch before Cabal planning: v2 hashes must describe the same source it
  # later packages. Keep symbol registration consistent with the target ABI
  # and Android API level.
  src = lib.mkForce (pkgs.pkgsBuildBuild.applyPatches {
    name = "android-${if arm32 then "arm32" else "arm64"}-iserv-proxy-source";
    src = pkgs.haskell-nix.sources.iserv-proxy;
    patches = if arm32 then [
      ./android-iserv-static-libc.patch
      # The API26 project declares GNU libiconv for ghc-internal. Its real
      # definitions need explicit registration in the static interpreter.
      ./android-iserv-gnu-iconv.patch
      ./android-iserv-arm32-entropy.patch
    ] else [
      ./android-iserv-arm64-api.patch
    ];
  });
}
