{ config, lib, pkgs, ... }:
lib.mkIf (
  pkgs.stdenv.hostPlatform.isWindows
  && pkgs.stdenv.hostPlatform.isx86_64
  && pkgs.stdenv.buildPlatform.isLinux
  && config.compiler-nix-name == "ghc914-sh"
  && config.builderVersion == 2
) {
  # Cross projects build target boot libraries here, including both iserv
  # flavours. Keep their Haskell ways and the native compiler unchanged.
  modules = [
    ({ lib, ... }: {
      packages.rts.patches = lib.mkAfter [ ./ghc914-rts-windows-dll-search-path.patch ];
    })
  ];
}
