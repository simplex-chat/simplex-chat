{ config, lib, pkgs, ... }:
lib.mkIf (
  pkgs.stdenv.hostPlatform.isMusl
  && pkgs.stdenv.hostPlatform.isx86_32
  && pkgs.stdenv.buildPlatform.isLinux
  && config.compiler-nix-name == "ghc914-sh"
  && config.builderVersion == 2
) {
  # Cross projects build their target RTS alongside the boot libraries.
  # Cover both application and auxiliary interpreter projects, including
  # profiling, while keeping the native compiler and its bootstrap intact.
  modules = [
    ({ lib, ... }: {
      packages.rts.patches = lib.mkAfter [ ./ghc914-rts-elf-comdat-functions.patch ];
    })
  ];
}
