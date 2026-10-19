{ config, lib, pkgs, ... }:
let
  inherit (pkgs.stdenv) buildPlatform hostPlatform targetPlatform;
in
lib.mkIf (
  hostPlatform.isLinux && hostPlatform.isx86_64
  && buildPlatform.config == hostPlatform.config
  && hostPlatform.config == targetPlatform.config
  && config.name == "ghc914-sh-stage2"
  && config.compiler-nix-name == "ghc914-sh-stage1"
) {
  modules = [
    ({ lib, ... }: {
      # Local RTS package patches reach the v2 tarball without changing the
      # compiler's configured source, stage1 bootstrap, plan, or package IDs.
      # A valid ELF section may have an unaligned file offset; the loader
      # must still honor its declared memory alignment for TH object code.
      packages.rts.patches = lib.mkAfter [ ./ghc914-rts-elf-section-alignment.patch ];
    })
  ];
}
