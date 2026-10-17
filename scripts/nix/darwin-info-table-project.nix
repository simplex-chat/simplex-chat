{ config, lib, pkgs, ... }:
let
  inherit (pkgs.stdenv) buildPlatform hostPlatform targetPlatform;
in
lib.mkIf (
  hostPlatform.isDarwin && hostPlatform.isx86_64
  && buildPlatform.config == hostPlatform.config
  && hostPlatform.config == targetPlatform.config
  && lib.elem config.name [ "ghc914-sh-stage1" "ghc914-sh-stage2" ]
) {
  modules = [
    ({ lib, ... }: {
      # Stage1 must emit correct info tables for the stage2 boot libraries;
      # stage2 must retain that fix for application and interpreter objects.
      packages.ghc.patches = lib.mkAfter [ ./ghc914-macho-info-tables.patch ];
    })
  ];
}
