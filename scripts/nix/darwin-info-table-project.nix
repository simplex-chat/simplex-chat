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

      # Patch the native compiler's RTS without changing its configured
      # source or stage1 bootstrap. External Mach-O SIGNED_N relocations
      # carry a trailing-byte bias that must be normalized exactly once.
      packages.rts.patches = lib.mkIf (
        config.name == "ghc914-sh-stage2"
        && config.compiler-nix-name == "ghc914-sh-stage1"
      ) (lib.mkAfter [ ./ghc914-rts-macho-signed-addends.patch ]);
    })
  ];
}
