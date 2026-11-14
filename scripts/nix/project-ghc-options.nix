{ lib, hostPlatform, packageOptions ? {} }:
let
  common = [ "-j1" ];
  explicit = lib.recursiveUpdate
    (lib.optionalAttrs hostPlatform.isAndroid {
      simplex-chat = [ "-pie" ];
    }) packageOptions;
in {
  inherit common;
  # HN inherits common options as mkDefault. A package-specific list replaces
  # that default, so retain it explicitly before any deliberate local override.
  packages = lib.mapAttrs (_: options: common ++ options) explicit;
}
