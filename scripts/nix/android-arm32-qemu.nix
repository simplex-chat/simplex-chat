{ haskellNix, pkgs }:
(import (haskellNix + "/overlays/qemu-linux-user.nix") {
  qemu = pkgs.qemu.override {
    userOnly = true;
    hostCpuTargets = [ "arm-linux-user" ];
  };
  inherit (pkgs) lib;
  buildPlatform = pkgs.stdenv.hostPlatform;
  qemuSuffix = "arm";
}).overrideAttrs (old: {
  patches = (old.patches or []) ++ [ ./simplex-qemu-arm-personality.patch ];
})
