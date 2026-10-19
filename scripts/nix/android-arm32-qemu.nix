{ haskellNix, pkgs, hostPlatform }:
import (haskellNix + "/overlays/qemu-linux-user.nix") {
  qemu = pkgs.qemu.override {
    userOnly = true;
    hostCpuTargets = [ "arm-linux-user" ];
  };
  inherit (pkgs) lib;
  buildPlatform = pkgs.stdenv.hostPlatform;
  inherit hostPlatform;
  qemuSuffix = "arm";
}
