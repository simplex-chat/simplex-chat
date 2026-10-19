{ system ? builtins.currentSystem }:
let
  flake = builtins.getFlake (toString ../..);
  haskellNix = flake.inputs.haskellNix;
  nativePackages = import ./native-packages.nix { inherit haskellNix; };
  pkgs = nativePackages system;
  # The pinned NDK runs on x86_64 Linux. Keep the tiny payload compilation on
  # that existing target, even when QEMU itself is tested on native ARM64.
  buildPkgs = nativePackages "x86_64-linux";
  target = import buildPkgs.path {
    localSystem = { system = "x86_64-linux"; };
    inherit (buildPkgs) overlays;
    config = buildPkgs.config // { allowUnfree = true; };
    crossSystem = buildPkgs.lib.systems.examples.armv7a-android-prebuilt
      // { androidSdkVersion = "26"; };
  };
  emulator = import ./android-arm32-qemu.nix {
    inherit haskellNix pkgs;
    hostPlatform = target.stdenv.hostPlatform;
  };
  payload = buildPkgs.runCommand "android-arm32-personality-payload" {} ''
    mkdir -p "$out"
    # API 26's crtbegin predates Bionic's aligned TLS input. Add the same
    # layout constraint for this static C fixture without raising its API.
    ${target.stdenv.cc}/bin/${target.stdenv.cc.targetPrefix}cc \
      -march=armv7-a -c ${./android-personality-tls.S} -o tls-align.o
    # The NDK wrapper recognizes bare -no-pie; Clang itself reports it unused
    # for this static link. Keep that wrapper flag while suppressing its warning.
    ${target.stdenv.cc}/bin/${target.stdenv.cc.targetPrefix}cc \
      -std=c11 -Werror=vla -pedantic -Wall -Wextra \
      -march=armv7-a -static -no-pie -Wno-unused-command-line-argument \
      -Wl,--gc-sections -pthread \
      ${./android-personality-check.c} tls-align.o -o "$out/probe"
  '';
in assert pkgs.lib.assertMsg
  (builtins.elem system [ "aarch64-linux" "x86_64-linux" ])
  "The Android personality check needs a Linux builder; pass --argstr system aarch64-linux or x86_64-linux.";
pkgs.runCommand "android-arm32-personality-check" {} ''
  set -euo pipefail
  mkdir -p "$out"
  cp ${payload}/probe "$out/probe"
  uname -m | tee "$out/builder-architecture"
  ${emulator}/bin/qemu-arm -version | tee "$out/qemu-version"
  timeout -k 5 60 ${emulator}/bin/qemu-arm "$out/probe" 2>&1 | tee "$out/probe.log"
''
