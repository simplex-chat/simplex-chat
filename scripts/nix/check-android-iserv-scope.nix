# Run without importing nixpkgs: nix eval --json --file scripts/nix/check-android-iserv-scope.nix
let
  overlay = import ./android-emulator-overlay.nix { haskellNix = /unused-haskell-nix; };
  lib = {
    optionalAttrs = condition: attrs: if condition then attrs else {};
    versionAtLeast = actual: minimum: builtins.compareVersions actual minimum >= 0;
    versionOlder = actual: maximum: builtins.compareVersions actual maximum < 0;
  };
  apply = { android ? true, arm32 ? false, arm64 ? false, linux ? true, qemu ? "11.0.1" }:
    let
      prev = {
        inherit lib;
        stdenv = {
          hostPlatform = { isAndroid = android; isAarch32 = arm32; isAarch64 = arm64; };
          buildPlatform.isLinux = linux;
        };
        pkgsBuildBuild.qemu.version = qemu;
        haskell-nix = {
          cabalProject' = modules: modules;
          templateHaskell = {};
        };
      };
    in overlay prev prev;
  arm64 = apply { arm64 = true; };
  arm32 = apply { arm32 = true; };
  unsupported = apply { arm32 = true; qemu = "12.0"; };
  darwin = apply { arm32 = true; linux = false; };
  projectModules = result: result.haskell-nix.cabalProject' [ "caller-module" ];
  patchedProject = result: projectModules result == [ ./android-iserv-project.nix "caller-module" ];
  emulatorUnchanged = result:
    builtins.attrNames result.haskell-nix == [ "cabalProject'" "templateHaskell" ]
    && result.haskell-nix.templateHaskell == {};
in
assert patchedProject arm64;
assert emulatorUnchanged arm64;
assert patchedProject arm32;
assert arm32.haskell-nix.emulatorSystemFeatures == [];
assert builtins.hasAttr "ghc914-sh" arm32.haskell-nix.templateHaskell;
assert patchedProject unsupported && emulatorUnchanged unsupported;
assert patchedProject darwin && emulatorUnchanged darwin;
assert apply { android = false; arm64 = true; } == {};
assert apply {} == {};
{
  androidArm64SourcePatched = true;
  androidArm64EmulatorUnchanged = true;
  androidArm32SourceAndSupportedEmulatorPatched = true;
  unsupportedQemuAndDarwinOnlyPatchSource = true;
  otherTargetsUnchanged = true;
}
