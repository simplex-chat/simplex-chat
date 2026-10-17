# Evaluate the real pinned HN plan -> package -> component option inheritance.
# No project plan, compiler, or source producer is built.
{ lib, haskellNix, scenario ? "checks" }:
let
  select = import ./project-ghc-options.nix;
  selected = android: options: select {
    inherit lib;
    hostPlatform.isAndroid = android;
    packageOptions = options;
  };
  evaluate = common: packages: (lib.evalModules {
    modules = [
      (haskellNix + "/modules/plan.nix")
      (haskellNix + "/modules/error-handler.nix")
      {
        options = {
          hackage.configs = lib.mkOption { type = lib.types.unspecified; default = {}; };
          hsPkgs = lib.mkOption { type = lib.types.unspecified; default = {}; };
          cabal = {
            system = lib.mkOption { type = lib.types.unspecified; default = {}; };
            compiler = lib.mkOption { type = lib.types.unspecified; default = {}; };
          };
        };
        config = {
          _module.args = {
            pkgs = { inherit lib; };
            pkgconfPkgs = {};
            haskellLib.types = import (haskellNix + "/lib/types.nix") { inherit lib; };
          };
          use-package-keys = false;
          ghcOptions = lib.mkDefault common;
          packages = lib.mapAttrs (_: ghcOptions: { inherit ghcOptions; }) packages;
          plan.pkg-def = _: {
            compiler = { nix-name = "ghc914-sh"; version = "9.14.0"; packages = {}; };
            packages = lib.genAttrs [ "simplex-chat" "another-package" ] (_: {
              revision = _: { components.library = {}; };
            });
          };
        };
      }
    ];
  }).config.packages;
  optionsOf = packages: packages.simplex-chat.components.library.ghcOptions;
  merged = value: evaluate value.common value.packages;
  native = selected false {};
  mobile = selected false { simplex-chat = [ "-DIOS" ]; };
  android = selected true {};
  flags = selected false { simplex-chat = [ "-DIOS" "-optP-DswiftJSON" ]; };
  override = selected false { simplex-chat = [ "-j2" ]; };
  baseline = optionsOf (evaluate [ "-j1" ] { simplex-chat = [ "-DIOS" ]; });
  platformOptions = system: android: options: merged (select {
    inherit lib;
    hostPlatform = { inherit system; isAndroid = android; };
    packageOptions = options;
  });
  checks = {
    originalPackageListReplacesCommonDefault = baseline == [ "-DIOS" ];
    nativeDefaultUnchanged = native.packages == {} && optionsOf (merged native) == [ "-j1" ];
    darwinMobileRetainsCommonDefault = optionsOf (merged mobile) == [ "-j1" "-DIOS" ];
    androidPieRetainsCommonDefault = optionsOf (merged android) == [ "-j1" "-pie" ];
    explicitFlagsPreserved = optionsOf (merged flags) == [ "-j1" "-DIOS" "-optP-DswiftJSON" ];
    deliberateLaterParallelOverridePreserved = optionsOf (merged override) == [ "-j1" "-j2" ];
    otherPackageDefaultUnchanged = (merged mobile).another-package.components.library.ghcOptions == [ "-j1" ];
    bothDarwinArchitecturesKeepIosFlag = builtins.all (system:
      optionsOf (platformOptions system false { simplex-chat = [ "-DIOS" ]; }) == [ "-j1" "-DIOS" ]
    ) [ "x86_64-darwin" "aarch64-darwin" ];
    bothAndroidAbisKeepPie = builtins.all (system:
      optionsOf (platformOptions system true {}) == [ "-j1" "-pie" ]
    ) [ "armv7a-android" "aarch64-android" ];
    nativeLinuxAndMacDefaultsUnchanged = builtins.all (system:
      optionsOf (platformOptions system false {}) == [ "-j1" ]
    ) [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
  };
in
if scenario == "baseline" then
  assert lib.elem "-j1" baseline; baseline
else
  assert scenario == "checks";
  assert builtins.all (value: value) (builtins.attrValues checks);
  checks
