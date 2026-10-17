# Exercise the pinned HN plan, package, component types, and argument fixed point.
# No compiler, source producer, or project plan is built.
{ lib, haskellNix, scenario ? "checks" }:
let
  select = import ./darwin-bitvec-runtime.nix;
  provider = builtins.toFile "bitvec-resolver-test-provider" "";
  basePkgs = {
    inherit lib;
    otherLibrary = provider;
    fetchurl = args: args;
    applyPatches = args: args;
  };
  selected = select {
    pkgs = basePkgs;
    hostPlatform = { isDarwin = true; isx86_64 = true; };
    cc = "/selected-target-clang";
  };
  evaluate = mapping: extra: lib.evalModules {
    modules = [ (haskellNix + "/modules/plan.nix") (haskellNix + "/modules/error-handler.nix") {
      options = {
        hackage.configs = lib.mkOption { type = lib.types.unspecified; default = {}; };
        hsPkgs = lib.mkOption { type = lib.types.unspecified; default = {}; };
        cabal = {
          system = lib.mkOption {
            type = lib.types.unspecified;
            default = { isJavaScript = false; isWasm32 = false; isOsx = true; isX86_64 = true; };
          };
          compiler = lib.mkOption { type = lib.types.unspecified; default = {}; };
        };
      };
      config = {
        _module.args = {
          pkgs = basePkgs;
          pkgconfPkgs = {};
          haskellLib.types = import (haskellNix + "/lib/types.nix") { inherit lib; };
        };
        use-package-keys = false;
        plan.pkg-def = _: let
          revision = { pkgs, flags, system, errorHandler, ... }: {
            flags.simd = true;
            # Exact generated runtime expression from the failed component.
            components.library.libs = pkgs.lib.optionals (flags.simd && !system.isJavaScript && !system.isWasm32) (pkgs.lib.optional (system.isOsx && system.isX86_64) (pkgs."clang_rt.osx" or (errorHandler.sysDepError "clang_rt.osx")))
              ++ lib.optional (extra == "provided") pkgs.otherLibrary
              ++ lib.optional (extra == "missing") (pkgs.unrelated or (errorHandler.sysDepError "unrelated"));
          };
        in {
          compiler = { nix-name = "ghc914-sh"; version = "9.14.0"; packages = {}; };
          packages = { bitvec = { inherit revision; }; another-package = { inherit revision; }; };
        };
      };
    } ] ++ lib.optionals mapping selected.modules;
  };
  libs = mapping: extra: (evaluate mapping extra).config.packages.bitvec.components.library.libs;
  succeeds = value: (builtins.tryEval (builtins.deepSeq value true)).success;
  checks = {
    originalMissingRuntimeRejected = !succeeds (libs false "none");
    compilerOwnedRuntimeResolved = libs true "none" == [];
    otherBitvecLibrariesPreserved = libs true "provided" == [ provider ];
    missingBitvecLibraryStillRejected = !succeeds (libs true "missing");
    otherPackageMappingUnchanged = !succeeds (evaluate true "none").config.packages.another-package.components.library.libs;
  };
in
if scenario == "baseline" then libs false "none"
else if scenario == "unrelated" then libs true "missing"
else assert scenario == "checks";
assert builtins.all (value: value) (builtins.attrValues checks);
checks
