# Exercise the actual HN plan/package/component argument fixed point.
# No source producer, toolchain, compiler, or Cabal planner is built.
{ lib, haskellNix, scenario ? "checks" }:
let
  provider = builtins.toFile "crypt32-resolver-other-provider" "";
  basePkgs = { inherit lib; Crypt32 = null; otherLibrary = provider; };
  evaluate = mapping: windows: compiler: extra: lib.evalModules {
    modules = [ (haskellNix + "/modules/plan.nix") (haskellNix + "/modules/error-handler.nix") {
      options = {
        compiler-nix-name = lib.mkOption { type = lib.types.str; default = compiler; };
        hackage.configs = lib.mkOption { type = lib.types.unspecified; default = {}; };
        hsPkgs = lib.mkOption { type = lib.types.unspecified; default = {}; };
        cabal = {
          system = lib.mkOption { type = lib.types.unspecified; default = { isWindows = windows; }; };
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
          revision = { pkgs, system, errorHandler, ... }: {
            # Exact expression emitted for the failed crypton-x509-system plan.
            components.library.libs = pkgs.lib.optional (system.isWindows)
              (pkgs."crypt32" or (errorHandler.sysDepError "crypt32"))
              ++ lib.optional (extra == "provided") pkgs.otherLibrary
              ++ lib.optional (extra == "missing")
                (pkgs.unrelated or (errorHandler.sysDepError "unrelated"));
          };
        in {
          compiler = { nix-name = compiler; version = "9.14.0"; packages = {}; };
          packages = { crypton-x509-system = { inherit revision; }; another-package = { inherit revision; }; };
        };
      };
    } ] ++ lib.optional mapping (import ./windows-crypt32.nix { hostPlatform.isWindows = windows; });
  };
  libs = mapping: windows: compiler: extra:
    (evaluate mapping windows compiler extra).config.packages.crypton-x509-system.components.library.libs;
  succeeds = value: (builtins.tryEval (builtins.deepSeq value true)).success;
  disabled = windows: compiler: (lib.evalModules {
    modules = [ (import ./windows-crypt32.nix { hostPlatform.isWindows = windows; }) {
      options = {
        compiler-nix-name = lib.mkOption { type = lib.types.str; default = compiler; };
        packages = lib.mkOption { type = lib.types.unspecified; default = {}; };
      };
      config._module.args.pkgs = throw "Disabled resolver forced its package set";
    } ];
  }).config.packages;
  checks = {
    oldCaseOnlyMappingRejected = !succeeds (libs false true "ghc914-sh" "none");
    lowercaseImportLibraryResolved = libs true true "ghc914-sh" "none" == [];
    otherLibrariesPreserved = libs true true "ghc914-sh" "provided" == [ provider ];
    unrelatedMissingLibraryRejected = !succeeds (libs true true "ghc914-sh" "missing");
    otherPackageMappingUnchanged = !succeeds
      (evaluate true true "ghc914-sh" "none").config.packages.another-package.components.library.libs;
    nonWindowsUnchanged = libs true false "ghc914-sh" "provided" == [ provider ];
    otherCompilerUnchanged = !succeeds (libs true true "ghc912" "none");
    nonWindowsPackageSetLazy = disabled false "ghc914-sh" == {};
    otherCompilerPackageSetLazy = disabled true "ghc912" == {};
  };
in
if scenario == "baseline" then libs false true "ghc914-sh" "none"
else if scenario == "unrelated" then libs true true "ghc914-sh" "missing"
else assert scenario == "checks";
assert builtins.all (value: value) (builtins.attrValues checks);
checks
