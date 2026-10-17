# Use the pinned lib and the real project module; no compiler or project IFD.
{ lib
, haskellNix ? builtins.fetchTree
    (builtins.fromJSON (builtins.readFile ../../flake.lock)).nodes.haskellNix.locked
, projectModule ? import (haskellNix + "/modules/stable-haskell-rts-comdat.nix")
}:
let
  apply = { musl ? true, i386 ? true, linux ? true,
    compiler ? "ghc914-sh", builderVersion ? 2,
    name ? "simplex-chat", profiling ? false }:
    let
      project = lib.evalModules {
        modules = [ projectModule {
          options = {
            name = lib.mkOption { type = lib.types.str; };
            profiling = lib.mkOption { type = lib.types.bool; };
            compiler-nix-name = lib.mkOption { type = lib.types.str; };
            builderVersion = lib.mkOption { type = lib.types.int; };
            modules = lib.mkOption { type = lib.types.listOf lib.types.anything; default = []; };
          };
          config = {
            inherit name profiling builderVersion;
            compiler-nix-name = compiler;
            # Match cabalProject's module fixed point, not specialArgs.
            _module.args.pkgs.stdenv = {
              hostPlatform = { isMusl = musl; isx86_32 = i386; };
              buildPlatform.isLinux = linux;
            };
          };
        } ];
      };
      package = lib.evalModules {
        modules = [ {
          options.packages = lib.mkOption {
            type = lib.types.attrsOf (lib.types.submodule {
              options.patches = lib.mkOption { type = lib.types.listOf lib.types.path; default = []; };
            });
            default = {};
          };
          # Existing patches must remain ordered before this one.
          config.packages.rts.patches = [ ./musl-iserv-public-libc.patch ];
        } ] ++ project.config.modules;
      };
    in map toString package.config.packages.rts.patches;
  unchanged = map toString [ ./musl-iserv-public-libc.patch ];
  selected = unchanged ++ [ (toString (haskellNix + "/overlays/patches/ghc914-rts-elf-comdat-functions.patch")) ];
in
assert apply {} == selected;
assert apply { name = "iserv-proxy"; } == selected;
assert apply { profiling = true; } == selected;
assert apply { musl = false; } == unchanged;
assert apply { i386 = false; } == unchanged;
assert apply { linux = false; } == unchanged;
assert apply { compiler = "ghc963"; } == unchanged;
assert apply { compiler = "ghc914-sh-stage1"; } == unchanged;
assert apply { builderVersion = 1; } == unchanged;
{
  moduleArgumentFixedPointEvaluated = true;
  i386MuslApplicationAndInterpreterCovered = true;
  profilingCovered = true;
  existingPatchOrderPreserved = true;
  otherPlatformsCompilersAndBuildersUnchanged = true;
}
