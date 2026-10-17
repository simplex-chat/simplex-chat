# Exercise the real module fixed point without target tools or project IFD.
{ lib, projectModule ? import ./windows-rts-project.nix }:
let
  apply = { windows ? true, x86_64 ? true, linux ? true,
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
            _module.args.pkgs.stdenv = {
              hostPlatform = { isWindows = windows; isx86_64 = x86_64; };
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
          config.packages.rts.patches = [ ./ghc914-rts-elf-section-alignment.patch ];
        } ] ++ project.config.modules;
      };
    in package.config.packages.rts.patches;
  unchanged = [ ./ghc914-rts-elf-section-alignment.patch ];
  selected = unchanged ++ [ ./ghc914-rts-windows-dll-search-path.patch ];
in
assert apply {} == selected;
assert apply { name = "iserv-proxy"; } == selected;
assert apply { name = "iserv-proxy"; profiling = true; } == selected;
assert apply { windows = false; } == unchanged;
assert apply { x86_64 = false; } == unchanged;
assert apply { linux = false; } == unchanged;
assert apply { compiler = "ghc963"; } == unchanged;
assert apply { compiler = "ghc914-sh-stage1"; } == unchanged;
assert apply { builderVersion = 1; } == unchanged;
{
  moduleArgumentFixedPointEvaluated = true;
  windows64ApplicationAndInterpreterCovered = true;
  profilingCovered = true;
  existingPatchOrderPreserved = true;
  otherPlatformsCompilersAndBuildersUnchanged = true;
}
