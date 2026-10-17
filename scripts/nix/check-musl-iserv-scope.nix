# Use the pinned nixpkgs lib; no compiler, VM, or project IFD is evaluated.
# nix eval --impure --json --file scripts/nix/check-musl-iserv-scope.nix \
#   --apply 'f: f { lib = (builtins.getFlake (toString ./.)).inputs.nixpkgs.lib; }'
{ lib, overlay ? import ./musl-iserv-overlay.nix }:
let
  apply = { musl ? true, linux ? true, compiler ? "ghc914-sh", name ? "iserv-proxy", builderVersion ? 2, i386 ? true, profiling ? false }:
    let
      prev = {
        inherit lib;
        stdenv = { hostPlatform = { isMusl = musl; isx86_32 = i386; }; buildPlatform.isLinux = linux; };
        pkgsBuildBuild.applyPatches = args: args;
        haskell-nix = {
          cabalProject' = modules: modules;
          sources.iserv-proxy = "original-iserv-proxy-source";
        };
      };
      result = overlay prev prev;
      modules = if result ? haskell-nix then result.haskell-nix.cabalProject' [] else [];
      evaluated = lib.evalModules {
        modules = [ {
          options = {
            name = lib.mkOption { type = lib.types.str; };
            compiler-nix-name = lib.mkOption { type = lib.types.str; };
            builderVersion = lib.mkOption { type = lib.types.int; };
            profiling = lib.mkOption { type = lib.types.bool; };
            cabalProjectLocal = lib.mkOption { type = lib.types.lines; default = ""; };
            src = lib.mkOption { type = lib.types.anything; default = null; };
            modules = lib.mkOption { type = lib.types.listOf lib.types.anything; default = []; };
          };
          config = {
            inherit name builderVersion profiling;
            compiler-nix-name = compiler;
            # Real cabalProject' supplies this through the module fixed point,
            # not specialArgs. Eager optionalAttrs on pkgs recurses here.
            _module.args.pkgs = prev;
          };
        } ] ++ modules;
      };
    in {
      inherit (evaluated.config) cabalProjectLocal src;
    };
  selected = apply {};
  disabled = { cabalProjectLocal = ""; src = null; };
in
assert selected.src.src == "original-iserv-proxy-source";
assert selected.src.patches == [ ./musl-iserv-public-libc.patch ];
assert builtins.match ".*executable-static: False.*" (builtins.replaceStrings [ "\n" ] [ " " ] selected.cabalProjectLocal) != null;
assert apply { profiling = true; } == selected;
assert (apply { i386 = false; }).src == null;
assert (apply { i386 = false; }).cabalProjectLocal == selected.cabalProjectLocal;
assert apply { name = "simplex-chat"; } == disabled;
assert apply { musl = false; } == disabled;
assert apply { linux = false; } == disabled;
assert apply { compiler = "ghc963"; } == disabled;
assert apply { builderVersion = 1; } == disabled;
{
  moduleArgumentFixedPointEvaluated = true;
  muslInterpreterCLoadingEnabled = true;
  i386SharedLibcSourcePatchedBeforePlanning = true;
  otherMuslSymbolTablesUnchanged = true;
  applicationPolicyUnchanged = true;
  nonMuslAndDarwinBuildersUnchanged = true;
  otherCompilersAndLegacyBuilderUnchanged = true;
}
