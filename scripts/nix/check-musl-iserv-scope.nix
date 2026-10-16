# Pure policy regression: nix eval --json --file scripts/nix/check-musl-iserv-scope.nix
let
  overlay = import ./musl-iserv-overlay.nix;
  lib = {
    optionalAttrs = condition: attrs: if condition then attrs else {};
    mkIf = condition: attrs: if condition then attrs else {};
    mkAfter = value: value;
  };
  apply = { musl ? true, linux ? true, compiler ? "ghc914-sh", name ? "iserv-proxy", builderVersion ? 2 }:
    let
      prev = {
        inherit lib;
        stdenv = { hostPlatform.isMusl = musl; buildPlatform.isLinux = linux; };
        haskell-nix.cabalProject' = modules: modules;
      };
      result = overlay prev prev;
    in if result ? haskell-nix then
      (builtins.head (result.haskell-nix.cabalProject' [])) {
        inherit lib;
        pkgs = prev;
        config = { inherit name builderVersion; compiler-nix-name = compiler; };
      }
    else {};
  selected = apply {};
in
assert builtins.attrNames selected == [ "cabalProjectLocal" ];
assert builtins.match ".*executable-static: False.*" (builtins.replaceStrings [ "\n" ] [ " " ] selected.cabalProjectLocal) != null;
assert apply { name = "simplex-chat"; } == {};
assert apply { musl = false; } == {};
assert apply { linux = false; } == {};
assert apply { compiler = "ghc963"; } == {};
assert apply { builderVersion = 1; } == {};
{
  muslInterpreterCLoadingEnabled = true;
  applicationPolicyUnchanged = true;
  nonMuslAndDarwinBuildersUnchanged = true;
  otherCompilersAndLegacyBuilderUnchanged = true;
}
