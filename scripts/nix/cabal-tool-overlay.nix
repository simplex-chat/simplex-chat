final: prev:
let
  # Patch the Cabal library linked into the build tool, preserving the fork's
  # existing patches, compiler, and per-evaluation-platform tool variants.
  patchTool = tool:
    (tool.project.appendModule {
      modules = [{
        packages.Cabal.patches = [ ./cabal-hsc2hs-preserve-options.patch ];
      }];
    }).hsPkgs.cabal-install.components.exes.cabal;
  original = prev.haskell-nix.v2-cabal-install;
in prev.lib.optionalAttrs prev.stdenv.buildPlatform.isDarwin {
  haskell-nix = prev.haskell-nix // {
    v2-cabal-install = patchTool original // {
      evalWith = prev.lib.mapAttrs (_: patchTool) original.evalWith;
    };
  };
}
