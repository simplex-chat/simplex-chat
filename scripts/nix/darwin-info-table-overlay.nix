final: prev:
let
  inherit (prev.stdenv) buildPlatform hostPlatform targetPlatform;
  active = hostPlatform.isDarwin && hostPlatform.isx86_64
    && buildPlatform.config == hostPlatform.config
    && hostPlatform.config == targetPlatform.config;
in prev.lib.optionalAttrs active {
  haskell-nix = prev.haskell-nix // {
    cabalProject' = projectModule: prev.haskell-nix.cabalProject' (
      [ ./darwin-info-table-project.nix ]
      ++ (if builtins.isList projectModule then projectModule else [ projectModule ])
    );
  };
}
