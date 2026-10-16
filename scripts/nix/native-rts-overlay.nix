final: prev:
let
  inherit (prev.stdenv) buildPlatform hostPlatform targetPlatform;
  active = hostPlatform.isLinux && hostPlatform.isx86_64
    && buildPlatform.config == hostPlatform.config
    && hostPlatform.config == targetPlatform.config;
in prev.lib.optionalAttrs active {
  haskell-nix = prev.haskell-nix // {
    # Cover the compiler's own stage2 project as well as evalWith variants.
    # Reusing the public project hook keeps native/auxiliary/cross users on
    # one compiler rather than patching only an application's final link.
    cabalProject' = projectModule: prev.haskell-nix.cabalProject' (
      [ ./native-rts-project.nix ]
      ++ (if builtins.isList projectModule then projectModule else [ projectModule ])
    );
  };
}
