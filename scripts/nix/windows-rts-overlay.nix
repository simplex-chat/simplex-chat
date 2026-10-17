final: prev:
prev.lib.optionalAttrs (
  prev.stdenv.hostPlatform.isWindows
  && prev.stdenv.hostPlatform.isx86_64
  && prev.stdenv.buildPlatform.isLinux
) {
  haskell-nix = prev.haskell-nix // {
    cabalProject' = projectModule: prev.haskell-nix.cabalProject' (
      [ ./windows-rts-project.nix ]
      ++ (if builtins.isList projectModule then projectModule else [ projectModule ])
    );
  };
}
