final: prev:
prev.lib.optionalAttrs (
  prev.stdenv.hostPlatform.isMusl && prev.stdenv.buildPlatform.isLinux
) {
  haskell-nix = prev.haskell-nix // {
    cabalProject' = projectModule: prev.haskell-nix.cabalProject' (
      [ ({ config, lib, pkgs, ... }:
        lib.mkIf (
          pkgs.stdenv.hostPlatform.isMusl
          && pkgs.stdenv.buildPlatform.isLinux
          && config.compiler-nix-name == "ghc914-sh"
          && config.name == "iserv-proxy"
          && config.builderVersion == 2
        ) {
          # The build-time interpreter loads foreign libraries for TH.
          # Fully static musl uses a dlopen stub; retain static Haskell
          # libraries while allowing its C loader. Final CLI projects keep
          # haskell.nix's default executable-static setting.
          cabalProjectLocal = lib.mkAfter ''
            package iserv-proxy
              executable-static: False
          '';
        }) ]
      ++ (if builtins.isList projectModule then projectModule else [ projectModule ])
    );
  };
}
