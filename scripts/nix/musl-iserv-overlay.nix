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
        ) (lib.mkMerge [ {
          # The build-time interpreter loads foreign libraries for TH.
          # Fully static musl uses a dlopen stub; retain static Haskell
          # libraries while allowing its C loader. Final CLI projects keep
          # haskell.nix's default executable-static setting.
          cabalProjectLocal = lib.mkAfter ''
            package iserv-proxy
              executable-static: False
          '';
        } (lib.mkIf pkgs.stdenv.hostPlatform.isx86_32 {
          # Keep module shape independent of pkgs: cabalProject' supplies it
          # through _module.args, so eager optionalAttrs recurses in config.
          # This helper links shared libc. Keep public close registered, but
          # do not take the address of musl's hidden AIO implementation hook.
          src = lib.mkForce (pkgs.pkgsBuildBuild.applyPatches {
            name = "musl-iserv-proxy-source";
            src = pkgs.haskell-nix.sources.iserv-proxy;
            patches = [ ./musl-iserv-public-libc.patch ];
          });
        }) ])) ]
      ++ (if builtins.isList projectModule then projectModule else [ projectModule ])
    );
  };
}
