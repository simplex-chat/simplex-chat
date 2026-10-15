final: prev:
prev.lib.optionalAttrs prev.stdenv.hostPlatform.isAndroid {
  # haskell.nix supplies static-only GNU libiconv on Android. Its objects
  # also become part of libsimplex.so, so build the archive with PIC.
  libiconv = prev.libiconv.overrideAttrs (old: {
    configureFlags = old.configureFlags ++ [ "--with-pic" ];
  });
  haskell-nix = prev.haskell-nix // {
    # iserv has its own boot-library projects, including evalWith variants.
    # Add the paths before each plan, rather than only to the app's modules.
    cabalProject' = projectModule: prev.haskell-nix.cabalProject' (
      [ ./android-iconv-project.nix ]
      ++ (if builtins.isList projectModule then projectModule else [ projectModule ])
    );
  };
}
