{ haskellNix }:
final: prev:
let
  inherit (prev.stdenv) buildPlatform hostPlatform targetPlatform;
  nativePackages = import ./native-packages.nix { inherit haskellNix; };
  # Auxiliary projects such as iserv select the package set's compiler directly.
  # Pin that native compiler too, without changing application pkg-config maps.
  nativeCompilers = ((nativePackages buildPlatform.system).appendOverlays [
    (import (haskellNix + "/test/overlay.nix"))
  ]).haskell-nix.compiler;
  fullyNative = buildPlatform.config == hostPlatform.config
    && buildPlatform.config == targetPlatform.config;
in {
  haskell-nix = prev.haskell-nix // {
    compiler = prev.haskell-nix.compiler // final.lib.optionalAttrs fullyNative {
      ghc914-sh = nativeCompilers.ghc914-sh;
      ghc914-sh-stage1 = nativeCompilers.ghc914-sh-stage1;
    };
  };
}
