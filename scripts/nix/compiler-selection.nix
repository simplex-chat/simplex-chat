{ haskellNix }:
pkgs:
let
  inherit (pkgs.stdenv) buildPlatform hostPlatform targetPlatform;
  nativePackages = import ./native-packages.nix { inherit haskellNix; };
  # Extending a buildPackages splice can restore its parent cross compiler.
  # Start native compiler selection from a fresh native set so Cabal's build
  # tools see the installed boot libraries, rather than an empty target DB.
  # Compare full triples: a GNU-to-musl cross has the same system string.
  compilerPkgs = if buildPlatform.config == hostPlatform.config
      && buildPlatform.config == targetPlatform.config
    then nativePackages buildPlatform.system
    else pkgs;
in
# Hydra's compiler builds use this overlay. Its pkg-config metadata changes
# the Cabal build tool's hash, and therefore every compiler slice's hash.
# Keep that context inside compiler selection: application dependencies still
# use the project's package set and its current libsodium mapping.
(compilerPkgs.appendOverlays [ (import (haskellNix + "/test/overlay.nix")) ]).haskell-nix.compiler
