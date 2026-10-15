{ haskellNix }:
pkgs:
# Hydra's compiler builds use this overlay. Its pkg-config metadata changes
# the Cabal build tool's hash, and therefore every compiler slice's hash.
# Keep that context inside compiler selection: application dependencies still
# use the project's package set and its current libsodium mapping.
(pkgs.appendOverlays [ (import (haskellNix + "/test/overlay.nix")) ]).haskell-nix.compiler
