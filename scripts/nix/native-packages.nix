{ haskellNix }:
system:
# Keep haskell.nix's existing host pins, including Intel Darwin's 26.05 pin.
# Its flake omits ARM64 Linux, so construct that native set from the same inputs.
haskellNix.legacyPackages.${system} or (import haskellNix.inputs.nixpkgs {
  localSystem = { inherit system; };
  config = haskellNix.config;
  overlays = [ haskellNix.overlay ];
})
