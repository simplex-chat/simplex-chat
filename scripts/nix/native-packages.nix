{ haskellNix }:
system:
# Keep haskell.nix's existing host pins, including Intel Darwin's 26.05 pin.
# Its flake omits ARM64 Linux, so construct that native set from the same inputs.
let
  native = haskellNix.legacyPackages.${system} or (import haskellNix.inputs.nixpkgs {
    localSystem = { inherit system; };
    config = haskellNix.config;
    overlays = [ haskellNix.overlay ];
  });
in
if system == "x86_64-linux"
then native.appendOverlays [ (import ./native-rts-overlay.nix) ]
else if system == "x86_64-darwin"
then native.appendOverlays [ (import ./darwin-info-table-overlay.nix) ]
else native
