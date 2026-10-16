{ pkgs }:
pkgs.applyPatches {
  name = "composition-1.0.2.2-ghc914-source";
  src = pkgs.fetchurl {
    url = "https://hackage.haskell.org/package/composition-1.0.2.2/composition-1.0.2.2.tar.gz";
    hash = "sha256-tQv3qW8ftWsVmPmORIw3lj6PRX5CSvuw/VYurOpE1C4=";
  };
  patches = [ ./composition-ghc-internal.patch ];
}
