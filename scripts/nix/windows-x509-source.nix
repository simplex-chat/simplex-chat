{ pkgs, haskellNix }:
pkgs.applyPatches {
  name = "crypton-x509-system-1.6.8-windows-source";
  src = pkgs.fetchurl {
    url = "https://hackage.haskell.org/package/crypton-x509-system-1.6.8/crypton-x509-system-1.6.8.tar.gz";
    hash = "sha256-6AsTPXCP/ubbb9IbE6Qw5rJ6iuLEeoSNDN1b0AagG44=";
  };
  patches = [ "${haskellNix}/overlays/patches/crypton-x509-system.patch" ];
  patchFlags = [ "-p1" "--no-backup-if-mismatch" ];
}
