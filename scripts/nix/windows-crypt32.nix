# The patched x509 Cabal file uses the import library's lowercase name.
# MinGW supplies crypt32 implicitly, just like HN's legacy Crypt32 alias.
{ hostPlatform }:
{ lib, pkgs, config, ... }:
lib.mkIf (hostPlatform.isWindows && config.compiler-nix-name == "ghc914-sh") {
  packages.crypton-x509-system._module.args.pkgs = lib.mkForce (pkgs // {
    crypt32 = null;
  });
}
