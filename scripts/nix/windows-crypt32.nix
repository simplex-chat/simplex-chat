# The patched x509 Cabal file uses the import library's lowercase name.
# MinGW supplies crypt32 implicitly, just like HN's legacy Crypt32 alias.
{ hostPlatform, compilerNixName }:
{ lib, pkgs, ... }:
# An excluded project may not declare this package in its keyed schema.
# Decide the module shape from the same external compiler/platform selection
# as the project, before exposing any package definition to HN.
lib.optionalAttrs (hostPlatform.isWindows && compilerNixName == "ghc914-sh") {
  packages.crypton-x509-system._module.args.pkgs = lib.mkForce (pkgs // {
    crypt32 = null;
  });
}
