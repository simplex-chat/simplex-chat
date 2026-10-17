# Clang's x86 CPU builtins use compiler-rt's model and initializer. Declare
# that dependency for GHC's object linker, which does not add it implicitly.
{ pkgs, hostPlatform, cc }:
let
  intelDarwin = hostPlatform.isDarwin && hostPlatform.isx86_64;
in {
  packageSources = pkgs.lib.optional intelDarwin (pkgs.applyPatches {
    name = "bitvec-1.1.6.0-darwin-source";
    src = pkgs.fetchurl {
      url = "https://hackage.haskell.org/package/bitvec-1.1.6.0/bitvec-1.1.6.0.tar.gz";
      hash = "sha256-GkLTPONBlz9OTz8OENAHYAVqCsoOKlcYCZS/KsJsTis=";
    };
    patches = [ ./bitvec-darwin-compiler-rt.patch ];
  });
  # The selected compiler wrapper owns the matching target runtime archive.
  # Keep this former Hackage dependency's tests and benchmarks disabled.
  cabalProjectLocal = if intelDarwin then ''
    package bitvec
      tests: False
      benchmarks: False
      extra-lib-dirs: ${cc}/resource-root/lib/darwin
  '' else "";
  modules = pkgs.lib.optional intelDarwin ({ lib, pkgs, ... }: {
    # This name belongs to the selected CC resource directory above, rather
    # than a top-level Nixpkgs package. Keep all other dependency mappings.
    packages.bitvec._module.args.pkgs = lib.mkForce (pkgs // { "clang_rt.osx" = null; });
  });
}
