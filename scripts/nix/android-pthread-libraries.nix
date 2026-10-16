# Bionic supplies pthread symbols in libc, with no separate libpthread.
# Remove the two Linux library declarations before planning so Cabal's v2
# source and package registrations agree; keep compiler -pthread flags intact.
{ pkgs, hostPlatform }:
{
  packageSources = pkgs.lib.optional hostPlatform.isAndroid (pkgs.applyPatches {
    name = "crypton-0.34-android-source";
    src = pkgs.fetchurl {
      url = "https://hackage.haskell.org/package/crypton-0.34/crypton-0.34.tar.gz";
      hash = "sha256-RESEaSTKVWFfzhBJE6WmhnWhgM/q3DUKsrEk+6G8HtY=";
    };
    patches = [ ./crypton-android-pthread.patch ];
  });
  # A local metadata override must keep the former Hackage dependency scope.
  # haskell.nix otherwise enables local tests/benchmarks while making the plan.
  cabalProjectLocal = if hostPlatform.isAndroid then ''
    package crypton
      tests: False
      benchmarks: False
  '' else "";
  directSqlcipherPatches = pkgs.lib.optional hostPlatform.isAndroid
    ./direct-sqlcipher-android-pthread.patch;
}
