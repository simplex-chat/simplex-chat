# nix eval --json --file scripts/nix/check-android-pthread-scope.nix
let
  select = import ./android-pthread-libraries.nix;
  pkgs = {
    lib.optional = condition: value: if condition then [ value ] else [];
    fetchurl = args: args;
    applyPatches = args: args;
  };
  android = select { inherit pkgs; hostPlatform.isAndroid = true; };
  # The false branch must not even evaluate Android sources on other targets.
  other = select {
    pkgs = pkgs // {
      fetchurl = _: throw "Unexpected Android source fetch";
      applyPatches = _: throw "Unexpected Android source patch";
    };
    hostPlatform.isAndroid = false;
  };
  source = builtins.head android.packageSources;
in
assert builtins.length android.packageSources == 1;
assert source.name == "crypton-0.34-android-source";
assert source.src.url == "https://hackage.haskell.org/package/crypton-0.34/crypton-0.34.tar.gz";
assert source.src.hash == "sha256-RESEaSTKVWFfzhBJE6WmhnWhgM/q3DUKsrEk+6G8HtY=";
assert source.patches == [ ./crypton-android-pthread.patch ];
assert android.directSqlcipherPatches == [ ./direct-sqlcipher-android-pthread.patch ];
assert android.cabalProjectLocal == "package crypton\n  tests: False\n  benchmarks: False\n";
assert other == { packageSources = []; directSqlcipherPatches = []; cabalProjectLocal = ""; };
{
  androidUsesBothPreplanningPatches = true;
  cryptonVersionAndSourcePinned = true;
  cryptonRetainsDependencyOnlyGoals = true;
  otherPlatformsUnchangedAndLazy = true;
}
