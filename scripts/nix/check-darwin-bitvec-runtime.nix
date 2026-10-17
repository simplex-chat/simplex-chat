# nix eval --json --file scripts/nix/check-darwin-bitvec-runtime.nix
let
  select = import ./darwin-bitvec-runtime.nix;
  pkgs = {
    lib.optional = condition: value: if condition then [ value ] else [];
    fetchurl = args: args;
    applyPatches = args: args;
  };
  intel = select {
    inherit pkgs;
    hostPlatform = { isDarwin = true; isx86_64 = true; };
    cc = "/selected-target-clang";
  };
  other = platform: select {
    pkgs = pkgs // {
      fetchurl = _: throw "Unexpected Intel bitvec source fetch";
      applyPatches = _: throw "Unexpected Intel bitvec source patch";
    };
    hostPlatform = platform;
    cc = throw "Unexpected Intel compiler runtime dependency";
  };
  source = builtins.head intel.packageSources;
in
assert builtins.length intel.packageSources == 1;
assert source.name == "bitvec-1.1.6.0-darwin-source";
assert source.src.url == "https://hackage.haskell.org/package/bitvec-1.1.6.0/bitvec-1.1.6.0.tar.gz";
assert source.src.hash == "sha256-GkLTPONBlz9OTz8OENAHYAVqCsoOKlcYCZS/KsJsTis=";
assert source.patches == [ ./bitvec-darwin-compiler-rt.patch ];
assert builtins.length intel.modules == 1;
assert intel.cabalProjectLocal == ''
  package bitvec
    tests: False
    benchmarks: False
    extra-lib-dirs: /selected-target-clang/resource-root/lib/darwin
'';
assert builtins.all (p: other p == { packageSources = []; cabalProjectLocal = ""; modules = []; }) [
  { isDarwin = true; isx86_64 = false; }
  { isDarwin = false; isx86_64 = true; }
  { isDarwin = false; isx86_64 = false; }
];
{
  intelDeclaresMatchingCompilerRuntime = true;
  bitvecRetainsDependencyOnlyGoals = true;
  otherPlatformsUnchangedAndLazy = true;
}
