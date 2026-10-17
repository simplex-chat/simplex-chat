# No target tools or Haskell planning: nix eval --json --file this-file.nix
let
  overlay = import ./mingw-stdio-overlay.nix;
  existing = ./ghc914-rts-windows-dll-search-path.patch;
  fix = ./mingw-w64-swprintf-exact-fit.patch;
  apply = { windows ? true, libc ? "msvcrt", version ? "14.0.0", patches ? [ existing ] }:
    let
      runtime = {
        inherit version patches;
        marker = "existing-runtime-attributes";
        overrideAttrs = f: runtime // f runtime;
      };
      scope = {
        mingw_w64 = runtime;
        mingw_w64_pthreads.CPPFLAGS = "";
        unchanged = "other-Windows-packages";
        overrideScope = f: scope // f scope scope;
      };
      # haskell.nix adds UCRT's flags with a shallow `windows // { ...; }`.
      # overrideScope still closes over the original scope and drops that field.
      customized = scope // (if libc == "ucrt" then {
        mingw_w64_pthreads = { CPPFLAGS = "-D_UCRT"; marker = "existing-UCRT-customization"; };
      } else {});
      prev = {
        lib.optionalAttrs = condition: attrs: if condition then attrs else {};
        stdenv.hostPlatform = { isWindows = windows; inherit libc; };
        # Non-Windows evaluation must not force any Windows dependencies.
        windows = if windows then customized else throw "Windows scope was forced";
      };
    in overlay prev prev;
  selected = apply {};
in
assert selected.windows.mingw_w64.patches == [ existing fix ];
assert selected.windows.mingw_w64.marker == "existing-runtime-attributes";
assert selected.windows.unchanged == "other-Windows-packages";
assert selected.windows.mingw_w64_pthreads.CPPFLAGS == "";
assert (apply { libc = "ucrt"; }).windows.mingw_w64_pthreads == {
  CPPFLAGS = "-D_UCRT";
  marker = "existing-UCRT-customization";
};
assert (apply { libc = "ucrt"; }).windows.mingw_w64.patches == [ existing fix ];
assert (apply { patches = []; }).windows.mingw_w64.patches == [ fix ];
assert (apply { version = "13.0.0"; }).windows.mingw_w64.patches == [ existing fix ];
assert (apply { version = "12.0.0"; }).windows.mingw_w64.patches == [ existing ];
assert (apply { version = "15.0.0"; }).windows.mingw_w64.patches == [ existing ];
assert apply { windows = false; } == {};
{
  verifiedMingw13And14RuntimePatched = true;
  existingPatchOrderAndWindowsScopePreserved = true;
  shallowUcrtPthreadCustomizationPreserved = true;
  unrelatedPlatformsAndVersionsUnchanged = true;
  nonWindowsDependenciesRemainLazy = true;
}
