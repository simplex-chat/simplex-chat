final: prev:
prev.lib.optionalAttrs prev.stdenv.hostPlatform.isWindows {
  # The CRT owns libmingwex.a. Patching headers or GHC's callers alone leaves
  # ISO swprintf rejecting successful exact-fit strings at runtime.
  windows = prev.windows.overrideScope (_self: super: {
    # Inspect the version only after constructing the Windows scope: forcing
    # the CRT in the outer condition recurses through the cross libc bootstrap.
    mingw_w64 = super.mingw_w64.overrideAttrs (old:
      prev.lib.optionalAttrs (builtins.elem old.version [ "13.0.0" "14.0.0" ]) {
        patches = (old.patches or []) ++ [ ./mingw-w64-swprintf-exact-fit.patch ];
      });
  });
}
