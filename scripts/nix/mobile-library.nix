{ pkgs }:
# V2 slices ignore component.override and do not run postBuild/postInstall.
# Relink while their full package database still exists, then package after
# the normal install phase. The compiler is the exact slice/TH wrapper.
let withComponentPhases = component: change: component.overrideAttrs (old:
  change old // {
    # overrideAttrs drops the project/package handles that haskell.nix adds
    # outside the derivation. Keep them for focused component diagnostics.
    passthru = (old.passthru or {}) // (pkgs.lib.intersectAttrs {
      project = null;
      package = null;
    } component) // {
      inherit (component) checkAgainstPlan;
      isSlice = true;
    };
  }); in
let mobileLibrary = { kind, packageName ? "simplex-chat", linkOptions ? [], packageDbs ? [], postInstall }: old:
  let compiler = builtins.head old.nativeBuildInputs;
      prefix = compiler.targetPrefix or "";
  in {
    buildPhase = old.buildPhase + ''
      discoverStore
      test -n "$ghcDir"
      MOBILE_LIBRARY_PACKAGE_DBS=${pkgs.lib.escapeShellArg (pkgs.lib.concatStringsSep ":" packageDbs)} \
      ${pkgs.bash}/bin/bash ${./link-mobile-library.sh} \
        ${compiler}/bin/${prefix}ghc ${compiler}/bin/${prefix}ghc-pkg \
        "$ghcDir/$storePkgDb" ${pkgs.lib.escapeShellArg packageName} \
        ${pkgs.lib.escapeShellArg kind} "$buildRoot/mobile-bundle" \
        ${pkgs.lib.escapeShellArgs linkOptions}
    '';
    installPhase = old.installPhase + ''
      (
        cd "$buildRoot/mobile-bundle"
        ${postInstall}
      )
    '';
  }; in
let withMobileLibrary = component: options:
  withComponentPhases component (mobileLibrary options); in
{ inherit withComponentPhases withMobileLibrary; }
