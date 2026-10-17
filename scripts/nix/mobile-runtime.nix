{ pkgs, compilerSelection }:
let
  lib = pkgs.lib;
  # Keep the path value: the v2 builder reads its basename before storing it.
  patch = ./simplex-ghc914-rts-small-address-space.patch;
  rtsModule = { packages.rts.patches = [ patch ]; };
  nativeProject = (compilerSelection pkgs).ghc914-sh.stage2Project;
  rtsProject = nativeProject.appendModule { modules = [ rtsModule ]; };
  textProject = nativeProject.appendModule {
    modules = [ ({ lib, ... }: { packages.text.ghcOptions = lib.mkAfter [ "-j1" ]; }) ];
  };
  # mac2ios changes Mach-O metadata, not the instructions. The native Darwin
  # compiler otherwise defaults to M1, which emits LSE/RCPC in RTS and simdutf
  # code that runs before CPU dispatch. Match the existing iOS ARM baseline.
  baseline = component:
    if pkgs.stdenv.hostPlatform.isAarch64 then component.overrideAttrs (old:
      let variable = "NIX_CFLAGS_COMPILE_${lib.replaceStrings [ "-" "." ] [ "_" "_" ] pkgs.stdenv.hostPlatform.config}";
      in { ${variable} = (old.${variable} or "") + " -mcpu=apple-a7 -march=armv8-a+norcpc"; })
    else component;
  libraries = [
    rtsProject.hsPkgs.rts.components.library
    (baseline rtsProject.hsPkgs.rts.components.sublibs.threaded-nodebug)
  ] ++ lib.optional pkgs.stdenv.hostPlatform.isAarch64
    (baseline textProject.hsPkgs.text.components.library);
  ghcArch = if pkgs.stdenv.hostPlatform.isAarch64 then "aarch64" else "x86_64";
in {
  # Cross compilers plan boot libraries from source inside the app project.
  androidModule = rtsModule;
  # Nixpkgs C libraries use structured attributes: export flags through env.
  darwinCLibrary = library:
    if pkgs.stdenv.hostPlatform.isAarch64 then library.overrideAttrs (old: {
      env = (old.env or {}) // {
        NIX_CFLAGS_COMPILE_arm64_apple_darwin =
          (old.env.NIX_CFLAGS_COMPILE_arm64_apple_darwin or "")
          + " -mcpu=apple-a7 -march=armv8-a+norcpc";
      };
    }) else library;
  # Native Darwin uses the compiler's boot libraries. Replace only these
  # registrations at final link, after checking that their ABIs are unchanged.
  darwinPackageDbs = assert pkgs.stdenv.hostPlatform.isDarwin;
    map (library: "${library}/store/host/${ghcArch}-apple-darwin/package.conf.d") libraries;
}
