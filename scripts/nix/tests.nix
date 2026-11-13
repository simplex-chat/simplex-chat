# Hydra jobs for the checks that belong with this compiler pin.
# Pure asserts are recorded by forcing them at evaluation time. The Cabal
# suite, the CLI smoke, and xcodetest are real builds.
{ pkgs, lib, haskellNix, system, nativePackages, nativeProject, iosArchiveConverter, compiler }:
let
  haskellLib = pkgs.haskell-nix.haskellLib;
  source = haskellLib.cleanGit {
    name = "simplex-chat-source";
    src = ../..;
  };

  record = name: value: pkgs.runCommand "test-${name}" {} ''
    mkdir -p $out
    cat > $out/result <<'EOF'
    ${builtins.toJSON value}
    EOF
  '';

  exports = pkgs.runCommand "test-exports" {} ''
    set -eu
    root=$TMPDIR/repo
    mkdir -p "$root/scripts/desktop" "$root/scripts/nix" "$root/src/Simplex/Chat"
    install -m 0755 ${../desktop/test-exports.sh} "$root/scripts/desktop/test-exports.sh"
    install -m 0755 ${../desktop/check-exports.sh} "$root/scripts/desktop/check-exports.sh"
    install -m 0755 ${../desktop/build-lib-linux.sh} "$root/scripts/desktop/build-lib-linux.sh"
    install -m 0755 ${../desktop/build-lib-mac.sh} "$root/scripts/desktop/build-lib-mac.sh"
    install -m 0755 ${../desktop/build-lib-windows.sh} "$root/scripts/desktop/build-lib-windows.sh"
    install -m 0644 ${./chat-exports.txt} "$root/scripts/nix/chat-exports.txt"
    install -m 0644 ${../../src/Simplex/Chat/Mobile.hs} "$root/src/Simplex/Chat/Mobile.hs"
    install -m 0644 ${../../libsimplex.dll.def} "$root/libsimplex.dll.def"
    bash "$root/scripts/desktop/test-exports.sh"
    mkdir -p $out
    echo ok > $out/result
  '';

  linkMobile = pkgs.runCommand "test-link-mobile-library" {
    nativeBuildInputs = [ pkgs.python3 ];
  } ''
    set -eu
    dir=$TMPDIR/scripts
    mkdir -p "$dir"
    install -m 0755 ${./link-mobile-library.sh} "$dir/link-mobile-library.sh"
    install -m 0644 ${./test-link-mobile-library.py} "$dir/test-link-mobile-library.py"
    python3 "$dir/test-link-mobile-library.py"
    mkdir -p $out
    echo ok > $out/result
  '';

  windowsFsScript = pkgs.runCommand "test-windows-fs-script" {
    nativeBuildInputs = [ pkgs.python3 ];
  } ''
    set -eu
    dir=$TMPDIR/scripts
    mkdir -p "$dir/windows-fs"
    install -m 0644 ${./test-windows-fs.py} "$dir/test-windows-fs.py"
    install -m 0644 ${./check-windows-fs.nix} "$dir/check-windows-fs.nix"
    install -m 0755 ${./windows-fs/run.sh} "$dir/windows-fs/run.sh"
    install -m 0644 ${./windows-fs/Probe.c} "$dir/windows-fs/Probe.c"
    python3 "$dir/test-windows-fs.py"
    mkdir -p $out
    echo ok > $out/result
  '';

  mingwCC = pkgs.pkgsCross.mingwW64.stdenv.cc;
  windowsPackaging = pkgs.runCommand "test-windows-packaging" {
    nativeBuildInputs = [ pkgs.python3 pkgs.nix pkgs.zip pkgs.binutils pkgs.bash ];
    requiredSystemFeatures = [ "recursive-nix" ];
  } ''
    set -eu
    root=$TMPDIR/repo
    mkdir -p "$root/scripts/nix"
    install -m 0644 ${../../flake.nix} "$root/flake.nix"
    install -m 0644 ${./package-windows.nix} "$root/scripts/nix/package-windows.nix"
    install -m 0644 ${./test-windows-packaging.py} "$root/scripts/nix/test-windows-packaging.py"
    python3 "$root/scripts/nix/test-windows-packaging.py" \
      --cc ${mingwCC}/bin/${mingwCC.targetPrefix}gcc \
      --output $out
  '';

  cli = nativeProject.simplex-chat.components.exes.simplex-chat;
  smoke = pkgs.runCommand "test-smoke-cli" {
    nativeBuildInputs = [ pkgs.python3 ];
  } ''
    set -eu
    python3 ${./smoke-cli.py} ${cli}/bin/simplex-chat
    mkdir -p $out
    echo ok > $out/result
  '';

  cabalTest = haskellLib.check nativeProject.simplex-chat.components.tests.simplex-chat-test;

  xcodetest = pkgs.runCommand "xcodetest" {
    nativeBuildInputs = [ pkgs.python3 pkgs.bash ];
  } ''
    set -eu
    bash ${./xcodetest.sh} ${source} $out
  '';

  # These asserts do not depend on the build host. One Hydra system runs them.
  scope = lib.optionalAttrs (system == "x86_64-linux") {
    "test:android-iserv-scope" = record "android-iserv-scope" (import ./check-android-iserv-scope.nix);
    "test:android-pthread-scope" = record "android-pthread-scope" (import ./check-android-pthread-scope.nix);
    "test:mingw-stdio-scope" = record "mingw-stdio-scope" (import ./check-mingw-stdio-scope.nix);
    "test:musl-iserv-scope" = record "musl-iserv-scope" (import ./check-musl-iserv-scope.nix { inherit lib; });
    "test:musl-rts-scope" = record "musl-rts-scope" (import ./check-musl-rts-scope.nix { inherit lib haskellNix; });
    "test:windows-rts-scope" = record "windows-rts-scope" (import ./check-windows-rts-scope.nix { inherit lib; });
    "test:windows-crypt32" = record "windows-crypt32" (import ./check-windows-crypt32.nix { inherit lib haskellNix; });
    "test:project-ghc-options" = record "project-ghc-options" (import ./check-project-ghc-options.nix { inherit lib haskellNix; });
    "test:darwin-bitvec-runtime" = record "darwin-bitvec-runtime" (import ./check-darwin-bitvec-runtime.nix);
    "test:darwin-bitvec-resolver" = record "darwin-bitvec-resolver" (import ./check-darwin-bitvec-resolver.nix { inherit lib haskellNix; });
  };

  linuxHeavy = lib.optionalAttrs (system == "x86_64-linux") {
    "test:windows-fs" = import ./check-windows-fs.nix { inherit pkgs; };
    "test:windows-fs-script" = windowsFsScript;
    "test:windows-blst-linkage" = import ./check-windows-blst-linkage.nix { inherit pkgs; };
    "test:windows-packaging" = windowsPackaging;
    "test:ghc-elf-comdat-functions" = import ./check-elf-comdat-functions.nix {
      inherit pkgs compiler;
      patch = haskellNix + "/overlays/patches/ghc914-rts-elf-comdat-functions.patch";
    };
    "test:ghc-elf-alignment" = import ./check-elf-alignment.nix { inherit pkgs compiler; };
    "test:android-personality" = import ./check-android-personality.nix {
      inherit system haskellNix nativePackages;
    };
  };

  native = {
    "test:exports" = exports;
    "test:link-mobile-library" = linkMobile;
    "test:simplex-chat" = cabalTest;
    "test:smoke-cli" = smoke;
  };

  darwin = lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
    xcodetest = xcodetest;
    "test:ios-archive-converter" = iosArchiveConverter;
  } // lib.optionalAttrs (system == "x86_64-darwin") {
    "test:ghc-macho-info-tables" = import ./check-macho-info-tables.nix {
      inherit pkgs compiler;
    };
  };
in scope // linuxHeavy // native // darwin
