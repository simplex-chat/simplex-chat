{
  description = "nix flake for simplex-chat";
  # hkm/stable-haskell: use the same compiler and boot libraries on every target.
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/fd96dff55bcf6f81ccb96872f16a2ec3970edab0";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs";
  inputs.mac2ios.url = "github:zw3rk/mobile-core-tools";
  inputs.hackage = {
    # Match the compiler integration's package index to reuse its cached builds.
    url = "github:input-output-hk/hackage.nix/dc08bb4627efd6ff85d421b6dde5f52ada218afa";
    flake = false;
  };
  inputs.haskellNix.inputs.hackage.follows = "hackage";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, haskellNix, nixpkgs, flake-utils, mac2ios, ... }:
    let systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ]; in
    flake-utils.lib.eachSystem systems (system:
      # Preserve Android 8 as the minimum native API for both architectures.
      let android26 = final: prev: {
        pkgsCross = prev.pkgsCross // {
          aarch64-android-prebuilt = import prev.path {
            inherit system;
            overlays = prev.overlays ++ [ (import ./scripts/nix/android-iconv-overlay.nix) ];
            # Permit the SDK/NDK's unfree packages only in these Android sets.
            # Some SDK fetchers use unprefixed names such as platform-tools.
            config = prev.config // { allowUnfree = true; };
            crossSystem = prev.lib.systems.examples.aarch64-android-prebuilt // { androidSdkVersion = "26"; };
          };
          armv7a-android-prebuilt = import prev.path {
            inherit system;
            overlays = prev.overlays ++ [ (import ./scripts/nix/android-iconv-overlay.nix) ];
            # Permit the SDK/NDK's unfree packages only in these Android sets.
            # Some SDK fetchers use unprefixed names such as platform-tools.
            config = prev.config // { allowUnfree = true; };
            crossSystem = prev.lib.systems.examples.armv7a-android-prebuilt // { androidSdkVersion = "26"; };
          };
        };
      }; in
      # `appendOverlays` with a singleton is identical to `extend`.
      let nativePackages = import ./scripts/nix/native-packages.nix { inherit haskellNix; }; in
      let compilerCache = import ./scripts/nix/compiler-cache-overlay.nix { inherit haskellNix; }; in
      let androidEmulator = import ./scripts/nix/android-emulator-overlay.nix { inherit haskellNix; }; in
      let muslIserv = import ./scripts/nix/musl-iserv-overlay.nix; in
      let windowsRts = import ./scripts/nix/windows-rts-overlay.nix; in
      let mingwStdio = import ./scripts/nix/mingw-stdio-overlay.nix; in
      let pkgs = (nativePackages system).appendOverlays [android26 compilerCache androidEmulator muslIserv windowsRts mingwStdio]; in
      let compilerSelection = import ./scripts/nix/compiler-selection.nix { inherit haskellNix; }; in
      let mobileRuntime = import ./scripts/nix/mobile-runtime.nix { inherit pkgs compilerSelection; }; in
      let drv' = { extra-modules, pkgs', package-flags ? {}, package-ghc-options ? {}, mobile ? false, ... }:
      let compilerNixName = "ghc914-sh";
      packageFlags = pkgs.lib.recursiveUpdate {
        direct-sqlcipher = { openssl = true; commoncrypto = false; tomcrypt = false; };
      } package-flags;
      projectGhcOptions = import ./scripts/nix/project-ghc-options.nix {
        inherit (pkgs) lib;
        hostPlatform = pkgs'.stdenv.hostPlatform;
        packageOptions = package-ghc-options;
      };
      packageGhcOptions = projectGhcOptions.packages;
      # Cabal 3.17 drops repeated option names in hsc2hs arguments. The
      # Darwin archives need both -framework Security and CoreFoundation.
      projectPkgs = if mobile && pkgs'.stdenv.hostPlatform.isDarwin
        then pkgs'.appendOverlays [ (import ./scripts/nix/cabal-tool-overlay.nix) ]
        else pkgs';
      in projectPkgs.haskell-nix.project ({ config, ... }:
      let
        # Keep the patched Windows x509 metadata in the original plan too;
        # post-plan Cabal edits can change native package unit IDs.
        androidPthreads = import ./scripts/nix/android-pthread-libraries.nix {
          pkgs = config.evalPackages;
          hostPlatform = pkgs'.stdenv.hostPlatform;
        };
        darwinBitvec = import ./scripts/nix/darwin-bitvec-runtime.nix {
          pkgs = config.evalPackages;
          hostPlatform = pkgs'.stdenv.hostPlatform;
          cc = pkgs'.stdenv.cc;
        };
        patchedPackageSources = pkgs.lib.optional (
          config.compiler-nix-name == "ghc914-sh"
          && pkgs'.stdenv.buildPlatform.config != pkgs'.stdenv.hostPlatform.config
        ) (import ./scripts/nix/composition-source.nix { pkgs = config.evalPackages; })
        ++ pkgs.lib.optional pkgs'.stdenv.hostPlatform.isWindows
          (import ./scripts/nix/windows-x509-source.nix { pkgs = config.evalPackages; inherit haskellNix; })
        ++ androidPthreads.packageSources
        ++ darwinBitvec.packageSources;
      in {
        compiler-nix-name = compilerNixName;
        inherit compilerSelection;
        # We need this, to specify we want the cabal project.
        # If the stack.yaml was dropped, this would not be necessary.
        projectFileName = "cabal.project";
        # The v2 builder follows planned flags for source-repository packages.
        # Render the same flags before planning and into the package modules.
        cabalProjectLocal = pkgs.lib.concatStringsSep "\n" (
          pkgs.lib.mapAttrsToList (package: flags: ''
            package ${package}
              flags: ${pkgs.lib.concatStringsSep " " (
                pkgs.lib.mapAttrsToList (flag: enabled: "${if enabled then "+" else "-"}${flag}") flags
              )}
          '') packageFlags
        ) + "\n" + pkgs.lib.concatStringsSep "\n" (
          pkgs.lib.mapAttrsToList (package: options: ''
            package ${package}
              ghc-options: ${pkgs.lib.concatStringsSep " " options}
          '') packageGhcOptions
        ) + pkgs.lib.optionalString pkgs'.stdenv.hostPlatform.isDarwin ''
          -- The compiler's ghc-internal package omits iconv's library directory.
          package *
            extra-lib-dirs: ${pkgs'.libiconv}/lib
        '' + pkgs.lib.optionalString (patchedPackageSources != []) ''
          -- composition deliberately avoids base, but GHC 9.14's wired-in
          -- function types still need ghc-internal in a cross slice's DB.
          -- Patch before planning so the v2 slice declares the same dependency.
          packages: ${pkgs.lib.concatStringsSep " " (map toString patchedPackageSources)}
        '' + androidPthreads.cabalProjectLocal + darwinBitvec.cabalProjectLocal + pkgs.lib.optionalString (mobile && pkgs'.stdenv.hostPlatform.isDarwin && pkgs'.stdenv.hostPlatform.isAarch64) ''
          -- Keep every app C/C++ stub compatible with the same iOS ARM baseline.
          package *
            ghc-options: -optc-mcpu=apple-a7 -optc-march=armv8-a+norcpc -optcxx-mcpu=apple-a7 -optcxx-march=armv8-a+norcpc
        '';
        src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
          name = "simplex-chat";
          src = pkgs.haskell-nix.haskellLib.cleanGit {
            name = "simplex-chat";
            src = ./.;
          };
          # This generated website asset is deployed separately, not as Cabal
          # data. Its checkout symlink is dangling until the website is built,
          # while the v2 source packager dereferences every included symlink.
          filter = path: type: !(type == "symlink" && pkgs.lib.hasSuffix
            "/apps/simplex-directory-service/web/directory.js" (toString path));
        };
        sha256map = import ./scripts/nix/sha256map.nix;
        # The v2 builder compiles source-repository packages from a separate
        # minimal Git repository, which currently skips package.patches.
        # Patch the input before planning so both source paths agree.
        inputMap = import ./scripts/nix/source-repository-patches.nix {
          pkgs = config.evalPackages;
          cabalProject = builtins.readFile ./cabal.project;
          sha256map = import ./scripts/nix/sha256map.nix;
          patchesByUrl = {
            "https://github.com/simplex-chat/direct-sqlcipher.git" =
              if pkgs'.stdenv.hostPlatform.isWindows
              then [ ./scripts/nix/direct-sqlcipher-2.3.27-win.patch ]
              else [ ./scripts/nix/direct-sqlcipher-2.3.27.patch ]
                ++ pkgs.lib.optional pkgs'.stdenv.hostPlatform.isAndroid
                  ./scripts/nix/direct-sqlcipher-android-log.patch
                ++ androidPthreads.directSqlcipherPatches
                ++ pkgs.lib.optional pkgs'.stdenv.hostPlatform.isMusl
                  ./scripts/nix/direct-sqlcipher-static-libraries.patch;
          } // pkgs.lib.optionalAttrs pkgs'.stdenv.hostPlatform.isDarwin {
            "https://github.com/simplex-chat/simplexmq.git" =
              [ ./scripts/nix/simplexmq-blst-darwin-extern.patch ];
          } // pkgs.lib.optionalAttrs pkgs'.stdenv.hostPlatform.isWindows {
            "https://github.com/simplex-chat/simplexmq.git" =
              [ ./scripts/nix/simplexmq-blst-windows-extern.patch ];
          } // pkgs.lib.optionalAttrs pkgs'.stdenv.hostPlatform.isMusl {
            # -fully-static uses each producer's registered static C libraries.
            "https://github.com/simplex-chat/simplexmq.git" =
              [ ./scripts/nix/simplexmq-static-libraries.patch ];
          };
        };
        modules = [
          { packages = pkgs.lib.recursiveUpdate
              (pkgs.lib.mapAttrs (_: flags: { inherit flags; }) packageFlags)
              (pkgs.lib.mapAttrs (_: ghcOptions: { inherit ghcOptions; }) packageGhcOptions);
          }
          ({ pkgs, lib, config, ... }:
            {
              # Package-specific lists carry the same common options explicitly.
              ghcOptions = lib.mkDefault projectGhcOptions.common;
            }
          )

          ] ++ darwinBitvec.modules ++ [
            (import ./scripts/nix/windows-crypt32.nix {
              hostPlatform = pkgs'.stdenv.hostPlatform;
              inherit compilerNixName;
            })
          ] ++ pkgs.lib.optional pkgs'.stdenv.hostPlatform.isWindows ({ lib, ... }: {
            # The preplanned local source already applies the modern patch.
            # Avoid applying it again after v2 source packaging.
            packages.crypton-x509-system.patches = lib.mkForce [];
            # HANDLE needs a signed pointer-width constant. Convert via Int:
            # Basement's RebindableSyntax has no HasNegation IntPtr instance.
            packages.basement.patches = lib.mkAfter [ ./scripts/nix/basement-windows-intptr.patch ];
          }) ++ pkgs.lib.optional pkgs'.stdenv.hostPlatform.isAndroid ({ lib, ... }: {
            # SplitMix's Unix initializer uses getentropy, available from API 28.
            # Keep API 26 builds on Bionic's established arc4random_buf API.
            packages.splitmix.patches = lib.mkAfter [ ./scripts/nix/splitmix-android-api.patch ];
          }) ++ pkgs.lib.optional pkgs'.stdenv.hostPlatform.isAndroid mobileRuntime.androidModule ++ extra-modules;
      }); in
      # by defualt we don't need to pass extra-modules.
      let drv = pkgs': drv' { extra-modules = []; inherit pkgs'; }; in
      let inherit (import ./scripts/nix/mobile-library.nix { inherit pkgs; })
        withComponentPhases withMobileLibrary; in
      let chatExports = import ./scripts/nix/chat-exports.nix; in
      # This will package up all *.a in $out into a pkg.zip that can
      # be downloaded from hydra.
      let withHydraLibPkg = pkg: pkg.overrideAttrs (old: {
        postInstall = ''
          mkdir -p $out/_pkg
          find $out/lib -name "*.a" -exec cp {} $out/_pkg \;
          (cd $out/_pkg; ${pkgs.zip}/bin/zip -r -9 $out/pkg.zip *)
          rm -fR $out/_pkg
          mkdir -p $out/nix-support
          echo "file binary-dist \"$(echo $out/*.zip)\"" \
              > $out/nix-support/hydra-build-products
        '';
      }); in
      let darwinOpenSSL = mobileRuntime.darwinCLibrary (pkgs.openssl.override { static = true; }); in
      let iosPostInstall = bundleName: ''
        ${pkgs.tree}/bin/tree $out
        mkdir tmp
        find ./dist -name "libHS*-ghc*.a" -exec cp {} tmp \;
        (cd tmp; ${pkgs.tree}/bin/tree .; ar x libHS*.a; for o in *.o; do if /usr/bin/otool -xv $o|grep ldadd ; then echo $o; fi; done; cd ..; rm -fR tmp)
        mkdir -p $out/_pkg
        # copy over includes, we might want those, but maybe not.
        # cp -r $out/lib/*/*/include $out/_pkg/
        # find the libHS...ghc-X.Y.Z.a static library; this is the
        # rolled up one with all dependencies included.
        find ./dist -name "libHS*.a" -exec cp {} $out/_pkg \;
        find ${mobileRuntime.darwinCLibrary (pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; }))}/lib -name "*.a" -exec cp {} $out/_pkg \;
        find ${mobileRuntime.darwinCLibrary (pkgs.gmp6.override { withStatic = true; })}/lib -name "*.a" -exec cp {} $out/_pkg \;
        # There is no static libc
        ${pkgs.tree}/bin/tree $out/_pkg
        for pkg in $out/_pkg/*.a; do
          chmod +w $pkg
          ${mac2ios.packages.${system}.mac2ios}/bin/mac2ios $pkg
          [[ "$pkg" == *simplex-chat* ]] && ${pkgs.stdenv.cc.targetPrefix}strip -x $pkg
          chmod -w $pkg
        done

        mkdir tmp
        find $out/_pkg -name "libHS*-ghc*.a" -exec cp {} tmp \;
        (cd tmp; ${pkgs.tree}/bin/tree .; ar x libHS*.a; for o in *.o; do if /usr/bin/otool -xv $o|grep ldadd ; then echo $o; fi; done; cd ..; rm -fR tmp)

        sha256sum $out/_pkg/*.a

        (cd $out/_pkg; ${pkgs.zip}/bin/zip -r -9 $out/${bundleName}.zip *)
        rm -fR $out/_pkg
        mkdir -p $out/nix-support
        echo "file binary-dist \"$(echo $out/*.zip)\"" \
            > $out/nix-support/hydra-build-products
      ''; in
      let iosOverrides = bundleName: {
        kind = "ios";
        packageDbs = mobileRuntime.darwinPackageDbs;
        postInstall = iosPostInstall bundleName;
      }; in
      rec {
        packages = {
            "lib:simplex-chat" = (drv pkgs).simplex-chat.components.library;
            "exe:simplex-chat" = (drv pkgs).simplex-chat.components.exes.simplex-chat;
        } // ({
            "x86_64-linux" =
              let
                  androidPkgs = pkgs.pkgsCross.aarch64-android-prebuilt;
                  android32Pkgs = pkgs.pkgsCross.armv7a-android-prebuilt;
                  # Similarly to icovn, for reasons beyond my current knowledge, nixpkgs andorid
                  # toolchain makes configure believe we have MEMFD_CREATE, which we don't in
                  # sdk 23.
                  androidFFI = androidPkgs.libffi.overrideAttrs (old: {
                      dontDisableStatic = true;
                      hardeningDisable = [ "fortify" ];
                  });
                  android32FFI = android32Pkgs.libffi.overrideAttrs (old: {
                      dontDisableStatic = true;
                      hardeningDisable = [ "fortify" ];
                  }
              );in {
              # STATIC x86_64-linux
              "${pkgs.pkgsCross.musl64.hostPlatform.system}-static:exe:simplex-chat" = (drv pkgs.pkgsCross.musl64).simplex-chat.components.exes.simplex-chat;
              # STATIC i686-linux
              "${pkgs.pkgsCross.musl32.hostPlatform.system}-static:exe:simplex-chat" = (drv' {
                pkgs' = pkgs.pkgsCross.musl32;
                extra-modules = [{
                  # 32 bit patches
                  packages.basement.patches = [
                    ./scripts/nix/basement-pr-573.patch
                  ];
                  packages.memory.patches = [
                    ./scripts/nix/memory-pr-99.patch
                  ];
                }];
              }).simplex-chat.components.exes.simplex-chat;
              # WINDOWS x86_64-mingwW64
              "${pkgs.pkgsCross.mingwW64.hostPlatform.system}:exe:simplex-chat" = withComponentPhases (drv' {
                pkgs' = pkgs.pkgsCross.mingwW64;
                package-flags = {
                  direct-sqlcipher.openssl = true;
                  bitvec.simd = false;
                };
                extra-modules = [{
                  packages.direct-sqlcipher.components.library.libs = pkgs.lib.mkForce [
                    (pkgs.pkgsCross.mingwW64.openssl) #.override) # { static = true; enableKTLS = false; })
                  ];
                  packages.simplexmq.components.library.libs = pkgs.lib.mkForce [
                    (pkgs.pkgsCross.mingwW64.openssl) #.override) # { static = true; enableKTLS = false; })
                  ];
                  packages.unix-time.postPatch = ''
                    sed -i 's/mingwex//g' unix-time.cabal
                  '';
                 }];
                }).simplex-chat.components.exes.simplex-chat (old: {
                installPhase = old.installPhase + ''
                  set -x
                  ${pkgs.tree}/bin/tree $out
                  mkdir -p $out/_pkg
                  cp $out/bin/* $out/_pkg
                  ${pkgs.tree}/bin/tree $out/_pkg
                  (cd $out/_pkg; ${pkgs.zip}/bin/zip -r -9 $out/${pkgs.pkgsCross.mingwW64.hostPlatform.system}-simplex-chat.zip *)
                  rm -fR $out/_pkg
                  mkdir -p $out/nix-support
                  echo "file binary-dist \"$(echo $out/*.zip)\"" \
                      > $out/nix-support/hydra-build-products
                '';
              });
              "${pkgs.pkgsCross.mingwW64.hostPlatform.system}:lib:simplex-chat" = withMobileLibrary (drv' rec {
                pkgs' = pkgs.pkgsCross.mingwW64;
                package-flags = {
                  direct-sqlcipher.openssl = true;
                  # simd will try to read __cpu_model, which we don't expose
                  # from the rts (yet!).
                  bitvec.simd = false;
                  simplex-chat.client_library = true;
                  simplexmq.client_library = true;
                };
                extra-modules = [{
                  packages.direct-sqlcipher.components.library.libs = pkgs.lib.mkForce [
                    pkgs.pkgsCross.mingwW64.openssl
                  ];
                  packages.simplexmq.components.library.libs = pkgs.lib.mkForce [
                    pkgs.pkgsCross.mingwW64.openssl
                  ];
                  packages.unix-time.postPatch = ''
                    sed -i 's/mingwex//g' unix-time.cabal
                  '';
                }];
              }).simplex-chat.components.library
              {
                kind = "windows";
                linkOptions = [
                  "-optl-lffi"
                  "-optl-Wl,--out-implib,libsimplex.dll.a"
                  "${./libsimplex.dll.def}"
                ];
                postInstall = ''
                  set -x
                  function deps() {
                    ${pkgs.binutils}/bin/strings "$1" | grep '.\.dll'|grep -v -E 'Winsock|ADVAPI32|dbghelp|KERNEL32|msvcrt|ntdll|ole32|RPCRT4|SHELL32|USER32|WINMM|WS2_32|kernel32|GDI32'|grep -v "$1"
                  }
                  ${pkgs.tree}/bin/tree $out
                  mkdir -p $out/_pkg
                  cp libsimplex.dll $out/_pkg
                  cp libsimplex.dll.a $out/_pkg
                  mkdir $out/libs
                  find ${pkgs.lib.getBin pkgs.pkgsCross.mingwW64.openssl}             -name "*.dll" -exec cp {} $out/libs \;
                  find ${pkgs.lib.getBin pkgs.pkgsCross.mingwW64.libffi}              -name "*.dll" -exec cp {} $out/libs \;
                  find ${pkgs.lib.getBin pkgs.pkgsCross.mingwW64.gmp}                 -name "*.dll" -exec cp {} $out/libs \;
                  find ${pkgs.lib.getBin pkgs.pkgsCross.mingwW64.stdenv.cc.cc}        -name "*.dll" -exec cp {} $out/libs \;
                  find ${pkgs.lib.getBin pkgs.pkgsCross.mingwW64.windows.mcfgthreads} -name "*.dll" -exec cp {} $out/libs \;

                  pushd $out/_pkg
                  function copyDeps() {
                    for dep in $(deps "$1"); do
                      if [ ! -f "$dep" ]; then
                        if [ ! -f ../libs/"$dep" ]; then
                          echo "WARN: $1 -> $dep not found!"
                        else
                          cp ../libs/"$dep" .
                          copyDeps "$dep"
                        fi
                      fi
                    done
                  }
                  copyDeps libsimplex.dll
                  popd
                  ${pkgs.tree}/bin/tree $out/_pkg
                  (cd $out/_pkg; ${pkgs.zip}/bin/zip -r -9 $out/pkg-${pkgs.pkgsCross.mingwW64.hostPlatform.system}-libsimplex.zip *)
                  rm -fR $out/_pkg
                  mkdir -p $out/nix-support
                  echo "file binary-dist \"$(echo $out/*.zip)\"" \
                      > $out/nix-support/hydra-build-products
                '';
              };
              # "${pkgs.pkgsCross.muslpi.hostPlatform.system}-static:exe:simplex-chat" = (drv pkgs.pkgsCross.muslpi).simplex-chat.components.exes.simplex-chat;

              # STATIC aarch64-linux
              "${pkgs.pkgsCross.aarch64-multiplatform-musl.hostPlatform.system}-static:exe:simplex-chat" = (drv pkgs.pkgsCross.aarch64-multiplatform-musl).simplex-chat.components.exes.simplex-chat;
              "armv7a-android:lib:support" = withMobileLibrary (drv android32Pkgs).android-support.components.library {
                kind = "android-support";
                packageName = "android-support";
                linkOptions = [
                  "-optl-Wl,-u,setLineBuffering"
                  "-optl-Wl,-u,pipe_std_to_socket"
                ];

                postInstall = ''

                  mkdir -p $out/_pkg
                  cp libsupport.so $out/_pkg
                  ${pkgs.patchelf}/bin/patchelf --remove-needed libunwind.so.1 $out/_pkg/libsupport.so
                  (cd $out/_pkg; ${pkgs.zip}/bin/zip -r -9 $out/pkg-armv7a-android-libsupport.zip *)
                  rm -fR $out/_pkg

                  mkdir -p $out/nix-support
                  echo "file binary-dist \"$(echo $out/*.zip)\"" \
                        > $out/nix-support/hydra-build-products
                '';
              };
              # The android-support package is at
              # https://github.com/simplex-chat/android-support
              "aarch64-android:lib:support" = withMobileLibrary (drv androidPkgs).android-support.components.library {
                kind = "android-support";
                packageName = "android-support";
                linkOptions = [
                  "-optl-Wl,-u,setLineBuffering"
                  "-optl-Wl,-u,pipe_std_to_socket"
                  "-optl-Wl,-z,max-page-size=16384"
                ];

                postInstall = ''

                  mkdir -p $out/_pkg
                  cp libsupport.so $out/_pkg
                  ls -lah $out/_pkg/*
                  ${pkgs.patchelf}/bin/patchelf --remove-needed libunwind.so.1 $out/_pkg/libsupport.so
                  (cd $out/_pkg; ${pkgs.zip}/bin/zip -r -9 $out/pkg-aarch64-android-libsupport.zip *)
                  rm -fR $out/_pkg

                  mkdir -p $out/nix-support
                  echo "file binary-dist \"$(echo $out/*.zip)\"" \
                        > $out/nix-support/hydra-build-products
                '';
              };
              "armv7a-android:lib:simplex-chat" = withMobileLibrary (drv' {
                pkgs' = android32Pkgs;
                package-flags = {
                  text.simdutf = false;
                  direct-sqlcipher.openssl = true;
                  simplex-chat.client_library = true;
                  simplexmq.client_library = true;
                };
                extra-modules = [{
                  packages.direct-sqlcipher.components.library.libs = pkgs.lib.mkForce [
                    (android32Pkgs.openssl.override { static = true; enableKTLS = false; })
                  ];
                  packages.simplexmq.components.library.libs = pkgs.lib.mkForce [
                    (android32Pkgs.openssl.override { static = true; enableKTLS = false; })
                  ];
                  # 32 bit patches
                  packages.basement.patches = [
                    ./scripts/nix/basement-pr-573.patch
                  ];
                  packages.memory.patches = [
                    ./scripts/nix/memory-pr-99.patch
                  ];
                }];
              }).simplex-chat.components.library {
                kind = "android";
                # Link the threaded RTS only at final link, outside TH/iserv.
                # GHC 9.14's RTS package declares its bundled libffi archive.
                linkOptions = map (sym: "-optl-Wl,-u,${sym}") chatExports;
                postInstall = ''
                  set -x
                  ${pkgs.tree}/bin/tree $out
                  mkdir -p $out/_pkg
                  # copy over includes, we might want those, but maybe not.
                  # cp -r $out/lib/*/*/include $out/_pkg/
                  # find the libHS...ghc-X.Y.Z.a static library; this is the
                  # rolled up one with all dependencies included.
                  cp libsimplex.so $out/_pkg
                  # find ./dist -name "lib*.so" -exec cp {} $out/_pkg \;
                  # find ./dist -name "libHS*-ghc*.a" -exec cp {} $out/_pkg \;
                  # find ${android32FFI}/lib -name "*.a" -exec cp {} $out/_pkg \;
                  # find ${android32Pkgs.gmp6.override { withStatic = true; }}/lib -name "*.a" -exec cp {} $out/_pkg \;
                  # find ${android32Pkgs.stdenv.cc.libc}/lib -name "*.a" -exec cp {} $out/_pkg \;
                  echo ${android32Pkgs.openssl.override { enableKTLS = false; }}
                  find ${(android32Pkgs.openssl.override { enableKTLS = false; }).out}/lib -name "*.so" -exec cp {} $out/_pkg \;

                  # remove the .1 and other version suffixes from .so's. Androids linker
                  # doesn't play nice with them.
                  for lib in $out/_pkg/*.so; do
                    for dep in $(${pkgs.patchelf}/bin/patchelf --print-needed "$lib"); do
                      if [[ "''${dep##*.so}" ]]; then
                        echo "$lib : $dep -> ''${dep%%.so*}.so"
                        chmod +w "$lib"
                        ${pkgs.patchelf}/bin/patchelf --replace-needed "$dep" "''${dep%%.so*}.so" "$lib"
                      fi
                    done
                  done

                  for lib in $out/_pkg/*.so; do
                    chmod +w "$lib"
                    ${pkgs.patchelf}/bin/patchelf --remove-needed libunwind.so "$lib"
                    [[ "$lib" != *libsimplex.so ]] && ${pkgs.patchelf}/bin/patchelf --set-soname "$(basename -a $lib)" "$lib"
                  done

                  ${pkgs.tree}/bin/tree $out/_pkg

                  # Strip from debug symbols
                  find "$out/_pkg" -type f -name "*.so" -exec ${android32Pkgs.stdenv.cc.targetPrefix}strip --strip-unneeded {} +

                  # Normalize permissions + timestamps
                  find "$out/_pkg" -type f -exec chmod 644 {} +
                  find "$out/_pkg" -type d -exec chmod 755 {} +
                  find "$out/_pkg" -exec touch -h -d '@1764547200' {} +

                  (cd $out/_pkg; ${pkgs.zip}/bin/zip -r -9 -X $out/pkg-armv7a-android-libsimplex.zip *)
                  rm -fR $out/_pkg
                  mkdir -p $out/nix-support
                  echo "file binary-dist \"$(echo $out/*.zip)\"" \
                      > $out/nix-support/hydra-build-products
                '';
              };
              "aarch64-android:lib:simplex-chat" = withMobileLibrary (drv' {
                pkgs' = androidPkgs;
                package-flags = {
                  text.simdutf = false;
                  direct-sqlcipher.openssl = true;
                  simplex-chat.client_library = true;
                  simplexmq.client_library = true;
                };
                extra-modules = [{
                  packages.direct-sqlcipher.components.library.libs = pkgs.lib.mkForce [
                    (androidPkgs.openssl.override { static = true; })
                  ];
                  packages.simplexmq.components.library.libs = pkgs.lib.mkForce [
                    (androidPkgs.openssl.override { static = true; })
                  ];
                }];
              }).simplex-chat.components.library {
                kind = "android";
                # Link the threaded RTS only at final link, outside TH/iserv.
                # GHC 9.14's RTS package declares its bundled libffi archive.
                linkOptions = [
                  "-optl-Wl,-z,max-page-size=16384"
                ] ++ map (sym: "-optl-Wl,-u,${sym}") chatExports;
                postInstall = ''
                  set -x
                  ${pkgs.tree}/bin/tree $out
                  mkdir -p $out/_pkg
                  # copy over includes, we might want those, but maybe not.
                  # cp -r $out/lib/*/*/include $out/_pkg/
                  # find the libHS...ghc-X.Y.Z.a static library; this is the
                  # rolled up one with all dependencies included.
                  cp libsimplex.so $out/_pkg
                  # find ./dist -name "lib*.so" -exec cp {} $out/_pkg \;
                  # find ./dist -name "libHS*-ghc*.a" -exec cp {} $out/_pkg \;
                  # find ${androidFFI}/lib -name "*.a" -exec cp {} $out/_pkg \;
                  # find ${androidPkgs.gmp6.override { withStatic = true; }}/lib -name "*.a" -exec cp {} $out/_pkg \;
                  # find ${androidPkgs.stdenv.cc.libc}/lib -name "*.a" -exec cp {} $out/_pkg \;
                  echo ${androidPkgs.openssl}
                  find ${androidPkgs.openssl.out}/lib -name "*.so" -exec cp {} $out/_pkg \;

                  # remove the .1 and other version suffixes from .so's. Androids linker
                  # doesn't play nice with them.
                  for lib in $out/_pkg/*.so; do
                    for dep in $(${pkgs.patchelf}/bin/patchelf --print-needed "$lib"); do
                      if [[ "''${dep##*.so}" ]]; then
                        echo "$lib : $dep -> ''${dep%%.so*}.so"
                        chmod +w "$lib"
                        ${pkgs.patchelf}/bin/patchelf --replace-needed "$dep" "''${dep%%.so*}.so" "$lib"
                      fi
                    done
                  done

                  for lib in $out/_pkg/*.so; do
                    chmod +w "$lib"
                    ${pkgs.patchelf}/bin/patchelf --remove-needed libunwind.so "$lib"
                    [[ "$lib" != *libsimplex.so ]] && ${pkgs.patchelf}/bin/patchelf --set-soname "$(basename -a $lib)" "$lib"
                  done

                  ${pkgs.tree}/bin/tree $out/_pkg

                  # Strip from debug symbols
                  find "$out/_pkg" -type f -name "*.so" -exec ${androidPkgs.stdenv.cc.targetPrefix}strip --strip-unneeded {} +

                  # Normalize permissions + timestamps
                  find "$out/_pkg" -type f -exec chmod 644 {} +
                  find "$out/_pkg" -type d -exec chmod 755 {} +
                  find "$out/_pkg" -exec touch -h -d '@1764547200' {} +

                  (cd $out/_pkg; ${pkgs.zip}/bin/zip -r -9 -X $out/pkg-aarch64-android-libsimplex.zip *)
                  rm -fR $out/_pkg
                  mkdir -p $out/nix-support
                  echo "file binary-dist \"$(echo $out/*.zip)\"" \
                      > $out/nix-support/hydra-build-products
                '';
              };
            };

            # builds for iOS and iOS simulator
            "aarch64-darwin" = {
              # aarch64-darwin iOS build (to be patched with mac2ios)
              "aarch64-darwin-ios:lib:simplex-chat" = withMobileLibrary (drv' {
                pkgs' = pkgs;
                mobile = true;
                package-ghc-options.simplex-chat = [ "-DIOS" ];
                package-flags = {
                  simplex-chat.swift = true;
                  simplexmq.swift = true;
                  direct-sqlcipher.commoncrypto = true;
                  direct-sqlcipher.openssl = false;
                  simplexmq.commoncrypto = true;
                  entropy.DoNotGetEntropy = true;
                  simplex-chat.client_library = true;
                  simplexmq.client_library = true;
                };
                extra-modules = [{
                  packages.simplexmq.components.library.libs = pkgs.lib.mkForce [
                    # TODO: have a cross override for iOS, that sets this.
                    darwinOpenSSL
                  ];
                }];
              }).simplex-chat.components.library (
                iosOverrides "pkg-ios-aarch64-swift-json"
              );
	            # aarch64-darwin build with tagged JSON format (for Mac & Flutter)
              "aarch64-darwin:lib:simplex-chat" = withMobileLibrary (drv' {
                pkgs' = pkgs;
                mobile = true;
                package-ghc-options.simplex-chat = [ "-DIOS" ];
                package-flags = {
                  direct-sqlcipher.commoncrypto = true;
                  direct-sqlcipher.openssl = false;
                  simplexmq.commoncrypto = true;
                  entropy.DoNotGetEntropy = true;
                  simplex-chat.client_library = true;
                  simplexmq.client_library = true;
                };
                extra-modules = [{
                  packages.simplexmq.components.library.libs = pkgs.lib.mkForce [
                    darwinOpenSSL
                  ];
                }];
              }).simplex-chat.components.library (
                iosOverrides "pkg-ios-aarch64-tagged-json"
              );
            };
            "x86_64-darwin" = {
              # x86_64-darwin iOS simulator build (to be patched with mac2ios)
              "x86_64-darwin-ios:lib:simplex-chat" = withMobileLibrary (drv' {
                pkgs' = pkgs;
                mobile = true;
                package-ghc-options.simplex-chat = [ "-DIOS" ];
                package-flags = {
                  simplex-chat.swift = true;
                  simplexmq.swift = true;
                  direct-sqlcipher.commoncrypto = true;
                  direct-sqlcipher.openssl = false;
                  simplexmq.commoncrypto = true;
                  entropy.DoNotGetEntropy = true;
                  simplex-chat.client_library = true;
                  simplexmq.client_library = true;
                };
                extra-modules = [{
                  packages.simplexmq.components.library.libs = pkgs.lib.mkForce [
                    darwinOpenSSL
                  ];
                }];
              }).simplex-chat.components.library (
                iosOverrides "pkg-ios-x86_64-swift-json"
              );
              # x86_64-darwin build with tagged JSON format (for Mac & Flutter iOS simulator)
              "x86_64-darwin:lib:simplex-chat" = withMobileLibrary (drv' {
                pkgs' = pkgs;
                mobile = true;
                package-ghc-options.simplex-chat = [ "-DIOS" ];
                package-flags = {
                  direct-sqlcipher.commoncrypto = true;
                  direct-sqlcipher.openssl = false;
                  simplexmq.commoncrypto = true;
                  entropy.DoNotGetEntropy = true;
                  simplex-chat.client_library = true;
                  simplexmq.client_library = true;
                };
                extra-modules = [{
                  packages.simplexmq.components.library.libs = pkgs.lib.mkForce [
                    darwinOpenSSL
                  ];
                }];
              }).simplex-chat.components.library (
                iosOverrides "pkg-ios-x86_64-tagged-json"
              );
            };
        }.${system} or {});
        # build all packages in hydra.
        hydraJobs = packages;

        checks = pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          windows-rts-fs = import ./scripts/nix/check-windows-fs.nix { inherit pkgs; };
          windows-blst-linkage = import ./scripts/nix/check-windows-blst-linkage.nix { inherit pkgs; };
          ghc-elf-comdat-functions = import ./scripts/nix/check-elf-comdat-functions.nix {
            inherit pkgs;
            compiler = (compilerSelection pkgs).ghc914-sh;
            patch = haskellNix + "/overlays/patches/ghc914-rts-elf-comdat-functions.patch";
          };
          ghc-elf-alignment = import ./scripts/nix/check-elf-alignment.nix {
            inherit pkgs;
            compiler = (compilerSelection pkgs).ghc914-sh;
          };
        } // pkgs.lib.optionalAttrs (system == "x86_64-darwin") {
          ghc-macho-info-tables = import ./scripts/nix/check-macho-info-tables.nix {
            inherit pkgs;
            compiler = (compilerSelection pkgs).ghc914-sh;
          };
        };

        devShell = let
	updateCmd = pkgs.writeShellApplication {
          name = "update-sha256map";
          runtimeInputs = [ pkgs.nix-prefetch-git pkgs.jq pkgs.gawk ];
          text = ''
            gawk -f ./scripts/nix/update-sha256.awk cabal.project > ./scripts/nix/sha256map.nix
          '';
        }; in
        (drv pkgs).shellFor {
          withHoogle = false;
          buildInputs = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [ pkgs.libiconv ];
          nativeBuildInputs = [
            updateCmd
            pkgs.haskell-nix.nix-tools-unchecked-sh.exes.cabal
          ];
        };
      }
    );
}
