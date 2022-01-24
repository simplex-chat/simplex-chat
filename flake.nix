{
  description = "nix flake for simplex-chat";
  inputs.nixpkgs.url = "github:angerman/nixpkgs/patch-1"; # based on 21.11, still need this, until everything is merged into 21.11.
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix?ref=angerman/android-static";
  inputs.haskellNix.inputs.nixpkgs.follows = "nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, haskellNix, nixpkgs, flake-utils }:
    let systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ]; in
    flake-utils.lib.eachSystem systems (system:
      let pkgs = haskellNix.legacyPackages.${system}; in
      let drv = pkgs': pkgs'.haskell-nix.project {
        compiler-nix-name = "ghc8107";
        index-state = "2022-01-15T00:00:00Z";
        # We need this, to specify we want the cabal project.
        # If the stack.yaml was dropped, this would not be necessary.
        projectFileName = "cabal.project";
        src = pkgs.haskell-nix.haskellLib.cleanGit {
          name = "simplex-chat";
          src = ./.;
        };
        sha256map = import ./sha256map.nix;
        modules = [{
          packages.direct-sqlite.patches = [ ./direct-sqlite-2.3.26.patch ];
        }
        ({ pkgs,lib, ... }: lib.mkIf (pkgs.stdenv.hostPlatform.isAndroid) {
          packages.simplex-chat.components.library.ghcOptions = [ "-pie" ];
        })];
      }; in
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
      rec {
        packages = {
            "lib:simplex-chat" = (drv pkgs).simplex-chat.components.library;
            "exe:simplex-chat" = (drv pkgs).simplex-chat.components.exes.simplex-chat;
        } // ({
            "x86_64-linux" =
                let
                    androidPkgs = pkgs.pkgsCross.aarch64-android;
                    # For some reason building libiconv with nixpgks android setup produces
                    # LANGINFO_CODESET to be found, which is not compatible with android sdk 23;
                    # so we'll patch up iconv to not include that.
                    androidIconv = (androidPkgs.libiconv.override { enableStatic = true; }).overrideAttrs (old: {
                        postConfigure = ''
                        echo "#undef HAVE_LANGINFO_CODESET" >> libcharset/config.h
                        echo "#undef HAVE_LANGINFO_CODESET" >> lib/config.h
                        '';
                    });
                    # Similarly to icovn, for reasons beyond my current knowledge, nixpkgs andorid
                    # toolchain makes configure believe we have MEMFD_CREATE, which we don't in
                    # sdk 23.
                    androidFFI = androidPkgs.libffi.overrideAttrs (old: {
                        dontDisableStatic = true;
                        hardeningDisable = [ "fortify" ];
                        postConfigure = ''
                        echo "#undef HAVE_MEMFD_CREATE" >> aarch64-unknown-linux-android/fficonfig.h
                        '';
                    }
                );in {
                "aarch64-android:lib:support" = (drv androidPkgs).android-support.components.library.override {
                   smallAddressSpace = true; enableShared = false;
                   setupBuildFlags = map (x: "--ghc-option=${x}") [ "-shared" "-o" "libsupport.so" ];
                   postInstall = ''

                     mkdir -p $out/_pkg
                     cp libsupport.so $out/_pkg
                     ${pkgs.patchelf}/bin/patchelf --remove-needed libunwind.so.1 $out/_pkg/libsupport.so
                     (cd $out/_pkg; ${pkgs.zip}/bin/zip -r -9 $out/pkg.zip *)
                     rm -fR $out/_pkg

                     mkdir -p $out/nix-support
                     echo "file binary-dist \"$(echo $out/*.zip)\"" \
                           > $out/nix-support/hydra-build-products
                   '';
                };
                "aarch64-android:lib:simplex-chat" = (drv androidPkgs).simplex-chat.components.library.override {
                    smallAddressSpace = true; enableShared = false;
                    # for android we build a shared library, passing these arguments is a bit tricky, as
                    # we want only the threaded rts (HSrts_thr) and ffi to be linked, but not fed into iserv for
                    # template haskell cross compilation. Thus we just pass them as linker options (-optl).
                    setupBuildFlags = map (x: "--ghc-option=${x}") [ "-shared" "-o" "libsimplex.so" "-optl-lHSrts_thr" "-optl-lffi"];
                      postInstall = ''
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
                        # find ${androidIconv}/lib -name "*.a" -exec cp {} $out/_pkg \;
                        # find ${androidPkgs.stdenv.cc.libc}/lib -name "*.a" -exec cp {} $out/_pkg \;

                        ${pkgs.patchelf}/bin/patchelf --remove-needed libunwind.so.1 $out/_pkg/libsimplex.so

                        ${pkgs.tree}/bin/tree $out/_pkg
                        (cd $out/_pkg; ${pkgs.zip}/bin/zip -r -9 $out/pkg.zip *)
                        rm -fR $out/_pkg
                        mkdir -p $out/nix-support
                        echo "file binary-dist \"$(echo $out/*.zip)\"" \
                           > $out/nix-support/hydra-build-products
                      '';
                    };
                };
            "aarch64-darwin" = {
              "aarch64-darwin:lib:simplex-chat" = (drv pkgs).simplex-chat.components.library.override {
                smallAddressSpace = true; enableShared = false;
                # we need threaded here, otherwise all the queing logic doesn't work properly.
                # for iOS we also use -staticlib, to get one rolled up library.
                # still needs mac2ios patching of the archives.
                ghcOptions = [ "-staticlib" "-threaded" ];
                postInstall = ''
                  ${pkgs.tree}/bin/tree $out
                  mkdir -p $out/_pkg
                  # copy over includes, we might want those, but maybe not.
                  # cp -r $out/lib/*/*/include $out/_pkg/
                  # find the libHS...ghc-X.Y.Z.a static library; this is the
                  # rolled up one with all dependencies included.
                  find ./dist -name "libHS*.a" -exec cp {} $out/_pkg \;
                  find ${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib -name "*.a" -exec cp {} $out/_pkg \;
                  find ${pkgs.gmp6.override { withStatic = true; }}/lib -name "*.a" -exec cp {} $out/_pkg \;
                  # There is no static libc
                  ${pkgs.tree}/bin/tree $out/_pkg
                  (cd $out/_pkg; ${pkgs.zip}/bin/zip -r -9 $out/pkg.zip *)
                  rm -fR $out/_pkg
                  mkdir -p $out/nix-support
                  echo "file binary-dist \"$(echo $out/*.zip)\"" \
                      > $out/nix-support/hydra-build-products
                '';
              };
            };
        }.${system} or {});
        # build all packages in hydra.
        hydraJobs = packages;

        devShell = let
	updateCmd = pkgs.writeShellApplication {
          name = "update-sha256map";
          runtimeInputs = [ pkgs.nix-prefetch-git pkgs.jq pkgs.gawk ];
          text = ''
            gawk -f update-sha256.awk cabal.project > sha256map.nix
          '';
        }; in
	pkgs.mkShell {
          buildInputs = [ updateCmd ];
          shellHook = ''
            echo "welcome to the shell!"
          '';
        };
      }
    );
}
