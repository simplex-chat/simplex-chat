{ pkgs, windowsPkgs, copyFiles, entryPoint, zipName }:
''
  set -x
  function deps() {
    ${pkgs.binutils}/bin/strings "$1" | grep '.\.dll'|grep -v -E 'Winsock|ADVAPI32|dbghelp|KERNEL32|msvcrt|ntdll|ole32|RPCRT4|SHELL32|USER32|WINMM|WS2_32|kernel32|GDI32'|grep -v "$1"
  }
  ${pkgs.tree}/bin/tree $out
  mkdir -p $out/_pkg
  ${copyFiles}
  mkdir $out/libs
  # GCC and mcfgthreads can expose the same read-only DLL; keep the first copy.
  find ${pkgs.lib.getBin windowsPkgs.openssl}             -name "*.dll" -exec cp -n {} $out/libs \;
  find ${pkgs.lib.getBin windowsPkgs.zlib}                -name "*.dll" -exec cp -n {} $out/libs \;
  find ${pkgs.lib.getBin windowsPkgs.libffi}              -name "*.dll" -exec cp -n {} $out/libs \;
  find ${pkgs.lib.getBin windowsPkgs.gmp}                 -name "*.dll" -exec cp -n {} $out/libs \;
  find ${pkgs.lib.getBin windowsPkgs.stdenv.cc.cc}        -name "*.dll" -exec cp -n {} $out/libs \;
  find ${pkgs.lib.getBin windowsPkgs.windows.mcfgthreads} -name "*.dll" -exec cp -n {} $out/libs \;

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
  copyDeps ${pkgs.lib.escapeShellArg entryPoint}
  popd
  ${pkgs.tree}/bin/tree $out/_pkg
  (cd $out/_pkg; ${pkgs.zip}/bin/zip -r -9 "$out"/${pkgs.lib.escapeShellArg zipName} *)
  rm -fR $out/_pkg
  mkdir -p $out/nix-support
  echo "file binary-dist \"$(echo $out/*.zip)\"" \
      > $out/nix-support/hydra-build-products
''
