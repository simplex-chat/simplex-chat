{ config, lib, pkgs, ... }:
lib.mkIf (pkgs.stdenv.hostPlatform.isAndroid && config.compiler-nix-name == "ghc914-sh") {
  # API 26 predates Bionic's iconv. Keep the GNU header and static library
  # together; the configure arguments also retain their paths in the .conf.
  # A path module is deduplicated when appendModule re-enters cabalProject'.
  cabalProjectLocal = lib.mkAfter ''
    package ghc-internal
      extra-include-dirs: ${lib.getDev pkgs.libiconv}/include
      extra-lib-dirs: ${lib.getLib pkgs.libiconv}/lib
      configure-options: --with-iconv-includes=${lib.getDev pkgs.libiconv}/include --with-iconv-libraries=${lib.getLib pkgs.libiconv}/lib
  '';
}
