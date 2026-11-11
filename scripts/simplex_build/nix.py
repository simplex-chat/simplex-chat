"""Nix, which builds the Android and iOS core libraries."""

import os
from pathlib import Path

from .common import BuildError, download, run, sha256

# zw3rk's Hydra builds the Android jobs of master and IOG caches the haskell.nix toolchains; without them the core
# library takes 12-24 hours. Substituted paths are the same as built ones, but come from these caches' builders.
NIX_CACHE_CONFIG = (
    "extra-substituters = https://cache.zw3rk.com https://cache.iog.io\n"
    "extra-trusted-public-keys = loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk= "
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=\n"
    "cores = 0"
)


# the 2.22 installer gives build users UIDs from 301, which macOS 15 and later reserve
NIX_INSTALLER = "https://releases.nixos.org/nix/nix-2.34.6/install"


NIX_INSTALLER_SHA256 = "bf6d12da4aeaae38ab509dc736df648597bcdee051046ea76d3d53d5b18fa54a"


NIX_PROFILE = Path("/nix/var/nix/profiles/default")


# The certificate bundles nix-daemon.sh looks for, in its order
NIX_CERT_FILES = (
    Path("/etc/ssl/certs/ca-certificates.crt"),
    Path("/etc/ssl/ca-bundle.pem"),
    Path("/etc/ssl/certs/ca-bundle.crt"),
    Path("/etc/pki/tls/certs/ca-bundle.crt"),
    NIX_PROFILE / "etc/ssl/certs/ca-bundle.crt",
    NIX_PROFILE / "etc/ca-bundle.crt",
)


def nix_env() -> dict[str, str]:
    """What nix-daemon.sh sets for running nix: its bin on PATH and a certificate bundle."""
    env = {"PATH": f"{NIX_PROFILE / 'bin'}:{os.environ.get('PATH', '')}"}
    if "NIX_SSL_CERT_FILE" not in os.environ:
        cert = next((c for c in NIX_CERT_FILES if c.is_file()), None)
        if cert is None:
            raise BuildError("no certificate bundle for Nix found")
        env["NIX_SSL_CERT_FILE"] = str(cert)
    return env


def install_nix(work: Path) -> None:
    installer = work / "nix-install"
    download(NIX_INSTALLER, installer)
    if sha256(installer) != NIX_INSTALLER_SHA256:
        raise BuildError("the Nix installer's sha256 doesn't match")
    conf = work / "nix.conf"
    conf.write_text(f"experimental-features = nix-command flakes\nmax-jobs = auto\n{NIX_CACHE_CONFIG}\n")
    run(["sh", installer, "--daemon", "--yes", "--no-channel-add", "--nix-extra-conf-file", conf])
