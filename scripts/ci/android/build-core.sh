#!/bin/bash

# Builds the arm64 Android core (libsimplex.so with the libraries it needs, and libsupport.so) with Nix, as
# scripts/android/build-android.sh does, and packs them as scripts/android/download-libs.sh lays them out.
# Usage: scripts/ci/android/build-core.sh <output .tar.gz>
# Installs the Nix version that build-android.sh pins, in single-user mode, with the zw3rk Hydra's cache, which
# builds these jobs for master: without it, the toolchain and every dependency would be compiled too.

set -euo pipefail

nix_ver=nix-2.22.0
nix_hash=4fed7db867186c01ce2a2077da4a6950ed16232efbf78d0cd19700cff80559f9
jobs=(".#hydraJobs.x86_64-linux.aarch64-android:lib:simplex-chat" ".#hydraJobs.x86_64-linux.aarch64-android:lib:support")

install_nix() {
  local tmp
  tmp=$(mktemp -d)
  sudo mkdir -p /nix
  sudo chown "$USER" /nix
  df -h /
  # the Nix sandbox needs user namespaces, which Ubuntu 24.04 restricts
  sudo sysctl -w kernel.apparmor_restrict_unprivileged_userns=0
  curl --proto '=https' --tlsv1.2 -sSf -o "$tmp/install" "https://releases.nixos.org/nix/$nix_ver/install"
  echo "$nix_hash  $tmp/install" | sha256sum -c
  mkdir -p ~/.config/nix
  cat > ~/.config/nix/nix.conf << EOF
sandbox = true
max-jobs = auto
cores = 0
experimental-features = nix-command flakes
substituters = https://cache.nixos.org https://cache.zw3rk.com https://cache.iog.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
EOF
  sh "$tmp/install" --no-daemon
  # shellcheck disable=SC1091
  . "$HOME/.nix-profile/etc/profile.d/nix.sh"
  nix --version
}

mkdir -p "$(dirname "$1")"
out="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
install_nix
pkg=$(mktemp -d)
for job in "${jobs[@]}"; do
  echo "::group::nix build $job"
  started=$SECONDS
  nix build -L --out-link "$RUNNER_TEMP/result" "$job"
  echo "::endgroup::"
  echo "$job built in $(((SECONDS - started) / 60)) min"
  unzip -o "$RUNNER_TEMP"/result/pkg-*.zip -d "$pkg"
done
ls -la "$pkg"
tar -C "$pkg" -czf "$out" .
ls -la "$out"
