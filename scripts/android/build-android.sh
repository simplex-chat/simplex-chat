#!/usr/bin/env sh
# Safety measures
set -eu

u="$USER"
tmp=$(mktemp -d -t)
commands="nix git gradle unzip curl"

nix_install() {
  # Pre-setup nix
  [ ! -d /nix ] && sudo sh -c "mkdir -p /nix && chown -R $u /nix"

  # Install nix
  nix_ver="nix-2.10.3"
  nix_url="https://releases.nixos.org/nix/$nix_ver/install"
  nix_hash="2e96a9c4abb5648a805480e8679de3d9fecff30453603f11c26bb4e7176c7ebe"

  curl -sSf "$nix_url" -o "$tmp/nix-install"
  printf "%s %s" "$nix_hash" "$tmp/nix-install" | sha256sum -c
  chmod +x "$tmp/nix-install" && "$tmp/nix-install" --no-daemon

  . "$HOME/.nix-profile/etc/profile.d/nix.sh"
}

nix_setup() {
  printf "sandbox = true\nmax-jobs = auto\nexperimental-features = nix-command flakes\nextra-substituters = https://cache.zw3rk.com\ntrusted-public-keys = loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk=" > "$tmp/nix.conf"
  export NIX_CONF_DIR="$tmp/"
}

git_setup() {
  # Clone simplex
  git clone https://github.com/simplex-chat/simplex-chat "$tmp/simplex-chat"

  # Switch to nix-android branch
  git -C "$tmp/simplex-chat" checkout nix-android

  # Create missing folders
  mkdir -p "$tmp/simplex-chat/apps/android/app/src/main/cpp/libs/arm64-v8a"
}

checks() {
  set +u
  for i in $commands; do
    case $i in
      nix)
        if ! command -v "$i" > /dev/null 2>&1 || [ ! -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
          nix_install
        fi
        nix_setup
        ;;
      *)
        if ! command -v "$i" > /dev/null 2>&1; then
          commands_failed="$i $commands_failed"
        fi
        ;;
    esac
  done

  if [ -n "$commands_failed" ]; then
    commands_failed=${commands_failed% *}
    printf "%s is not found in your \$PATH. Please install them and re-run the script.\n" "$commands_failed"
    exit 1
  fi

  set -u
}

build() {
  # Build simplex lib
  nix build "$tmp/simplex-chat/#packages.x86_64-linux.aarch64-android:lib:simplex-chat"
  unzip -o "$PWD/result/pkg-aarch64-android-libsimplex.zip" -d "$tmp/simplex-chat/apps/android/app/src/main/cpp/libs/arm64-v8a"

  # Build android suppprt lib
  nix build "$tmp/simplex-chat/#packages.x86_64-linux.aarch64-android:lib:support"
  unzip -o "$PWD/result/pkg-aarch64-android-libsupport.zip" -d "$tmp/simplex-chat/apps/android/app/src/main/cpp/libs/arm64-v8a"

  gradle -p "$tmp/simplex-chat/apps/android/" clean build
}

final() {
  cp "$tmp/simplex-chat/apps/android/app/build/outputs/apk/release/app-release-unsigned.apk" "$PWD/simplex-chat.apk"
  printf "Simplex-chat was successfully compiled: %s/simplex-chat.apk\nDelete nix and gradle caches with 'rm -rf /nix && rm \$HOME/.nix* && \$HOME/.gradle/caches' in case if no longer needed.\n" "$PWD"
}

main() {
  checks
  git_setup
  build
  final
}

main
