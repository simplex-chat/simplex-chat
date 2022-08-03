#!/usr/bin/env sh
# Safety measures
set -eu

u="$USER"
tmp=$(mktemp -d -t)
conf="${XDG_CONFIG_HOME:-$HOME/.config}"
commands="nix git gradle unzip curl"

nix_setup() {
  # Pre-setup nix
  [ ! -d /nix ] && sudo sh -c "mkdir -p /nix && chown -R $u /nix"

  [ ! -d "$conf/nix" ] && mkdir -p "$conf/nix"

  printf "sandbox = true\nmax-jobs = auto\nexperimental-features = nix-command flakes\nextra-substituters = https://cache.zw3rk.com\ntrusted-public-keys = loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk=" > "$conf/nix/nix.conf"

  # Install nix
  curl -L https://nixos.org/nix/install -o "$tmp/nix-install"
  chmod +x "$tmp/nix-install" && "$tmp/nix-install" --no-daemon

  . "$HOME/.nix-profile/etc/profile.d/nix.sh"
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
    for i in $commands; do
      case $i in
        nix)
          if ! command -v "$i" > /dev/null 2>&1 || [ ! -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
            nix_setup
          fi
          ;;
        *) 
          if ! command -v "$i" > /dev/null 2>&1; then
            commands_failed="$i $commands_failed"
          fi
          ;;
      esac
  done
	
  set +u

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
