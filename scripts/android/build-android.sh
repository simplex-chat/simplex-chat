#!/usr/bin/env sh
# Safety measures
set -eu

u="$USER"
tmp=$(mktemp -d -t)
source="github:simplex-chat/simplex-chat"
commit="$1"
commands="nix git curl gradle zip unzip zipalign"

nix_install() {
  # Pre-setup nix
  [ ! -d /nix ] && sudo sh -c "mkdir -p /nix && chown -R $u /nix"

  # Install nix
  nix_ver="nix-2.11.1"
  nix_url="https://releases.nixos.org/nix/$nix_ver/install"
  nix_hash="4569a01dc5f62056f29f3195673bc3242fc70bf2474927fb5d8549c4d997402d"

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
  git -C "$tmp/simplex-chat" checkout "$commit"

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
  nix build "$source/$commit#hydraJobs.aarch64-android:lib:simplex-chat.x86_64-linux"
  unzip -o "$PWD/result/pkg-aarch64-android-libsimplex.zip" -d "$tmp/simplex-chat/apps/android/app/src/main/cpp/libs/arm64-v8a"

  # Build android suppprt lib
  nix build "$source/$commit#hydraJobs.aarch64-android:lib:support.x86_64-linux"
  unzip -o "$PWD/result/pkg-aarch64-android-libsupport.zip" -d "$tmp/simplex-chat/apps/android/app/src/main/cpp/libs/arm64-v8a"

  sed -i.bak 's/${extract_native_libs}/true/' "$tmp/simplex-chat/apps/android/app/src/main/AndroidManifest.xml"

  gradle -p "$tmp/simplex-chat/apps/android/" clean build assembleRelease

  mkdir -p "$tmp/android"
  unzip -oqd "$tmp/android/" "$tmp/simplex-chat/apps/android/app/build/outputs/apk/release/app-release-unsigned.apk"
  
  (cd "$tmp/android" && zip -rq5 "$tmp/simplex-chat.apk" . && zip -rq0 "$tmp/simplex-chat.apk" resources.arsc res)

  zipalign -p -f 4 "$tmp/simplex-chat.apk" "$PWD/simplex-chat.apk"
}

final() {
  printf "Simplex-chat was successfully compiled: %s/simplex-chat.apk\nDelete nix and gradle caches with 'rm -rf /nix && rm \$HOME/.nix* && \$HOME/.gradle/caches' in case if no longer needed.\n" "$PWD"
}

main() {
  checks
  git_setup
  build
  final
}

main
