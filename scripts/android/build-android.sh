#!/usr/bin/env bash
# Safety measures
set -euo pipefail
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR
IFS=$'\n\t'

u="$USER"
tmp=$(mktemp -d -t)
folder="$tmp/simplex-chat"
commands="nix git curl gradle zip unzip zipalign"

nix_install() {
  # Pre-setup nix
  [ ! -d /nix ] && sudo sh -c "mkdir -p /nix && chown -R $u /nix"

  # Install nix
  nix_ver="nix-2.14.1"
  nix_url="https://releases.nixos.org/nix/$nix_ver/install"
  nix_hash="565974057264f0536f600c68d59395927cd73e9fc5a60f33c1906e8f7bc33fcf"

  curl -sSf "$nix_url" -o "$tmp/nix-install"
  printf "%s %s" "$nix_hash" "$tmp/nix-install" | sha256sum -c
  chmod +x "$tmp/nix-install" && "$tmp/nix-install" --no-daemon

  . "$HOME/.nix-profile/etc/profile.d/nix.sh"
}

nix_setup() {
  printf "sandbox = true\nmax-jobs = auto\nexperimental-features = nix-command flakes\n" > "$tmp/nix.conf"
  export NIX_CONF_DIR="$tmp/"
}

git_setup() {
  [ "$folder" != "." ] && {
    git clone --depth=1 https://github.com/simplex-chat/simplex-chat "$folder"
  }

  # Switch to nix-android branch
  git -C "$folder" checkout "$commit"

  # Create missing folders
  mkdir -p "$folder/apps/android/app/src/main/cpp/libs/arm64-v8a"
  mkdir -p "$folder/apps/android/app/src/main/cpp/libs/armeabi-v7a"
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
  nix build "$folder#hydraJobs.aarch64-android:lib:simplex-chat.x86_64-linux"
  unzip -o "$PWD/result/pkg-aarch64-android-libsimplex.zip" -d "$folder/apps/android/app/src/main/cpp/libs/arm64-v8a"
  
  nix build "$folder#hydraJobs.armv7a-android:lib:simplex-chat.x86_64-linux"
  unzip -o "$PWD/result/pkg-armv7a-android-libsimplex.zip" -d "$folder/apps/android/app/src/main/cpp/libs/armeabi-v7a"

  # Build android suppprt lib
  nix build "$folder#hydraJobs.aarch64-android:lib:support.x86_64-linux"
  unzip -o "$PWD/result/pkg-aarch64-android-libsupport.zip" -d "$folder/apps/android/app/src/main/cpp/libs/arm64-v8a"

  nix build "$folder#hydraJobs.armv7a-android:lib:support.x86_64-linux"
  unzip -o "$PWD/result/pkg-armv7a-android-libsupport.zip" -d "$folder/apps/android/app/src/main/cpp/libs/armeabi-v7a"

  sed -i.bak 's/${extract_native_libs}/true/' "$folder/apps/android/app/src/main/AndroidManifest.xml"
  sed -i.bak '/android {/a lint {abortOnError false}' "$folder/apps/android/app/build.gradle"

  gradle -p "$folder/apps/android/" clean build assembleRelease

  mkdir -p "$tmp/android-aarch64"
  unzip -oqd "$tmp/android-aarch64/" "$folder/apps/android/app/build/outputs/apk/release/app-arm64-v8a-release-unsigned.apk"
  (cd "$tmp/android-aarch64" && zip -rq5 "$tmp/simplex-chat-aarch64.apk" . && zip -rq0 "$tmp/simplex-chat-aarch64.apk" resources.arsc res)
  zipalign -p -f 4 "$tmp/simplex-chat-aarch64.apk" "$PWD/simplex-chat-aarch64.apk"
  
  mkdir -p "$tmp/android-armv7"
  unzip -oqd "$tmp/android-armv7/" "$folder/apps/android/app/build/outputs/apk/release/app-armeabi-v7a-release-unsigned.apk"
  (cd "$tmp/android-armv7" && zip -rq5 "$tmp/simplex-chat-armv7.apk" . && zip -rq0 "$tmp/simplex-chat-armv7.apk" resources.arsc res)
  zipalign -p -f 4 "$tmp/simplex-chat-armv7.apk" "$PWD/simplex-chat-armv7.apk"
}

final() {
  printf "Simplex-chat was successfully compiled: %s/simplex-chat.apk\nDelete nix and gradle caches with 'rm -rf /nix && rm \$HOME/.nix* && \$HOME/.gradle/caches' in case if no longer needed.\n" "$PWD"
}

main() {
  while getopts ":s" opt; do
    case $opt in
      s) folder="." ;;
      *) printf "Flag '-%s' doesn't exist.\n" "$OPTARG"; exit 1 ;;
    esac
  done
  shift $(( OPTIND - 1 ))
  commit="$1"; shift 1
  checks
  git_setup
  build
  final
}

main "$@"
