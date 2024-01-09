#!/usr/bin/env sh
# Safety measures
set -eu

repo="https://github.com/simplex-chat/simplex-chat"

u="$USER"
tmp="$(mktemp -d -t)"
folder="$tmp/simplex-chat"

nix_ver="nix-2.19.2"
nix_url="https://releases.nixos.org/nix/$nix_ver/install"
nix_hash="435f0d7e11f7c7dffeeab0ec9cc55723f6d3c03352379d785633cf4ddb5caf90"
nix_config="sandbox = true
max-jobs = auto
experimental-features = nix-command flakes"

commands="nix git curl gradle zip unzip zipalign"
arches="${ARCHES:-aarch64 armv7a}"

arch_map() {
  case $1 in
    aarch64) android_arch="arm64-v8a" ;;
    armv7a) android_arch="armeabi-v7a" ;;
  esac
}

nix_install() {
  # Pre-setup nix
  [ ! -d /nix ] && sudo sh -c "mkdir -p /nix && chown -R $u /nix"

  # Install nix
  curl -sSf "$nix_url" -o "$tmp/nix-install"
  printf "%s %s" "$nix_hash" "$tmp/nix-install" | sha256sum -c
  chmod +x "$tmp/nix-install" && "$tmp/nix-install" --no-daemon

  . "$HOME/.nix-profile/etc/profile.d/nix.sh"
}

nix_setup() {
  printf "%s" "$nix_config" > "$tmp/nix.conf"
  export NIX_CONF_DIR="$tmp/"
}

git_setup() {
  [ "$folder" != "." ] && {
    git clone "$repo" "$folder"
  }

  # Switch to nix-android branch
  git -C "$folder" checkout "$commit"
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
      gradle)
        if ! command -v "$i" > /dev/null 2>&1; then
          commands_failed="$i $commands_failed"
        else
          gradle_ver_local="$(gradle -v | grep Gradle | awk '{print $2}')"
          gradle_ver_local_compare="$(printf ${gradle_ver_local:-0.0} | awk -F. '{print $1$2}')"
          gradle_ver_remote="$(grep distributionUrl ${folder}/apps/multiplatform/gradle/wrapper/gradle-wrapper.properties)"
          gradle_ver_remote="${gradle_ver_remote#*-}"
          gradle_ver_remote="${gradle_ver_remote%-*}"
          gradle_ver_remote_compare="$(printf ${gradle_ver_remote} | awk -F. '{print $1$2}')"
        
          if [ "$gradle_ver_local_compare" != "$gradle_ver_remote_compare" ]; then
            commands_failed="$i[installed=${gradle_ver_local},required=${gradle_ver_remote}] $commands_failed"
          fi
        fi
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
  # Build preparations
  sed -i.bak 's/${extract_native_libs}/true/' "$folder/apps/multiplatform/android/src/main/AndroidManifest.xml"
  sed -i.bak 's/jniLibs.useLegacyPackaging =.*/jniLibs.useLegacyPackaging = true/' "$folder/apps/multiplatform/android/build.gradle.kts"
  sed -i.bak '/android {/a lint {abortOnError = false}' "$folder/apps/multiplatform/android/build.gradle.kts"

  for arch in $arches; do

    tag_full="$(git tag --points-at HEAD)"
    tag_version="${tag_full%%-*}"

    if [ "$arch" = "armv7a" ] && [ -n "$tag_full" ] ; then
      git checkout "${tag_version}-armv7a"
      android_simplex_lib="${folder}#hydraJobs.${arch}-android:lib:simplex-chat.x86_64-linux"
      android_support_lib="${folder}#hydraJobs.${arch}-android:lib:support.x86_64-linux"
    else
      android_simplex_lib="${folder}#hydraJobs.x86_64-linux.${arch}-android:lib:simplex-chat"
      android_support_lib="${folder}#hydraJobs.x86_64-linux.${arch}-android:lib:support"
    fi
    
    android_simplex_lib_output="${PWD}/result/pkg-${arch}-android-libsimplex.zip"
    android_support_lib_output="${PWD}/result/pkg-${arch}-android-libsupport.zip"

    arch_map "$arch"

    android_tmp_folder="${tmp}/android-${arch}"
    android_apk_output="${folder}/apps/multiplatform/android/build/outputs/apk/release/android-${android_arch}-release-unsigned.apk"
    android_apk_output_final="simplex-chat-${android_arch}.apk"
    libs_folder="${folder}/apps/multiplatform/common/src/commonMain/cpp/android/libs"

    # Create missing folders
    mkdir -p "$libs_folder/$android_arch"

    nix build "$android_simplex_lib"
    unzip -o "$android_simplex_lib_output" -d "$libs_folder/$android_arch"

    nix build "$android_support_lib"
    unzip -o "$android_support_lib_output" -d "$libs_folder/$android_arch"

    # Build only one arch
    sed -i.bak "s/include(.*/include(\"${android_arch}\")/" "$folder/apps/multiplatform/android/build.gradle.kts"
    gradle -p "$folder/apps/multiplatform/" clean :android:assembleRelease

    mkdir -p "$android_tmp_folder"
    unzip -oqd "$android_tmp_folder" "$android_apk_output"

    (
     cd "$android_tmp_folder" && \
     zip -rq5 "$tmp/$android_apk_output_final" . && \
     zip -rq0 "$tmp/$android_apk_output_final" resources.arsc res
    )

    zipalign -p -f 4 "$tmp/$android_apk_output_final" "$PWD/$android_apk_output_final"

    rm -rf "$libs_folder/$android_arch"

    if [ "$arch" = "armv7a" ] && [ -n "$tag_full" ] ; then
      git checkout "${tag_full}"
    fi
  done
}

final() {
  printf 'Simplex-chat was successfully compiled: %s/simplex-chat-*.apk\nDelete nix and gradle caches with "rm -rf /nix && rm $HOME/.nix* && $HOME/.gradle/caches" in case if no longer needed.\n' "$PWD"
}

main() {
  while getopts ":s" opt; do
    case $opt in
      s) folder="." ;;
      *) printf "Flag '-%s' doesn't exist.\n" "$OPTARG"; exit 1 ;;
    esac
  done
  shift $(( $OPTIND - 1 ))
  commit="$1"; shift 1
  git_setup
  checks
  build
  final
}

main "$@"
