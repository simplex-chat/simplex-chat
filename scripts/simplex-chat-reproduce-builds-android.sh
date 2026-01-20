#!/usr/bin/env sh
set -eu

SIMPLEX_KEY='3C:52:C4:FD:3C:AD:1C:07:C9:B0:0A:70:80:E3:58:FA:B9:FE:FC:B8:AF:5A:EC:14:77:65:F1:6D:0F:21:AD:85'

REPO_NAME="simplex-chat"
REPO="https://github.com/simplex-chat/${REPO_NAME}"

IMAGE_NAME='sx-local-android'
CONTAINER_NAME='sx-builder-android'
DOCKER_PATH_PROJECT='/project'
DOCKER_PATH_VERIFY='/verify'

export DOCKER_BUILDKIT=1

SIMPLEX_REPO='simplex-chat/simplex-chat'
CMDS="curl git docker"

INIT_DIR="$PWD"
TEMPDIR="$(mktemp -d)"
PID_MAX_ORIGINAL="$(sysctl -n kernel.pid_max)"

ARCHES="${ARCHES:-aarch64 armv7a}"

COLOR_CYAN="\033[36m"
COLOR_RESET="\033[0m"

SUFFIX_BUILT='built'
SUFFIX_DOWNLOADED='downloaded'
SUFFIX_BUILT_WITH_SIGNATURE='built-with-downloaded-signature'

cleanup() {
  rm -rf -- "${TEMPDIR}"
  docker rm --force "${CONTAINER_NAME}" 2>/dev/null || :
  docker image rm "${IMAGE_NAME}" 2>/dev/null || :

  if [ "$(sysctl -n kernel.pid_max)" != "$PID_MAX_ORIGINAL" ]; then
    printf 'Adjusting kernel.pid_max back to original value...\n'
    if $SUDO sysctl kernel.pid_max="$PID_MAX_ORIGINAL"; then
      printf 'Successfully adjusted kernel.pid_max\n'
    else
      printf 'Failed to adjust kernel.pid_max. Please set the value manually with: %s sysctl kernel.pid_max=%s\n' "$SUDO" "$PID_MAX_ORIGINAL"
    fi
  fi
}
trap 'cleanup' EXIT INT

check() {
  commands="$1"

  set +u

  for i in $commands; do
    if ! command -v "$i" > /dev/null 2>&1; then
      commands_failed="$i $commands_failed"
    fi
  done

  if [ -n "$commands_failed" ]; then
    commands_failed=${commands_failed% *}
    printf "%s is not found in your \$PATH. Please install them and re-run the script.\n" "$commands_failed"
    exit 1
  fi

  if [ "$PID_MAX_ORIGINAL" -gt 65535 ]; then
    SUDO=$(command -v sudo || command -v doas) || { echo "No sudo or doas"; exit 1; }

    printf 'Adjusting kernel.pid_max value to 65535...\n'

    if $SUDO sysctl kernel.pid_max=65535; then
      printf 'Successfully adjusted kernel.pid_max\n'
    else
      printf 'Failed to adjust kernel.pid_max, aborting.\n'
      exit 1
    fi
  fi

  set -u
}

download_apk() {
  tag="$1"
  filename="$2"
  file_out="$3"

  curl -L "${REPO}/releases/download/${tag}/${filename}" -o "$file_out"
}

setup_git() {
  workdir="$1"
  name="$2"

  git -C "$workdir" clone "${REPO}.git" "$name"
}

checkout_git() {
  git_dir="$1"
  tag="$2"

  git -C "$git_dir" reset --hard
  git -C "$git_dir" clean -dfx
  git -C "$git_dir" checkout "$tag"
}

check_apk() {
  apk_name="$1"
  expected="$2"

  actual=$(docker exec "${CONTAINER_NAME}" apksigner verify --print-certs "${DOCKER_PATH_VERIFY}/${apk_name}" | grep 'SHA-256' | awk '{print $NF}' | fold -w2 | paste -sd: | tr '[:lower:]' '[:upper:]')

  if [ "$expected" = "$actual" ]; then
    return 0
  else
    return 1
  fi
}

verify_apk() {
  apk_name="$1"

  # https://github.com/obfusk/apksigcopier?tab=readme-ov-file#what-about-signatures-made-by-apksigner-from-build-tools--3500-rc1
  docker exec "${CONTAINER_NAME}" repro-apk zipalign --page-size 16 --pad-like-apksigner --replace "${DOCKER_PATH_VERIFY}/${apk_name}.${SUFFIX_BUILT}" \
                                                                                                   "${DOCKER_PATH_VERIFY}/${apk_name}.aligned"
  docker exec "${CONTAINER_NAME}" mv "${DOCKER_PATH_VERIFY}/${apk_name}.aligned" \
                                     "${DOCKER_PATH_VERIFY}/${apk_name}.${SUFFIX_BUILT}"

  docker exec "${CONTAINER_NAME}" apksigcopier copy "${DOCKER_PATH_VERIFY}/${apk_name}.${SUFFIX_DOWNLOADED}" \
                                                    "${DOCKER_PATH_VERIFY}/${apk_name}.${SUFFIX_BUILT}" \
                                                    "${DOCKER_PATH_VERIFY}/${apk_name}.${SUFFIX_BUILT_WITH_SIGNATURE}"

  downloaded_apk_hash=$(docker exec "${CONTAINER_NAME}" sha256sum "${DOCKER_PATH_VERIFY}/${apk_name}.${SUFFIX_DOWNLOADED}" | awk '{print $1}')
  built_apk_hash=$(docker exec "${CONTAINER_NAME}" sha256sum "${DOCKER_PATH_VERIFY}/${apk_name}.${SUFFIX_BUILT_WITH_SIGNATURE}" | awk '{print $1}')

  if [ "$downloaded_apk_hash" = "$built_apk_hash" ]; then
    return 0
  else
    return 1
  fi
}

print_vercode() {
  build_dir="$1"
  awk -F'=' '/android.version_code=/ {print $2}' "${build_dir}/apps/multiplatform/gradle.properties"
}

setup_container() {
  dir_git="$1"
  dir_apk="$2"

  docker build \
    --no-cache \
    -f "${dir_git}/Dockerfile.build" \
    -t "${IMAGE_NAME}" \
    --build-arg=USER_UID="$(id -u)" \
    --build-arg=USER_GID="$(id -g)" \
    .

  # Run container in background
  docker run -t -d \
    --name "${CONTAINER_NAME}" \
    --device /dev/fuse \
    --cap-add SYS_ADMIN \
    --security-opt apparmor:unconfined \
    --security-opt seccomp:unconfined \
    -v "${dir_git}:${DOCKER_PATH_PROJECT}" \
    -v "${dir_apk}:${DOCKER_PATH_VERIFY}" \
    "${IMAGE_NAME}"
}

build_apk() {
  arch="$1"
  vercode="$2"

  apk_out="simplex-${arch}.apk.${SUFFIX_BUILT}"

  # Gradle setup
  docker exec -i "${CONTAINER_NAME}" sh << EOF
cd $DOCKER_PATH_PROJECT/apps/multiplatform
./gradlew
EOF

  docker exec -i "${CONTAINER_NAME}" sh << EOF
GRADLE_BIN=\$(find \$HOME/.gradle/wrapper/dists -name "gradle" -type f -executable 2>/dev/null | head -1)
GRADLE_DIR=\$(dirname "\$GRADLE_BIN")
export PATH="\$GRADLE_DIR:\$PATH"

ARCHES="$arch" ./scripts/android/build-android.sh -gs "$vercode" || ARCHES="$arch" ./scripts/android/build-android.sh -gs "$vercode"

APK_FILE=\$(find . -maxdepth 1 -type f -name '*.apk')

mv "\$APK_FILE" $DOCKER_PATH_VERIFY/$apk_out
EOF
}

main() {
  tag="$1"

  build_directory="${TEMPDIR}/${REPO_NAME}"
  final_directory="$INIT_DIR/${tag}-${REPO_NAME}"
  apk_directory="${final_directory}/android"

  printf 'This script will:
1) build docker container.
2) download APK from GitHub and validate signatures.
3) build core library with nix (12-24 hours).
4) build APK and compare with downloaded one

The script will ask for sudo password to adjust kernel.pid_max (needed for armv7a build)
and set it back to otiginal value when the build is done.

Continue?'

  read _

  check "$CMDS"

  mkdir -p "${apk_directory}"

  # Setup initial git for Dockerfile.build
  setup_git "$TEMPDIR" "$REPO_NAME"
  checkout_git "$build_directory" "$tag"

  printf "${COLOR_CYAN}Building Docker container...${COLOR_RESET}\n"
  setup_container "$build_directory" "$apk_directory"

  # Check phase
  for arch in $ARCHES; do
    filename="simplex-${arch}.apk"
  
    download_apk "$tag" "$filename" "${apk_directory}/${filename}.${SUFFIX_DOWNLOADED}"

    if check_apk "${filename}.${SUFFIX_DOWNLOADED}" "$SIMPLEX_KEY"; then
      printf "${COLOR_CYAN}APK for %s is signed by valid key.${COLOR_RESET}\n" "$arch"
    else
      printf "${COLOR_CYAN}Signature of APK for %s is invalid., aborting the script.${COLOR_RESET}\n" "$arch"
      exit 1
    fi
  done

  # Build phase
  for arch in $ARCHES; do
    case "$arch" in
      armv7a)
        build_tag="${tag}-armv7a"
        ;;
      aarch64)
        build_tag="${tag}"
        ;;
      *)
        printf "${COLOR_CYAN}Unknown architecture: %s! Skipping the build...${COLOR_RESET}\n" "$arch"
        continue
    esac

    # Setup the code
    checkout_git "$build_directory" "$build_tag"
    vercode=$(print_vercode "$build_directory")

    printf "${COLOR_CYAN}Building APK for for %s...${COLOR_RESET}\n" "$arch"
    build_apk "$arch" "$vercode"
  done

  # Verification phase
  for arch in $ARCHES; do
    filename="simplex-${arch}.apk"

    if ! verify_apk "$filename"; then
      printf "${COLOR_CYAN}Failed to verify %s! Aborting.\n${COLOR_RESET}" "$filename"
      exit 1
    fi
  done

  printf "${COLOR_CYAN}%s is reproducible.${COLOR_RESET}\n" "$tag"

  cleanup
}

main "$@"
