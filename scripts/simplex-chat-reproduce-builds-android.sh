#!/usr/bin/env sh
set -eu

SIMPLEX_KEY='3C:52:C4:FD:3C:AD:1C:07:C9:B0:0A:70:80:E3:58:FA:B9:FE:FC:B8:AF:5A:EC:14:77:65:F1:6D:0F:21:AD:85'

REPO_NAME="simplex-chat"
REPO="https://github.com/simplex-chat/${repo_name}"

IMAGE_NAME='sx-local-android'
CONTAINER_NAME='sx-builder-android'

export DOCKER_BUILDKIT=1

SIMPLEX_REPO='simplex-chat/simplex-chat'
CMDS="curl git docker"

INIT_DIR="$PWD"
TEMPDIR="$(mktemp -d)"

check() {
  set +u

  for i in $commands; do
    case $i in
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

download_apk() {
  tag="$1"
  release="$2"
  out="$3"

  curl -L "${REPO}/releases/download/${tag}/${release}" -o "$out"
}

setup_git() {
  dir="$1"
  tag="$2"

  git -C "${dir}" clone "${REPO}.git"

  (
    cd "${dir}/${REPO_NAME}"
    git checkout "${tag}"
  )
}

setup_tag_structure() {
  dir="$1"
  tag="$2"

  mkdir -p "${dir}/${tag}-${REPO_NAME}/from-source" "${dir}/${tag}-${REPO_NAME}/prebuilt"
}

check_apk() {
  apk_file="$1"
  expected="$2"

  actual=$(docker exec -t "${CONTAINER_NAME}" apksigner verify --print-certs "${apk_file}" | grep 'SHA-256' | awk '{print \$NF}' | fold -w2 | paste -sd: | tr '[:lower:]' '[:upper:]')

  if [ "$expected" = "$actual" ]; then
    return 0
  else
    return 1
  fi
}

print_vercode() {
  awk -F'=' '/android.version_code=/ {print $2}' "${dir}/${REPO_NAME}/apps/multiplatform/gradle.properties"
}

setup_container() {
  dir="$1"

  docker build \
    --no-cache \
    -f "${dir}/${REPO_NAME}/Dockerfile.build" \
    -t "${IMAGE_NAME}" \
    .

  # Run container in background
  docker run -t -d \
    --name "${CONTAINER_NAME}" \
    --device /dev/fuse \
    --cap-add SYS_ADMIN \
    --security-opt apparmor:unconfined \
    -v "${dir}/${REPO_NAME}:/project" \
    "${IMAGE_NAME}"
}

build_apk() {
  arch="${1}"
  vercode="${2}"

  # Gradle setup
  docker exec -t "${CONTAINER_NAME}" sh << EOF
cd /project/apps/multiplatform
./gradlew
EOF

  docker exec -t "${CONTAINER_NAME}" sh << EOF
GRADLE_BIN=\$(find \$HOME/.gradle/wrapper/dists -name "gradle" -type f -executable 2>/dev/null | head -1)
GRADLE_DIR=\$(dirname "\$GRADLE_BIN")
export PATH="\$GRADLE_DIR:\$PATH"

ARCHES="$arch" ./scripts/android/build-android.sh -gs "$vercode"
EOF
}

main() {
  TAG="$1"

  check

  setup_tag_structure "$INIT_DIR" "$TAG"

  setup_git "$TEMPDIR" "$TAG"
  setup_container "$TEMPDIR"

  vercode=$(print_vercode "$TEMPDIR")

  for arch in armv7a aarch64; do
    case "$arch" in
      armv7a)
        vercode_adjusted="$((vercode-1))"
        ;;
      aarch64)
        vercode_adjusted="$vercode"
        ;;
    esac

    release="simplex-${arch}.apk"
  
    download_apk "$TAG" "$release" "${TEMPDIR}/${REPO_NAME}/${release}"
    if check_apk "${TEMPDIR}/${REPO_NAME}/${release}" "$SIMPLEX_KEY"; then
      echo "Looks good"
      mv "${TEMPDIR}/${REPO_NAME}/${release}" "${INIT_DIR}/${tag}-${REPO_NAME}/prebuilt/"
    else
      echo "Looks bad"
      exit 1
    fi

    build_apk "$arch" "$vercode_adjusted"
  done
}

main "$@"
