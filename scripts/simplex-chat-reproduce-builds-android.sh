#!/usr/bin/env sh
set -eu

SIMPLEX_KEY='3C:52:C4:FD:3C:AD:1C:07:C9:B0:0A:70:80:E3:58:FA:B9:FE:FC:B8:AF:5A:EC:14:77:65:F1:6D:0F:21:AD:85'
SIMPLEX_REPO='simplex-chat/simplex-chat'
AARCH64_RELEAS_NAME='simplex-aarch64.apk'
ARMV7A_RELEASE_NAME='simplex-armv7a.apk'
CMDS="curl apksigner docker"

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
  curl -L "https://github.com/${SIMPLEX_REPO}/releases/download/${tag}/${release}" -o "$out"
}

check_apk() {
  file="$1"
  expected="$2"

  actual=$(apksigner verify --print-certs "$file" | grep 'SHA-256' | awk '{print $NF}' | fold -w2 | paste -sd: | tr '[:lower:]' '[:upper:]')

  if [ "$expected" = "$actual" ]; then
    return 0
  else
    return 1
  fi
}

build_apk() {
  
}

main() {
  
}


main "$@"
