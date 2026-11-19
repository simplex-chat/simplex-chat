#!/bin/bash

set -euo pipefail

FLATHUB_REPO=https://dl.flathub.org/repo/flathub.flatpakrepo
FLATPAK_APP_ID=chat.simplex.simplex
kind=$1
package=$(realpath "$2")
export UI_SESSION=$3
out=$4
here=$(cd "$(dirname "$0")" && pwd)
export UI_STATE=${RUNNER_TEMP:-/tmp}/ui-$kind-$UI_SESSION
checks=$out/e2e-checks.txt
services_log=$out/desktop-services.log
mkdir -p "$out" "$UI_STATE"
: > "$checks"

case "$kind" in
  deb)
    sudo apt-get install -y "$package"
    target=/opt/simplex/bin/simplex
    entry_id=simplex-simplex
    ;;
  appimage)
    chmod +x "$package"
    export APPIMAGE_EXTRACT_AND_RUN=1
    target=$package
    entry_id=chat.simplex.app
    ;;
  flatpak)
    flatpak remote-add --user --if-not-exists flathub "$FLATHUB_REPO"
    flatpak install --user -y --noninteractive "$package"
    app_home=$HOME/.var/app/$FLATPAK_APP_ID
    for dir in config data cache; do
      mkdir -p "$app_home/$dir"
      ln -sfn "$app_home/$dir" "$UI_STATE/$dir"
    done
    target=$UI_STATE/flatpak-run
    printf '#!/bin/sh\nexec env -u JAVA_TOOL_OPTIONS flatpak run %s "$@"\n' "$FLATPAK_APP_ID" > "$target"
    chmod +x "$target"
    mkdir -p "$HOME/.config/xdg-desktop-portal"
    printf '[preferred]\ndefault=gtk\norg.freedesktop.impl.portal.Secret=gnome-keyring\n' > "$HOME/.config/xdg-desktop-portal/portals.conf"
    entry_id=$FLATPAK_APP_ID
    ;;
  *)
    echo "error: the package kind is deb, appimage or flatpak, not \"$kind\""
    exit 1
    ;;
esac

check() {
  local what=$1
  shift
  if "$@"; then
    echo "PASS $kind $UI_SESSION: $what" | tee -a "$checks"
  else
    echo "FAIL $kind $UI_SESSION: $what" | tee -a "$checks"
    echo "::error::$kind $UI_SESSION: $what"
  fi
}

report() {
  if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
    {
      echo "### End-to-end checks: $kind, $UI_SESSION"
      echo
      sed 's/^/- /' "$checks"
    } >> "$GITHUB_STEP_SUMMARY"
  fi
  "$here/ui" stop || true
}
trap report EXIT

eval "$(printf '\n' | gnome-keyring-daemon --unlock --components=secrets)"
export GNOME_KEYRING_CONTROL
python3 "$here/desktop-services.py" "$services_log" &
until grep -q '"ready"' "$services_log" 2> /dev/null; do
  sleep 0.5
done

SMOKE_SETTINGS="DesktopCloseBehavior=MinimizeToTray" "$here/smoke-test.sh" "$target" "$out" layouts || smoke_status=$?

identity=$(cat "$UI_STATE/window-identity")
echo "window: $identity"
case "$UI_SESSION" in
  x11)
    check "the window's WM_CLASS is $entry_id" grep -q "\"$entry_id\"" <<< "$identity"
    ;;
  wayland)
    check "the window is a native Wayland surface" [ "$(jq -r .shell <<< "$identity")" = xdg_shell ]
    check "the window's app_id is $entry_id" [ "$(jq -r .app_id <<< "$identity")" = "$entry_id" ]
    ;;
  xwayland)
    check "the window is an X11 window under XWayland" [ "$(jq -r .shell <<< "$identity")" = xwayland ]
    check "the window's WM_CLASS is $entry_id" [ "$(jq -r .class <<< "$identity")" = "$entry_id" ]
    ;;
esac

check "the app starts no gsettings process" bash -c '! pgrep -x gsettings'
check "the app writes the renderer to its log" grep -q "OpenGL renderer:" "$out/app.log"

settings=$UI_STATE/config/simplex/settings.properties
passphrase=$(sed -n 's/^EncryptedDBPassphrase=//p' "$settings")
initialization_vector=$(sed -n 's/^InitializationVectorDBPassphrase=//p' "$settings")
check "settings hold the database passphrase" [ -n "$passphrase" ]
check "settings hold the database passphrase only as ciphertext" [ "$passphrase" != "$initialization_vector" ]
if [ "$kind" = flatpak ]; then
  check "the sandbox keyring of the Secret portal exists" [ -s "$UI_STATE/data/keyrings/default.keyring" ]
else
  check "the keyring holds the key of the database passphrase" bash -c 'secret-tool lookup application chat.simplex.app alias databasePassword | grep -q .'
fi
check "settings are readable only by the user" [ "$(stat -c %a "$(dirname "$settings")")" = 700 ]
check "the tray item registers with the StatusNotifierWatcher" grep -q '"tray_registered"' "$services_log"

if [ "${smoke_status:-0}" -ne 0 ]; then
  echo "::error::$kind $UI_SESSION: the smoke test failed"
  exit "$smoke_status"
fi
if grep -q '^FAIL' "$checks"; then
  echo "error: $(grep -c '^FAIL' "$checks") of $(wc -l < "$checks") end-to-end checks failed"
  exit 1
fi
echo "end-to-end test passed: $kind under $UI_SESSION"
