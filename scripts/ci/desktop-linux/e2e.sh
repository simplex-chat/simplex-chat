#!/bin/bash

set -euo pipefail

FLATHUB_REPO=https://dl.flathub.org/repo/flathub.flatpakrepo
FLATPAK_APP_ID=chat.simplex.simplex
kind=$1
package=$(realpath "$2")
export UI_SESSION=$3
out=$4
export UI_OUT=$out
here=$(cd "$(dirname "$0")" && pwd)
export UI_STATE=${RUNNER_TEMP:-/tmp}/ui-$kind-$UI_SESSION
checks=$out/e2e-checks.txt
services_log=$out/desktop-services.log
mkdir -p "$out" "$UI_STATE"
: > "$checks"
mkdir -p "$HOME/.config/xdg-desktop-portal"
printf '[preferred]\ndefault=gtk\norg.freedesktop.impl.portal.Secret=gnome-keyring\n' > "$HOME/.config/xdg-desktop-portal/portals.conf"

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
    printf '#!/bin/sh\nexec env -u JAVA_TOOL_OPTIONS -u XDG_DATA_HOME -u XDG_CONFIG_HOME -u XDG_CACHE_HOME flatpak run %s "$@"\n' "$FLATPAK_APP_ID" > "$target"
    chmod +x "$target"
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

ui=$here/ui
peer_dir=$UI_STATE/peer
peer_name=ci_peer
app_user=citester
other_chat="seeded notes"
light_luminance=0.6
dark_luminance=0.4
scheme_wait=20
pane_right_offset=390

eventually() {
  local secs=$1 deadline
  shift
  deadline=$((SECONDS + secs))
  until "$@"; do
    [ "$SECONDS" -lt "$deadline" ] || return 1
    sleep 1
  done
}

window_is() {
  local value
  value=$("$ui" luminance)
  case "$1" in
    light) awk -v v="$value" -v t="$light_luminance" 'BEGIN { exit !(v > t) }' ;;
    dark) awk -v v="$value" -v t="$dark_luminance" 'BEGIN { exit !(v < t) }' ;;
  esac
}

follow_colour_scheme() {
  check "the window is light with the desktop's default colour scheme" window_is light
  check "the desktop's colour scheme is set to dark" gsettings set org.gnome.desktop.interface color-scheme prefer-dark
  gdbus call --session --dest org.freedesktop.portal.Desktop --object-path /org/freedesktop/portal/desktop \
    --method org.freedesktop.portal.Settings.ReadOne org.freedesktop.appearance color-scheme || true
  check "the window turns dark when the desktop prefers dark" eventually "$scheme_wait" window_is dark
  "$ui" shot dark-scheme
  gsettings set org.gnome.desktop.interface color-scheme default || true
  check "the window turns light with the desktop's default colour scheme again" eventually "$scheme_wait" window_is light
}

activate_from_tray() {
  local item service destination path
  item=$(jq -c 'select(.event == "tray_registered")' "$services_log" | tail -n 1)
  if [ -z "$item" ]; then
    check "a tray item to activate" false
    return
  fi
  service=$(jq -r .service <<< "$item")
  if [[ "$service" == /* ]]; then
    destination=$(jq -r .sender <<< "$item")
    path=$service
  else
    destination=$service
    path=/StatusNotifierItem
  fi
  "$ui" close || true
  check "closing the window hides it to the tray" "$ui" hidden 15
  gdbus call --session --dest "$destination" --object-path "$path" --method org.kde.StatusNotifierItem.Activate 0 0 || true
  check "activating the tray item shows the window" "$ui" shown 15
  "$ui" shot tray-activated
}

# Runs one command in the CLI peer, which stays online for <secs> after it.
peer() {
  local secs=$1 cmd=$2
  "$SIMPLEX_CLI" -d "$peer_dir/peer" -e "$cmd" -t "$secs" --execute-log all --show-full-links --user-display-name "$peer_name" -y \
    < /dev/null > "$peer_dir/out" 2>&1 || true
  { echo "> $cmd"; cat "$peer_dir/out"; } >> "$out/peer.log"
}

peer_connected() {
  peer 10 /contacts
  grep -q "$app_user: contact is connected" "$out/peer.log"
}

notification_with() {
  jq -c --arg text "$1" 'select(.event == "notify" and (.body | contains($text)))' "$services_log" | tail -n 1
}

notified() {
  [ -n "$(notification_with "$1")" ]
}

# one notification per chat: the next message replaces the shown one, or the shown one is closed first
replaced() {
  local first second
  first=$(notification_with "first message" | jq -r .id)
  second=$(notification_with "second message")
  [ -n "$first" ] && [ -n "$second" ] || return 1
  [ "$(jq -r .replaces_id <<< "$second")" = "$first" ] || jq -e --argjson id "$first" 'select(.event == "close" and .id == $id)' "$services_log" > /dev/null
}

closed() {
  jq -e --argjson id "$1" 'select(.event == "close" and .id == $id)' "$services_log" > /dev/null
}

urgent() {
  jq -e 'select(.event == "launcher" and .urgent)' "$services_log" > /dev/null
}

right_pane_shows() {
  local window_x line
  read -r window_x _ < "$UI_STATE/window-rect"
  while IFS= read -r line; do
    [ "${line%%,*}" -gt $((window_x + pane_right_offset)) ] && [[ "${line#* }" == *"$1"* ]] && return 0
  done < <("$ui" text)
  return 1
}

notify_messages() {
  local link notification id
  if [ ! -x "${SIMPLEX_CLI:-}" ]; then
    check "the CLI peer is installed" false
    return
  fi
  mkdir -p "$peer_dir"
  peer 5 /c
  link=$(grep -oE 'https://simplex\.chat/invitation#[^[:space:]]+' "$peer_dir/out" | head -n 1 || true)
  if [ -z "$link" ]; then
    check "the CLI peer creates a one-time link" false
    return
  fi
  "$ui" copy "$link"
  "$ui" click-text "Search or paste SimpleX link" 10 || true
  "$ui" key ctrl+v
  check "the pasted one-time link offers to connect" "$ui" wait "Use current profile" 30
  "$ui" shot pasted-link
  "$ui" click-text "Use current profile" 10 || true
  check "the CLI peer connects to the app" eventually 120 peer_connected
  "$ui" key Escape
  "$ui" click-text "$other_chat" 10 || true
  "$ui" shot other-chat-open
  peer 5 "@$app_user first message"
  peer 5 "@$app_user second message"
  check "a message notification is shown" eventually 30 notified "first message"
  check "the chat's next message replaces its notification" eventually 30 replaced
  notification=$(notification_with "second message")
  echo "notification: $notification"
  check "the notification is titled with the contact's name" grep -q "$peer_name" <<< "$(jq -r .summary <<< "$notification")"
  check "the notification names the desktop entry $entry_id" [ "$(jq -r .desktop_entry <<< "$notification")" = "$entry_id" ]
  check "the notification opens its chat by the default action" jq -e '.actions | index("default")' <<< "$notification"
  check "the launcher entry is marked urgent" urgent
  id=$(jq -r .id <<< "$notification")
  gdbus call --session --dest org.freedesktop.Notifications --object-path /org/freedesktop/Notifications \
    --method chat.simplex.ci.Notifications.Invoke "$id" default || true
  check "clicking the notification opens its chat" eventually 20 right_pane_shows "second message"
  "$ui" shot notification-opened
  check "reading the chat withdraws its notification" eventually 20 closed "$id"
}

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

if [ "${smoke_status:-0}" -eq 0 ]; then
  follow_colour_scheme
  activate_from_tray
  notify_messages
fi

if [ "${smoke_status:-0}" -ne 0 ]; then
  echo "::error::$kind $UI_SESSION: the smoke test failed"
  exit "$smoke_status"
fi
if grep -q '^FAIL' "$checks"; then
  echo "error: $(grep -c '^FAIL' "$checks") of $(wc -l < "$checks") end-to-end checks failed"
  exit 1
fi
echo "end-to-end test passed: $kind under $UI_SESSION"
