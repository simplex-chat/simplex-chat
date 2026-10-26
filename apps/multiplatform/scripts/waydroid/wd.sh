#!/usr/bin/env bash
# wd.sh — headless Waydroid (Android 13 / LineageOS 20, x86_64 + arm64 translation) for GUI testing.
# Works on Qubes AppVMs (root FS reset on restart; everything is rebuilt from $WD_HOME) and on other Linux hosts with
# systemd, binderfs and passwordless sudo. See README.md next to this script.
set -uo pipefail

SELF_DIR=$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")
WD=${WD_HOME:-$HOME/waydroid}
WD2=${WD2_HOME:-$HOME/waydroid2}
find_adb() {
  local c
  for c in "${ADB:-}" "$HOME/Android/Sdk/platform-tools/adb" "${ANDROID_HOME:+$ANDROID_HOME/platform-tools/adb}" "${ANDROID_SDK_ROOT:+$ANDROID_SDK_ROOT/platform-tools/adb}" "$(command -v adb 2>/dev/null)"; do
    [[ -n $c && -x $c ]] && { echo "$c"; return; }
  done
  echo adb
}
ADB=$(find_adb)
ADB_KEY=${ANDROID_USER_HOME:-$HOME/.android}/adbkey.pub
WESTON_INI=$WD/weston.ini; [[ -f $WESTON_INI ]] || WESTON_INI=$SELF_DIR/weston.ini
ANDROID_VERSION=${WD_ANDROID_VERSION:-13}
is_qubes() { [[ -d /run/qubes ]] && command -v qubesdb-read >/dev/null; }
# Two independent Androids can run side by side (WD_INSTANCE=1|2, default 1). Instance 2 is a patched copy of
# Waydroid in $WD2_HOME (own state, LXC name, bridge waydroid1, private D-Bus, binder2 nodes); see the README.
INST=${WD_INSTANCE:-1}
case $INST in
  1) STATE=$WD;  VARLIB=/var/lib/waydroid;  BR=waydroid0; WAYLAND_SOCK=wayland-waydroid;  CSVC=waydroid-container
     WDY=(waydroid) ;;
  2) STATE=$WD2; VARLIB=/var/lib/waydroid2; BR=waydroid1; WAYLAND_SOCK=wayland-waydroid2; CSVC=waydroid2-container
     W2ENV=("DBUS_SYSTEM_BUS_ADDRESS=unix:path=$STATE/run/sys.sock" "DBUS_SESSION_BUS_ADDRESS=unix:path=$STATE/run/ses.sock" "XDG_DATA_HOME=$STATE/xdg")
     WDY=(env "${W2ENV[@]}" python3 "$STATE/lib/waydroid.py") ;;
  *) echo "wd: error: WD_INSTANCE must be 1 or 2" >&2; exit 1 ;;
esac
RUN=$STATE/run
LOCK=$STATE/.lock
OWNER_FILE=$STATE/.owner
CLAIM_TTL=${WD_CLAIM_TTL:-1800}
SCREEN_W=${WD_WIDTH:-720}
SCREEN_H=${WD_HEIGHT:-1440}
TRANSLATION=${WD_TRANSLATION:-libndk}
BOOT_TIMEOUT=${WD_BOOT_TIMEOUT:-300}
WESTON_SHELL=${WD_WESTON_SHELL:-desktop-shell.so}
export XDG_RUNTIME_DIR=${XDG_RUNTIME_DIR:-/run/user/$(id -u)}
mkdir -p "$RUN"

die() { echo "wd: error: $*" >&2; exit 1; }
note() { echo "wd: $*" >&2; }

usage() {
  cat <<'EOF'
usage: wd.sh <command> [args]
 lifecycle:  setup | up | down | status | who | claim <name> [--force] | release [--force] | wait-free [secs]
 app:        install <apk> | uninstall <pkg> | launch <pkg> [activity] | stop-app <pkg> | reset-app <pkg> | packages
 screen:     shot <file.png> [--half|--crop x,y,w,h] | dump [file.xml] | nodes | find <text> | tap x y | tap-text <text>
             tap-row <row label>  (the toggle/dropdown to the right of a labelled settings row)
             wait <text> [secs] | gone <text> [secs] | swipe x1 y1 x2 y2 [ms]
             text "<string>" | key <keycode|name> | back | home
 debug:      logcat [logcat args] | shell <cmd...> | rootsh <cmd...> | adb <args...> | serial
             bt [pkg]  native backtrace of every thread of the running app (debuggerd -b; default chat.simplex.app.debug)
             adb-path  the adb binary wd.sh uses (ADB, ~/Android/Sdk, ANDROID_HOME, ANDROID_SDK_ROOT, then PATH)
 network:    reverse <device-port> <host-port> | reverse-clear | net isolate | net open | net status
Owner: set WD_OWNER=<name> on every call after `claim` (claims expire after WD_CLAIM_TTL s of inactivity, default 1800).
Instance: WD_INSTANCE=1 (default) or 2 selects one of two independent Androids; each has its own claim.
State: WD_HOME (default ~/waydroid) and WD2_HOME (default ~/waydroid2); see README.md next to this script.
EOF
}

# ---------- locking / ownership ----------
# Every command except the read-only ones and claim/release holds an exclusive flock for its duration (serialises sessions). Long-lived exclusive use is
# a "claim" in $OWNER_FILE: "<name> <epoch-of-last-use>". Commands from a different WD_OWNER fail while a
# claim is live; read-only commands (status/who/serial/nodes/dump/shot/find/wait/gone/logcat/packages/wait-free) are always allowed.
with_lock() { exec {LFD}>>"$LOCK"; flock -w 600 "$LFD" || die "timed out waiting 600 s for $LOCK (another wd.sh command is running: $(cat "$RUN/current" 2>/dev/null))"; echo "${WD_OWNER:-anonymous} pid $$ $(date -Is) $*" > "$RUN/current"; }
claim_live() { [[ -s $OWNER_FILE ]] || return 1; local n t; read -r n t < "$OWNER_FILE"; (( $(date +%s) - t < CLAIM_TTL )); }
check_owner() {
  claim_live || return 0
  local n t; read -r n t < "$OWNER_FILE"
  [[ "${WD_OWNER:-}" == "$n" ]] || die "Waydroid $INST is claimed by '$n' (last use $(( $(date +%s) - t )) s ago, expires after ${CLAIM_TTL}s idle). Run 'wd.sh who', 'wd.sh wait-free', or claim with --force if the owner is gone."
  echo "$n $(date +%s)" > "$OWNER_FILE"
}

# ---------- helpers ----------
wdy() { "${WDY[@]}" "$@"; }
swdy() { sudo "${WDY[@]}" "$@"; }
serial() { local ip; ip=$(wdy status 2>/dev/null | awk '/IP address/ {print $3}'); [[ $ip =~ ^[0-9.]+$ ]] || return 1; echo "$ip:5555"; }
need_dev() {
  local s; s=$(serial) || die "Waydroid is not running or has no IP (run: wd.sh up)"
  if ! timeout 5 "$ADB" -s "$s" get-state >/dev/null 2>&1; then
    "$ADB" connect "$s" >/dev/null 2>&1; sleep 1
    timeout 5 "$ADB" -s "$s" get-state >/dev/null 2>&1 || die "adb cannot reach $s (container frozen? try: wd.sh up)"
  fi
  DEV=$s
}
a() { "$ADB" -s "$DEV" "$@"; }
container_state() { wdy status 2>/dev/null | awk -F'\t' '/^Container:/ {print $2}'; }
unfreeze() { [[ "$(container_state)" == FROZEN ]] && { note "container was FROZEN, unfreezing"; swdy container unfreeze; }; return 0; }
weston_pid() { local p; p=$(cat "$RUN/weston.pid" 2>/dev/null) && [[ $(cat "/proc/$p/comm" 2>/dev/null) == weston ]] \
  && tr '\0' '\n' < "/proc/$p/cmdline" 2>/dev/null | grep -qx -- "--socket=$WAYLAND_SOCK" && echo "$p"; }   # a pid file outlives a reboot
weston_alive() { [[ -S "$XDG_RUNTIME_DIR/$WAYLAND_SOCK" ]] && weston_pid >/dev/null; }
weston_render_args() {   # weston 12 added --renderer and short backend names (--use-pixman stays as a deprecated alias)
  local v; v=$(weston --version 2>/dev/null | grep -oE '[0-9]+' | head -1)
  if (( ${v:-10} >= 12 )); then echo --backend=headless --renderer=pixman
  else echo --backend=headless-backend.so --use-pixman; fi
}

# ---------- setup ----------
install_packages() {
  if [[ -d $WD/debs ]] && command -v apt-get >/dev/null; then
    note "installing cached debs from $WD/debs"
    sudo DEBIAN_FRONTEND=noninteractive apt-get install -y --no-download "$WD"/debs/*.deb && return 0
    note "offline install failed, trying with network"
  fi
  if command -v apt-get >/dev/null; then
    local codename; codename=$(. /etc/os-release && echo "${VERSION_CODENAME:-}")
    [[ $codename ]] || die "cannot tell the distribution codename from /etc/os-release; install waydroid, weston and dnsmasq yourself"
    [[ -f /usr/share/keyrings/waydroid.gpg ]] || sudo curl -sSf https://repo.waydro.id/waydroid.gpg -o /usr/share/keyrings/waydroid.gpg
    echo "deb [signed-by=/usr/share/keyrings/waydroid.gpg] https://repo.waydro.id/ $codename main" | sudo tee /etc/apt/sources.list.d/waydroid.list >/dev/null
    sudo apt-get update -qq && sudo DEBIAN_FRONTEND=noninteractive apt-get install -y waydroid weston lzip dnsmasq-base unzip
  elif command -v dnf >/dev/null; then
    sudo dnf install -y waydroid weston dnsmasq unzip
  else
    die "no apt-get or dnf: install waydroid, weston (with weston-screenshooter) and dnsmasq with your package manager, then rerun"
  fi
}
# waydroid_script runs as root: only this reviewed commit, and its Python dependencies only as these wheels
WAYDROID_SCRIPT_URL=https://github.com/casualsnek/waydroid_script
WAYDROID_SCRIPT_COMMIT=48dbfaf34a6ddbe78688c530f9ba1c26522aafb2
WAYDROID_SCRIPT_REQS='certifi==2026.7.22 --hash=sha256:62f22742b58a1a33014a2b6b706588a8d7e2a88ae7bd1a6ebe8c992928483775
charset-normalizer==3.5.2 --hash=sha256:b6b751274acb69d77b3323d6b7dbaa3c7fdfc1eb829b7eb61d262f32e1af9685
idna==3.20 --hash=sha256:ab7ae7122974553370f0bdb919e1a960b2cd1bc1ef0276416d896db81c14582c
inquirerpy==0.3.4 --hash=sha256:c65fdfbac1fa00e3ee4fb10679f4d3ed7a012abf4833910e63c295827fe2a7d4
pfzy==0.3.4 --hash=sha256:5f50d5b2b3207fa72e7ec0ef08372ef652685470974a107d0d4999fc5a903a96
prompt_toolkit==3.0.53 --hash=sha256:01c0891d7f9237d5e339f7d3e42cdae80b7534abb1c7c0e3352efba6231492f2
requests==2.34.2 --hash=sha256:2a0d60c172f83ac6ab31e4554906c0f3b3588d37b5cb939b1c061f4907e278e0
tqdm==4.70.1 --hash=sha256:c293e525e6fef9c20e8728fd4612df02a0aa31bb5fe91ecd93e123b1b7bffa73
urllib3==2.8.0 --hash=sha256:0cf3cae568d36aa9576b28dfb35f11328f1cb974ca7647d9475ebb86c75ac6e3
wcwidth==0.9.2 --hash=sha256:89ca642c5bf0101157a09366be69fad0379db1f700ae39a920e103234573670e'
install_translation() {
  local ws=$WD/waydroid_script
  [[ -d $ws/.git ]] || git init -q "$ws" || die "git init of $ws failed"
  if [[ $(git -C "$ws" rev-parse -q --verify HEAD) != "$WAYDROID_SCRIPT_COMMIT" ]]; then
    note "fetching waydroid_script $WAYDROID_SCRIPT_COMMIT into $ws"
    git -C "$ws" fetch -q --depth 1 "$WAYDROID_SCRIPT_URL" "$WAYDROID_SCRIPT_COMMIT" && git -C "$ws" checkout -q --detach "$WAYDROID_SCRIPT_COMMIT" \
      || die "fetching waydroid_script $WAYDROID_SCRIPT_COMMIT failed"
  fi
  [[ $(git -C "$ws" rev-parse HEAD) == "$WAYDROID_SCRIPT_COMMIT" && -z $(git -C "$ws" status --porcelain --untracked-files=no) ]] \
    || die "$ws is not at waydroid_script $WAYDROID_SCRIPT_COMMIT or has local changes"
  if [[ "$(cat "$WD/venv/installed-requirements.txt" 2>/dev/null)" != "$WAYDROID_SCRIPT_REQS" ]]; then
    python3 -m venv "$WD/venv" && printf '%s\n' "$WAYDROID_SCRIPT_REQS" > "$WD/venv/requirements.txt" \
      && "$WD/venv/bin/pip" install -q --require-hashes --only-binary :all: --no-deps -r "$WD/venv/requirements.txt" \
      && cp "$WD/venv/requirements.txt" "$WD/venv/installed-requirements.txt" \
      || die "venv for waydroid_script failed (python3-venv installed? Python 3.10 or newer?)"
  fi
  mkdir -p "$HOME/.cache/waydroid-script/downloads"
  cp -n "$WD"/translation/*.zip "$HOME/.cache/waydroid-script/downloads/" 2>/dev/null
  sudo env HOME="$HOME" SUDO_USER="$(id -un)" "$WD/venv/bin/python" "$ws/main.py" -a "$ANDROID_VERSION" install "$TRANSLATION" \
    || die "translation install failed"
}
cmd_setup() {
  # 1. packages (Qubes: from cached debs, offline-capable; the root FS loses them on every restart)
  command -v waydroid >/dev/null && command -v weston >/dev/null || install_packages || die "package install failed"
  command -v dnsmasq >/dev/null || { command -v apt-get >/dev/null && sudo DEBIAN_FRONTEND=noninteractive apt-get install -y dnsmasq-base; } \
    || die "dnsmasq missing (Waydroid DHCP needs it)"
  # 2. images: local ones in $WD/images if present, otherwise `waydroid init` downloads them
  local have_images=; [[ -f $WD/images/system.img && -f $WD/images/vendor.img ]] && have_images=1
  is_qubes && [[ ! $have_images ]] && die "missing $WD/images/{system,vendor}.img (run fetch-images.sh; see README, Troubleshooting: 'images lost')"
  if [[ $have_images ]]; then
    sudo mkdir -p /etc/waydroid-extra
    [[ "$(readlink /etc/waydroid-extra/images)" == "$WD/images" ]] || sudo ln -sfn "$WD/images" /etc/waydroid-extra/images
  fi
  # 3. state: on Qubes (and wherever $WD/var exists) /var/lib/waydroid is a bind mount of $WD/var, so it survives a root FS reset
  if is_qubes || [[ -d $WD/var ]]; then
    mkdir -p "$WD/var"; sudo mkdir -p /var/lib/waydroid
    mountpoint -q /var/lib/waydroid || sudo mount --bind "$WD/var" /var/lib/waydroid || die "bind mount of $WD/var failed"
  fi
  # 4. init only once (config, lxc, overlay)
  if [[ ! -f /var/lib/waydroid/waydroid.cfg || ! -d /var/lib/waydroid/lxc/waydroid ]]; then
    note "waydroid init ($([[ $have_images ]] && echo 'from local images' || echo 'downloading images'))"
    sudo waydroid init -f || die "waydroid init failed"
  fi
  # 5. arm translation (overlay + props persist with the state; re-applied if init regenerated the props)
  if ! grep -q '^ro.dalvik.vm.native.bridge=lib' /var/lib/waydroid/waydroid_base.prop 2>/dev/null; then
    note "installing $TRANSLATION arm translation"
    install_translation
  fi
  # 6. container service + firewall
  sudo systemctl start waydroid-container || die "waydroid-container service failed"
  net_allow
  note "setup done: $(grep -E '^ro.dalvik.vm.native.bridge' /var/lib/waydroid/waydroid_base.prop)"
}
net_allow() {
  if sudo nft list chain ip qubes custom-input >/dev/null 2>&1; then
    # Qubes' input policy is drop: allow DHCP/DNS/host servers from the Waydroid bridge
    sudo nft list chain ip qubes custom-input 2>/dev/null | grep -q "iifname \"$BR\" accept" \
      || sudo nft add rule ip qubes custom-input iifname "$BR" accept 2>/dev/null || note "could not add nft input rule"
  elif command -v firewall-cmd >/dev/null && sudo firewall-cmd --state >/dev/null 2>&1; then
    sudo firewall-cmd --zone=trusted --query-interface="$BR" >/dev/null 2>&1 \
      || sudo firewall-cmd --zone=trusted --add-interface="$BR" >/dev/null 2>&1 || note "could not add $BR to the firewalld trusted zone"
  elif command -v ufw >/dev/null && sudo ufw status 2>/dev/null | grep -q '^Status: active'; then
    sudo ufw status 2>/dev/null | grep -q "on $BR" || note "ufw is active and may block DHCP/DNS from $BR; if the container gets no IP: sudo ufw allow in on $BR && sudo ufw route allow in on $BR"
  fi
}
ensure_setup() {
  local mounted=1; if is_qubes || [[ -d $WD/var ]]; then mountpoint -q /var/lib/waydroid || mounted=; fi
  command -v waydroid >/dev/null && [[ $mounted ]] && [[ -d /var/lib/waydroid/lxc/waydroid ]] || cmd_setup
  [[ $INST == 2 ]] && setup2
  return 0
}
# Instance 2 runtime pieces (mounts, binder nodes, D-Bus; lost on reboot or a qube restart, so re-created on every `up`; idempotent).
setup2() {
  [[ -f $STATE/lib/waydroid.py && -d $STATE/var/lxc/waydroid2 ]] || die "instance 2 is not provisioned ($STATE/lib, $STATE/var): run $SELF_DIR/provision2.sh (instance 1 must be set up first; see README)"
  [[ -e /dev/binderfs/binder-control ]] || sudo systemctl start waydroid-container   # instance 1's manager mounts binderfs
  sudo mkdir -p "$VARLIB"; mountpoint -q "$VARLIB" || sudo mount --bind "$STATE/var" "$VARLIB" || die "bind mount of $STATE/var failed"
  sudo env "${W2ENV[@]}" python3 -c 'import sys; sys.path.insert(0, sys.argv[1]); from tools.helpers.drivers import allocBinderNodes; allocBinderNodes(None, ["binder2", "vndbinder2", "hwbinder2"])' "$STATE/lib" \
    || die "could not allocate binder2 nodes"
  local n; for n in binder2 vndbinder2 hwbinder2; do [[ -e /dev/$n ]] || sudo ln -s "/dev/binderfs/$n" "/dev/$n"; done
  binder_guard
  mkdir -p "$STATE/run" "$STATE/xdg"
  if ! env "${W2ENV[@]}" dbus-send --system --print-reply --dest=org.freedesktop.DBus / org.freedesktop.DBus.ListNames >/dev/null 2>&1; then
    rm -f "$STATE/run/sys.sock"; sudo dbus-daemon --config-file="$STATE/sys.conf" --fork --print-pid {LFD}>&- > "$STATE/run/sysbus.pid" || die "private system bus failed"
  fi
  if ! env "${W2ENV[@]}" dbus-send --session --print-reply --dest=org.freedesktop.DBus / org.freedesktop.DBus.ListNames >/dev/null 2>&1; then
    rm -f "$STATE/run/ses.sock"; dbus-daemon --session --address="unix:path=$STATE/run/ses.sock" --fork --print-pid {LFD}>&- > "$STATE/run/sesbus.pid" || die "private session bus failed"
  fi
}
# Two Androids on one binder context corrupt each other's service registry (seen 2026-10-13: instance 1 lost
# its display service). Refuse to boot instance 2 unless its container gets binder devices of its own.
binder_guard() {
  local nodes=$STATE/var/lxc/waydroid2/config_nodes d own other
  for d in binder vndbinder hwbinder; do
    grep -qx "lxc.mount.entry = /dev/${d}2 dev/$d none bind,create=file,optional 0 0" "$nodes" \
      || die "binder guard: $nodes does not map /dev/${d}2 to dev/$d; refusing to start instance 2"
    own=$(stat -L -c %t:%T "/dev/${d}2" 2>/dev/null) other=$(stat -L -c %t:%T "/dev/$d" 2>/dev/null)
    [[ $own && $own != "$other" ]] || die "binder guard: /dev/${d}2 ($own) is not distinct from /dev/$d ($other)"
  done
}
csvc_start() {
  if [[ $INST == 1 ]]; then sudo systemctl start waydroid-container; return; fi
  sudo systemctl reset-failed "$CSVC" 2>/dev/null
  sudo systemd-run --unit="$CSVC" --setenv="${W2ENV[0]}" --setenv="${W2ENV[1]}" /usr/bin/python3 "$STATE/lib/waydroid.py" container start >/dev/null 2>&1
  # systemd-run returns at once (instance 1's unit waits for its BusName); the session needs the name owned
  local _; for _ in $(seq 30); do
    env "${W2ENV[@]}" dbus-send --system --print-reply --dest=org.freedesktop.DBus / org.freedesktop.DBus.NameHasOwner \
      string:id.waydro.Container 2>/dev/null | grep -q 'boolean true' && return 0
    sleep 0.5
  done
  die "instance 2 container service did not come up; see: sudo journalctl -u $CSVC"
}

# ---------- lifecycle ----------
cmd_up() {
  ensure_setup
  systemctl is-active -q "$CSVC" || csvc_start
  net_allow
  if ! weston_alive; then
    rm -f "$XDG_RUNTIME_DIR/$WAYLAND_SOCK" "$XDG_RUNTIME_DIR/$WAYLAND_SOCK.lock"
    # --debug enables the screenshooter protocol used by `shot` (adb screencap does not work in Waydroid)
    local wargs; read -ra wargs <<< "$(weston_render_args)"
    setsid weston --config="$WESTON_INI" "${wargs[@]}" --debug --socket=$WAYLAND_SOCK \
      --width="$SCREEN_W" --height="$SCREEN_H" --shell="$WESTON_SHELL" --idle-time=0 </dev/null >"$RUN/weston.log" 2>&1 {LFD}>&- &
    echo $! > "$RUN/weston.pid"
    for _ in $(seq 50); do [[ -S "$XDG_RUNTIME_DIR/$WAYLAND_SOCK" ]] && break; sleep 0.2; done
    [[ -S "$XDG_RUNTIME_DIR/$WAYLAND_SOCK" ]] || die "weston did not start, see $RUN/weston.log"
  fi
  local t0; t0=$(date +%s)
  if ! wdy status 2>/dev/null | grep -q '^Session:.*RUNNING'; then
    # a previous session that died mid-teardown can leave rootfs mounted; waydroid then fails with "umount rootfs"
    findmnt "$VARLIB/rootfs" >/dev/null 2>&1 && { note "unmounting stale rootfs"; sudo umount -R "$VARLIB/rootfs" 2>/dev/null || sudo umount -R -l "$VARLIB/rootfs"; }
    # show-full-ui = session start + a full-screen Waydroid window on weston (needed for `shot`; apps run without it)
    WAYLAND_DISPLAY=$WAYLAND_SOCK setsid "${WDY[@]}" show-full-ui </dev/null >"$RUN/session.log" 2>&1 {LFD}>&- &
    echo $! > "$RUN/session.pid"
  fi
  unfreeze
  local ok=
  for _ in $(seq "$BOOT_TIMEOUT"); do
    [[ "$(swdy shell -- getprop sys.boot_completed 2>/dev/null | tr -d '\r')" == 1 ]] && { ok=1; break; }
    sleep 1
  done
  [[ $ok ]] || die "Android did not boot in ${BOOT_TIMEOUT}s; see $RUN/session.log and $VARLIB/waydroid.log"
  # never freeze the container, never sleep the screen
  wdy prop set persist.waydroid.suspend false >/dev/null 2>&1
  local key; key=$(cat "$ADB_KEY" 2>/dev/null) || { "$ADB" start-server >/dev/null; key=$(cat "$ADB_KEY"); }
  printf '%s\n' "$key" | swdy shell -- sh -c 'k=$(cat); f=/data/misc/adb/adb_keys; mkdir -p /data/misc/adb; touch "$f"; [ -z "$k" ] || grep -qxF "$k" "$f" || printf "%s\n" "$k" >> "$f"; chown system:shell "$f"; chmod 640 "$f"'
  swdy shell -- cat /data/misc/adb/adb_keys | tr -d '\r' | grep -xF -- "$key" >/dev/null || die "could not add $ADB_KEY to the container's adb keys"
  local s=
  for _ in $(seq 30); do s=$(serial) && break; sleep 1; done
  [[ $s ]] || die "container has no IPv4 (DHCP blocked by the host firewall? Qubes: 'sudo nft list chain ip qubes custom-input'; see README, Networking)"
  "$ADB" connect "$s" >/dev/null
  for _ in $(seq 30); do timeout 5 "$ADB" -s "$s" get-state 2>/dev/null | grep -q device && break; "$ADB" connect "$s" >/dev/null 2>&1; sleep 1; done
  DEV=$s
  a shell 'svc power stayon true; settings put system screen_off_timeout 2147483647; input keyevent KEYCODE_WAKEUP; wm dismiss-keyguard' >/dev/null 2>&1
  note "up: android booted ($(( $(date +%s) - t0 )) s), adb serial $s, screen ${SCREEN_W}x${SCREEN_H}"
  echo "$s"
}
cmd_down() {
  local s; s=$(serial 2>/dev/null) && "$ADB" disconnect "$s" >/dev/null 2>&1
  wdy session stop >/dev/null 2>&1
  for _ in $(seq 30); do wdy status 2>/dev/null | grep -q '^Session:.*RUNNING' || break; sleep 1; done
  local w; w=$(weston_pid) && kill "$w" 2>/dev/null
  rm -f "$RUN/weston.pid" "$RUN/session.pid"
  sudo systemctl stop "$CSVC" 2>/dev/null
  findmnt "$VARLIB/rootfs" >/dev/null 2>&1 && { sudo umount -R "$VARLIB/rootfs" 2>/dev/null || sudo umount -R -l "$VARLIB/rootfs"; }
  note "down: session stopped, weston stopped, container service stopped"
}
cmd_status() {
  echo "instance:   $INST (bridge $BR, state $STATE)"
  echo "packages:   $(command -v waydroid >/dev/null && echo "waydroid $(waydroid --version 2>/dev/null)" || echo 'NOT INSTALLED (run: wd.sh setup)')"
  echo "var mount:  $(mountpoint -q "$VARLIB" && echo "$VARLIB <- $STATE/var" || { [[ $INST == 1 ]] && ! is_qubes && [[ ! -d $WD/var ]] && echo "$VARLIB (not a bind mount)" || echo 'NOT MOUNTED'; })"
  echo "weston:     $(weston_alive && echo "running pid $(cat "$RUN/weston.pid") socket $WAYLAND_SOCK" || echo 'not running')"
  command -v waydroid >/dev/null && wdy status 2>/dev/null | sed 's/^/waydroid:   /'
  local s; if s=$(serial); then
    echo "adb:        $s $(timeout 5 "$ADB" -s "$s" get-state 2>/dev/null || echo 'not connected')"
    echo "boot:       $(timeout 5 "$ADB" -s "$s" shell getprop sys.boot_completed 2>/dev/null | tr -d '\r')"
    echo "abilist:    $(timeout 5 "$ADB" -s "$s" shell getprop ro.product.cpu.abilist 2>/dev/null | tr -d '\r')"
  fi
  cmd_who
}
cmd_who() {
  if claim_live; then local n t; read -r n t < "$OWNER_FILE"; echo "claim:      '$n', last use $(( $(date +%s) - t )) s ago (expires after ${CLAIM_TTL}s idle)"
  else echo "claim:      free"; fi
  echo "last cmd:   $(cat "$RUN/current" 2>/dev/null)"
}
cmd_claim() {
  local name=${1:-} force=${2:-}; [[ $name ]] || die "usage: wd.sh claim <name> [--force]"
  if claim_live; then local n t; read -r n t < "$OWNER_FILE"
    [[ $n == "$name" || $force == --force ]] || die "already claimed by '$n' ($(( $(date +%s) - t )) s ago); use 'wd.sh wait-free' or --force"
  fi
  echo "$name $(date +%s)" > "$OWNER_FILE"; note "claimed as '$name' — pass WD_OWNER=$name on every call"
}
cmd_release() { if claim_live; then local n t; read -r n t < "$OWNER_FILE"; [[ "${WD_OWNER:-}" == "$n" || "${1:-}" == --force ]] || die "claimed by '$n', not '${WD_OWNER:-}' (use --force)"; fi; rm -f "$OWNER_FILE"; note "released"; }
cmd_wait_free() { local max=${1:-3600} t0; t0=$(date +%s); while claim_live; do (( $(date +%s) - t0 > max )) && die "still claimed after ${max}s"; sleep 10; done; note "free"; }

# ---------- app ----------
cmd_install() {
  local apk=${1:-}; [[ -f $apk ]] || die "usage: wd.sh install <apk> (file not found: '$apk')"
  need_dev; unfreeze
  local abis; abis=$(unzip -l "$apk" 2>/dev/null | grep -oE 'lib/[^/]+/' | sort -u | tr '\n' ' ')
  note "installing $apk (native: ${abis:-none})"
  timeout 900 "$ADB" -s "$DEV" install -r -g "$apk" || die "install failed"
}
cmd_uninstall() { [[ ${1:-} ]] || die "usage: wd.sh uninstall <pkg>"; need_dev; a uninstall "$1"; }
cmd_launch() {
  local pkg=${1:-} act=${2:-}; [[ $pkg ]] || die "usage: wd.sh launch <pkg> [activity]"
  need_dev; unfreeze
  [[ $act ]] || act=$(a shell cmd package resolve-activity --brief -c android.intent.category.LAUNCHER "$pkg" | tr -d '\r' | tail -1)
  [[ $act == */* ]] || die "no launcher activity for '$pkg' (installed? see: wd.sh packages)"
  # am start -W never returns when the process is killed during start (low memory); don't hold the lock forever
  timeout 60 "$ADB" -s "$DEV" shell am start -W -n "$act" || die "launch did not complete in 60 s (process start timeout or low memory? see: wd.sh logcat -d -t 200, free -m)"
}
cmd_stop_app() { [[ ${1:-} ]] || die "usage: wd.sh stop-app <pkg>"; need_dev; a shell am force-stop "$1"; }
cmd_reset_app() { [[ ${1:-} ]] || die "usage: wd.sh reset-app <pkg>"; need_dev; a shell pm clear "$1"; }
cmd_packages() { need_dev; a shell pm list packages -3; }

# ---------- screen ----------
cmd_shot() {
  local out=${1:-}; [[ $out ]] || die "usage: wd.sh shot <file.png>"
  weston_alive || die "weston not running (wd.sh up)"
  local d; d=$(mktemp -d)
  (cd "$d" && WAYLAND_DISPLAY=$WAYLAND_SOCK timeout 20 weston-screenshooter >/dev/null 2>&1)
  local f; f=$(ls "$d"/*.png 2>/dev/null | head -1)
  [[ -s $f ]] || { rm -rf "$d"; die "screenshot failed (weston must run with --debug: wd.sh down && wd.sh up)"; }
  mkdir -p "$(dirname "$out")"; mv "$f" "$out"; rm -rf "$d"
  case ${2:-} in
    "") ;;
    --half) python3 -I -c 'import sys; from PIL import Image; im=Image.open(sys.argv[1]); im.resize((im.width//2, im.height//2), Image.LANCZOS).save(sys.argv[1])' "$out" ;;
    --crop) [[ ${3:-} =~ ^[0-9]+,[0-9]+,[0-9]+,[0-9]+$ ]] || die "usage: wd.sh shot <file> --crop x,y,w,h"
            python3 -I -c 'import sys; from PIL import Image; x,y,w,h=map(int,sys.argv[2].split(",")); Image.open(sys.argv[1]).crop((x,y,x+w,y+h)).save(sys.argv[1])' "$out" "$3" ;;
    *) die "usage: wd.sh shot <file.png> [--half|--crop x,y,w,h]" ;;
  esac
  echo "$out"
}
dump_xml() {
  need_dev; unfreeze
  local x i
  for i in 1 2 3 4; do   # uiautomator dump fails while the screen animates (video, transitions)
    x=$(a exec-out uiautomator dump /dev/tty 2>/dev/null); x=${x%UI hierchary dumped to*}
    [[ $x == *"</hierarchy>"* ]] && { printf '%s' "$x"; return 0; }
    sleep 0.7
  done
  die "uiautomator dump failed 4 times (screen animating? use: wd.sh shot)"
}
cmd_dump() { if [[ ${1:-} ]]; then dump_xml > "$1" && echo "$1"; else dump_xml; echo; fi; }
nodes_py='
import sys, re, xml.etree.ElementTree as ET
root = ET.fromstring(sys.stdin.read())
want = sys.argv[1] if len(sys.argv) > 1 else None
best = len(sys.argv) > 2 and sys.argv[2] == "best"
hits = []
def b(n): return [int(v) for v in re.findall(r"-?\d+", n.get("bounds"))]
def walk(n, clickable_anc):
    ca = n if n.get("clickable") == "true" or n.get("long-clickable") == "true" else clickable_anc
    label = n.get("text") or n.get("content-desc") or ""
    rid = n.get("resource-id") or ""
    if n.tag == "node" and (label or rid or n.get("clickable") == "true" or n.get("class","").endswith("EditText")):
        x1,y1,x2,y2 = b(n); t = ca if ca is not None else n; tx1,ty1,tx2,ty2 = b(t)
        tap = ((tx1+tx2)//2, (ty1+ty2)//2)
        flags = "".join(f for f,k in (("C","clickable"),("F","focused"),("S","scrollable"),("K","checked"),("P","password")) if n.get(k)=="true")
        cls = n.get("class", "").split(".")[-1]
        line = f"{tap[0]:>4} {tap[1]:>4}  {flags:<3} {cls:<14} {rid:<28} {label!r}"
        if want is None or (want.lower() in label.lower() or want == rid):
            if best: hits.append((label.strip().lower() == want.strip().lower() or want == rid, line))
            else: print(line)
    for c in n: walk(c, ca)
walk(root, None)
if best and hits:
    exact = [l for e, l in hits if e]
    pick = exact[0] if exact else hits[0][1]
    if not exact and len(hits) > 1:
        print(f"wd: {len(hits)} nodes contain {want!r} and none equals it; tapping the first. Candidates:", file=sys.stderr)
        for _, l in hits[:10]: print("   " + l, file=sys.stderr)
    print(pick)
'
cmd_nodes() { echo " tapX tapY flg class          resource-id                  label   (tap = centre of nearest clickable ancestor)"; dump_xml | python3 -I -c "$nodes_py"; }
cmd_find() { [[ ${1:-} ]] || die "usage: wd.sh find <text|resource-id>"; local r; r=$(dump_xml | python3 -I -c "$nodes_py" "$1"); [[ $r ]] || die "no node matching '$1'"; echo "$r"; }
cmd_tap() { [[ ${1:-} =~ ^[0-9]+$ && ${2:-} =~ ^[0-9]+$ ]] || die "usage: wd.sh tap <x> <y>"; need_dev; a shell input tap "$1" "$2"; }
cmd_tap_text() {
  [[ ${1:-} ]] || die "usage: wd.sh tap-text <text|resource-id>"
  need_dev
  local r; r=$(dump_xml | python3 -I -c "$nodes_py" "$1" best); [[ $r ]] || die "no node matching '$1' (see: wd.sh nodes)"
  local x y; read -r x y _ <<< "$r"; note "tap $x $y  ($r)"; a shell input tap "$x" "$y"
}
row_py='
import sys, re, xml.etree.ElementTree as ET
root = ET.fromstring(sys.stdin.read()); want = sys.argv[1].strip().lower()
def b(n): return [int(v) for v in re.findall(r"-?\d+", n.get("bounds"))]
def lab(n): return (n.get("text") or n.get("content-desc") or "").strip()
nodes = list(root.iter("node"))
rows = [n for n in nodes if lab(n).lower() == want] or [n for n in nodes if want in lab(n).lower()]
if not rows: sys.exit(2)
if any(abs((b(n)[1] + b(n)[3]) - (b(rows[0])[1] + b(rows[0])[3])) // 2 > 24 for n in rows): print(f"wd: {len(rows)} labels on different lines match {want!r}; using the first", file=sys.stderr)
lx1, ly1, lx2, ly2 = b(rows[0]); cy = (ly1 + ly2) // 2
c = []
for n in nodes:
    if n.get("clickable") != "true": continue
    x1, y1, x2, y2 = b(n)
    if x1 <= lx1 and x2 >= lx2 and y1 <= ly1 and y2 >= ly2: continue   # a container of the label (the row itself)
    if x1 < lx2 or not (y1 - 4 <= cy <= y2 + 4): continue             # must sit to the right, level with the label (not the row below a header)
    c.append((abs((y1 + y2) // 2 - cy), (x1 + x2) // 2, (y1 + y2) // 2, lab(n)))
if not c: sys.exit(3)
d, x, y, l = min(c, key=lambda t: (t[0] > 8, -t[1])); print(x, y, repr(lab(rows[0])), repr(l))   # rightmost on the line: an info icon may sit between label and toggle
'
cmd_tap_row() {   # tap-row <row label>: the clickable (toggle, dropdown) to the right of a row label; toggles have no label and every dropdown is labelled "Dropdown menu"
  [[ ${1:-} ]] || die "usage: wd.sh tap-row <row label>"
  need_dev
  local x r rc; x=$(dump_xml) || exit 1; r=$(python3 -I -c "$row_py" "$1" <<< "$x"); rc=$?
  (( rc == 2 )) && die "no row labelled '$1' (see: wd.sh nodes)"
  (( rc == 1 )) && die "could not parse the UI dump"
  [[ $r ]] || die "no toggle or dropdown to the right of '$1'"
  local x y; read -r x y _ <<< "$r"; note "tap-row $x $y  ($r)"; a shell input tap "$x" "$y"
}
cmd_wait() {   # wait|gone <text|resource-id> [secs]: poll the UI tree until a node matches (wait) / none matches (gone)
  local mode=$1 want=${2:-} secs=${3:-30} t0 x r ok=0; [[ $want ]] || die "usage: wd.sh $mode <text|resource-id> [secs]"
  need_dev; t0=$(date +%s)
  while (( $(date +%s) - t0 < ${secs%.*} )); do
    x=$(dump_xml 2>/dev/null) || { sleep 0.5; continue; }
    r=$(python3 -I -c "$nodes_py" "$want" <<< "$x" 2>/dev/null) || { sleep 0.5; continue; }
    ok=1
    if [[ $mode == wait && $r ]]; then echo "$(( $(date +%s) - t0 ))s $(head -1 <<< "$r")"; return 0; fi
    if [[ $mode == gone && -z $r ]]; then echo "$(( $(date +%s) - t0 ))s '$want' gone"; return 0; fi
    sleep 0.5
  done
  ((ok)) || die "timeout ${secs}s: no UI dump succeeded (screen animating, or adb lost? use: wd.sh shot / wd.sh status)"
  die "timeout ${secs}s: '$want' still $([[ $mode == wait ]] && echo absent || echo present)"
}
cmd_swipe() { [[ $# -ge 4 ]] || die "usage: wd.sh swipe x1 y1 x2 y2 [ms]"; need_dev; a shell input swipe "$1" "$2" "$3" "$4" "${5:-300}"; }
cmd_text() {
  [[ $# -ge 1 ]] || die "usage: wd.sh text \"<string>\""
  [[ $1 != *$'\n'* && $1 != *$'\r'* ]] || die "text: no newlines (the device shell would run the rest as a command); send multi-line text from a peer"
  need_dev
  # `input text` treats space as separator and needs shell metachars escaped
  local s=$1 e; e=$(printf '%s' "$s" | sed -e 's/[][{}\\'"'"'"`$&|;<>()*~!?#]/\\&/g' -e 's/ /%s/g')
  a shell "input text $e"
}
cmd_key() { [[ ${1:-} ]] || die "usage: wd.sh key <keycode|KEYCODE_NAME|name>"; need_dev; local k=$1; [[ $k =~ ^[0-9]+$ || $k == KEYCODE_* ]] || k=KEYCODE_${k^^}; a shell input keyevent "$k"; }

# ---------- network ----------
cmd_reverse() { [[ ${1:-} =~ ^[0-9]+$ && ${2:-} =~ ^[0-9]+$ ]] || die "usage: wd.sh reverse <device-port> <host-port>"; need_dev; a reverse "tcp:$1" "tcp:$2" >/dev/null && a reverse --list; }
cmd_reverse_clear() { need_dev; a reverse --remove-all; }
# Forwarding from the bridge is dropped in Qubes' custom-forward chain on Qubes, and in a table of our own elsewhere.
fwd_chain() {
  if sudo nft list chain ip qubes custom-forward >/dev/null 2>&1; then echo "ip qubes custom-forward"; return; fi
  sudo nft list chain inet wd_uitest forward >/dev/null 2>&1 \
    || { sudo nft add table inet wd_uitest && sudo nft add chain inet wd_uitest forward '{ type filter hook forward priority -10; policy accept; }'; } \
    || die "could not create the nft table inet wd_uitest (nftables missing?)"
  echo "inet wd_uitest forward"
}
cmd_net() {
  sudo nft --version >/dev/null 2>&1 || die "nft (nftables) is required for wd.sh net"
  local chain; read -ra chain <<< "$(fwd_chain)"; [[ ${#chain[@]} == 3 ]] || exit 1
  case ${1:-status} in
    isolate) sudo nft list chain "${chain[@]}" | grep -q "iifname \"$BR\" drop" || sudo nft add rule "${chain[@]}" iifname "$BR" drop
             note "instance $INST can reach only the host (its bridge $BR + adb reverse), no internet" ;;
    open)    local h; for h in $(sudo nft -a list chain "${chain[@]}" | awk -v br="$BR" 'index($0, "iifname \"" br "\" drop") {print $NF}'); do sudo nft delete rule "${chain[@]}" handle "$h"; done
             note "instance $INST has internet via the host's NAT" ;;
    status)  sudo nft list chain "${chain[@]}" | grep -q "iifname \"$BR\" drop" && echo isolated || echo open ;;
    *) die "usage: wd.sh net isolate|open|status" ;;
  esac
}

# ---------- dispatch ----------
cmd=${1:-}; shift || true
case $cmd in
  ""|-h|--help|help) usage; exit 0 ;;
  status|who|serial|nodes|dump|shot|find|wait|gone|logcat|packages|wait-free|adb-path) ;;   # read-only: no ownership check
  claim|release) ;;
  *) [[ $cmd =~ ^(setup|up|down|install|uninstall|launch|stop-app|reset-app|tap|tap-text|tap-row|swipe|text|key|back|home|shell|rootsh|bt|adb|reverse|reverse-clear|net)$ ]] \
       || { usage; die "unknown command '$cmd'"; }
     # Start the adb server before taking the lock: a server spawned under the lock inherits its fd and would hold
     # the lock forever (first adb use after a reboot).
     "$ADB" start-server >/dev/null 2>&1
     with_lock "$cmd" "$@"; check_owner ;;
esac
if [[ $cmd == bt ]]; then   # native backtrace through rootsh (debuggerd needs root); a frozen app shows where each thread waits
  need_dev; bt_pid=$(a shell pidof "${1:-chat.simplex.app.debug}" | tr -d '\r'); bt_pid=${bt_pid%% *}
  [[ $bt_pid ]] || die "${1:-chat.simplex.app.debug} is not running"
  note "native backtrace of ${1:-chat.simplex.app.debug} pid $bt_pid (scudo::HybridMutex::lockSlow in several threads = the allocator deadlock under arm64 translation; re-run, it is not an app result)"
  set -- debuggerd -b "$bt_pid"; cmd=rootsh
fi
case $cmd in
  setup) cmd_setup; if [[ $INST == 2 ]]; then setup2; fi ;; up) cmd_up ;; down) cmd_down ;; status) cmd_status ;; who) cmd_who ;;
  claim) cmd_claim "$@" ;; release) cmd_release "$@" ;; wait-free) cmd_wait_free "$@" ;;
  install) cmd_install "$@" ;; uninstall) cmd_uninstall "$@" ;; launch) cmd_launch "$@" ;;
  stop-app) cmd_stop_app "$@" ;; reset-app) cmd_reset_app "$@" ;; packages) cmd_packages ;;
  shot) cmd_shot "$@" ;; dump) cmd_dump "$@" ;; nodes) cmd_nodes ;; find) cmd_find "$@" ;;
  tap) cmd_tap "$@" ;; tap-text) cmd_tap_text "$@" ;; tap-row) cmd_tap_row "$@" ;; swipe) cmd_swipe "$@" ;;
  wait|gone) cmd_wait "$cmd" "$@" ;;
  text) cmd_text "$@" ;; key) cmd_key "$@" ;; back) cmd_key KEYCODE_BACK ;; home) cmd_key KEYCODE_HOME ;;
  logcat) need_dev; if [[ $# -eq 0 ]]; then a logcat -d -t 300; else a logcat "$@"; fi ;;
  shell) need_dev; a shell "$@" ;;
  rootsh) # root shell in the container (run-as is blocked); `waydroid shell` takes over files its stdio points at, so only pipes
          # `waydroid shell` drops the exit code, so the command reports it on stderr as the last line
          [[ $# -ge 1 ]] || die "usage: wd.sh rootsh <cmd...>"
          # Helpers must not inherit the lock fd: a `cat` left reading a never-closing stdin would hold the lock.
          rcf=$(mktemp)
          exec {RIN}< <(exec {LFD}>&-; exec cat); rin=$!
          exec {RERR}> >(exec {LFD}>&-; tee "$rcf" | grep -v '^wd-rc=' >&2); rerr=$!
          swdy shell -- sh -c '"$@"; echo "wd-rc=$?" >&2' sh "$@" <&"$RIN" 2>&"$RERR" | cat
          exec {RIN}<&- {RERR}>&-; kill "$rin" 2>/dev/null; wait "$rerr" 2>/dev/null
          rc=$(sed -n 's/^wd-rc=//p' "$rcf"); rm -f "$rcf"; exit "${rc:-1}" ;;
  adb) need_dev; a "$@" ;;
  serial) serial || die "not running" ;;
  adb-path) echo "$ADB" ;;
  reverse) cmd_reverse "$@" ;; reverse-clear) cmd_reverse_clear ;; net) cmd_net "$@" ;;
  *) usage; die "unknown command '$cmd'" ;;
esac
