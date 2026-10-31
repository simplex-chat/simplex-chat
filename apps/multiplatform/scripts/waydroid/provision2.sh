#!/usr/bin/env bash
# provision2.sh: (re)build Waydroid instance 2 in $WD2_HOME (default ~/waydroid2) from the installed Waydroid and
# instance 1's state (/var/lib/waydroid, so instance 1 must have been set up with `wd.sh setup` or `wd.sh up`).
# Run once, or again after a Waydroid package update; instance 2 must be down. Runtime (mounts, binder2 nodes,
# private D-Bus, firewall) is re-created by `WD_INSTANCE=2 wd.sh up` after every reboot.
set -euo pipefail
WD=${WD_HOME:-$HOME/waydroid}
W2=${WD2_HOME:-$HOME/waydroid2}
mkdir -p "$W2"
die() { echo "provision: $*" >&2; exit 1; }
! sudo lxc-info -P /var/lib/waydroid2/lxc -n waydroid2 2>/dev/null | grep -q RUNNING || die "instance 2 is running: WD_INSTANCE=2 wd.sh down"
[[ -f /var/lib/waydroid/waydroid.cfg && -d /var/lib/waydroid/lxc/waydroid ]] || die "instance 1 is not set up (/var/lib/waydroid): run wd.sh setup first"
mountpoint -q /var/lib/waydroid2 && sudo umount /var/lib/waydroid2

# 1. code: a copy of /usr/lib/waydroid with every instance identifier renamed
sudo rm -rf "$W2/lib"; cp -a /usr/lib/waydroid "$W2/lib"; find "$W2/lib" -name __pycache__ -prune -exec rm -rf {} +
python3 - "$W2/lib" <<'EOF'
import os, re, sys
root = sys.argv[1]
for dp, _, fs in os.walk(root):
    for f in fs:
        p = os.path.join(dp, f)
        try: s = open(p).read()
        except (UnicodeDecodeError, OSError): continue
        o = s
        s = re.sub(r'/var/lib/waydroid(?!2)', '/var/lib/waydroid2', s)
        s = s.replace('defaults["lxc"] + "/waydroid"', 'defaults["lxc"] + "/waydroid2"')
        s = s.replace('defaults["lxc"] + "/waydroid/', 'defaults["lxc"] + "/waydroid2/')
        s = s.replace('/lxc/waydroid/', '/lxc/waydroid2/').replace('"-n", "waydroid"', '"-n", "waydroid2"')
        if p.endswith(('waydroid-net.sh', 'config_1', 'config_3', 'net.py')):
            s = s.replace('waydroid0', 'waydroid1')
        if p.endswith('waydroid-net.sh'):
            s = s.replace('192.168.240.', '192.168.241.').replace('00:16:3e:00:00:01', '00:16:3e:00:00:02').replace('/run/waydroid-lxc', '/run/waydroid2-lxc')
        if p.endswith(('config_1', 'config_3')):
            s = s.replace('00:16:3e:f9:d3:03', '00:16:3e:f9:d3:04')
        if p.endswith('container_manager.py'):
            a = '        if helpers.drivers.probeBinderDriver(args) != 0:'
            assert a in s, "container_manager.py changed upstream: re-check the binderfs probe"
            s = s.replace(a, '        if False:  # waydroid2: never (re)mount binderfs; instance 1 owns /dev/binderfs')
        if p.endswith('helpers/ipc.py'):
            s = ('import os as _os\nif "waydroid2" not in _os.environ.get("DBUS_SYSTEM_BUS_ADDRESS", ""):\n'
                 '    raise SystemExit("waydroid2: refusing to run without the private DBUS_SYSTEM_BUS_ADDRESS (would talk to instance 1)")\n') + s
        if s != o: open(p, 'w').write(s)
EOF
left=$(grep -rnE '/var/lib/waydroid([^2]|$)|"-n", "waydroid"|waydroid0|192\.168\.240|waydroid-lxc|lxc/waydroid/|\+ "/waydroid"' "$W2/lib" | grep -v '\.pyc' | grep -v 'waydroid_user_state' || true)   # user state: isolated by XDG_DATA_HOME
[[ -z $left ]] || die "unpatched instance-1 identifiers left: $left"

# 2. state: instance 1's config/overlay (not its rootfs mount, overlay writes or logs), retargeted
sudo rm -rf "$W2/var"
sudo rsync -a --exclude /rootfs --exclude /overlay_rw --exclude /overlay_work --exclude /cache_http --exclude /waydroid.log --exclude /images \
  /var/lib/waydroid/ "$W2/var/"
sudo mkdir -p "$W2/var/rootfs" "$W2/var/overlay_rw/system" "$W2/var/overlay_rw/vendor" "$W2/var/overlay_work"
sudo mv "$W2/var/lxc/waydroid" "$W2/var/lxc/waydroid2"
sudo rm -f "$W2/var/lxc/waydroid2/config_session"
sudo sed -i -E 's#/var/lib/waydroid(/|$)#/var/lib/waydroid2\1#g; s#/lxc/waydroid/#/lxc/waydroid2/#g; s#waydroid0#waydroid1#g; s#00:16:3e:f9:d3:03#00:16:3e:f9:d3:04#g' \
  "$W2/var/lxc/waydroid2/config" "$W2/var/lxc/waydroid2/config_nodes"
# config_nodes is written only by `waydroid init`: map instance 2's own binder nodes (sharing instance 1's breaks it)
sudo sed -i -E 's#^lxc.mount.entry = /dev/(binder|vndbinder|hwbinder) dev/#lxc.mount.entry = /dev/\12 dev/#' "$W2/var/lxc/waydroid2/config_nodes"
echo 'lxc.cgroup2.memory.max = 2500M' | sudo tee -a "$W2/var/lxc/waydroid2/config" >/dev/null
sudo sed -i -E 's#^binder = .*#binder = binder2#; s#^vndbinder = .*#vndbinder = vndbinder2#; s#^hwbinder = .*#hwbinder = hwbinder2#' "$W2/var/waydroid.cfg"
if [[ -f $WD/images/system.img ]]; then sudo sed -i -E "s#^images_path = .*#images_path = $WD/images#" "$W2/var/waydroid.cfg"; fi

# 3. private system bus config; it lets whoever reaches its socket own any name, so its directory is private to this user
mkdir -p "$W2/run" "$W2/xdg"
chmod 700 "$W2/run"
cat > "$W2/sys.conf" <<EOF
<!DOCTYPE busconfig PUBLIC "-//freedesktop//DTD D-Bus Bus Configuration 1.0//EN" "http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd">
<busconfig>
  <type>system</type>
  <listen>unix:path=$W2/run/sys.sock</listen>
  <auth>EXTERNAL</auth>
  <policy context="default">
    <allow user="*"/>
    <allow own="*"/>
    <allow send_destination="*" eavesdrop="true"/>
    <allow receive_sender="*"/>
  </policy>
</busconfig>
EOF
echo "provisioned $W2 (Android data in $W2/xdg is kept); start with: WD_INSTANCE=2 $(dirname "$(readlink -f "$0")")/wd.sh up"
