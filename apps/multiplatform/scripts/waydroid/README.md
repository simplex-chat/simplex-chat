# wd.sh — headless Android (Waydroid) for GUI testing

A headless Waydroid container: LineageOS 20 (Android 13, x86_64) with libndk arm64 translation, so it runs the real
SimpleX debug APKs, which ship arm64-v8a native libraries only. One script, `wd.sh`, drives everything. It works
from a fresh shell with no environment set.

`scripts/uitest-android.sh` uses it to run the Android UI tests (`plans/ui-tests/`), on instance 1 by default. To run
the tests on another device or emulator instead, set `UITEST_SERIAL` and Waydroid is not used.

## Why Waydroid and not the Android emulator

- On hosts without `/dev/kvm` (a Qubes AppVM, most CI VMs), the emulator runs only x86_64 AVDs under QEMU TCG, with
  no ARM translation. An x86_64 image boots in about 150 s, and a 266 MB APK install did not finish in 16 min.
- Waydroid is an LXC container. Android runs natively on the host CPU and only the app's arm64 `.so` files are
  translated. Android boots in 37–56 s, an APK installs in 5–8 s, and the app cold-starts in 2–3 s.

## Prerequisites

- Linux with systemd, a kernel with `CONFIG_ANDROID_BINDER_IPC=y` and `CONFIG_ANDROID_BINDERFS=y` (or the binder
  modules), nftables, and passwordless `sudo` (or run `sudo -v` first: test runs call `wd.sh` in the background).
- Packages: `waydroid`, `weston` (with `weston-screenshooter`), `dnsmasq`; `python3-venv` and `git` for the arm
  translation; optional `python3-pil` (`shot --half/--crop`) and `unzip`. `wd.sh setup` installs the first three with
  `apt-get` (Debian/Ubuntu, from repo.waydro.id) or `dnf` (Fedora); on other distributions install them yourself.
- Android SDK platform-tools. `adb` is taken from `$ADB`, `~/Android/Sdk/platform-tools`, `$ANDROID_HOME`,
  `$ANDROID_SDK_ROOT`, then `PATH` (`wd.sh adb-path` prints the one used).
- Weston 10 to 14 work: `wd.sh` picks the pixman and headless-backend flags by version.

## Files

The scripts live here; the state lives outside the repo, in `WD_HOME` (default `~/waydroid`) and, only for the
optional second instance, `WD2_HOME` (default `~/waydroid2`).

| Path | What |
|---|---|
| `wd.sh` | the tool |
| `weston.ini` | weston config: no panel, no locking (`$WD_HOME/weston.ini` overrides it) |
| `fetch-images.sh` | download the system/vendor images into `$WD_HOME/images` (parallel, resumable, sha256-checked) |
| `repair-zip.py` | repair a corrupt image download: re-fetch the 1 MiB blocks around given offsets with HTTP Range requests and patch the ones that differ (Troubleshooting) |
| `provision2.sh` | optional: build instance 2 in `$WD2_HOME` from the installed Waydroid and instance 1's state |
| `watchdog2.sh [secs]` | optional: kill instance 2 if instance 1 loses its display service (use while changing instance 2's setup) |
| `$WD_HOME/images/` | optional local images; without them `waydroid init` downloads them (Qubes requires them, below) |
| `$WD_HOME/var/` | optional: when present (always on Qubes), `/var/lib/waydroid` is a bind mount of it |
| `$WD_HOME/debs/` | optional: `.deb` files for an offline package install (Qubes) |
| `$WD_HOME/waydroid_script/`, `venv/`, `translation/` | the arm translation installer (cloned on first `setup`) and cached zips |
| `$WD_HOME/run/` | weston/session logs and pids; `current` = the last command that took the lock |
| `~/.local/share/waydroid/` | instance 1's Android `/data` (Waydroid's default) |

## Qubes OS

A Qubes AppVM resets its root FS (`/usr`, `/etc`, `/var`) on every restart. `wd.sh` detects Qubes and keeps
everything in `$WD_HOME`:
- packages are reinstalled from `$WD_HOME/debs` (offline) when missing;
- `/var/lib/waydroid` is a bind mount of `$WD_HOME/var`, and `/etc/waydroid-extra/images` links to `$WD_HOME/images`
  (download them once with `fetch-images.sh`);
- Qubes' firewall drops input from unknown interfaces, which breaks DHCP and DNS for the container: `setup`/`up` add
  `nft add rule ip qubes custom-input iifname "waydroid0" accept` (not persistent; re-added by `up`).

`wd.sh up` notices a reset root FS and runs `setup` itself (offline, about a minute). Android's data survives restarts.

## Other hosts

- `/var/lib/waydroid` is used directly (unless `$WD_HOME/var` exists), and `waydroid init` downloads the images
  if `$WD_HOME/images` has none.
- Firewall: with firewalld running, `setup`/`up` add the bridge to the `trusted` zone (runtime only). With ufw active
  they only print the commands to allow it. Other firewalls must accept DHCP/DNS from `waydroid0` (and `waydroid1`).
- `net isolate` adds a drop rule to an nft table of its own (`inet wd_uitest`); `net open` removes it.

## Quick start

```bash
W=scripts/waydroid/wd.sh                           # from apps/multiplatform
$W who                                             # free? (instance 1, shared; see "Optional second instance")
$W claim <you>; export WD_OWNER=<you>              # claim before changing anything; <you> = a name only you use
$W up                                              # setup if needed, then weston + Android; prints the adb serial
$W install android-arm64-v8a-debug.apk
$W reset-app chat.simplex.app.debug                # pm clear = fresh app data
$W launch chat.simplex.app.debug
$W shot /tmp/s.png                                 # PNG, 720x1440, same coordinates as dump/tap
$W nodes                                           # compact list: tapX tapY flags class resource-id label
$W wait "Get started" 90 && $W tap-text "Get started"
$W release                                         # leave it running; `down` frees its RAM
```

Run `wd.sh help` for all commands: `setup up down status who claim release wait-free install uninstall launch
stop-app reset-app wait gone rootsh packages shot dump nodes find tap tap-text tap-row swipe text key back home logcat
shell bt adb adb-path serial reverse reverse-clear net`.

## Sharing between sessions (locking)

- Every command except the read-only ones and claim/release takes an exclusive `flock` on the instance's `.lock`
  while it runs, so state-changing commands from two sessions never interleave. A command waits up to 600 s for the
  lock, then fails and names the holder.
- For a whole test run, **claim** the instance: `wd.sh claim <name>`, then pass `WD_OWNER=<name>` on every call.
  While the claim is live, any other owner (or a call with no owner) fails with a message saying who holds it. The
  read-only commands (`status who serial nodes dump shot find wait gone logcat packages wait-free adb-path`) are
  always allowed and do not refresh the claim.
- A claim expires after `WD_CLAIM_TTL` seconds without use (default 1800). `wd.sh who` shows the owner and its idle
  time, `wd.sh wait-free [secs]` blocks until the claim is free, `wd.sh claim <name> --force` takes it over (only when
  the owner is known to be gone), and `wd.sh release` ends a claim.

## Optional second instance (WD_INSTANCE=2)

Instance 1 is the default for every command and for `scripts/uitest-android.sh`. Instance 2 is an optional second
container for running the UI tests next to an interactive instance 1: a test run clears the app's data (`pm clear`)
on the instance it uses. Skip this section unless you need both at once.

Upstream Waydroid supports one container per host. Instance 2 is a patched copy of Waydroid's code in
`$WD2_HOME/lib` that renames every per-instance identifier:
- state `/var/lib/waydroid2` (a bind of `$WD2_HOME/var`), LXC container `waydroid2`;
- bridge `waydroid1` 192.168.241.0/24 (the device is `192.168.241.113:5555`), Wayland socket `wayland-waydroid2`;
- private system and session D-Bus daemons (sockets in `$WD2_HOME/run`), the transient unit `waydroid2-container`;
- its own binder devices `binder2`/`vndbinder2`/`hwbinder2` in the same binderfs;
- Android data in `$WD2_HOME/xdg`; the system/vendor images are shared read-only. Its memory is capped at 2.5 GB.

```bash
scripts/waydroid/provision2.sh                     # once (instance 1 set up first), and after a Waydroid update
WD_INSTANCE=2 WD_OWNER=<you> scripts/waydroid/wd.sh claim <you>
WD_INSTANCE=2 WD_OWNER=<you> scripts/waydroid/wd.sh up
WD_INSTANCE=2 WD_OWNER=<you> scripts/waydroid/wd.sh down    # frees its RAM (~1.5–2.5 GB)
```

- `up` re-creates all runtime pieces after a reboot: mounts, binder nodes, D-Bus, firewall rule, unit. If instance 1
  has not run since then, it starts instance 1's container service only to mount binderfs.
- **Binder guard:** `up` refuses to boot instance 2 unless `$WD2_HOME/var/lxc/waydroid2/config_nodes` maps
  `/dev/binder2` (etc.) and those devices differ from instance 1's. Two Androids on one binder context corrupt each
  other's service registry (instance 1 lost its `display` service until `wd.sh down && wd.sh up`).
- To run the UI tests on instance 2 and keep instance 1 free: `WD_INSTANCE=2 scripts/uitest-android.sh <app.apk> <test.apk>`.

## Driving the GUI from a script

Loop: **observe → act → re-observe**.

1. `wd.sh nodes` (2–3 s) lists every labelled or clickable node with tap coordinates: the centre of the nearest
   clickable ancestor, because Compose puts the text on a non-clickable child. Flags: `C` clickable, `F` focused,
   `S` scrollable, `K` checked, `P` password.
2. `wd.sh shot file.png` (0.5 s) for anything visual. Pixel coordinates match `nodes` and `tap`.
3. Act with `tap-text "<label>"` (preferred) or `tap x y`, `swipe`, `text "…"` (focus the field first), `key BACK|ENTER|…`.
4. Wait with a poll, not a sleep: `wd.sh wait "Continue" 20`, `wd.sh gone TEXT [secs]`. Both see only the top window.

Compose notes: test tags show up as `resource-id` only with `testTagsAsResourceId = true` (SimpleX does not set it),
so nodes are found by text or content description. The soft keyboard covers the lower half of the screen after typing,
and the first `key BACK` only hides it.

## Networking

- Instance 1 is `192.168.240.112` on bridge `waydroid0` (host side `192.168.240.1`); instance 2 is on `waydroid1`
  (192.168.241.x). adb runs over TCP (`wd.sh serial`).
- Host servers listening on the bridge address or `0.0.0.0` are reachable from Android at `192.168.240.1:<port>`.
  Servers listening only on `127.0.0.1` are reachable through `wd.sh reverse <device-port> <host-port>` (adb reverse);
  `wd.sh reverse-clear` removes them.
- Internet: Android reaches it through the host's NAT. `wd.sh net isolate` drops forwarding from the bridge, so Android
  reaches only the host (bridge IP and adb reverse still work); `wd.sh net open` undoes it; `wd.sh net status` shows it.

## Security

- `wd.sh setup` adds the Waydroid apt repository and its key under `/etc` (Debian/Ubuntu), installs packages with
  `sudo`, and changes firewall rules.
- For the arm translation it clones the third-party installer `casualsnek/waydroid_script` at the commit pinned in
  `wd.sh`, installs its `requirements.txt` from PyPI into a venv, and runs it **as root** to patch the Android image.
  Review that commit and its requirements before the first `setup`, or run the tool in a disposable VM.
- The UI test SMP server listens on all host interfaces with the repo's fixture TLS key while a run is in progress,
  and `pm clear` wipes whatever app `UITEST_APP_ID` names: never point a test run at a personal device.

## Known limitations

- **No `adb screencap`** (empty file in Waydroid): `wd.sh shot` captures the weston output (weston runs with `--debug`).
- **Window needs desktop-shell**: the Waydroid window is created with `waydroid show-full-ui`; kiosk-shell never maps it.
- **Container freeze**: Waydroid freezes the container when Android sleeps, and adb then hangs. `up` sets
  `persist.waydroid.suspend=false` and keeps the screen on; every device command unfreezes first.
- **arm translation**: libndk (default) or libhoudini (`WD_TRANSLATION=libhoudini`); only libndk was tested. Set
  `WD_ANDROID_VERSION` if the images are not Android 13. The Haskell core's first database creation takes ~45 s.
- **No GPU** (SwiftShader) and **no camera** (tests that open the QR scanner are skipped on Waydroid).
- **Two instances at most.** Screen size is fixed per `up` (`WD_WIDTH`/`WD_HEIGHT`, default 720x1440).
- Tested on a Qubes OS Debian 12 AppVM (weston 10), including the own nft table (with the Qubes check switched off).
  The weston 12+ flags were checked against weston's sources. The apt/dnf install, image download and firewalld
  paths follow the upstream tools' documented behaviour but were not run on a non-Qubes host.

## Troubleshooting

| Symptom | Fix |
|---|---|
| `Waydroid is not running or has no IP` | `wd.sh up` |
| `container has no IPv4` | host firewall blocks DHCP from the bridge (see Qubes / Other hosts); `dnsmasq` must be installed |
| `Android did not boot` + `umount …/rootfs` in `run/session.log` | stale mount: `wd.sh down && wd.sh up` |
| adb commands hang | container frozen: `waydroid status` shows `FROZEN`; wd.sh unfreezes before device commands |
| black screenshot | weston started without `--debug` or with kiosk-shell: `wd.sh down && wd.sh up` |
| `INSTALL_FAILED_NO_MATCHING_ABIS` | translation missing: `grep native.bridge /var/lib/waydroid/waydroid_base.prop`; rerun `wd.sh setup` |
| a command waits for the lock | `wd.sh who`, `cat $WD_HOME/run/current` |
| logs | `$WD_HOME/run/weston.log`, `run/session.log`, `sudo waydroid log`, `wd.sh logcat` |
| instance 2 | `WD_INSTANCE=2 wd.sh status`, `$WD2_HOME/run/*.log`, `/var/lib/waydroid2/waydroid.log`, `sudo journalctl -u waydroid2-container` |
| images lost (Qubes) | `fetch-images.sh`, then `wd.sh setup` |
| `fetch-images.sh` ends with `sha256 mismatch` | a corrupt part (for example two downloaders appended to one file): find the bad offset with an inflate test (`unzip -t`), then `python3 -I repair-zip.py <zip> <zip>.url <offset> <radius-MiB>` in `$WD_HOME/downloads`, and run `fetch-images.sh` again |

Never `pkill -f wd.sh` or `pgrep -f …` with a pattern that appears in your own command line: it kills your own shell.
