#!/bin/bash
# Kills instance 2's container the moment instance 1 loses its display service; exits after $1 s (default 900).
wd="$(dirname "$(readlink -f "$0")")/wd.sh"
ADB=$("$wd" adb-path); s1=$(WD_INSTANCE=1 "$wd" serial) || { echo "watchdog: instance 1 is not running" >&2; exit 1; }
t=$(date +%s)
while (( $(date +%s) - t < ${1:-900} )); do
  r=$(timeout 10 "$ADB" -s "$s1" shell service check display 2>&1)
  if [[ $r != *"found"* || $r == *"not found"* ]]; then
    sudo lxc-stop -P /var/lib/waydroid2/lxc -n waydroid2 -k; echo "$(date +%T) WATCHDOG: #1 lost display ($r); killed #2"; exit 1
  fi
  sleep 2
done; echo "$(date +%T) watchdog: #1 healthy for ${1:-900} s"
