#!/usr/bin/env bash
# Download the Waydroid images into $WD_HOME/images (default ~/waydroid/images), faster than `waydroid init`.
# Needed on Qubes (wd.sh setup refuses to start without local images) and optional elsewhere. Resumable; never run two copies at once.
# If the final sha256 mismatches: python3 -I repair-zip.py <zip> <zip>.url <centre-offset> <radius-MiB> (find the offset with an inflate test).
# Download Waydroid LineageOS 20 VANILLA system + MAINLINE vendor images with parallel range requests
# (waydroid init downloads serially at ~0.4 MB/s here), verify sha256, unzip into $IMG.
# Optional $1: a partial system zip to resume from (its bytes are reused as part 0).
set -euo pipefail
R=${WD_HOME:-$HOME/waydroid}; T=$R/downloads; IMG=$R/images; mkdir -p "$T" "$IMG"
N=${N:-6}
meta() { curl -s "$1" | python3 -c 'import json,sys; r=json.load(sys.stdin)["response"][0]; print(r["url"], r["filename"], r["id"], r["size"])'; }
get() { # url file sha size [partial]  -- resumable: parts are kept and completed on re-run
  local url=$1 f=$T/$2 sha=$3 size=$4 partial=${5:-} start=0
  if [[ -f "$f" ]] && [[ "$(sha256sum < "$f" | cut -d' ' -f1)" == "$sha" ]]; then echo "$2 ok (cached)"; return; fi
  local real; real=$(cat "$f.url" 2>/dev/null || true)
  [[ -n "$real" ]] || { real=$(curl -sIL -o /dev/null -w '%{url_effective}' "$url"); echo "$real" > "$f.url"; }
  [[ -f "$f.p0" ]] || { if [[ -n "$partial" && -s "$partial" ]]; then cp "$partial" "$f.p0"; else : > "$f.p0"; fi; }
  start=$(stat -c %s "$f.p0")
  local chunk=$(( (size - start + N - 1) / N )) pids=()
  for i in $(seq 1 $N); do
    local a=$((start + (i-1)*chunk)) b=$((start + i*chunk - 1)); (( b >= size )) && b=$((size-1))
    (
      touch "$f.p$i"
      for try in $(seq 1 50); do
        local have; have=$(stat -c %s "$f.p$i"); (( a + have > b )) && exit 0
        curl -sL --fail -r "$((a + have))-$b" "$real" >> "$f.p$i" || sleep 2
      done; exit 1
    ) & pids+=($!)
  done
  for p in "${pids[@]}"; do wait "$p"; done
  cat "$f.p0" $(for i in $(seq 1 $N); do echo "$f.p$i"; done) > "$f"
  [[ "$(stat -c %s "$f")" == "$size" ]] || { echo "$2 size mismatch $(stat -c %s "$f") != $size"; exit 1; }
  [[ "$(sha256sum < "$f" | cut -d' ' -f1)" == "$sha" ]] || { echo "$2 sha256 mismatch"; exit 1; }
  rm -f "$f".p* "$f.url"; echo "$2 ok"
}
read -r SU SF SS SZ < <(meta https://ota.waydro.id/system/lineage/waydroid_x86_64/VANILLA.json)
read -r VU VF VS VZ < <(meta https://ota.waydro.id/vendor/waydroid_x86_64/MAINLINE.json)
get "$SU" "$SF" "$SS" "$SZ" "${1:-}" & P1=$!
get "$VU" "$VF" "$VS" "$VZ" & P2=$!
wait $P1; wait $P2
(cd "$IMG" && unzip -o "$T/$SF" && unzip -o "$T/$VF")
ls -la "$IMG"
