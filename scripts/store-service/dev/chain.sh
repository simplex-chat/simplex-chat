#!/bin/sh
# The local chain of compose.dev.yml. Runs in the webapp's dev image, which has anvil, node, the deploy script and the
# compiled contracts.
#   chain.sh                      anvil with its state kept in $CHAIN_DIR, the names contracts deployed on the first start,
#                                 and $CONFIG_DIR/store_service.ini written for them unless it exists
#   chain.sh topup [address] [eth]   sets an account's balance, by default the service key's account to 10000 ETH
set -eu

CHAIN_DIR="${CHAIN_DIR:-/chain}"
RPC_PORT="${RPC_PORT:-8545}"
ANVIL="${ANVIL:-anvil}"
WEBAPP_DIR="${WEBAPP_DIR:-/repo/webapp}"
RPC_URL="http://127.0.0.1:$RPC_PORT"
DEPLOYMENT="$CHAIN_DIR/deployment.json"
CONFIG_DIR="${CONFIG_DIR:-/config}"
CONFIG="$CONFIG_DIR/store_service.ini"
# the store service's user, which must own its config
USER_UID="${USER_UID:-1000}"
USER_GID="${USER_GID:-1000}"
# A public test key, like anvil's: made once with `simplex-chat badge keygen`. The service trusts it only through
# [issuer] dev_keys, which it accepts only on chain 31337, and refuses it at an index clients trust (1 to 8).
DEV_ISSUER_INDEX=999
DEV_ISSUER_SECRET=Grc_c1bKCSa3U8mMwRy6XNTJNwSveJi332wAqyyCy2o=
TERMS_VERSION=2026-10-23
TERMS_URL=https://simplex.chat/
# compose's healthcheck waits for this file, written once the contracts are on the running chain
READY="$CHAIN_DIR/ready"
# the dumps are taken this often and on exit, so a restart keeps the contracts and every name registered on them
STATE_INTERVAL_SECONDS=10
# only a few recent states stay in memory; without this anvil writes every old state under ~/.foundry
MEMORY_STATES=8

rpc() {
  curl -fs -X POST -H 'content-type: application/json' --data "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"$1\",\"params\":$2}" "$RPC_URL"
}

if [ "${1:-}" = topup ]; then
  address="${2:-$(node -p "require('$DEPLOYMENT').registrar")}"
  eth="${3:-10000}"
  wei=$(node -p "'0x' + (BigInt('$eth') * 10n ** 18n).toString(16)")
  rpc anvil_setBalance "[\"$address\", \"$wei\"]" > /dev/null
  echo "$address now has $eth ETH"
  exit
fi

STATE="$CHAIN_DIR/state.json"
# anvil rewrites its dump in place, so a kill during a write leaves it cut short. The good copy, taken at each start and
# clean stop, replaces it; with no good copy the chain starts over, and the contracts deploy again at the same addresses.
GOOD_STATE="$STATE.good"
parses() {
  node -e 'JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"))' "$1" 2> /dev/null
}

rm -f "$READY"
if [ -e "$STATE" ] && ! parses "$STATE"; then
  mv "$STATE" "$STATE.broken"
  if [ -e "$GOOD_STATE" ] && parses "$GOOD_STATE"; then
    cp "$GOOD_STATE" "$STATE"
    echo "the chain's state was cut short; restored its last good copy (cut file: $STATE.broken)"
  else
    echo "the chain's state was cut short and there is no good copy; starting a fresh chain (cut file: $STATE.broken)"
  fi
fi
if [ -e "$STATE" ]; then cp "$STATE" "$GOOD_STATE"; fi
"$ANVIL" --chain-id 31337 --host 127.0.0.1 --port "$RPC_PORT" --block-time 1 --prune-history "$MEMORY_STATES" \
  --state "$STATE" --state-interval "$STATE_INTERVAL_SECONDS" --silent &
anvil_pid=$!
# anvil writes its last dump when it stops, so a stop or a failed deploy stops it, waits for it, and keeps the dump as
# the good copy once it parses
stop() {
  kill "$anvil_pid" 2> /dev/null
  wait "$anvil_pid" || true
  if [ -e "$STATE" ] && parses "$STATE"; then cp "$STATE" "$GOOD_STATE"; fi
}
trap stop EXIT INT TERM
until rpc eth_chainId '[]' > /dev/null; do sleep 0.2; done

deployed() {
  [ -s "$DEPLOYMENT" ] || return 1
  controller=$(node -p "require('$DEPLOYMENT').controller")
  ! rpc eth_getCode "[\"$controller\", \"latest\"]" | grep -q '"result":"0x"'
}
if deployed; then
  echo "the contracts are already on the chain ($DEPLOYMENT)"
else
  (cd "$WEBAPP_DIR" && RPC_URL="$RPC_URL" LOCAL_CHAIN_OUT="$DEPLOYMENT" node dev/deploy.ts)
fi

field() {
  node -p "require('$DEPLOYMENT').$1"
}

write_config() {
  tmp="$CONFIG.tmp"
  rm -f "$tmp"
  (umask 077 && cat > "$tmp" <<INI)
; written by dev/chain.sh for the local chain on its first start, and never rewritten
[listener]
host = 127.0.0.1
badges_port = 8080
names_port = 8081
serve_webapp = on
trust_forwarded_for = on

[poll]
waiting_seconds = 3
idle_seconds = 60

[issuer]
index = $DEV_ISSUER_INDEX
private_key = $DEV_ISSUER_SECRET
dev_keys = $DEV_ISSUER_INDEX:$DEV_ISSUER_SECRET

[names]
tld = $(field tld)
terms_version = $TERMS_VERSION
terms_url = $TERMS_URL

[chain]
rpc_url = $(field rpcUrl)
chain_id = $(field chainId)
controller = $(field controller)
sponsor = $(field sponsor)
base_registrar = $(field baseRegistrar)
resolver = $(field resolver)
service_key = $(field registrarKey)
confirmations = 1

[dev]
accept_unverified_store_receipts = off

; dev_keys accepts only Stripe test keys: sk_test_ or rk_test_, and pk_test_
;[stripe]
;secret_key = rk_test_replace-me
;publishable_key = pk_test_replace-me
;webhook_secret = whsec_replace-me

; dev_keys accepts only a BTCPay on loopback: 127.0.0.1, ::1 or localhost
;[btcpay]
;host = https://127.0.0.1
;api_key = replace-me
;store_id = replace-me
;webhook_secret = replace-me
INI
  chown "$USER_UID:$USER_GID" "$CONFIG_DIR" "$tmp"
  chmod 700 "$CONFIG_DIR"
  mv "$tmp" "$CONFIG"
  echo "wrote $CONFIG"
}
if [ -e "$CONFIG" ]; then
  echo "kept $CONFIG"
else
  write_config
fi
touch "$READY"

wait "$anvil_pid"
