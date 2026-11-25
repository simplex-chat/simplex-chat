#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

ENV_FILE="$PWD/lightning.env"
SERVICE_PORT="${STORE_LIGHTNING_SERVICE_PORT:-8099}"
BTCPAY="http://127.0.0.1:49392"
BITCOIND="http://ceiwHEbqWI83:DwubwWsoo3@127.0.0.1:43782"
MERCHANT="http://127.0.0.1:35531"
PAYER="http://127.0.0.1:35532"
EMAIL="admin@example.com"
PASSWORD="regtest-Passw0rd"
BTC_USD="108000.12"
MATURE_BLOCKS=110
PAYER_FUNDING_BTC=1
CHANNEL_SAT=5000000
CONFIRMATION_BLOCKS=6
TRY_SECONDS=2
NODE_TRIES=90
BTCPAY_TRIES=150
CHANNEL_TRIES=60
FUNDING_TRIES=30
STORE_PERMISSIONS="cancreateinvoice canviewinvoices canviewstoresettings canmodifyinvoices"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

log() { echo "[$(date -u +%H:%M:%S)] $*" >&2; }

wait_for() {
  local what="$1" tries="$2"
  shift 2
  for _ in $(seq "$tries"); do
    if "$@" >/dev/null 2>&1; then
      log "ready: $what"
      return 0
    fi
    sleep "$TRY_SECONDS"
  done
  log "timed out: $what"
  return 1
}

rpc() {
  local wallet="$1" method="$2" params="$3"
  curl -sS --fail-with-body -H 'Content-Type: application/json' \
    --data-binary "{\"jsonrpc\":\"1.0\",\"id\":\"lightning\",\"method\":\"$method\",\"params\":$params}" \
    "$BITCOIND$wallet"
}

api() {
  local method="$1" path="$2" body="${3:-}" status
  local args=(-sS -o "$TMP/response.json" -w '%{http_code}' -X "$method" -u "$EMAIL:$PASSWORD" -H 'Accept: application/json' -H 'Content-Type: application/json')
  if [ -n "$body" ]; then args+=(--data-binary "$body"); fi
  status="$(curl "${args[@]}" "$BTCPAY$path")"
  case "$status" in
    2*) cat "$TMP/response.json" ;;
    *)
      log "$method $path answered $status: $(cat "$TMP/response.json")"
      return 1
      ;;
  esac
}

lnd_synced() { [ "$(curl -sf "$1/v1/getinfo" | jq -r .synced_to_chain)" = "true" ]; }
btcpay_synced() { [ "$(curl -sf "$BTCPAY/api/v1/health" | jq -r .synchronized)" = "true" ]; }
active_channels() { curl -sf "$1/v1/channels" | jq '[(.channels // [])[] | select(.active)] | length'; }
payer_funded() { [ "$(curl -sf "$PAYER/v1/balance/blockchain" | jq -r .confirmed_balance)" != "0" ]; }

docker compose -f lightning.yml up -d

log "waiting for the nodes"
wait_for "bitcoind" "$NODE_TRIES" rpc "" getblockchaininfo "[]"
wait_for "merchant_lnd" "$NODE_TRIES" curl -sf "$MERCHANT/v1/getinfo"
wait_for "payer_lnd" "$NODE_TRIES" curl -sf "$PAYER/v1/getinfo"

if [ "$(rpc "" listwallets "[]" | jq '.result | length')" = "0" ]; then
  rpc "" createwallet '["miner"]' >/dev/null
fi
WALLET="/wallet/$(rpc "" listwallets "[]" | jq -r '.result[0]')"
MINER_ADDRESS="$(rpc "$WALLET" getnewaddress "[]" | jq -r .result)"
mine() { rpc "$WALLET" generatetoaddress "[$1,\"$MINER_ADDRESS\"]" >/dev/null; }

if [ "$(rpc "" getblockcount "[]" | jq -r .result)" -lt "$MATURE_BLOCKS" ]; then
  log "mining the first blocks"
  mine "$MATURE_BLOCKS"
fi
wait_for "merchant_lnd synced" "$NODE_TRIES" lnd_synced "$MERCHANT"
wait_for "payer_lnd synced" "$NODE_TRIES" lnd_synced "$PAYER"
wait_for "btcpay synchronized" "$BTCPAY_TRIES" btcpay_synced

channel_active() {
  mine 1
  [ "$(active_channels "$PAYER")" = "1" ] && [ "$(active_channels "$MERCHANT")" = "1" ]
}
open_channel() {
  curl -sS -X POST "$PAYER/v1/channels" \
    --data-binary "{\"node_pubkey\":\"$MERCHANT_PUBKEY_B64\",\"local_funding_amount\":\"$CHANNEL_SAT\"}" |
    jq -e '.funding_txid_bytes // .funding_txid_str'
}

if [ "$(active_channels "$PAYER")" = "0" ]; then
  log "funding payer_lnd and opening its channel to merchant_lnd"
  PAYER_ADDRESS="$(curl -sf "$PAYER/v1/newaddress?type=WITNESS_PUBKEY_HASH" | jq -r .address)"
  rpc "$WALLET" sendtoaddress "[\"$PAYER_ADDRESS\",$PAYER_FUNDING_BTC]" >/dev/null
  mine "$CONFIRMATION_BLOCKS"
  wait_for "payer_lnd funded" "$CHANNEL_TRIES" payer_funded
  MERCHANT_PUBKEY="$(curl -sf "$MERCHANT/v1/getinfo" | jq -r .identity_pubkey)"
  curl -sS -o /dev/null -X POST "$PAYER/v1/peers" \
    --data-binary "{\"addr\":{\"pubkey\":\"$MERCHANT_PUBKEY\",\"host\":\"merchant_lnd:9735\"},\"perm\":true}"
  MERCHANT_PUBKEY_B64="$(echo -n "$MERCHANT_PUBKEY" | xxd -r -p | base64 -w0)"
  wait_for "channel funding broadcast" "$FUNDING_TRIES" open_channel
  wait_for "channel active on both nodes" "$CHANNEL_TRIES" channel_active
  wait_for "merchant_lnd synced" "$CHANNEL_TRIES" lnd_synced "$MERCHANT"
fi

log "creating the store, its payment methods, webhook and API key"
curl -sS -o /dev/null -X POST -H 'Content-Type: application/json' "$BTCPAY/api/v1/users" \
  --data-binary "$(jq -n --arg e "$EMAIL" --arg p "$PASSWORD" '{email: $e, password: $p, isAdministrator: true}')"
STORE="$(api POST /api/v1/stores '{"name":"store service","defaultCurrency":"USD"}' | jq -r .id)"
S="/api/v1/stores/$STORE"
api PUT "$S/rates/configuration" "$(jq -n --arg r "$BTC_USD" '{spread: 0, isCustomScript: true, effectiveScript: "BTC_USD = \($r);"}')" >/dev/null
api POST "$S/payment-methods/BTC-CHAIN/wallet/generate" '{"savePrivateKeys":false}' >/dev/null
api PUT "$S/payment-methods/BTC-LN" '{"enabled":true,"config":{"connectionString":"Internal Node"}}' >/dev/null
WEBHOOK_SECRET="$(openssl rand -hex 16)"
api POST "$S/webhooks" "$(jq -n --arg u "http://host.docker.internal:$SERVICE_PORT/webhooks/btcpay" --arg s "$WEBHOOK_SECRET" \
  '{url: $u, secret: $s, authorizedEvents: {everything: true}}')" >/dev/null
API_KEY="$(api POST /api/v1/api-keys "$(jq -n --arg s "$STORE" --arg p "$STORE_PERMISSIONS" \
  '{label: "store service", permissions: ($p | split(" ") | map("btcpay.store.\(.):\($s)"))}')" | jq -r .apiKey)"

ENABLED="$(curl -sS --fail-with-body -H "Authorization: token $API_KEY" "$BTCPAY$S/payment-methods?onlyEnabled=true" | jq -c '[.[].paymentMethodId] | sort')"
if [ "$ENABLED" != '["BTC-CHAIN","BTC-LN"]' ]; then
  log "store $STORE has enabled $ENABLED, not BTC-CHAIN and BTC-LN"
  exit 1
fi

umask 077
cat >"$ENV_FILE" <<EOF
STORE_LIGHTNING_BTCPAY_URL=$BTCPAY
STORE_LIGHTNING_BTCPAY_API_KEY=$API_KEY
STORE_LIGHTNING_BTCPAY_STORE_ID=$STORE
STORE_LIGHTNING_BTCPAY_WEBHOOK_SECRET=$WEBHOOK_SECRET
STORE_LIGHTNING_PAYER_URL=$PAYER
STORE_LIGHTNING_SERVICE_PORT=$SERVICE_PORT
EOF
log "store $STORE is ready; its settings are in $ENV_FILE"
