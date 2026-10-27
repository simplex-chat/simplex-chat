#!/bin/sh
# The local chain of compose.dev.yml. Runs in the webapp's dev image, which has anvil, node, the deploy script and the
# compiled contracts.
#   dev-chain.sh                  anvil with its state kept in $CHAIN_DIR, and the names contracts deployed on the first start
#   dev-chain.sh topup [address] [eth]   sets an account's balance, by default the service key's account to 10000 ETH
set -eu

CHAIN_DIR="${CHAIN_DIR:-/chain}"
RPC_PORT="${RPC_PORT:-8545}"
ANVIL="${ANVIL:-anvil}"
WEBAPP_DIR="${WEBAPP_DIR:-/repo/webapp}"
RPC_URL="http://127.0.0.1:$RPC_PORT"
DEPLOYMENT="$CHAIN_DIR/deployment.json"
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

rm -f "$READY"
"$ANVIL" --chain-id 31337 --host 127.0.0.1 --port "$RPC_PORT" --block-time 1 --prune-history "$MEMORY_STATES" \
  --state "$CHAIN_DIR/state.json" --state-interval "$STATE_INTERVAL_SECONDS" --silent &
anvil_pid=$!
# anvil writes its last dump when it stops, so a stop or a failed deploy stops it and waits for it
trap 'kill "$anvil_pid" 2>/dev/null; wait "$anvil_pid"' EXIT INT TERM
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

node -e '
const d = require(process.argv[1])
console.log(`[chain] for config/store_service.ini:
rpc_url = ${d.rpcUrl}
chain_id = ${d.chainId}
controller = ${d.controller}
sponsor = ${d.sponsor}
base_registrar = ${d.baseRegistrar}
resolver = ${d.resolver}
service_key = ${d.registrarKey}
confirmations = 1`)' "$DEPLOYMENT"
touch "$READY"

wait "$anvil_pid"
