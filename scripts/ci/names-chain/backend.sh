#!/bin/bash

set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
rpc_port=8545
rpc_url=http://127.0.0.1:$rpc_port
terms_url=http://127.0.0.1:8098
resolver_port=18001
smp_port=5224
service_port=18002
service_url=http://127.0.0.1:$service_port
min_commitment_age=5
issuer_index=999
issuer_secret=Grc_c1bKCSa3U8mMwRy6XNTJNwSveJi332wAqyyCy2o=
codes=("name 6" "name 6" "name 6" "name 7 years 5" "name 8")
free_label=cifreename
unlinked_label=harbour
owner_label=ciowner
startup_seconds=120

start_part() {
  local part=$1
  shift
  nohup "$@" < /dev/null >> "$work/$part.log" 2>&1 &
  echo $! > "$work/$part.pid"
}

until_ready() {
  local part=$1 what=$2 deadline=$((SECONDS + startup_seconds))
  shift 2
  until "$@" > /dev/null 2>&1; do
    if ! kill -0 "$(cat "$work/$part.pid")" 2> /dev/null || [ "$SECONDS" -ge "$deadline" ]; then
      echo "error: $what"
      cat "$work/$part.log"
      return 1
    fi
    sleep 1
  done
}

stop_process() {
  local pid deadline
  pid=$(cat "$work/$1.pid")
  kill "-$2" "$pid" 2> /dev/null || true
  deadline=$((SECONDS + 30))
  while kill -0 "$pid" 2> /dev/null; do
    if [ "$SECONDS" -ge "$deadline" ]; then
      kill -KILL "$pid" 2> /dev/null || true
    fi
    sleep 1
  done
}

show_log() {
  echo "::group::$1 log"
  cat "$work/$1.log" 2> /dev/null || true
  echo "::endgroup::"
}

field() {
  jq -er ".$1" "$work/chain.json"
}

answers_rpc() {
  curl -sf -X POST -H 'content-type: application/json' --data '{"jsonrpc":"2.0","id":1,"method":"eth_chainId"}' "$rpc_url"
}

start_chain() {
  start_part chain-anvil "$names/anvil" --chain-id 31337 --host 127.0.0.1 --port "$rpc_port" --block-time 1 --prune-history 8 --silent
  until_ready chain-anvil "anvil does not answer on $rpc_url" answers_rpc
  start_part chain-terms env TERMS_DIR="$work/names-terms" node "$names/webapp/dev/serve-terms.ts"
  until_ready chain-terms "the terms server does not answer on $terms_url" curl -s -o /dev/null "$terms_url/terms/"
  if ! env RPC_URL="$rpc_url" LOCAL_CHAIN_OUT="$work/chain.json" MIN_COMMITMENT_AGE="$min_commitment_age" ENS_CONTRACTS_DIR="$names/contracts" \
    TERMS_DIR="$work/names-terms" node "$names/webapp/dev/deploy.ts" > "$work/chain-deploy.log" 2>&1; then
    echo "error: the names contracts were not deployed"
    cat "$work/chain-deploy.log"
    return 1
  fi
  cat "$work/chain-deploy.log"
}

start_resolver() {
  local registry registrar controller
  registry=$(field ens)
  registrar=$(field baseRegistrar)
  controller=$(field controller)
  start_part chain-resolver env PYTHONPATH="$names/resolver/lib" SNRC_RPC="$rpc_url" SNRC_BIND=127.0.0.1 SNRC_PORT="$resolver_port" SNRC_WORKERS=1 \
    SNRC_REGISTRY_SIMPLEX="$registry" SNRC_REGISTRAR_SIMPLEX="$registrar" SNRC_CONTROLLER_SIMPLEX="$controller" python3 "$names/resolver/snrc-resolve.py"
  until_ready chain-resolver "the resolver does not answer on port $resolver_port" curl -sf "http://127.0.0.1:$resolver_port/health"
}

start_chain_smp() {
  local cfg=$work/chain-smp/etc logs=$work/chain-smp/log ini host_option
  case "$host" in
    *[!0-9.]*) host_option=--fqdn ;;
    *) host_option=--ip ;;
  esac
  if ! SMP_SERVER_CFG_PATH=$cfg SMP_SERVER_LOG_PATH=$logs "$bin/smp-server" init -y --disable-store-log --no-password "$host_option" "$host" --disable-web >> "$work/chain-smp.log" 2>&1; then
    echo "error: the second SMP server was not initialized"
    cat "$work/chain-smp.log"
    return 1
  fi
  ini=$cfg/smp-server.ini
  sed -e "s/^port = 5223,443\$/port = $smp_port/" \
    -e '/^\[NAMES\]/,/^\[/{s/^enable: off$/enable: on/;s|^# resolver_endpoint: http://127.0.0.1:8000$|resolver_endpoint: http://127.0.0.1:'"$resolver_port"'|;}' "$ini" > "$ini.new"
  mv "$ini.new" "$ini"
  if ! grep -qx "port = $smp_port" "$ini" ||
    ! sed -n '/^\[NAMES\]/,/^\[/p' "$ini" | grep -qx 'enable: on' ||
    ! sed -n '/^\[NAMES\]/,/^\[/p' "$ini" | grep -qx "resolver_endpoint: http://127.0.0.1:$resolver_port"; then
    echo "error: $ini has a different port or [NAMES] than expected:"
    grep -E '^port|^\[NAMES\]|enable|resolver' "$ini"
    return 1
  fi
  start_part chain-smp env SMP_SERVER_CFG_PATH="$cfg" SMP_SERVER_LOG_PATH="$logs" "$bin/smp-server" start
  until_ready chain-smp "the second SMP server is not listening on $smp_port" nc -z 127.0.0.1 "$smp_port"
  chain_server="smp://$(cat "$cfg/fingerprint")@$host:$smp_port"
  echo "$chain_server" > "$work/names-chain-server"
  echo "names chain server: $chain_server"
}

write_service_ini() {
  cat > "$service_ini" << INI
[listener]
host = 127.0.0.1
badges_port = $service_port
serve_webapp = off

[issuer]
index = $issuer_index
private_key = $issuer_secret
dev_keys = $issuer_index:$issuer_secret

[names]
tld = $(field tld)
record_edits_per_year = 100

[chain]
rpc_url = $(field rpcUrl)
chain_id = $(field chainId)
controller = $(field controller)
sponsor = $(field sponsor)
base_registrar = $(field baseRegistrar)
resolver = $(field resolver)
service_key = $(field registrarKey)
confirmations = 1
INI
  chmod 600 "$service_ini"
}

answers_code_check() {
  curl -s -o /dev/null -X POST -H 'content-type: application/json' --data '{"code":""}' "$service_url/api/code/check"
}

start_service() {
  start_part store-service "${service[@]}" -y
  until_ready store-service "the store service does not answer on $service_url" answers_code_check
}

service_address() {
  sqlite3 "$work/store-service_chat.db" "SELECT CAST(short_link_contact AS TEXT) FROM user_contact_links WHERE group_id IS NULL AND short_link_contact IS NOT NULL"
}

has_service_address() {
  [ -n "$(service_address)" ]
}

make_service() {
  write_service_ini
  start_service
  until_ready store-service "the store service did not create its address" has_service_address
  stop_process store-service INT
  service_address > "$work/store-service"
  echo "store service address: $(cat "$work/store-service")"
}

issue_codes() {
  if ! python3 -I "$here/issue-codes.py" "$startup_seconds" "$work/chain-codes.json" "${codes[@]}" -- "${service[@]}" --run-cli --no-address > "$work/store-service-cli.log" 2>&1; then
    echo "error: the codes were not issued"
    cat "$work/store-service-cli.log"
    return 1
  fi
  echo "issued codes: $(cat "$work/chain-codes.json")"
}

chain_chat() {
  local db=$1 user=$2 secs=$3 cmd=$4 status=0
  "$bin/simplex-chat" -d "$work/$db" -e "$cmd" -t "$secs" --execute-log all --user-display-name "$user" -s "$chain_server" < /dev/null > "$work/chain-out" 2>&1 || status=$?
  cat "$work/chain-out" >> "$work/$db-cli.log"
  echo "::group::$db: $cmd"
  cat "$work/chain-out"
  echo "::endgroup::"
  return "$status"
}

probe() {
  chain_chat chain_probe ci_probe 5 "$1"
}

expect_out() {
  if ! grep -qE "$1" "$work/chain-out"; then
    echo "error: \"$1\" not in the output"
    show_log chain-resolver
    return 1
  fi
}

until_shown() {
  local db=$1 user=$2 pattern=$3 cmd=$4 deadline=$((SECONDS + 60))
  until chain_chat "$db" "$user" 5 "$cmd" && grep -qE "$pattern" "$work/chain-out"; do
    if [ "$SECONDS" -ge "$deadline" ]; then
      echo "error: \"$pattern\" not shown by \"$cmd\" after 60 s"
      show_log chain-resolver
      return 1
    fi
  done
}

probe_names() {
  chain_chat chain_probe ci_probe 1 "/user"
  probe "/_connect plan 1 @$free_label.simplex" || true
  expect_out "SimpleX name $free_label\.simplex is available: "
  probe "/_connect plan 1 @$unlinked_label.simplex" || true
  expect_out "SimpleX name $unlinked_label\.simplex has no valid connection link"
  echo "names resolve from the chain: $free_label is available, $unlinked_label has no link"
}

check_code() {
  local code answer
  code=$(jq -er '.[1].code' "$work/chain-codes.json")
  answer=$(curl -s -X POST -H 'content-type: application/json' --data "$(jq -cn --arg c "$code" '{code: $c}')" "$service_url/api/code/check")
  echo "code check of $code: $answer"
  case "$answer" in
    *'"state":"unused"'*) ;;
    *)
      echo "error: the store service does not answer the issued code as unused"
      show_log store-service
      return 1
      ;;
  esac
}

order() {
  NAMES_WEBAPP=$names/webapp node "$here/order.mjs" "$@"
}

register_owner_name() {
  local code
  code=$(jq -er '.[0].code' "$work/chain-codes.json")
  if ! order register "$service_url" "$code" "$owner_label" "$work/name-code.json"; then
    show_log store-service
    return 1
  fi
}

set_link() {
  if ! order set-link "$service_url" "$work/name-code.json" "$1"; then
    show_log store-service
    return 1
  fi
}

claim_owner_name() {
  local address
  chain_chat chain_owner ci_owner 1 "/user"
  chain_chat chain_owner ci_owner 3 "/ad"
  address=$(awk '/Your new chat address is created!/ {getline; getline; print; exit}' "$work/chain-out")
  case "$address" in
    https://*/a#*) ;;
    *)
      echo "error: no short address for ci_owner in the output"
      return 1
      ;;
  esac
  set_link "$address"
  until_shown chain_owner ci_owner "^new contact address set$" "/_set domain 1 $owner_label.simplex"
  until_shown chain_probe ci_probe "contact address: ok to connect" "/_connect plan 1 @$owner_label.simplex"
  echo "@$owner_label.simplex resolves to ci_owner's address"
}

show_outputs() {
  local f
  for f in names-chain-server store-service name-codes.json name-code.json chain.json; do
    ls -l "$work/$f"
    cat "$work/$f"
    echo
  done
}

start() {
  echo "$bin" > "$work/chain-bin"
  names=$bin/names-chain
  service_ini=$work/store-service.ini
  service=("$bin/simplex-store-service" --service-config "$service_ini" -d "$work/store-service")
  start_chain
  start_resolver
  start_chain_smp
  make_service
  issue_codes
  start_service
  probe_names
  check_code
  jq '.[1:]' "$work/chain-codes.json" > "$work/name-codes.json"
  register_owner_name
  claim_owner_name
  show_outputs
}

stop() {
  local p
  for p in store-service chain-smp; do
    if [ -f "$work/$p.pid" ]; then
      stop_process "$p" INT
    fi
  done
  for p in chain-resolver chain-anvil chain-terms; do
    if [ -f "$work/$p.pid" ]; then
      stop_process "$p" TERM
    fi
  done
  for p in store-service store-service-cli chain-smp chain-resolver chain-deploy chain-terms chain-anvil; do
    show_log "$p"
  done
}

case "${1:-}" in
  start)
    bin=$2
    work=$3
    host=$4
    start
    ;;
  stop)
    work=$2
    stop
    ;;
  set-link)
    work=$2
    names=$(cat "$work/chain-bin")/names-chain
    set_link "$3"
    ;;
  *)
    echo "usage: $0 start <bin dir> <work dir> <server host> | stop <work dir> | set-link <work dir> <address>"
    exit 1
    ;;
esac
