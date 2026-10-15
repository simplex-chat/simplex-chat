#!/bin/bash

# Test backend on the app runner: a chat relay and a directory service on the preset servers, with one group
# and one channel registered and approved by a CLI peer, and a search request that must find both.
# Usage:
#   scripts/ci/ios-sim/backend.sh start <backend zip> <work dir>   writes the directory's short address to <work dir>/directory-link
#   scripts/ci/ios-sim/backend.sh stop <work dir>                  stops the relay and the directory, prints their logs
# The relay and the directory keep running between start and stop.

set -euo pipefail

directory_name="SimpleX Directory"
admin=ci_admin
search_text="ci seed"

# Runs one chat command in a CLI client that stays online for <secs> after it; the output is in $work/out.
chat() {
  local db=$1 secs=$2 cmd=$3
  shift 3
  local status=0
  "$bin/simplex-chat" -d "$work/$db" -e "$cmd" -t "$secs" --execute-log all "$@" < /dev/null > "$work/out" 2>&1 || status=$?
  cat "$work/out" >> "$work/$db-cli.log"
  echo "::group::$db: $cmd"
  cat "$work/out"
  echo "::endgroup::"
  return "$status"
}

admin() {
  chat "$admin" "$1" "$2" --user-display-name "$admin"
}

expect_out() {
  if ! grep -qE "$1" "$work/out"; then
    echo "error: \"$1\" not in the output"
    return 1
  fi
}

# Each run keeps the client online for a few seconds, which also lets the handshakes it waits for progress.
admin_until() {
  local pattern=$1 secs=$2 cmd=$3
  local deadline=$((SECONDS + secs))
  until admin 5 "$cmd" && grep -qE "$pattern" "$work/out"; do
    if [ "$SECONDS" -ge "$deadline" ]; then
      echo "error: \"$pattern\" not shown by \"$cmd\" after $secs s"
      return 1
    fi
  done
}

db_query() {
  sqlite3 "$work/$1_chat.db" "$2"
}

start_relay() {
  nohup "$bin/simplex-chat" -d "$work/relay" --relay --headless --user-display-name ci_relay < /dev/null > "$work/relay.log" 2>&1 &
  echo $! > "$work/relay.pid"
  local deadline=$((SECONDS + 120))
  until grep -qE '^https://[^ ]+/r#' "$work/relay.log"; do
    if ! kill -0 "$(cat "$work/relay.pid")" 2> /dev/null || [ "$SECONDS" -ge "$deadline" ]; then
      echo "error: the relay did not create its address"
      cat "$work/relay.log"
      return 1
    fi
    sleep 1
  done
  relay_link=$(grep -m 1 -oE '^https://[^ ]+/r#[^ ]+' "$work/relay.log")
  echo "relay address: $relay_link"
}

# The address is created here, like the directory tests do, with auto-accept on before the service subscribes to it.
create_directory() {
  chat directory 1 "/ad pq_ratchet=on" --create-bot-display-name "$directory_name"
  directory_link=$(awk '/Your new chat address is created!/ {getline; getline; print; exit}' "$work/out")
  case "$directory_link" in
    https://*/a#*) ;;
    *)
      echo "error: no short directory address in the output"
      return 1
      ;;
  esac
  chat directory 1 "/auto_accept on"
  expect_out "^auto_accept on"
  echo "$directory_link" > "$work/directory-link"
  echo "directory address: $directory_link"
}

start_directory() {
  # -y: the service adds its own tables to the database the CLI created, and would otherwise ask on stdin
  nohup "$bin/simplex-directory-service" -d "$work/directory" -y --super-users "$1" < /dev/null >> "$work/directory.log" 2>&1 &
  echo $! > "$work/directory.pid"
  sleep 5
  if ! kill -0 "$(cat "$work/directory.pid")" 2> /dev/null; then
    echo "error: the directory service exited"
    cat "$work/directory.log"
    return 1
  fi
}

stop_directory() {
  local pid deadline
  pid=$(cat "$work/directory.pid")
  kill -INT "$pid" 2> /dev/null || true
  deadline=$((SECONDS + 30))
  while kill -0 "$pid" 2> /dev/null; do
    if [ "$SECONDS" -ge "$deadline" ]; then
      kill -KILL "$pid" 2> /dev/null || true
    fi
    sleep 1
  done
}

# A super user is known by contact ID, which exists only after the admin connects, so the service starts
# with nobody and is restarted with the admin.
connect_admin() {
  admin 10 "/c $directory_link"
  expect_out "connection request sent!"
  admin_until "Welcome to $directory_name" 180 "/tail @'$directory_name' 10"
  local id
  id=$(db_query directory "SELECT contact_id FROM contacts WHERE local_display_name = '$admin'")
  stop_directory
  start_directory "$id:$admin"
  echo "directory restarted with super user $id:$admin"
}

# The directory sends each registration to super users as a message with the exact /approve command.
approve() {
  local name=$1 reply=$2
  admin_until "/approve [0-9]+:$name [0-9]+" 300 "/tail @'$directory_name' 20"
  local cmd
  cmd=$(grep -oE "/approve [0-9]+:$name [0-9]+" "$work/out" | tail -n 1)
  admin 5 "@'$directory_name' $cmd"
  admin_until "$reply" 120 "/tail @'$directory_name' 20"
}

seed_group() {
  admin 1 "/g ci_group CI seed group"
  expect_out "group #ci_group \(CI seed group\) is created"
  admin 5 "/a ci_group '$directory_name' admin"
  expect_out "invitation to join the group #ci_group sent to"
  approve ci_group "Group approved!"
}

seed_channel() {
  admin 1 "/relays name=ci_relay $relay_link"
  expect_out "^ok$"
  local relay_id
  relay_id=$(db_query "$admin" "SELECT chat_relay_id FROM chat_relays WHERE display_name = 'ci_relay' AND deleted = 0")
  admin 10 "/public group relays=$relay_id #ci_channel CI seed channel"
  expect_out "group #ci_channel \(CI seed channel\) is created"
  local group_id
  group_id=$(db_query "$admin" "SELECT group_id FROM groups WHERE local_display_name = 'ci_channel'")
  admin_until "relay id [0-9]+: active" 180 "/_get relays #$group_id"
  admin 5 "/share chat #ci_channel @'$directory_name'"
  expect_out "link to join channel #ci_channel"
  approve ci_channel "Channel approved!"
}

# What the app will send, from a client that never connected to the directory.
probe_search() {
  chat probe 1 "/_service_request 1 $directory_link timeout=30 {\"type\":\"search\",\"searchText\":\"$search_text\"}" --user-display-name ci_probe
  expect_out '"displayName":"ci_group"'
  expect_out '"displayName":"ci_channel"'
  echo "search for \"$search_text\" found ci_group and ci_channel"
}

dump_directory() {
  echo "::group::directory registrations"
  db_query directory "SELECT group_id, group_reg_status FROM sx_directory_group_regs" || true
  echo "::endgroup::"
}

start() {
  bin=$work/bin
  mkdir -p "$bin"
  unzip -o -q "$1" -d "$bin"
  "$bin/simplex-chat" --version
  "$bin/simplex-directory-service" --version

  start_relay
  create_directory
  start_directory "0:nobody"
  trap dump_directory EXIT
  connect_admin
  seed_group
  seed_channel
  probe_search
}

stop() {
  local p
  for p in directory relay; do
    if [ -f "$work/$p.pid" ]; then
      kill -INT "$(cat "$work/$p.pid")" 2> /dev/null || true
    fi
  done
  sleep 3
  for p in directory relay; do
    echo "::group::$p log"
    cat "$work/$p.log" 2> /dev/null || true
    echo "::endgroup::"
  done
}

case "${1:-}" in
  start)
    work=$3
    mkdir -p "$work"
    start "$2"
    ;;
  stop)
    work=$2
    stop
    ;;
  *)
    echo "usage: $0 start <backend zip> <work dir> | stop <work dir>"
    exit 1
    ;;
esac
