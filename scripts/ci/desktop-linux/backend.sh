#!/bin/bash

# Test backend on the app runner: a chat relay and a directory service on the preset servers, with one group
# and one channel registered and approved by a CLI peer, and another CLI peer that must find both by sending the
# directory the search text. Both have profile images as large as the apps make them.
# For SimpleX names, an SMP server on 127.0.0.1 resolves .testing names through a local resolver, and a CLI peer
# with an address on it claims the name seeded.testing.
# Usage:
#   scripts/ci/desktop-linux/backend.sh start <backend .tar.gz> <work dir>   writes the directory's short address to <work dir>/directory-link
#                                                                            and the names server's address to <work dir>/names-server
#   scripts/ci/desktop-linux/backend.sh stop <work dir>                      stops the servers, prints their logs
# The relay, the directory, the SMP server and the resolver keep running between start and stop.

set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
directory_name="SimpleX Directory"
admin=ci_admin
search_text="ci seed"
# the longest profile image data URI the apps make and the core accepts (maxProfileImageSize)
max_image_size=12500
name_label=seeded
resolver_port=8090

# Runs one chat command in a CLI client that stays online for <secs> after it; the output is in $work/out.
chat() {
  local db=$1 secs=$2 cmd=$3
  shift 3
  local status=0
  "$bin/simplex-chat" -d "$work/$db" -e "$cmd" -t "$secs" --execute-log all "$@" < /dev/null > "$work/out" 2>&1 || status=$?
  cat "$work/out" >> "$work/$db-cli.log"
  # commands with images are thousands of characters long
  echo "::group::$db: ${cmd:0:200}"
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
until_shown() {
  local client=$1 pattern=$2 secs=$3 cmd=$4
  local deadline=$((SECONDS + secs))
  until "$client" 5 "$cmd" && grep -qE "$pattern" "$work/out"; do
    if [ "$SECONDS" -ge "$deadline" ]; then
      echo "error: \"$pattern\" not shown by \"$cmd\" after $secs s"
      return 1
    fi
  done
}

admin_until() {
  until_shown admin "$@"
}

db_query() {
  sqlite3 "$work/$1_chat.db" "$2"
}

# Prints a profile image data URI made as the apps make one: JPEG at quality 85, scaled down by the square root
# of its excess, at most halved per step, until it is at most max_image_size characters. The picture fills the
# whole square, a gradient between two shades of one colour with a large white letter; noise keeps JPEG from
# compressing it much, so the image ends up near the limit.
avatar() {
  local top=$1 bottom=$2 letter=$3 source=$work/avatar-source.png file=$work/avatar.jpg size=512 uri font
  font=$(fc-match -f '%{file}' 'DejaVu Sans:bold')
  convert -size 512x512 gradient:"$top-$bottom" -seed 1 -attenuate 0.5 +noise Gaussian \
    -font "$font" -pointsize 320 -fill white -gravity center -annotate +0+0 "$letter" "$source"
  while :; do
    convert "$source" -resize "${size}x${size}" -quality 85 "$file"
    uri="data:image/jpg;base64,$(base64 -w 0 "$file")"
    [ "${#uri}" -gt "$max_image_size" ] || break
    size=$(awk -v s="$size" -v n="${#uri}" -v m="$max_image_size" 'BEGIN { r = sqrt(n / m); if (r > 2) r = 2; print int(s / r) }')
  done
  echo "avatar \"$letter\" over $top to $bottom: ${size}x${size} px, ${#uri} characters, mean colour $(convert "$file" -resize '1x1!' -format '%[pixel:p{0,0}]' info:)" >&2
  echo "$uri"
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

# The CLI's group commands can't set an image, so groups are created with the API commands, with the profiles
# that "/g" and "/public group" make plus the image.
group_json() {
  local name=$1 descr=$2 image=$3 prefs=$4
  echo "{\"displayName\":\"$name\",\"fullName\":\"\",\"shortDescr\":\"$descr\",\"image\":\"$image\",\"groupPreferences\":{\"directMessages\":{\"enable\":\"on\"},\"history\":{\"enable\":\"on\"}$prefs}}"
}

seed_group() {
  local user_id
  user_id=$(db_query "$admin" "SELECT user_id FROM users WHERE local_display_name = '$admin'")
  group_image=$(avatar "rgb(235,80,70)" "rgb(150,25,30)" G)
  admin 1 "/_group $user_id $(group_json ci_group "CI seeded group" "$group_image" "")"
  expect_out "group #ci_group \(CI seeded group\) is created"
  admin 5 "/a ci_group '$directory_name' admin"
  expect_out "invitation to join the group #ci_group sent to"
  approve ci_group "Group approved!"
}

seed_channel() {
  admin 1 "/relays name=ci_relay $relay_link"
  expect_out "^ok$"
  local user_id relay_id
  user_id=$(db_query "$admin" "SELECT user_id FROM users WHERE local_display_name = '$admin'")
  relay_id=$(db_query "$admin" "SELECT chat_relay_id FROM chat_relays WHERE display_name = 'ci_relay' AND deleted = 0")
  channel_image=$(avatar "rgb(80,200,90)" "rgb(25,120,45)" C)
  admin 10 "/_public group $user_id $relay_id $(group_json ci_channel "CI seeded channel" "$channel_image" ",\"support\":{\"enable\":\"off\"}")"
  expect_out "group #ci_channel \(CI seeded channel\) is created"
  local group_id
  group_id=$(db_query "$admin" "SELECT group_id FROM groups WHERE local_display_name = 'ci_channel'")
  admin_until "relay id [0-9]+: active" 180 "/_get relays #$group_id"
  admin 5 "/share chat #ci_channel @'$directory_name'"
  expect_out "link to join channel #ci_channel"
  approve ci_channel "Channel approved!"
}

# An SMP server on 127.0.0.1 whose [NAMES] section asks the local resolver, which answers from names.json.
start_names_server() {
  local cfg=$work/smp/etc logs=$work/smp/var ini deadline
  echo '{}' > "$work/names.json"
  nohup python3 "$here/names-resolver.py" "$resolver_port" "$work/names.json" < /dev/null > "$work/resolver.log" 2>&1 &
  echo $! > "$work/resolver.pid"
  SMP_SERVER_CFG_PATH=$cfg SMP_SERVER_LOG_PATH=$logs "$bin/smp-server" init -y --ip 127.0.0.1 --no-password --disable-web
  ini=$cfg/smp-server.ini
  # only 5223: 443 needs root
  sed -i -e 's/^port = 5223,443$/port = 5223/' \
    -e '/^\[NAMES\]/,/^\[/{s/^enable: off$/enable: on/;s|^# resolver_endpoint: http://127.0.0.1:8000$|resolver_endpoint: http://127.0.0.1:'"$resolver_port"'|}' "$ini"
  if ! grep -qx 'port = 5223' "$ini" ||
    ! sed -n '/^\[NAMES\]/,/^\[/p' "$ini" | grep -qx 'enable: on' ||
    ! sed -n '/^\[NAMES\]/,/^\[/p' "$ini" | grep -qx "resolver_endpoint: http://127.0.0.1:$resolver_port"; then
    echo "error: $ini has a different port or [NAMES] than expected:"
    grep -E '^port|^\[NAMES\]|enable|resolver' "$ini"
    return 1
  fi
  SMP_SERVER_CFG_PATH=$cfg SMP_SERVER_LOG_PATH=$logs nohup "$bin/smp-server" start < /dev/null > "$work/smp-server.log" 2>&1 &
  echo $! > "$work/smp-server.pid"
  deadline=$((SECONDS + 60))
  until (exec 3<> /dev/tcp/127.0.0.1/5223) 2> /dev/null; do
    if ! kill -0 "$(cat "$work/smp-server.pid")" 2> /dev/null || [ "$SECONDS" -ge "$deadline" ]; then
      echo "error: the SMP server is not listening on 5223"
      cat "$work/smp-server.log"
      return 1
    fi
    sleep 1
  done
  names_server="smp://$(cat "$cfg/fingerprint")@127.0.0.1"
  echo "$names_server" > "$work/names-server"
  echo "names server: $names_server"
}

# A CLI client started with -s uses only that server and, once its user exists, gives it every role, names included.
names_client() {
  local db=$1
  shift
  chat "$db" "$@" --user-display-name "$db" -s "$names_server"
}

# ci_contact's address is on the names server; the resolver maps seeded.testing to it, and ci_contact claims the name.
seed_name() {
  local image link
  image=$(avatar "rgb(70,130,240)" "rgb(30,50,170)" N)
  names_client ci_contact 1 "/_profile 1 {\"displayName\":\"ci_contact\",\"fullName\":\"\",\"shortDescr\":\"CI seeded contact\",\"image\":\"$image\"}"
  names_client ci_contact 3 "/ad"
  link=$(awk '/Your new chat address is created!/ {getline; getline; print; exit}' "$work/out")
  case "$link" in
    https://*/a#*) ;;
    *)
      echo "error: no short address for ci_contact in the output"
      return 1
      ;;
  esac
  jq -n --arg n "$name_label" --arg l "$link" '{($n): {simplexContact: [$l]}}' > "$work/names.json"
  names_client ci_contact 5 "/_set domain 1 $name_label.testing"
  # what the app does on the Search key, from a client that knows nothing but the names server; it finds the
  # address only if ci_contact's claim is in the address data
  names_client name_probe 1 "/user"
  names_client name_probe 5 "/_connect plan 1 @$name_label.testing"
  expect_out "contact address: ok to connect"
  echo "@$name_label.testing resolves to ci_contact's address"
}

searcher() {
  chat searcher "$1" "$2" --user-display-name ci_searcher
}

# A new user connects to the directory and sends the search text; it replies with the count, then one message per
# group or channel found, with its image.
search_by_message() {
  searcher 10 "/c $directory_link"
  expect_out "connection request sent!"
  until_shown searcher "Welcome to $directory_name" 180 "/tail @'$directory_name' 10"
  searcher 5 "@'$directory_name' $search_text"
  until_shown searcher "Found 2 group\(s\)" 120 "/tail @'$directory_name' 10"
  until_shown searcher "ci_group" 60 "/tail @'$directory_name' 10"
  until_shown searcher "ci_channel" 60 "/tail @'$directory_name' 10"
  echo "search for \"$search_text\" by message found ci_group and ci_channel"
}

dump_directory() {
  echo "::group::directory registrations"
  db_query directory "SELECT group_id, group_reg_status FROM sx_directory_group_regs" || true
  echo "::endgroup::"
}

start() {
  bin=$work/bin
  mkdir -p "$bin"
  tar -C "$bin" -xzf "$1"
  "$bin/simplex-chat" --version
  "$bin/simplex-directory-service" --version

  start_relay
  create_directory
  start_directory "0:nobody"
  trap dump_directory EXIT
  connect_admin
  seed_group
  seed_channel
  search_by_message
  start_names_server
  seed_name
}

stop() {
  local p
  for p in directory relay smp-server resolver; do
    if [ -f "$work/$p.pid" ]; then
      kill -INT "$(cat "$work/$p.pid")" 2> /dev/null || true
    fi
  done
  sleep 3
  for p in directory relay smp-server resolver; do
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
    echo "usage: $0 start <backend .tar.gz> <work dir> | stop <work dir>"
    exit 1
    ;;
esac
