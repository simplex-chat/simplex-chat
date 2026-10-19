#!/bin/bash

# Test backend on the app runner: a chat relay and a directory service on the preset servers, with groups and
# channels registered and approved by a CLI peer, and searches that must find them, sent as a message and as the
# app sends them; and for SimpleX names, a local SMP server with a name resolver, names claimed by channels and by
# contacts that accept connections.
# Usage:
#   scripts/ci/ios-sim/backend.sh start <backend zip> <work dir>   writes the directory's short address to <work dir>/directory-link,
#                                                                  the names server's address to <work dir>/names-server
#                                                                  and what was seeded to <work dir>/seed.md
#   scripts/ci/ios-sim/backend.sh resume <backend zip> <work dir>  starts again, on another runner, what start left stopped in <work dir>,
#                                                                  with the same addresses
#   scripts/ci/ios-sim/backend.sh stop <work dir>                  stops everything it started, prints the logs
#   scripts/ci/ios-sim/backend.sh directory-stop <work dir>        stops the directory only
#   scripts/ci/ios-sim/backend.sh directory-start <work dir>       starts it again
#   scripts/ci/ios-sim/backend.sh listings <work dir>              prints the directory's listed groups and channels
# Everything it starts keeps running between start and stop.

set -euo pipefail

directory_name="SimpleX Directory"
admin=ci_admin
search_text="ci seed"
avatar_search_text="ci avatar"
long_name=ci_long_name_entry_for_layout_truncation_checks
# maxServiceBodyLength in src/Simplex/Chat/Protocol.hs
max_service_body=10737
names_resolver_port=18080

# Runs one chat command in a CLI client that stays online for <secs> after it; the output is in $work/out.
chat() {
  local db=$1 secs=$2 cmd=$3
  shift 3
  local status=0
  "$bin/simplex-chat" -d "$work/$db" -e "$cmd" -t "$secs" --execute-log all "$@" < /dev/null > "$work/out" 2>&1 || status=$?
  cat "$work/out" >> "$work/$db-cli.log"
  echo "::group::$db: $(cut -c1-200 <<< "$cmd")"
  cat "$work/out"
  echo "::endgroup::"
  return "$status"
}

admin() {
  chat "$admin" "$1" "$2" --user-display-name "$admin"
}

# A command that resolves a SimpleX name: given -s, the client uses that server alone, with every role, names included.
# -s also saves it among the user's servers, and later runs would put queues on it that the directory can't reach
# (it routes to unknown servers through a preset proxy), so it is then kept for names only.
named() {
  local db=$1 user=$2 secs=$3 cmd=$4
  local status=0
  chat "$db" "$secs" "$cmd" --user-display-name "$user" -s "$names_server" || status=$?
  db_query "$db" "UPDATE protocol_servers SET role_storage = 0, role_proxy = 0, role_names = 1 WHERE host = 'localhost'"
  return "$status"
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

listings_query="SELECT gp.display_name AS name, gp.short_descr AS description, g.summary_current_members_count AS members, length(gp.image) AS image_characters
  FROM sx_directory_group_regs r
  JOIN groups g ON g.group_id = r.group_id
  JOIN group_profiles gp ON gp.group_profile_id = g.group_profile_id
  WHERE r.group_reg_status = 'active'
  ORDER BY g.summary_current_members_count DESC, r.group_id"

run_relay() {
  nohup "$bin/simplex-chat" -d "$work/relay" --relay --headless --user-display-name ci_relay < /dev/null >> "$work/relay.log" 2>&1 &
  echo $! > "$work/relay.pid"
}

start_relay() {
  run_relay
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
  directory_link=$(new_address)
  chat directory 1 "/auto_accept on"
  expect_out "^auto_accept on"
  echo "$directory_link" > "$work/directory-link"
  echo "directory address: $directory_link"
}

start_directory() {
  # -y: the service adds its own tables to the database the CLI created, and would otherwise ask on stdin
  nohup "$bin/simplex-directory-service" -d "$work/directory" -y --super-users "$1" < /dev/null >> "$work/directory.log" 2>&1 &
  echo $! > "$work/directory.pid"
  echo "$1" > "$work/super-user"
  wait_connected directory
}

# A chat process shows that its chat has started by connecting to its servers: its output is block-buffered.
wait_connected() {
  local pid deadline=$((SECONDS + 60))
  pid=$(cat "$work/$1.pid")
  until lsof -a -p "$pid" -iTCP -sTCP:ESTABLISHED > /dev/null 2>&1; do
    if ! kill -0 "$pid" 2> /dev/null || [ "$SECONDS" -ge "$deadline" ]; then
      echo "error: $1 did not start"
      cat "$work/$1.log"
      return 1
    fi
    sleep 1
  done
}

# Waits for the process to exit, which completes its output, and kills it after 30 s.
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

# A super user is known by contact ID, which exists only after the admin connects, so the service starts
# with nobody and is restarted with the admin.
connect_admin() {
  admin 10 "/c $directory_link"
  expect_out "connection request sent!"
  admin_until "Welcome to $directory_name" 180 "/tail @'$directory_name' 10"
  local id
  id=$(db_query directory "SELECT contact_id FROM contacts WHERE local_display_name = '$admin'")
  stop_process directory INT
  start_directory "$id:$admin"
  echo "directory restarted with super user $id:$admin"
}

# The directory sends each registration to super users as a message with the exact /approve command,
# and tells the owner "The group ID N (name) is approved".
approve() {
  local name=$1
  admin_until "/approve [0-9]+:$name [0-9]+" 300 "/tail @'$directory_name' 20"
  local cmd
  cmd=$(grep -oE "/approve [0-9]+:$name [0-9]+" "$work/out" | tail -n 1)
  admin 5 "@'$directory_name' $cmd"
  admin_until "\($name\) is approved" 120 "/tail @'$directory_name' 20"
}

# The profile the app sends to create a group or a channel (AddGroupView, AddChannelView), with an optional image file.
group_profile() {
  local name=$1 descr=$2 prefs=$3 image_file=${4:-}
  if [ -n "$image_file" ]; then
    jq -cn --arg n "$name" --arg d "$descr" --argjson p "$prefs" --rawfile i "$image_file" '{displayName: $n, fullName: "", shortDescr: $d, image: $i, groupPreferences: $p}'
  else
    jq -cn --arg n "$name" --arg d "$descr" --argjson p "$prefs" '{displayName: $n, fullName: "", shortDescr: $d, groupPreferences: $p}'
  fi
}

seed_group() {
  local name=$1 descr=$2 image_file=${3:-}
  admin 1 "/_group 1 $(group_profile "$name" "$descr" '{"history":{"enable":"on"}}' "$image_file")"
  expect_out "group #$name .*is created"
  admin 5 "/a $name '$directory_name' admin"
  expect_out "invitation to join the group #$name sent to"
  approve "$name"
}

setup_relay() {
  admin 1 "/relays name=ci_relay $relay_link"
  expect_out "^ok$"
  relay_id=$(db_query "$admin" "SELECT chat_relay_id FROM chat_relays WHERE display_name = 'ci_relay' AND deleted = 0")
}

create_channel() {
  local name=$1 descr=$2 image_file=${3:-}
  admin 10 "/_public group 1 $relay_id $(group_profile "$name" "$descr" '{"history":{"enable":"on"},"support":{"enable":"off"}}' "$image_file")"
  expect_out "group #$name .*is created"
  local group_id
  group_id=$(db_query "$admin" "SELECT group_id FROM groups WHERE local_display_name = '$name'")
  admin_until "relay id [0-9]+: active" 180 "/_get relays #$group_id"
}

list_channel() {
  local name=$1
  admin 5 "/share chat #$name @'$directory_name'"
  expect_out "link to join channel #$name"
  approve "$name"
}

seed_channel() {
  create_channel "$@"
  list_channel "$1"
}

channel_link() {
  admin 1 "/show link #$1" >&2
  grep -m 1 -oE 'https://[^ ]+/c#[^ ]+' "$work/out"
}

# The short address that /ad printed.
new_address() {
  local address
  address=$(awk '/Your new chat address is created!/ {getline; getline; print; exit}' "$work/out")
  case "$address" in
    https://*/a#*) echo "$address" ;;
    *)
      echo "error: no short address in the output" >&2
      return 1
      ;;
  esac
}

# Avatars at the size the app makes them (avatar.swift), with different amounts of detail;
# and small ones for ci_group, ci_channel and ci_cards, so that several fit in one size-limited search response.
make_avatars() {
  xcrun swiftc -O -o "$work/avatar" "$(dirname "$0")/avatar.swift"
  local i
  for i in 1 2 3 4 5 6; do
    "$work/avatar" "$i" "0.$((i + 2))" "$work/avatar-$i.txt"
  done
  for i in 7 8 9; do
    "$work/avatar" "$i" 0 "$work/avatar-$i.txt" 64
  done
}

seed_avatars() {
  local i
  for i in 1 2 3; do
    seed_group "ci_avatar_g$i" "CI avatar group $i" "$work/avatar-$i.txt"
    seed_channel "ci_avatar_c$i" "CI avatar channel $i" "$work/avatar-$((i + 3)).txt"
  done
}

# A local SMP server whose [NAMES] section points at names-resolver.py, which answers from names.json.
start_names() {
  echo "{}" > "$work/names.json"
  export SMP_SERVER_CFG_PATH=$work/smp-server/etc SMP_SERVER_LOG_PATH=$work/smp-server/log
  "$bin/smp-server" init -y --disable-store-log --no-password --fqdn localhost --disable-web > "$work/out" 2>&1 || {
    cat "$work/out"
    return 1
  }
  cat "$work/out"
  names_server=$(grep -m 1 -oE 'smp://[^ ]+@localhost' "$work/out")
  sed -i '' \
    -e 's/^enable: off$/enable: on/' \
    -e "s|^# resolver_endpoint: http://127.0.0.1:8000\$|resolver_endpoint: http://127.0.0.1:$names_resolver_port|" \
    -e 's/^port = 5223,443$/port = 5223/' \
    "$SMP_SERVER_CFG_PATH/smp-server.ini"
  grep -E '^(enable: on|resolver_endpoint|port = )' "$SMP_SERVER_CFG_PATH/smp-server.ini"
  run_names
  echo "$names_server" > "$work/names-server"
  echo "names server: $names_server"
}

run_names() {
  nohup python3 "$(dirname "$0")/names-resolver.py" "$names_resolver_port" "$work/names.json" < /dev/null >> "$work/names-resolver.log" 2>&1 &
  echo $! > "$work/names-resolver.pid"
  export SMP_SERVER_CFG_PATH=$work/smp-server/etc SMP_SERVER_LOG_PATH=$work/smp-server/log
  nohup "$bin/smp-server" start < /dev/null >> "$work/smp-server.log" 2>&1 &
  echo $! > "$work/smp-server.pid"
  local deadline=$((SECONDS + 60))
  until nc -z 127.0.0.1 5223 2> /dev/null; do
    if ! kill -0 "$(cat "$work/smp-server.pid")" 2> /dev/null || [ "$SECONDS" -ge "$deadline" ]; then
      echo "error: the names server did not start"
      cat "$work/smp-server.log" "$work/names-resolver.log"
      return 1
    fi
    sleep 1
  done
}

register_name() {
  local name=$1 contact=$2 channel=$3
  jq --arg n "$name" --arg ct "$contact" --arg ch "$channel" \
    '.[$n] = {simplexContact: ([$ct] | map(select(. != ""))), simplexChannel: ([$ch] | map(select(. != "")))}' \
    "$work/names.json" > "$work/names.json.new"
  mv "$work/names.json.new" "$work/names.json"
}

# A contact that claims a name and accepts every connection; it keeps running, so the app can connect to it.
start_contact() {
  local db=$1 display_name=$2 name=$3
  chat "$db" 1 "/ad" --user-display-name "$display_name"
  local address
  address=$(new_address)
  chat "$db" 1 "/auto_accept on" --user-display-name "$display_name"
  expect_out "^auto_accept on"
  register_name "$name" "$address" ""
  named "$db" "$display_name" 5 "/_set domain 1 $name"
  expect_out "^new contact address set$"
  run_contact "$db" "$display_name"
}

run_contact() {
  local db=$1 display_name=$2
  # --headless is for relays only; a command with a long -t keeps the client online, accepting contacts, for 6 hours
  nohup "$bin/simplex-chat" -d "$work/$db" -e "/auto_accept on" -t 21600 --user-display-name "$display_name" < /dev/null >> "$work/$db.log" 2>&1 &
  echo $! > "$work/$db.pid"
}

# A name is claimed before its channel is listed: a profile change after approval hides it until approved again.
seed_names() {
  local address link
  create_channel cinews "CI news channel"
  link=$(channel_link cinews)
  register_name cinews.testing "" "$link"
  named "$admin" "$admin" 5 "/public group access #cinews domain=cinews.testing"
  expect_out "updated public group access: domain=cinews.testing"
  list_channel cinews
  seed_group cinews_digest "CI news digest"
  seed_group cinews_weekly "CI news weekly"

  create_channel ciboth "CI both channel"
  admin 1 "/ad"
  address=$(new_address)
  link=$(channel_link ciboth)
  register_name ciboth.testing "$address" "$link"
  named "$admin" "$admin" 5 "/_set domain 1 ciboth.testing"
  expect_out "^new contact address set$"
  named "$admin" "$admin" 5 "/public group access #ciboth domain=ciboth.testing"
  expect_out "updated public group access: domain=ciboth.testing"

  start_contact ci_alice cialice cialice.testing
  start_contact ci_robert Robert cibobby.testing
}

# What the app will resolve, from a client that knows none of the names' owners.
probe_names() {
  local target pattern
  # -s gives its server the names role only for users that exist when the client starts
  chat probe 1 "/user" --user-display-name ci_probe
  for target in "#cinews.testing|ok to connect via relays" "@cialice.testing|contact address: ok to connect" "@cibobby.testing|contact address: ok to connect" "ciboth.testing|You can also connect to @ciboth"; do
    pattern=${target#*|}
    target=${target%%|*}
    named probe ci_probe 5 "/_connect plan 1 $target"
    expect_out "$pattern"
  done
  named probe ci_probe 5 "/_connect plan 1 #cinone.testing" || true
  echo "names resolve: cinews, cialice, cibobby, ciboth; cinone is not registered"
}

# The directory's reply to a search starts with "Found N group(s)"; earlier messages name the same groups.
in_search_reply() {
  local reply
  reply=$(sed -n '/Found [0-9]* group(s)/,$p' "$work/out")
  grep -qF "$1" <<< "$reply"
}

# A search sent as a message, which any client can send.
probe_search() {
  admin 5 "@'$directory_name' $search_text"
  local deadline=$((SECONDS + 120))
  until admin 5 "/tail @'$directory_name' 10" && in_search_reply "ci_group (CI seed group)" && in_search_reply "ci_channel (CI seed channel)"; do
    if [ "$SECONDS" -ge "$deadline" ]; then
      echo "error: the directory's reply to \"$search_text\" does not list ci_group and ci_channel"
      return 1
    fi
  done
  echo "search for \"$search_text\" found ci_group and ci_channel"
}

# What the app will send, from a client that never connected to the directory.
probe_service_search() {
  chat probe 1 "/_service_request 1 $directory_link timeout=30 {\"type\":\"search\",\"searchText\":\"$search_text\"}" --user-display-name ci_probe
  expect_out '"displayName":"ci_group"'
  expect_out '"displayName":"ci_channel"'
  echo "search request for \"$search_text\" found ci_group and ci_channel"
}

# Follows the search cursor to the end, recording what each response carried and its size against the limit.
probe_pages() {
  local text=$1 expected=$2
  local page=0 cursor=null req resp names=""
  {
    echo "## Search \"$text\", page by page"
    echo
    echo "Responses must fit $max_service_body bytes, as JSON or zstd-compressed (the zstd size is measured with the zstd CLI at level 3, a few bytes off the core's framing)."
    echo
    echo "| Page | Entries (avatar characters) | JSON bytes | zstd -3 bytes | More |"
    echo "|---|---|---|---|---|"
  } >> "$work/seed.md"
  while [ "$cursor" != end ]; do
    page=$((page + 1))
    if [ "$page" -gt 20 ]; then
      echo "error: more than 20 pages"
      return 1
    fi
    req=$(jq -cn --arg t "$text" --argjson c "$cursor" '{type: "search", searchText: $t} + (if $c == null then {} else {searchCursor: $c} end)')
    chat probe 1 "/_service_request 1 $directory_link timeout=30 $req" --user-display-name ci_probe
    resp=$(sed -n 's/^service response: //p' "$work/out")
    if [ -z "$resp" ]; then
      echo "error: no service response for page $page"
      return 1
    fi
    if jq -e '.searchCursor != null' <<< "$resp" > /dev/null; then
      cursor=$(jq -c '.searchCursor' <<< "$resp")
    else
      cursor=end
    fi
    names="$names$(jq -r '.entries[].displayName' <<< "$resp")"$'\n'
    echo "| $page | $(jq -r '[.entries[] | "\(.displayName) (\(.image // "" | length))"] | join(", ")' <<< "$resp") | $(printf '%s' "$resp" | wc -c | tr -d ' ') | $(printf '%s' "$resp" | zstd -3 -q -c --no-check | wc -c | tr -d ' ') | $([ "$cursor" = end ] && echo no || echo yes) |" >> "$work/seed.md"
  done
  local found repeated
  found=$(grep -c . <<< "$names" || true)
  repeated=$({ grep . <<< "$names" || true; } | sort | uniq -d | paste -sd ' ' -)
  {
    echo
    echo "$found entries in $page pages, $expected expected; repeated: ${repeated:-none}."
    echo
  } >> "$work/seed.md"
  echo "search for \"$text\": $found entries in $page pages, $expected expected, repeated: ${repeated:-none}"
  if [ "$found" -ne "$expected" ] || [ -n "$repeated" ]; then
    echo "::warning::search for \"$text\" returned $found entries ($expected expected), repeated: ${repeated:-none}"
  fi
}

write_seed() {
  {
    echo "# Test directory"
    echo
    echo "Directory address: $directory_link"
    echo
    echo "## Listed groups and channels"
    echo
    echo "| Search with | Entries | Seeded for |"
    echo "|---|---|---|"
    echo "| \`$search_text\` | ci_group (group), ci_channel (channel), each with a small avatar (64 px) | the smoke test; results on one page |"
    echo "| \`ci cards\` | ci_cards (channel, with a small avatar) | the smoke test joins it, so that the chat list shows its cards |"
    echo "| \`$avatar_search_text\` | ci_avatar_g1…3 (groups), ci_avatar_c1…3 (channels), each with an avatar the size the app makes | avatars and the response size limit: about one entry per page |"
    echo "| \`ci long\` | $long_name (group): a long name and a two-line description | D3 |"
    echo "| \`ci markdown\` | ci_markdown (group): markdown in the description | D5 |"
    echo "| \`cinews\` | cinews (channel, with the name cinews.testing), cinews_digest and cinews_weekly (groups) | a search that shows local chats, Names and Directory |"
    echo
    sqlite3 -markdown "$work/directory_chat.db" "$listings_query"
    echo
    echo "## SimpleX names"
    echo
    echo "Names server, to add in the app for resolving names: \`$names_server\`"
    echo
    echo "| Name | Resolves to |"
    echo "|---|---|"
    echo "| cinews.testing | the channel cinews, listed in the directory (\"CI news channel\") |"
    echo "| ciboth.testing | a contact address (ci_admin) and the channel ciboth, not listed |"
    echo "| cialice.testing | the contact cialice, online and accepting connections |"
    echo "| cibobby.testing | the contact Robert (display name without the name), online and accepting connections |"
    echo "| any other, e.g. cinone.testing | not registered |"
    echo
  } > "$work/seed.md"
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
  "$bin/smp-server" --version
  make_avatars

  start_names
  start_relay
  create_directory
  start_directory "0:nobody"
  trap dump_directory EXIT
  connect_admin
  seed_group ci_group "CI seed group" "$work/avatar-7.txt"
  setup_relay
  seed_channel ci_channel "CI seed channel" "$work/avatar-8.txt"
  seed_channel ci_cards "CI cards channel" "$work/avatar-9.txt"
  seed_avatars
  seed_group "$long_name" "CI long entry: a description long enough to take two lines in the directory results, to check how the row truncates"
  seed_group ci_markdown "CI markdown *bold* _italic_ [x](https://example.com)"
  seed_names
  probe_search
  probe_names
  probe_service_search
  write_seed
  probe_pages "$search_text" 2
  probe_pages "$avatar_search_text" 6
}

# The servers' keys and the clients' databases stay in the work dir, so its addresses are the same again.
resume() {
  bin=$work/bin
  mkdir -p "$bin"
  unzip -o -q "$1" -d "$bin"
  run_names
  run_relay
  wait_connected relay
  start_directory "$(cat "$work/super-user")"
  run_contact ci_alice cialice
  run_contact ci_robert Robert
  echo "resumed: directory $(cat "$work/directory-link"), names server $(cat "$work/names-server")"
}

stop() {
  local p processes="directory relay ci_alice ci_robert smp-server"
  for p in $processes; do
    if [ -f "$work/$p.pid" ]; then
      stop_process "$p" INT
    fi
  done
  # Python, started in the background by a script, ignores SIGINT
  if [ -f "$work/names-resolver.pid" ]; then
    stop_process names-resolver TERM
  fi
  for p in $processes names-resolver; do
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
  resume)
    work=$3
    resume "$2"
    ;;
  stop)
    work=$2
    stop
    ;;
  directory-stop)
    work=$2
    stop_process directory INT
    ;;
  directory-start)
    work=$2
    bin=$work/bin
    start_directory "$(cat "$work/super-user")"
    ;;
  listings)
    work=$2
    sqlite3 -markdown "$work/directory_chat.db" "$listings_query"
    ;;
  *)
    echo "usage: $0 start <backend zip> <work dir> | resume <backend zip> <work dir> | stop <work dir> | directory-stop <work dir> | directory-start <work dir> | listings <work dir>"
    exit 1
    ;;
esac
