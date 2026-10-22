#!/usr/bin/env bash
# Denies the store service's new outbound connections to private, loopback, link-local, unique-local,
# CGNAT, unspecified and multicast addresses, except the RPC node, BTCPay and the database it is
# configured with and the system's DNS resolvers. Run as root; see the README's egress section.
set -euo pipefail
shopt -s inherit_errexit

CHAIN=STORE_SERVICE_EGRESS
DEFAULT_DB_CONN=postgresql://simplex@127.0.0.1:5432/badge_service
DNS_PORT=53
POSTGRES_PORT=5432
HTTP_PORT=80
HTTPS_PORT=443
DOCKER_CHAIN=DOCKER-USER

# The ranges the link previews' host check refuses (refusedIPv4 and refusedIPv6 in
# apps/simplex-store-service/src/StoreService/Names/Profiles.hs).
DENIED_V4=(0.0.0.0/8 10.0.0.0/8 100.64.0.0/10 127.0.0.0/8 169.254.0.0/16 172.16.0.0/12 192.168.0.0/16 198.18.0.0/15 224.0.0.0/4 240.0.0.0/4)
DENIED_V6=(fc00::/7 fe80::/10 fec0::/10 ff00::/8)
# IPv4-compatible, IPv4-mapped and NAT64 addresses carry an IPv4 address in their last 32 bits.
V4_EMBEDDING_PREFIXES=(:: ::ffff: 64:ff9b::)

usage() {
  cat <<EOF
Usage: $0 [--dry-run | --check | --remove] (--uid UID | --bridge IFACE) [--ini FILE] [--db CONN]
          [--resolv-conf FILE] [--allow HOST:PORT]...

Installs the egress rule as the iptables and ip6tables chain $CHAIN, rebuilt on every run.

  --dry-run          print the commands an install would run, and change nothing
  --check            exit 1 unless every rule and jump an install writes is in place and the chain
                     holds no other rule
  --remove           delete the jumps an install adds for this --uid or --bridge, then the chain
  --uid UID          host networking: match the service's user, which must have no login shell
  --bridge IFACE     a Docker bridge network: match traffic from IFACE ($DOCKER_CHAIN and INPUT)
  --ini FILE         store_service.ini, read for [chain] rpc_url and [btcpay] host
                     (default ./store_service.ini)
  --db CONN          the database connection string the service is started with
                     (default \$BADGE_DB_CONN, else $DEFAULT_DB_CONN)
  --resolv-conf FILE the DNS resolvers the service uses (default /etc/resolv.conf)
  --allow HOST:PORT  another TCP endpoint to allow, such as a SOCKS proxy; repeatable
EOF
}

die() {
  echo "egress.sh: $*" >&2
  exit 2
}

mode=install
uid=
bridge=
ini=./store_service.ini
db_conn=${BADGE_DB_CONN:-$DEFAULT_DB_CONN}
resolv_conf=/etc/resolv.conf
extra=()

while (($#)); do
  case $1 in
    --dry-run) mode=dry-run ;;
    --check) mode=check ;;
    --remove) mode=remove ;;
    --uid) uid=${2:?--uid needs a value}; shift ;;
    --bridge) bridge=${2:?--bridge needs a value}; shift ;;
    --ini) ini=${2:?--ini needs a value}; shift ;;
    --db) db_conn=${2:?--db needs a value}; shift ;;
    --resolv-conf) resolv_conf=${2:?--resolv-conf needs a value}; shift ;;
    --allow) extra+=("${2:?--allow needs a value}"); shift ;;
    -h | --help) usage; exit 0 ;;
    *) usage >&2; exit 2 ;;
  esac
  shift
done

if [[ ! $bridge ]]; then
  [[ $uid ]] || die "--uid is required with host networking: the uid the service runs as (USER_UID in compose)"
  [[ $uid =~ ^[0-9]+$ ]] || die "--uid must be a numeric user id"
fi

ini_value() {
  awk -v section="$1" -v key="$2" '
    /^[[:space:]]*[;#]/ { next }
    /^[[:space:]]*\[/ { current = $0; gsub(/^[[:space:]]*\[[[:space:]]*|[[:space:]]*\][[:space:]]*$/, "", current); next }
    current == section {
      eq = index($0, "=")
      if (eq == 0) next
      k = substr($0, 1, eq - 1); v = substr($0, eq + 1)
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", k); gsub(/^[[:space:]]+|[[:space:]]+$/, "", v)
      if (k == key) { print v; exit }
    }' "$ini"
}

scheme_port() {
  case $1 in
    http) echo "$HTTP_PORT" ;;
    https) echo "$HTTPS_PORT" ;;
    postgres | postgresql) echo "$POSTGRES_PORT" ;;
    *) die "no default port for the scheme $1" ;;
  esac
}

# "host port" of an authority such as user@host:port or [v6]:port; the URL itself is never printed,
# since an RPC URL can carry a provider's API key.
authority_endpoint() {
  local authority=${1##*@} default_port=$2 host port
  if [[ $authority == \[* ]]; then
    host=${authority%%]*}
    host=${host#[}
    port=${authority#*]}
  else
    host=${authority%%:*}
    port=${authority#"$host"}
  fi
  port=${port#:}
  echo "$host ${port:-$default_port}"
}

url_endpoint() {
  local url=$1 rest port
  [[ $url == *://* ]] || die "a configured URL has no scheme"
  port=$(scheme_port "${url%%://*}")
  rest=${url#*://}
  rest=${rest%%/*}
  authority_endpoint "${rest%%\?*}" "$port"
}

# A URL, or libpq's key=value form; nothing when it names no host (a Unix socket).
db_endpoint() {
  local conn=$1 host='' port=$POSTGRES_PORT endpoint pair
  local -a pairs
  if [[ $conn == *://* ]]; then
    endpoint=$(url_endpoint "$conn")
    [[ ${endpoint%% *} ]] && echo "$endpoint"
    return 0
  fi
  read -ra pairs <<<"$conn"
  for pair in "${pairs[@]}"; do
    case $pair in
      host=* | hostaddr=*) host=${pair#*=} ;;
      port=*) port=${pair#*=} ;;
    esac
  done
  [[ $host && $host != /* ]] && echo "$host $port"
  return 0
}

addresses_of() {
  local host=$1 found
  if [[ $host =~ ^[0-9.]+$ || $host == *:* ]]; then
    echo "${host%%%*}"
    return
  fi
  found=$(getent ahosts "$host" | awk '{ print $1 }' | sort -u) || true
  [[ $found ]] || die "cannot resolve $host; pass its address with --allow ADDRESS:PORT"
  echo "$found"
}

# The owner match covers every process of the uid, so a user who logs in would lose private
# addresses too. An empty shell field in passwd means /bin/sh.
refuse_login_user() {
  local entry shell
  entry=$(getent passwd "$uid") || return 0
  shell=$(cut -d: -f7 <<<"$entry")
  case ${shell##*/} in
    nologin | false) ;;
    *) die "uid $uid has the login shell ${shell:-/bin/sh}, and the rule would restrict its own traffic too; run the service as a dedicated user without a login shell (USER_UID in compose) and pass that uid" ;;
  esac
}

allowed=()

allow() {
  local host=$1 port=$2 addresses address proto
  shift 2
  [[ $host ]] || die "a configured endpoint has no host"
  [[ $port =~ ^[0-9]+$ ]] || die "the port of $host is not a number"
  addresses=$(addresses_of "$host")
  while read -r address; do
    for proto in "$@"; do allowed+=("$address $proto $port"); done
  done <<<"$addresses"
}

# "host port" from one of the endpoint functions above, allowed for TCP
allow_endpoint() {
  local host port
  read -r host port <<<"$1"
  allow "$host" "$port" tcp
}

collect_allowed() {
  local setting section key url endpoint authority resolvers resolver
  [[ -r $ini ]] || die "cannot read $ini"
  for setting in "chain rpc_url" "btcpay host"; do
    read -r section key <<<"$setting"
    url=$(ini_value "$section" "$key")
    if [[ $url ]]; then
      endpoint=$(url_endpoint "$url")
      allow_endpoint "$endpoint"
    fi
  done
  endpoint=$(db_endpoint "$db_conn")
  if [[ $endpoint ]]; then allow_endpoint "$endpoint"; fi
  for authority in "${extra[@]}"; do
    [[ $authority == *:* ]] || die "--allow needs HOST:PORT"
    endpoint=$(authority_endpoint "$authority" "")
    allow_endpoint "$endpoint"
  done
  if [[ -r $resolv_conf ]]; then
    resolvers=$(awk '$1 == "nameserver" { print $2 }' "$resolv_conf")
    while read -r resolver; do
      if [[ $resolver ]]; then allow "$resolver" "$DNS_PORT" udp tcp; fi
    done <<<"$resolvers"
  fi
  if ((${#allowed[@]})); then mapfile -t allowed < <(printf '%s\n' "${allowed[@]}" | sort -u); fi
}

v4_in_v6() {
  local prefix=$1 range=$2 a b c d bits
  IFS=./ read -r a b c d bits <<<"$range"
  printf '%s%x:%x/%d\n' "$prefix" $((a << 8 | b)) $((c << 8 | d)) $((96 + bits))
}

# One rule per line, in chain order: replies, then the allowed endpoints, then the denied ranges.
rules_for() {
  local family=$1 entry address address_family proto port range prefix
  echo "-m conntrack --ctstate ESTABLISHED,RELATED -j RETURN"
  for entry in "${allowed[@]}"; do
    read -r address proto port <<<"$entry"
    if [[ $address == *:* ]]; then address_family=6; else address_family=4; fi
    if [[ $address_family == "$family" ]]; then
      echo "-d $address -p $proto --dport $port -j RETURN"
    fi
  done
  if [[ $family == 4 ]]; then
    for range in "${DENIED_V4[@]}"; do echo "-d $range -j REJECT"; done
  else
    for range in "${DENIED_V6[@]}"; do echo "-d $range -j REJECT"; done
    for prefix in "${V4_EMBEDDING_PREFIXES[@]}"; do
      for range in "${DENIED_V4[@]}"; do echo "-d $(v4_in_v6 "$prefix" "$range") -j REJECT"; done
    done
  fi
}

# "PARENT match..." per line: where the service's traffic enters the chain.
jumps_for() {
  local tool=$1
  if [[ $bridge ]]; then
    if "$tool" -w -S "$DOCKER_CHAIN" >/dev/null 2>&1; then
      echo "$DOCKER_CHAIN -i $bridge"
    fi
    echo "INPUT -i $bridge"
  else
    echo "OUTPUT -m owner --uid-owner $uid"
  fi
}

# The rules of a chain as `iptables -S` prints them, in order.
chain_rules() {
  local tool=$1 chain=$2 listing
  listing=$("$tool" -w -S "$chain" 2>/dev/null) || die "cannot read the $tool chain $chain; run as root"
  grep '^-A ' <<<"$listing" || true
}

# A jump must be the first rule of its parent, or a rule above it could accept what the chain refuses.
# Its other copies are deleted by number, counted after the insert.
jump_restore() {
  local tool=$1 parent match jump rules position
  local -a copies
  while read -r parent match; do
    jump="-A $parent $match -j $CHAIN"
    rules=$(chain_rules "$tool" "$parent")
    [[ ${rules%%$'\n'*} == "$jump" ]] && continue
    mapfile -t copies < <(grep -nxF -- "$jump" <<<"$rules" | cut -d: -f1 | sort -rn)
    echo "-I $parent 1 $match -j $CHAIN"
    for position in "${copies[@]}"; do echo "-D $parent $((position + 1))"; done
  done < <(jumps_for "$tool")
}

# With --noflush the chain line empties an existing chain, and the restore commits all or nothing,
# so the chain is never open or half filled.
family_restore() {
  local tool=$1 family=$2 line
  echo "*filter"
  echo ":$CHAIN - [0:0]"
  while read -r line; do echo "-A $CHAIN $line"; done < <(rules_for "$family")
  jump_restore "$tool"
  echo "COMMIT"
}

install_family() {
  local tool=$1 family=$2 payload
  payload=$(family_restore "$tool" "$family")
  if [[ $mode == dry-run ]]; then
    printf '%s\n' "$tool-restore -w --noflush <<EOF" "$payload" "EOF"
  else
    "$tool-restore" -w --noflush <<<"$payload"
  fi
}

check_family() {
  local tool=$1 family=$2 line parent jump rules expected installed failed=0
  local -a args
  if ! "$tool" -w -S "$CHAIN" >/dev/null 2>&1; then
    echo "$tool: no chain $CHAIN" >&2
    return 1
  fi
  expected=$(rules_for "$family" | wc -l)
  installed=$("$tool" -w -S "$CHAIN" | grep -c '^-A ' || true)
  if ((expected != installed)); then
    echo "$tool: $CHAIN has $installed rules, an install writes $expected" >&2
    failed=1
  fi
  while read -r line; do
    read -ra args <<<"$line"
    if ! "$tool" -w -C "$CHAIN" "${args[@]}" 2>/dev/null; then
      echo "$tool: missing in $CHAIN: $line" >&2
      failed=1
    fi
  done < <(rules_for "$family")
  while read -r parent line; do
    jump="-A $parent $line -j $CHAIN"
    rules=$(chain_rules "$tool" "$parent")
    if [[ ${rules%%$'\n'*} != "$jump" ]]; then
      echo "$tool: the first rule of $parent is not $jump" >&2
      failed=1
    fi
  done < <(jumps_for "$tool")
  return "$failed"
}

remove_family() {
  local tool=$1 line parent
  local -a args
  while read -r parent line; do
    read -ra args <<<"$line"
    while "$tool" -w -C "$parent" "${args[@]}" -j "$CHAIN" 2>/dev/null; do
      "$tool" -w -D "$parent" "${args[@]}" -j "$CHAIN"
    done
  done < <(jumps_for "$tool")
  if "$tool" -w -S "$CHAIN" >/dev/null 2>&1; then
    "$tool" -w -F "$CHAIN"
    "$tool" -w -X "$CHAIN"
  fi
}

case $mode in
  install | dry-run)
    if [[ ! $bridge ]]; then refuse_login_user; fi
    collect_allowed
    ;;
  check) collect_allowed ;;
  remove) ;;
  *) die "unknown mode $mode" ;;
esac

status=0
for family in 4 6; do
  tool=iptables
  [[ $family == 6 ]] && tool=ip6tables
  command -v "$tool" >/dev/null || die "$tool is not installed"
  case $mode in
    check) check_family "$tool" "$family" || status=1 ;;
    remove) remove_family "$tool" ;;
    *) install_family "$tool" "$family" ;;
  esac
done
if [[ $mode == check ]]; then
  ((status == 0)) && echo "egress rule installed as written"
fi
exit "$status"
