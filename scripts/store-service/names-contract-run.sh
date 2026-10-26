#!/usr/bin/env bash
# Runs the names webapp's end-to-end cases that need no payment against this service, all in one, on a
# local anvil chain deployed by the webapp's dev/deploy.ts. See the README's "Running the webapp's
# cases against the service". Arguments, if any, replace the default cases' title filter as
# Playwright test arguments.
set -euo pipefail
shopt -s inherit_errexit

WEBAPP_DIR="${WEBAPP_DIR:-$(cd "$(dirname "$0")/../.." && pwd)/apps/simplex-domains/webapp}"
: "${ENS_CONTRACTS_DIR:?set ENS_CONTRACTS_DIR to a compiled ens-contracts checkout, as for the webapp dev/start.sh}"
ANVIL="${ANVIL:-anvil}"
SERVICE_PORT="${SERVICE_PORT:-8180}"
CODES_PORT="${CODES_PORT:-8199}"
COMMUNITY_TERMS_PORT="${COMMUNITY_TERMS_PORT:-8198}"
REPO="$(cd "$(dirname "$0")/../.." && pwd)"
STORE_SERVICE_BIN="${STORE_SERVICE_BIN:-$(cd "$REPO" && cabal list-bin exe:simplex-store-service)}"

# The e2e helpers read the chain from the webapp's dev/local-chain-0.json, which slot 0 of dev/start.sh
# writes: anvil on 8545, a 5-second commitment age, and so the same contract addresses.
RPC_PORT=8545
RPC_URL="http://127.0.0.1:$RPC_PORT"
CHAIN_ID=31337
MIN_COMMITMENT_AGE=5
# as dev/start.sh: a block a second, and a few states kept in memory rather than gigabytes under ~/.foundry
MEMORY_STATES=8
SERVICE_URL="http://127.0.0.1:$SERVICE_PORT"
CODES_URL="http://127.0.0.1:$CODES_PORT"
# dev/deploy.ts sets the name policy's community terms to this file's keccak256, which the service checks as it starts
COMMUNITY_TERMS_URL="http://127.0.0.1:$COMMUNITY_TERMS_PORT/community-terms.txt"
STARTUP_SECONDS=120
CLI_TIMEOUT_SECONDS=120
# The test suite's public issuer secret (issuerSecret in tests/Bots/StoreService/ConfigTests.hs), trusted
# through [issuer] dev_keys, which the service accepts only on chain 31337. Never use it elsewhere.
DEV_ISSUER_INDEX=999
DEV_ISSUER_SECRET=Ea5wG-J2mQjPBu9YfSJRKPnGnzoIdEE-8VaMh_wY2Bg=
TERMS_VERSION=2026-10-23
TERMS_URL=https://simplex.chat/
# the community name the webapp's community cases search for
COMMUNITY_LABEL=bakers
COMMUNITY_TEXT="bakers and bakeries in Portugal"

# The cases at webapp 3b0eb5b. Each case is its own client by X-Forwarded-For, which the service trusts
# here, as the mock does. Each takes its prepaid code from POST /control/code, which the code server
# below answers from the codes issued here, so CODES holds one per case that asks for one. "search
# answers every kind of name" is not among them: it sets /control/search.
CASES=(
  "a search link's name is shown folded to lower case"
  "Back to a search from the links step shows its answer at once"
  "Check your code tells a prepaid code, a name code and anything else apart, and a code for longer names says so on the home"
  "Check your code opened again shows a prepaid code typed there, never a name code"
  "Check your code says nothing of a code being typed, and refuses one too short once Enter is pressed or focus leaves"
  "a name code the service finds no order for says so as its verdict, offers no Add, and keeps focus in the code's field"
  "a prepaid code from Check your code shows a name as Free and registers it with nothing to pay"
  "the registering page keeps its heading still on a phone from the commit to the name being registered"
  "a registration's stages are said as each is reached, not each second of the wait"
  "Edit links keeps several links on chain as the contact address they hold, and emptying the field removes them"
  "a prepaid code registers a name with its links, which its records on chain then hold"
  "a code order's name code, entered in another browser before the reveal, lets that browser reveal and register the name"
  "Edit links on a name registered with a code adds its links on chain, then changes one"
  "a code check past the service's limit counts down its wait, then checks the code again by itself"
  "a name's link whose data cannot be read now shows as its kind's default picture, and opens by the chat core's form"
  "a community name shows its marker after the name, a button whose popup says what the name is for"
  "Check your order of a community name asks for the community wording, its terms linked and its ⓘ saying what the name is for, and Pay waits for the tick"
  "a prepaid code registers a name the contracts' owner listed on chain, its register recording the owner's acceptance of the community terms"
)
CODES=("name 8" "name 6" "name 6 years 5" "name 6" "name 6" "name 6" "name 6" "name 6" "name 6" "name 6" "name 6")

RUN_DIR="$(mktemp -d "${TMPDIR:-/tmp}/names-contract-run.XXXXXX")"
INI="$RUN_DIR/store_service.ini"
DB="$RUN_DIR/db"

# Only jobs still running are stopped, so a PID reused since one ended is never killed. The run's
# folder is kept when the run fails, for its logs.
cleanup() {
  local status=$?
  local pids
  pids="$(jobs -p)"
  if [ -n "$pids" ]; then
    # shellcheck disable=SC2086 # one PID per word
    kill $pids 2>/dev/null || true
    wait 2>/dev/null || true
  fi
  if [ "$status" -eq 0 ]; then rm -rf "$RUN_DIR"; else echo "failed; logs in $RUN_DIR" >&2; fi
}
trap cleanup EXIT
trap 'exit 130' INT TERM

# Waits until the server started as PID answers the curl arguments, failing at once if it exits first.
answering() {
  local pid=$1 probe ended status=0
  shift
  curl -s -o /dev/null --retry "$STARTUP_SECONDS" --retry-delay 1 --retry-connrefused --retry-all-errors "$@" &
  probe=$!
  wait -n -p ended "$pid" "$probe" || status=$?
  if [ "$ended" != "$probe" ]; then
    echo "process $pid ended before it answered; see the logs" >&2
    exit 1
  fi
  return "$status"
}

# Another chain on the port would take the deployment, and the cases would read that chain.
refuse_taken_ports() {
  local url
  for url in "$RPC_URL" "$SERVICE_URL" "$CODES_URL" "$COMMUNITY_TERMS_URL"; do
    if curl -s -o /dev/null "$url"; then
      echo "something already answers on $url; stop it first" >&2
      exit 1
    fi
  done
}

start_chain() {
  "$ANVIL" --chain-id "$CHAIN_ID" --host 127.0.0.1 --port "$RPC_PORT" --block-time 1 --prune-history "$MEMORY_STATES" --silent \
    > "$RUN_DIR/anvil.log" 2>&1 &
  answering $! -X POST -H 'content-type: application/json' --data '{"jsonrpc":"2.0","id":1,"method":"eth_chainId"}' "$RPC_URL"
  (cd "$WEBAPP_DIR" && RPC_URL="$RPC_URL" LOCAL_CHAIN_OUT="$RUN_DIR/chain.json" MIN_COMMITMENT_AGE="$MIN_COMMITMENT_AGE" \
    ENS_CONTRACTS_DIR="$ENS_CONTRACTS_DIR" node dev/deploy.ts) > "$RUN_DIR/deploy.log" 2>&1
  if ! cmp -s "$RUN_DIR/chain.json" "$WEBAPP_DIR/dev/local-chain-0.json"; then
    echo "the deployment differs from $WEBAPP_DIR/dev/local-chain-0.json, which the e2e helpers read; run its dev/start.sh with SLOTS=1 MIN_COMMITMENT_AGE=$MIN_COMMITMENT_AGE once" >&2
    exit 1
  fi
}

start_terms_server() {
  mkdir -p "$RUN_DIR/terms"
  cp "$WEBAPP_DIR/dev/community-terms.txt" "$RUN_DIR/terms/"
  python3 -m http.server --bind 127.0.0.1 --directory "$RUN_DIR/terms" "$COMMUNITY_TERMS_PORT" > "$RUN_DIR/terms.log" 2>&1 &
  answering $! "$COMMUNITY_TERMS_URL"
}

build_site() {
  (cd "$WEBAPP_DIR" && npx vite build --outDir "$RUN_DIR/names" --emptyOutDir --logLevel warn) > "$RUN_DIR/build.log" 2>&1
}

# No [btcpay] and no [stripe]: these cases pay for nothing. The names site is served on the host the
# tests open; the badges site, on any other host, is not used.
write_ini() {
  local chain="$RUN_DIR/chain.json"
  cat > "$INI" <<INI
[listener]
host = 127.0.0.1
port = $SERVICE_PORT
static_dir = $REPO/apps/simplex-store-service/web/dist
serve_webapp = on
names_static_dir = $RUN_DIR/names
names_hosts = 127.0.0.1
trust_forwarded_for = on

[issuer]
index = $DEV_ISSUER_INDEX
private_key = $DEV_ISSUER_SECRET
dev_keys = $DEV_ISSUER_INDEX:$DEV_ISSUER_SECRET

[names]
tld = $(jq -r .tld "$chain")
terms_version = $TERMS_VERSION
terms_url = $TERMS_URL
community_terms_url = $COMMUNITY_TERMS_URL

[community]
$COMMUNITY_LABEL = $COMMUNITY_TEXT

[chain]
rpc_url = $RPC_URL
chain_id = $(jq -r .chainId "$chain")
controller = $(jq -r .controller "$chain")
sponsor = $(jq -r .sponsor "$chain")
base_registrar = $(jq -r .baseRegistrar "$chain")
resolver = $(jq -r .resolver "$chain")
service_key = $(jq -r .registrarKey "$chain")
INI
  chmod 600 "$INI"
}

start_service() {
  "$STORE_SERVICE_BIN" --no-address --service-config "$INI" -d "$DB" >> "$RUN_DIR/service.log" 2>&1 &
  SERVICE_PID=$!
  answering "$SERVICE_PID" "$SERVICE_URL/"
}

# The console started on a new database leaves its terminal in line mode and reads no keys, so the
# service's first start makes the database.
make_database() {
  start_service
  kill "$SERVICE_PID"
  wait "$SERVICE_PID" || true
}

# //issue in --run-cli, with the service stopped, as the README says. The console reads a terminal, so
# each command is typed into a pseudo-terminal, and its "Code: ..." answer read.
issue_codes() {
  python3 -I - "$CLI_TIMEOUT_SECONDS" "$RUN_DIR/codes.json" "${CODES[@]}" -- \
    "$STORE_SERVICE_BIN" --run-cli --no-address --service-config "$INI" -d "$DB" > "$RUN_DIR/issue.log" 2>&1 <<'PY'
import fcntl, json, os, pty, re, select, signal, struct, sys, termios, time

args = sys.argv[1:]
timeout, out = float(args[0]), args[1]
split = args.index("--")
commands, cli = args[2:split], args[split + 1:]
ANSI = re.compile(rb"\x1b\[[0-9;?]*[A-Za-z]")
CODE = re.compile(rb"Code: (SN(\d+)(?:-[0-9A-Z]+)+)")
REFUSED = re.compile(rb"bad chat command|chat error|[Ee]rror:")

pid, fd = pty.fork()
if pid == 0:
    os.execvp(cli[0], cli)
# the console's terminal library divides by the window's size
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 50, 200, 0, 0))
seen = b""

def fail(message):
    os.kill(pid, signal.SIGTERM)
    sys.exit(message)

def expect(pattern):
    global seen
    deadline = time.monotonic() + timeout
    while True:
        text = ANSI.sub(b"", seen)
        if (refused := REFUSED.search(text)):
            fail(f"refused: {text[refused.start():].splitlines()[0].decode(errors='replace')}")
        if (found := pattern.search(text)):
            seen = text[found.end():]
            return found
        left = deadline - time.monotonic()
        if left <= 0 or not select.select([fd], [], [], left)[0]:
            fail(f"no {pattern.pattern!r} within {timeout} s")
        try:
            chunk = os.read(fd, 65536)
        except OSError:
            chunk = b""
        if not chunk:
            fail(f"the console ended before {pattern.pattern!r}")
        sys.stdout.buffer.write(chunk)
        seen += chunk

# The console loses keys sent in a burst, so each key waits for the echo of the one before.
def type_line(line):
    for typed in range(1, len(line) + 1):
        os.write(fd, line[typed - 1].encode())
        expect(re.compile(b"> " + re.escape(line[:typed].encode())))
    os.write(fd, b"\r")

# the empty input line, which the welcome text's "> " lines are not
expect(re.compile(rb"Current user: .*\n(?:.*\n)*?.*>  "))
codes = []
for command in commands:
    type_line(f"//issue {command}")
    found = expect(CODE)
    years = re.search(r"years (\d+)", command)
    codes.append({"code": found[1].decode(), "minLength": int(found[2]), "years": int(years[1]) if years else 2})
type_line("/quit")
os.waitpid(pid, 0)
with open(out, "w") as f:
    json.dump(codes, f)
PY
}

# The tests' MOCK_URL. POST /control/code hands out the issued codes; the reset of /control/search
# that search.spec.ts sends after each case is the service's own state, so it is answered as done.
# Any other control answers 501, failing the case that sends it. /api is the service's.
start_code_server() {
  node --input-type=module - "$CODES_PORT" "$SERVICE_URL" "$RUN_DIR/codes.json" > "$RUN_DIR/codes.log" 2>&1 <<'JS' &
import { createServer } from "node:http"
import { readFileSync } from "node:fs"

const [port, service, codesFile] = process.argv.slice(2)
const codes = JSON.parse(readFileSync(codesFile, "utf8"))
const SEARCH_RESET = { chainDown: false, limitSeconds: 0, invoiceError: null, findLimitSeconds: 0 }
const sameJson = (a, b) => JSON.stringify(Object.entries(a).sort()) === JSON.stringify(Object.entries(b).sort())

async function bodyOf(req) {
  const chunks = []
  for await (const chunk of req) chunks.push(chunk)
  return Buffer.concat(chunks)
}

function json(res, status, value) {
  res.writeHead(status, { "content-type": "application/json" }).end(JSON.stringify(value))
}

function control(path, body) {
  if (path === "/control/code" && body.state === undefined) {
    const years = body.years ?? 2
    const at = codes.findIndex((c) => c.minLength === body.minLength && c.years === years)
    if (at < 0) return [501, { error: `no issued code left of ${body.minLength}+ letters for ${years} years` }]
    return [200, { code: codes.splice(at, 1)[0].code }]
  }
  if (path === "/control/search" && sameJson(body, SEARCH_RESET)) return [200, {}]
  return [501, { error: `${path} ${JSON.stringify(body)} is the mock's control, which the service has not` }]
}

// fetch has decoded and measured the body itself, so these headers no longer describe it
const NOT_PASSED = new Set(["content-encoding", "content-length", "transfer-encoding", "connection"])

// the case's client, so a case's own requests and its page's share the service's rate limits
const forwardedFor = (req) => (req.headers["x-forwarded-for"] ? { "x-forwarded-for": req.headers["x-forwarded-for"] } : {})

async function proxy(req, res, body) {
  const answer = await fetch(`${service}${req.url}`, {
    method: req.method,
    headers: { "content-type": req.headers["content-type"] ?? "application/json", ...forwardedFor(req) },
    body: req.method === "GET" ? undefined : body,
  })
  const headers = Object.fromEntries([...answer.headers].filter(([name]) => !NOT_PASSED.has(name)))
  res.writeHead(answer.status, headers).end(Buffer.from(await answer.arrayBuffer()))
}

async function serve(req, res) {
  const body = await bodyOf(req)
  if (req.url.startsWith("/control/")) {
    let parsed
    try {
      parsed = body.length ? JSON.parse(body) : {}
    } catch {
      return json(res, 400, { error: "bad_request" })
    }
    const [status, answer] = control(req.url, parsed)
    console.log(req.url, status, JSON.stringify(answer))
    return json(res, status, answer)
  }
  if (!req.url.startsWith("/api/")) return json(res, 404, { error: "not_found" })
  await proxy(req, res, body)
}

createServer((req, res) => {
  serve(req, res).catch((e) => {
    console.error(req.url, e)
    if (!res.headersSent) json(res, 502, { error: "the service did not answer" })
    else res.destroy()
  })
}).listen(Number(port), "127.0.0.1")
JS
  answering $! -X POST "$CODES_URL/control/none"
}

escape_regex() {
  sed 's/[][\\.*+?^${}()|/]/\\&/g' <<< "$1"
}

run_cases() {
  local filter=()
  if [ "$#" -gt 0 ]; then
    filter=("$@")
  else
    local titles=()
    for title in "${CASES[@]}"; do titles+=("$(escape_regex "$title")"); done
    # Playwright matches the project, the file and the title, joined by spaces
    filter=(--grep " ($(IFS='|'; echo "${titles[*]}"))\$")
  fi
  (cd "$WEBAPP_DIR" && BASE_URL="$SERVICE_URL" MOCK_URL="$CODES_URL" \
    npx playwright test --project=chromium --output "$RUN_DIR/test-results" "${filter[@]}")
}

refuse_taken_ports
start_chain
start_terms_server
build_site
write_ini
make_database
issue_codes
start_service
start_code_server
run_cases "$@"
