#!/usr/bin/env bash
# Runs the names webapp's end-to-end cases against this service, with the fake BTCPay and Stripe of its
# tests, on a local anvil chain deployed by the webapp's dev/deploy.ts. See the README's "Running the
# webapp's cases against the service". Arguments, if any, replace the default cases' title filter as
# Playwright test arguments.
set -euo pipefail
shopt -s inherit_errexit

WEBAPP_DIR="${WEBAPP_DIR:-$(cd "$(dirname "$0")/../../.." && pwd)/apps/simplex-domains/webapp}"
: "${ENS_CONTRACTS_DIR:?set ENS_CONTRACTS_DIR to the compiled ens-contracts submodule of a simplex-namespace-contract checkout, as for the webapp dev/start.sh}"
ANVIL="${ANVIL:-anvil}"
RPC_PORT="${RPC_PORT:-8545}"
SERVICE_PORT="${SERVICE_PORT:-8180}"
BADGES_PORT="${BADGES_PORT:-8181}"
RUNNER_PORT="${RUNNER_PORT:-8182}"
REPO="$(cd "$(dirname "$0")/../../.." && pwd)"
STORE_TEST_BIN="${STORE_TEST_BIN:-$(cd "$REPO" && cabal list-bin simplex-chat-test)}"

RPC_URL="http://127.0.0.1:$RPC_PORT"
CHAIN_ID=31337
MIN_COMMITMENT_AGE=1
# as dev/start.sh: a block a second, and a few states kept in memory rather than gigabytes under ~/.foundry
MEMORY_STATES=8
SERVICE_URL="http://127.0.0.1:$SERVICE_PORT"
BADGES_URL="http://127.0.0.1:$BADGES_PORT"
RUNNER_URL="http://127.0.0.1:$RUNNER_PORT"
MOCK_URL="http://127.0.0.1:8099"
PAGE_URL="http://127.0.0.1:5173"
TERMS_URL="http://127.0.0.1:8098"
STARTUP_SECONDS=120
# The test suite's public issuer secret (issuerSecret in tests/Bots/StoreService/ConfigTests.hs), trusted
# through [issuer] dev_keys, which the service accepts only on chain 31337. Never use it elsewhere.
DEV_ISSUER_INDEX=999
DEV_ISSUER_SECRET=Ea5wG-J2mQjPBu9YfSJRKPnGnzoIdEE-8VaMh_wY2Bg=
# the community name the webapp's community cases search for
COMMUNITY_LABEL=bakers
COMMUNITY_TEXT="bakers and bakeries in Portugal"

EXCLUDED=()

RUN_DIR="$(mktemp -d "${TMPDIR:-/tmp}/names-contract-run.XXXXXX")"
INI="$RUN_DIR/store_service.ini"
CHAIN="$WEBAPP_DIR/dev/local-chain-0.json"

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
  for url in "$RPC_URL" "$SERVICE_URL" "$BADGES_URL" "$RUNNER_URL" "$MOCK_URL" "$PAGE_URL" "$TERMS_URL"; do
    if curl -s -o /dev/null "$url"; then
      echo "something already answers on $url; stop it first" >&2
      exit 1
    fi
  done
}

start_chain() {
  "$ANVIL" --chain-id "$CHAIN_ID" --host 127.0.0.1 --port "$RPC_PORT" --block-time 1 --mixed-mining --prune-history "$MEMORY_STATES" --silent \
    > "$RUN_DIR/anvil.log" 2>&1 &
  answering $! -X POST -H 'content-type: application/json' --data '{"jsonrpc":"2.0","id":1,"method":"eth_chainId"}' "$RPC_URL"
  (cd "$WEBAPP_DIR" && RPC_URL="$RPC_URL" LOCAL_CHAIN_OUT="$CHAIN" MIN_COMMITMENT_AGE="$MIN_COMMITMENT_AGE" \
    ENS_CONTRACTS_DIR="$ENS_CONTRACTS_DIR" node dev/deploy.ts) > "$RUN_DIR/deploy.log" 2>&1
}

start_terms() {
  (cd "$WEBAPP_DIR" && node dev/serve-terms.ts) > "$RUN_DIR/terms.log" 2>&1 &
  answering $! "$TERMS_URL/terms/"
}

build_site() {
  (cd "$WEBAPP_DIR" && npx vite build --outDir "$RUN_DIR/names" --emptyOutDir --logLevel warn) > "$RUN_DIR/build.log" 2>&1
}

deployed() {
  node -p "require(process.argv[1]).$1" "$CHAIN"
}

write_ini() {
  mkdir -p "$RUN_DIR/badges"
  cat > "$INI" <<INI
[listener]
host = 127.0.0.1
badges_port = $BADGES_PORT
badges_static_dir = $RUN_DIR/badges
serve_webapp = on
names_static_dir = $RUN_DIR/names
names_port = $SERVICE_PORT
trust_forwarded_for = on

[issuer]
index = $DEV_ISSUER_INDEX
private_key = $DEV_ISSUER_SECRET
dev_keys = $DEV_ISSUER_INDEX:$DEV_ISSUER_SECRET

[names]
tld = $(deployed tld)

[community]
$COMMUNITY_LABEL = $COMMUNITY_TEXT

[chain]
rpc_url = $RPC_URL
chain_id = $(deployed chainId)
controller = $(deployed controller)
sponsor = $(deployed sponsor)
base_registrar = $(deployed baseRegistrar)
resolver = $(deployed resolver)
service_key = $(deployed registrarKey)
confirmations = 1
INI
  chmod 600 "$INI"
}

start_service() {
  (cd "$REPO" && NAMES_PAGE_RUN_DIR="$RUN_DIR" NAMES_PAGE_CONTROL_PORT="$RUNNER_PORT" \
    exec "$STORE_TEST_BIN" -m "/Names page against the store service/") >> "$RUN_DIR/service.log" 2>&1 &
  SERVICE_PID=$!
  answering "$SERVICE_PID" "$SERVICE_URL/"
  answering "$SERVICE_PID" -X POST "$RUNNER_URL/control/none"
}

start_mock() {
  (cd "$WEBAPP_DIR" && ENS_CONTRACTS_DIR="$ENS_CONTRACTS_DIR" exec node --input-type=module - "${MOCK_URL##*:}" "$SERVICE_URL" "$RUNNER_URL" "$CHAIN" "$RUN_DIR/rival-chain.json") \
    > "$RUN_DIR/mock.log" 2>&1 <<'JS' &
import { createServer } from "node:http"
import { readFileSync, writeFileSync } from "node:fs"
import { keccak256, toHex } from "viem"
import { mnemonicToAccount } from "viem/accounts"

const [port, service, runner, chainFile, rivalChainFile] = process.argv.slice(2)
const ANVIL_MNEMONIC = "test test test test test test test test test test test junk"
const [guardian, rival] = [1, 4].map((addressIndex) => mnemonicToAccount(ANVIL_MNEMONIC, { addressIndex }))
const ALLOWANCE = 1_000_000n * 10n ** 18n
const TAKE_SECONDS = 60
const SEARCH_RESET = { chainDown: false, limitSeconds: 0, invoiceError: null, findLimitSeconds: 0 }
const RUNNER_CONTROL = /^\/control\/(code|rate|(settle|partial|confirming|expire)\/[^/]+)$/

writeFileSync(rivalChainFile, JSON.stringify({ ...JSON.parse(readFileSync(chainFile, "utf8")), registrarKey: toHex(rival.getHdKey().privateKey) }))
process.env.LOCAL_CHAIN_OUT = rivalChainFile
const { controlCommunityTerms, controlGeneralTerms, controlTake, HttpError } = await import("./dev/mock/service.ts")
const { holder, ownerOf, send, sponsor } = await import("./dev/mock/chain.ts")
await send(sponsor, "setRegistrarAllowance", [rival.address, ALLOWANCE], undefined, guardian)

const sameJson = (a, b) => JSON.stringify(Object.entries(a).sort()) === JSON.stringify(Object.entries(b).sort())
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms))

async function taken(label) {
  const hash = keccak256(toHex(label))
  for (let left = TAKE_SECONDS * 4; left > 0; left--) {
    if ((await ownerOf(hash)) === holder.address) return
    await sleep(250)
  }
  throw new Error(`${label} was not registered to the holder`)
}

const CHAIN_CONTROLS = {
  "/control/take": async (body) => {
    const answer = controlTake(body.label)
    await taken(body.label)
    return answer
  },
  "/control/general-terms": controlGeneralTerms,
  "/control/community-terms": controlCommunityTerms,
}

async function bodyOf(req) {
  const chunks = []
  for await (const chunk of req) chunks.push(chunk)
  return Buffer.concat(chunks)
}

function json(res, status, value) {
  res.writeHead(status, { "content-type": "application/json" }).end(JSON.stringify(value))
}

async function control(path, body) {
  if (RUNNER_CONTROL.test(path)) {
    const answer = await fetch(`${runner}${path}`, { method: "POST", headers: { "content-type": "application/json" }, body: JSON.stringify(body) })
    return [answer.status, await answer.json()]
  }
  if (path === "/control/search" && sameJson(body, SEARCH_RESET)) return [200, {}]
  const chainControl = CHAIN_CONTROLS[path]
  if (!chainControl) return [501, { error: `${path} ${JSON.stringify(body)} is the mock's control, which the service has not` }]
  try {
    return [200, await chainControl(body)]
  } catch (e) {
    if (e instanceof HttpError) return [e.status, e.body]
    throw e
  }
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
  if (req.url === "/api/config") return json(res, answer.status, { ...(await answer.json()), stripePublishableKey: null })
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
    const [status, answer] = await control(req.url, parsed)
    console.log(req.url, status, JSON.stringify(answer))
    return json(res, status, answer)
  }
  if (!req.url.startsWith("/api/")) return json(res, 404, { error: "not_found" })
  await proxy(req, res, body)
}

createServer((req, res) => {
  serve(req, res).catch((e) => {
    console.error(req.url, e)
    if (!res.headersSent) json(res, 502, { error: String(e) })
    else res.destroy()
  })
}).listen(Number(port), "127.0.0.1")
JS
  answering $! -X POST "$MOCK_URL/control/none"
}

escape_regex() {
  sed 's/[][\\.*+?^${}()|/]/\\&/g' <<< "$1"
}

run_cases() {
  if [ "$#" -gt 0 ]; then
    (cd "$WEBAPP_DIR" && MOCK_URL="$MOCK_URL" SLOTS=1 npx playwright test --project=chromium --output "$RUN_DIR/test-results" "$@")
    return
  fi
  local titles=() i
  for ((i = 0; i < ${#EXCLUDED[@]}; i += 2)); do titles+=("$(escape_regex "${EXCLUDED[i]}")"); done
  local filter=()
  # Playwright matches the project, the file and the title, joined by spaces
  if [ "${#titles[@]}" -gt 0 ]; then filter=(--grep-invert " ($(IFS='|'; echo "${titles[*]}"))\$"); fi
  (cd "$WEBAPP_DIR" && MOCK_URL="$MOCK_URL" SLOTS=1 npx playwright test --project=chromium --output "$RUN_DIR/test-results" "${filter[@]}")
  (cd "$WEBAPP_DIR" && BASE_URL="$SERVICE_URL" MOCK_URL="$MOCK_URL" npx playwright test --project=chromium --output "$RUN_DIR/build-results" "${filter[@]}" e2e/offline.spec.ts)
}

refuse_taken_ports
start_chain
start_terms
build_site
write_ini
start_service
start_mock
run_cases "$@"
