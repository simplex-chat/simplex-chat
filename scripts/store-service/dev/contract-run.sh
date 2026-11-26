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
MIN_COMMITMENT_AGE=5
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

STATE="reads the mock's memory at /control/state or /control/mode"
MODE="switches the mock to codes only at /control/mode; the service reads codes_only from its ini at start"
SEARCH="injects a fault with /control/search: a chain down, a rate limit, a provider's refusal, an unsent transaction, an unread chain, a slow write or link read, or refused links"
STALE="drops an order's signed reveal with /control/stale, which only the mock does"
FAIL="fails an order's next commit or register with /control/fail, which only the mock does"
COMMUNITY="lists a community name with what it is for at /control/community; the service reads that text from its ini at start"
OVERDUE="ends an invoice's rate window at once with /control/overdue; the service's clock ends it"
LATE="holds a late payment's credit with /control/late; the service credits it on its own"
CREDIT="adds money no payment brought to a balance with /control/credit"
EDITS="sets the yearly limit of link edits or a name's counted edits at /control/edits; the service reads the limit from its ini and counts the edits it relayed"
CODE_STATE="sets a prepaid code's state with /control/code/state, or issues one used, reserved, expired or unpaid at /control/code, which only the mock does"
REFUND_ANSWER="compares the answer of /control/refund, which the mock defines; the service's refund is the operator's //record_refund"
LIMIT="sends more checkouts or cancels than the service takes from one client in a minute, which the mock does not limit"
AMOUNTS="expects the mock's coin amounts, a price at the invoice's rate, or its decimal text; the fake BTCPay answers its fixtures' amounts, written to 8 places as BTCPay writes them"
PROFILES="expects the profiles the mock makes up for its links; the service reads a link's profile from the link's server, which these links are not on"
MONERO="pays late in Monero, which the service credits only when BTCPay's Monero settled-confirmation threshold is 1 or more; the fake BTCPay's Monero fixture sets none"
UNREGISTER="ends a registration with /control/unregister, which only the mock does; on chain, unregister takes the cold key"
HELD="reads an order as the mock holds it at /control/invoice or /control/registration, which only the mock has"
MISMATCH="makes the mock answer other values than the page holds with /control/mismatch, which only the mock does"
DOWNTIME="switches the mock's downtime at /control/downtime; the service reads downtime from its ini at start"

EXCLUDED=(
  "the service's mode is at /api/config: open by default, codes once the mock takes codes only, the page the same in both" "$STATE"
  "the mock refuses every payment in codes-only mode, as the service does" "$MODE"
  "each mode's home is drawn from its first frame, never the other mode's first, however slowly the config answers" "$MODE"
  "without a code, the home's search is greyed, takes no pointer, focus or Enter, and Register with code leads on" "$MODE"
  "#/code opens Check your code from a cold load, its field ready for the code" "$MODE"
  "a valid prepaid code opens the search for the names it covers and registers one as a free code order" "$MODE"
  "a code taken off on Check your order leaves the code field in place of payment, and a code there registers" "$MODE"
  "an order paid before codes only shows no new invoice or other name, only Register with code and the refund" "$MODE"
  "a page opened before codes only, refused as it pays, says registration is by code and leads to a code" "$MODE"
  "a closed invoice's page opened before codes only, refused a new invoice, says so with a way to a code" "$MODE"
  "Edit links saves both links with the last edit left, then keeps Save off until the edits return" "$EDITS"
  "a Save of both links from a tab opened before the name's edits ran out says when they return, and changes neither link" "$EDITS"
  "a code on Check your order that is not valid, used, too short for the name or a name code says why, and the order is paid once it is removed" "$CODE_STATE"
  "Save pressed in Edit links while a link is still checked waits for its check, then saves it" "$SEARCH"
  "a failed code order opened again after its code was used elsewhere offers no other name, and from the first frame the time after" "$FAIL"
  "a failed code order is drawn before its code is checked, and says the code is kept only once the check answers" "$FAIL"
  "a failed code order whose code check meets the rate limit counts down its wait, then says the code is kept" "$FAIL"
  "a failed code order whose code is revoked or past its date while still held by it says the code is kept" "$FAIL"
  "an order restored in a browser without its code promises nothing of the code" "$FAIL"
  "a view whose sponsor nonce could not be read is read again after 5 then 15 seconds, without holding, and then signed" "$SEARCH"
  "a name whose terms can't be read now can't be ordered: its search says so, and an order sent meanwhile is refused with nothing charged" "$STATE"
  "a name put on the list meanwhile is refused for the community terms, which their step then asks for, the review showing its marker, and Your names marks the name" "$COMMUNITY"
  "Your names draws a community name's row with its marker on the name's last line, its popup saying what the name is for" "$COMMUNITY"
  "community terms changed on chain between the order and its reveal are agreed to again on the order's page before it signs, and the name registers under them" "$COMMUNITY"
  "changed community terms whose searches fail for now are asked for once a search is made again after a pause" "$COMMUNITY"
  "changed community terms of the name an order holds are asked for whatever a search answers for the name" "$COMMUNITY"
  "community terms changed after the reveal, before its relay, are agreed to again and the name registers under them" "$SEARCH"
  "the marker stands where the mockup draws it in the search's rows, available and taken" "$COMMUNITY"
  "the marker stands where the mockup draws it on Check your order" "$COMMUNITY"
  "the marker stands where the mockup draws it in Your names" "$COMMUNITY"
  "a name selected for a taken order asks for that name's terms first, a community name's on their own step in place of the general terms' box, and signs them anew after they changed" "$COMMUNITY"
  "an order is answered again only for an exact resend, at any age of its signature; anything else meeting it is order_exists" "$LIMIT"
  "an order refuses a wrong signer, a stale timestamp, free days off the term, a missing or oversized sealed, and a price the chain does not quote" "$LIMIT"
  "an order past its invoice's expiry is closed by the next order of its wallet and name only while nothing has reached it" "$OVERDUE"
  "a code order closes an overdue invoice of its wallet and name with nothing received" "$OVERDUE"
  "a code check answers every state, revoked included, and an unknown code is code_invalid" "$CODE_STATE"
  "a taken code order's own code is accepted past its expiry within the month after the take, and refused once revoked" "$CODE_STATE"
  "a name is selected only for a paid order taken first, at its own price, again after each take with no limit; a repeat changes nothing" "$STALE"
  "support's refund takes the whole balance by invoice id or registration id, and refuses other cents or none left" "$REFUND_ANSWER"
  "a paid order taken first keeps its whole price as its balance until support refunds it, after which no name is selected for it" "$REFUND_ANSWER"
  "a late payment is credited once settled, each confirmed figure once, at the lower of its invoice's rate and the current one" "$AMOUNTS"
  "a part payment confirmed on a closed invoice, also after a cancel, is credited at its share of the price, and the rest once it arrives" "$AMOUNTS"
  "an overpaid invoice registers its name, leaves nothing to pay, and credits the extra at its share of the price or the lower current rate" "$AMOUNTS"
  "find lists the 50 newest orders of a code and then every older one holding a balance" "$LIMIT"
  "late money says what its credit waits on" "$LATE"
  "an order signs the terms of its name's kind: the other kind or other terms is terms_changed with the terms and community now, a malformed one bad_request" "$LIMIT"
  "a selection signs the new name's terms: a community name needs the community terms, which are stored; the same body again is a repeat, also after the terms changed" "$COMMUNITY"
  "a code order meets changed terms before the code's state" "$CODE_STATE"
  "a plain name's committed view asks for the owner's acceptance of the general terms; its reveal without one or with the other kind is bad_request, and with it the register records it" "$STALE"
  "a Bitcoin order pays in two parts, registers on chain, takes its links on Edit links, and the code owns it" "$EDITS"
  "a purchase walks the steps a buyer expects, and the links set before paying are on chain" "$PROFILES"
  "a link's late preview never fills the field once it holds another link, whose own preview it waits for" "$SEARCH"
  "a link's profile comes in the very box its place held while it was read, at a laptop's and a phone's width" "$SEARCH"
  "Continue pressed while a link is checked is busy where it stands, says so, and goes on once the check ends" "$SEARCH"
  "the owner code restores an order in another browser, and a bad code is refused" "$STALE"
  "a part payment on a closed invoice, once credited, says so and gets an invoice for the whole price of the same name in the main button's place" "$AMOUNTS"
  "an invoice past its rate window that part of the payment reached stays open, asks for no more, and registers once paid" "$OVERDUE"
  "a browser restored before another browser made a new invoice carries on with that invoice" "$STATE"
  "a forgotten order whose code's lookup is rate limited waits as asked, then answers" "$SEARCH"
  "a code order's selection refused for its code says why in that refusal's words" "$CODE_STATE"
  "money paid after the close that the service cannot credit by itself sends the buyer to support, and the page stops reading it" "$LATE"
  "an order left before its wait ends asks for this browser in Your names" "$STATE"
  "Find the order shows a wait met elsewhere in the tab" "$SEARCH"
  "search answers every kind of name" "$SEARCH"
  "the checkout explains every refusal and keeps the order" "$SEARCH"
  "a new invoice for a method that is unavailable says so and offers another way, with the same name code" "$SEARCH"
  "a name code's lookup refused for a rate limit counts down its wait, and the code is judged again once it is over" "$SEARCH"
  "a search pause reads the same after leaving the page and coming back" "$SEARCH"
  "Back to a search whose check fails because the service is down no longer offers the remembered answer" "$SEARCH"
  "Back to a search whose check fails because searching is rate limited no longer offers the remembered answer" "$SEARCH"
  "as a search pause ends, the paused line gives way to a checking line rather than an empty room, until the answer" "$SEARCH"
  "a name an order holds is searched as that order in its browser, and as taken with nothing more in another" "$FAIL"
  "a failed registration keeps its payment for support, also in another tab" "$FAIL"
  "an order whose commitment lapses before its page signs is committed again by that page, which signs it then" "$STATE"
  "an order failed while its page was not read says until when the name stays its, and Continue signs it again and registers it" "$STATE"
  "a chain that sends nothing for a moment keeps the order where it was, then registers it" "$STATE"
  "the home keeps its headline and search still as each kind of answer comes, at a laptop's and a phone's size" "$SEARCH"
  "the words under the registering page's clock are centred on its icon" "$SEARCH"
  "a commit that does not move on for longer than usual says so in its words' place, and nothing moves" "$SEARCH"
  "a card invoice opened in place of a new Bitcoin one says why in the card page's one notice, above the form" "$LIMIT"
  "a whole payment after the close, once credited, says so and offers a new invoice beside the refund" "$MONERO"
  "a registration ended in its dispute period is shown with Register with code alone, as the other endings are" "$MODE"
  "a registration ended in its dispute period is shown as ended on its order's page, in Your names and in Edit links, and its name is available again" "$UNREGISTER"
  "a reveal signed over other dispute days than its order's payment's is bad_request, and the name is registered with the reveal over its payment's" "$STALE"
  "a load asks the service's config once, as the preload's answer is the one the page reads" "$MODE"
  "a prepaid code from Check your code shows a name as Free and registers it with nothing to pay" "$HELD"
  "a prepaid code registers a name with its links, which its records on chain then hold, its register recording the owner's acceptance of the general terms" "$HELD"
  "a prepaid code registers a name the contracts' owner listed on chain, its register recording the owner's acceptance of the community terms" "$HELD"
  "a prepaid code for 5 years registers a community name for its 2 years and the free days, saying the code's other years are not used" "$HELD"
  "a code held by this browser's failed order is noted as held by it, with a link to the order; one another order holds is refused; a revoked code says SimpleX withdrew it" "$FAIL"
  "a code applied to the tab is taken off once Your names or Check your code reads it as one that cannot be used" "$CODE_STATE"
  "an order refused for terms changed meanwhile unticks the box and says so in place, the box and Pay staying put, and signs the new terms once ticked" "$HELD"
  "a new invoice for the same name signs the order's own terms without asking while they are the name's, also restored elsewhere, and asks first once they changed" "$HELD"
  "a card form that did not load asks for changed terms before cancelling its invoice, which stays open until the buyer agrees" "$HELD"
  "changed community terms are asked for by the hash the order's view asks to accept, as the page holds them, whatever the service answers for that hash" "$COMMUNITY"
  "a held poll wakes on a new version, and of two retries at once of a lapsed order one wins and the other is bad_request, as the order is no longer lapsed" "$STALE"
  "a committed community name's view asks for the owner's acceptance of the community terms on chain; its reveal without one, or with one for another nonce, terms or kind, is bad_request, and with it the register records it" "$COMMUNITY"
  "a reveal without its order request signed by the order's name key, or by another key, is not_found, one signed too long ago bad_request, and the signed one registers the name" "$STALE"
  "a name whose search answers a price below or above the one the page holds can't be ordered, and shows the held price, unless a code registers it" "$MISMATCH"
  "an order whose sign answer asks for the acceptance under another policy is not sent, and Check your order says names can't be ordered now" "$MISMATCH"
  "an order whose sign answer asks for the acceptance of other terms is not sent, and Check your order says names can't be ordered now" "$MISMATCH"
  "an open invoice of another payment than its order asked for is drawn without its payment details, which could not be checked" "$MISMATCH"
  "an open Bitcoin invoice with an address of a key the page does not hold is drawn without its payment details, which could not be checked" "$MISMATCH"
  "an open Monero invoice with an address of a key the page does not hold is drawn without its payment details, which could not be checked" "$MISMATCH"
  "an open Bitcoin invoice with an amount other than its name's price is drawn without its payment details, which could not be checked" "$MISMATCH"
  "an open Monero invoice with an amount other than its name's price is drawn without its payment details, which could not be checked" "$MISMATCH"
  "an order's page gives support its address proof with its reference, as the service sent it, also when the address could not be checked" "$MISMATCH"
  "a card order reopened with its name code in another browser signs its reveal over a card's dispute days, whatever payment the service reports" "$MISMATCH"
  "a committed order whose view asks for the acceptance under another policy is not signed, and after three tries its page says the name could not be signed" "$HELD"
  "a committed order whose reveal the service answers as another signer's, not_found, says after three tries that the name could not be signed" "$HELD"
  "in the service's downtime the home says new orders open again soon and offers no Register, and Check your order says so, with no order made" "$DOWNTIME"
  "in the service's downtime a taken order's search for another name offers no Register, and the order stays taken" "$DOWNTIME"
  "an open invoice's page in the service's downtime withholds its payment details, and shows them once the downtime is over" "$DOWNTIME"
  "a committed order signs no reveal in the service's downtime, and is signed and registered once the downtime is over" "$HELD"
  "Back is in the window and reached by the keyboard on every screen that has one, at a laptop's and a phone's size" "$HELD"
  "a Monero invoice's wallet link is monero:// for Monero GUI on a laptop and monero: on a phone, and its QR keeps monero:" "$HELD"
  "a forgotten empty invoice never leads back to the older invoice it replaced" "$HELD"
  "a card form that does not load says so and offers a new invoice" "$HELD"
  "a registration link of an order paid by invoice points at its payment link" "$HELD"
  "a selection refused for a name taken meanwhile says the name is no longer available, and the next search is for the same order" "$HELD"
  "a drawn order that a newer order of its name replaces keeps its place until the newer one is read, and the end links stay" "$HELD"
  "every request for an order is signed with its owner's name key, polls, cancels and the reads to the name registered" "$STATE"
  "an order's link in a browser without its name code sends no request for the order and asks for the code" "$HELD"
  "a name taken first keeps its payment as its balance, with which a name selected for the order is registered, owned by the same code" "$HELD"
  "a taken order offers another name for it without searching its own name" "$HELD"
  "an order failed unread offers Continue only outside the service's downtime, asks at Continue for terms changed since, and signs and registers it once they are accepted" "$HELD"
  "a name of another price than a taken order's cannot be registered for it, and its search says the price the payment covers" "$HELD"
  "Edit links keeps still and fits both windows as it saves, for a name of 63 wide letters, and Check your code fits them refusing a code" "$HELD"
)

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
  if [ "$status" -eq 0 ]; then
    rm -rf "$RUN_DIR"
  else
    tail -n 100 "$RUN_DIR"/*.log >&2 || true
    echo "failed; logs in $RUN_DIR" >&2
  fi
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
  (cd "$WEBAPP_DIR" && exec node dev/serve-terms.ts) > "$RUN_DIR/terms.log" 2>&1 &
  answering $! "$TERMS_URL/terms/"
}

build_site() {
  (cd "$WEBAPP_DIR" && npx vite build --mode devchain --outDir "$RUN_DIR/names" --emptyOutDir --logLevel warn) > "$RUN_DIR/build.log" 2>&1
}

deployed() {
  node -p "require(process.argv[1]).$1" "$CHAIN"
}

dev_key() {
  node -p "require(process.argv[1]).$1" "$REPO/tests/fixtures/store-service/rest-api/vectors-address-proofs.json"
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

[btcpay]
host = https://127.0.0.1
api_key = replaced-by-the-fake
store_id = replaced-by-the-fake
webhook_secret = replaced-by-the-fake
btc_xpub = $(dev_key bitcoin.dev.xpub)
btc_hrp = $(dev_key bitcoin.dev.hrp)
xmr_address = $(dev_key monero.dev.primaryAddress)
xmr_view_key = $(dev_key monero.dev.viewKey)

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
const RUNNER_CONTROL = /^\/control\/(code|(settle|partial|confirming|expire|refund)\/[^/]+)$/

writeFileSync(rivalChainFile, JSON.stringify({ ...JSON.parse(readFileSync(chainFile, "utf8")), registrarKey: toHex(rival.getHdKey().privateKey) }))
process.env.LOCAL_CHAIN_OUT = rivalChainFile
const { controlGeneralTerms, controlTake, HttpError } = await import("./dev/mock/service.ts")
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

const taking = new Set()

const CHAIN_CONTROLS = {
  "/control/take": (body) => {
    const answer = controlTake(body.label)
    const done = taken(body.label).catch((e) => console.error(`take ${body.label}:`, e)).finally(() => taking.delete(done))
    taking.add(done)
    return answer
  },
  "/control/general-terms": controlGeneralTerms,
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
    await Promise.all(taking)
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
  (cd "$WEBAPP_DIR" && BASE_URL="$SERVICE_URL" MOCK_URL="$MOCK_URL" npx playwright test --project=chromium --output "$RUN_DIR/build-results" "${filter[@]}" e2e/offline.spec.ts e2e/track.spec.ts)
}

refuse_taken_ports
start_chain
start_terms
build_site
write_ini
start_service
start_mock
run_cases "$@"
