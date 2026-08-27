// The catalog module, unit-tested directly against the built output.
//
// The money formatter is tested here rather than through a rendered screen: a
// formatter checked only by reading HTML back is checked through three layers
// of coincidence, and the interesting inputs (a value under a major unit, one
// that needs the pad, the top of the range, an unknown currency) never appear
// in a fixture catalog at all.

import test from "node:test"
import assert from "node:assert/strict"

import {
  CATALOG_PATH,
  DURATIONS,
  MAX_MINOR_UNITS,
  MONTHS_WITHOUT_OFFER,
  TIERS,
  formatAmount,
  loadCatalog,
  offerForMonths,
  parseCatalog,
  priceForTier,
  savingOn,
  selectionFor,
} from "../dist/catalog.js"
import {LEGEND_PRICE_ID, SUPPORTER_PRICE_ID, catalogPayload, offer, offerOf, priceOf} from "./fixture.mjs"

const fixture = () => parseCatalog(catalogPayload())

// -- the money formatter ----------------------------------------------------

test("a minor-unit amount renders as major.minor with the currency's symbol", () => {
  for (const [minorUnits, expected] of [
    [0, "$0.00"],
    [1, "$0.01"],
    // Under one major unit: the whole part is 0 and must still be written.
    [5, "$0.05"],
    [99, "$0.99"],
    [100, "$1.00"],
    // The pad: a remainder under ten is two digits, not one.
    [705, "$7.05"],
    [710, "$7.10"],
    [700, "$7.00"],
    // A4's seeded totals, and the fixture's deliberately unround ones.
    [1400, "$14.00"],
    [1301, "$13.01"],
    [42007, "$420.07"],
    // The top of CurrencyAmount's Word32 range, which is not a round number
    // in either part.
    [MAX_MINOR_UNITS, "$42949672.95"],
  ]) {
    assert.equal(formatAmount(minorUnits, "usd"), expected, `${minorUnits} minor units`)
  }
})

test("a currency with no symbol renders as its ISO code before the digits", () => {
  // Never a bare number: an amount with no marker at all would be read as
  // dollars by most of this site's readers.
  assert.equal(formatAmount(1234, "eur"), "EUR 12.34")
  assert.equal(formatAmount(5, "chf"), "CHF 0.05")
  assert.equal(formatAmount(0, "jpy"), "JPY 0.00")
})

test("the currency is matched whatever its case, and only usd is the dollar", () => {
  assert.equal(formatAmount(700, "USD"), "$7.00")
  assert.equal(formatAmount(700, "Usd"), "$7.00")
  // A near miss must not inherit the symbol.
  assert.equal(formatAmount(700, "usdc"), "USDC 7.00")
  assert.equal(formatAmount(700, "aud"), "AUD 7.00")
})

test("the formatter refuses anything that is not a minor-unit amount", () => {
  // Reached only if parseCatalog is bypassed, which is exactly when a silent
  // "$NaN" or a rounded fraction of a cent would be worst.
  for (const bad of [-1, 0.5, 1.5, NaN, Infinity, MAX_MINOR_UNITS + 1, "700"]) {
    assert.throws(() => formatAmount(bad, "usd"), RangeError, `${String(bad)} must be refused`)
  }
})

test("no key of Object.prototype is a currency symbol", () => {
  // A symbol table indexed as a plain object answers "constructor" with a
  // function, and the amount would render with it prefixed.
  assert.equal(formatAmount(100, "constructor"), "CONSTRUCTOR 1.00")
  assert.equal(formatAmount(100, "toString"), "TOSTRING 1.00")
})

// -- parsing ----------------------------------------------------------------

test("the served payload parses into prices and offers", () => {
  const catalog = fixture()
  assert.deepEqual(
    catalog.prices.map((p) => p.priceId),
    [SUPPORTER_PRICE_ID, LEGEND_PRICE_ID]
  )
  assert.equal(catalog.prices[0].monthPrice, 700)
  assert.equal(catalog.offers.length, 4)
  assert.equal(offerForMonths(catalog, SUPPORTER_PRICE_ID, 3, "fresh").total, 1301)
})

test("an unknown field is ignored and an unknown badge type is simply not sold", () => {
  const payload = catalogPayload()
  payload.somethingNewer = true
  payload.prices[0].plan = "monthly"
  payload.prices.push({...priceOf(payload, LEGEND_PRICE_ID), priceId: "price-investor", badgeType: "investor"})
  const catalog = parseCatalog(payload)
  assert.equal(catalog.prices.length, 3)
  assert.deepEqual(
    TIERS.map((t) => t.badgeType),
    ["supporter", "legend"],
    "the site sells what TIERS lists, whatever else the payload carries"
  )
})

test("an offer with no priceId and one with no total parse as absent, not as zero", () => {
  const payload = catalogPayload()
  payload.offers = [
    {offerId: "unpinned", months: 3, status: "active", createdAt: "2026-08-01T09:00:00Z"},
    offer("unpriced", SUPPORTER_PRICE_ID, 3, null),
  ]
  const catalog = parseCatalog(payload)
  assert.equal(catalog.offers[0].priceId, null)
  assert.equal(catalog.offers[1].total, null)
  // Neither can be sold: an unpinned offer is pinned to no price, and an
  // unpriced one has no total to charge. A zero would be free.
  assert.equal(offerForMonths(catalog, SUPPORTER_PRICE_ID, 3, "chosen"), null)
})

test("a malformed payload is refused, naming the field", () => {
  const cases = [
    [(p) => (p.prices = {}), /prices is not an array/],
    [(p) => delete p.offers, /offers is not an array/],
    [(p) => (priceOf(p, SUPPORTER_PRICE_ID).monthPrice = 7.5), /prices\[0\]\.monthPrice/],
    [(p) => (priceOf(p, SUPPORTER_PRICE_ID).monthPrice = "700"), /prices\[0\]\.monthPrice/],
    [(p) => (priceOf(p, SUPPORTER_PRICE_ID).monthPrice = -1), /prices\[0\]\.monthPrice/],
    [(p) => (priceOf(p, SUPPORTER_PRICE_ID).currency = ""), /prices\[0\]\.currency/],
    [(p) => delete priceOf(p, SUPPORTER_PRICE_ID).status, /prices\[0\]\.status/],
    [(p) => (priceOf(p, SUPPORTER_PRICE_ID).createdAt = "whenever"), /prices\[0\]\.createdAt is not a timestamp/],
    [(p) => (offerOf(p, "offer-supporter-3").months = 0), /offers\[0\]\.months/],
    [(p) => (offerOf(p, "offer-supporter-3").total = 0.5), /offers\[0\]\.total/],
  ]
  for (const [break_, message] of cases) {
    const payload = catalogPayload()
    break_(payload)
    assert.throws(() => parseCatalog(payload), {name: "TypeError", message}, `${message} was accepted`)
  }
})

// -- which price and which offer --------------------------------------------

test("a deprecated price is not offered as a fresh choice, but is honoured once chosen", () => {
  const payload = catalogPayload()
  priceOf(payload, LEGEND_PRICE_ID).status = "deprecated"
  const catalog = parseCatalog(payload)
  assert.equal(priceForTier(catalog, "legend", "fresh"), null)
  assert.equal(priceForTier(catalog, "legend", "chosen").priceId, LEGEND_PRICE_ID)
  // D6 accepts deprecated and rejects disabled, and so does this.
  priceOf(payload, LEGEND_PRICE_ID).status = "disabled"
  const disabled = parseCatalog(payload)
  assert.equal(priceForTier(disabled, "legend", "fresh"), null)
  assert.equal(priceForTier(disabled, "legend", "chosen"), null)
})

test("a repriced tier sells at the newest active price, not the first row", () => {
  // Repricing appends and deprecates (UX §3), so both rows arrive, and the
  // deprecated one is first in the payload.
  const payload = catalogPayload()
  priceOf(payload, SUPPORTER_PRICE_ID).status = "deprecated"
  payload.prices.push({
    ...priceOf(payload, SUPPORTER_PRICE_ID),
    priceId: "price-supporter-2",
    monthPrice: 900,
    status: "active",
    createdAt: "2026-08-20T09:00:00Z",
  })
  const catalog = parseCatalog(payload)
  assert.equal(priceForTier(catalog, "supporter", "fresh").monthPrice, 900)
  // Two active rows: the newer one wins, whatever order they arrive in.
  const both = parseCatalog(catalogPayload())
  both.prices.push({...both.prices[0], priceId: "price-supporter-3", monthPrice: 950, createdAt: "2026-08-20T09:00:00Z"})
  assert.equal(priceForTier(both, "supporter", "fresh").monthPrice, 950)
  // ... including when the newer one is a fraction of a second newer, which a
  // lexicographic comparison of the two timestamps would get wrong.
  const closeTogether = parseCatalog(catalogPayload())
  closeTogether.prices.push({...closeTogether.prices[0], priceId: "price-supporter-4", monthPrice: 950, createdAt: "2026-08-01T09:00:00.5Z"})
  assert.equal(priceForTier(closeTogether, "supporter", "fresh").monthPrice, 950)
})

test("a deprecated offer is not offered fresh, and is honoured once chosen", () => {
  const payload = catalogPayload()
  offerOf(payload, "offer-supporter-12").status = "deprecated"
  const catalog = parseCatalog(payload)
  assert.equal(offerForMonths(catalog, SUPPORTER_PRICE_ID, 12, "fresh"), null)
  assert.equal(offerForMonths(catalog, SUPPORTER_PRICE_ID, 12, "chosen").total, 4207)
})

test("an offer is selected by the chosen tier's own priceId", () => {
  const catalog = fixture()
  assert.equal(offerForMonths(catalog, SUPPORTER_PRICE_ID, 3, "fresh").offerId, "offer-supporter-3")
  assert.equal(offerForMonths(catalog, LEGEND_PRICE_ID, 3, "fresh").offerId, "offer-legend-3")
  // A duration nobody offers is not sold at a guess.
  assert.equal(offerForMonths(catalog, SUPPORTER_PRICE_ID, 6, "fresh"), null)
})

// -- what gets charged ------------------------------------------------------

test("the amount is the service's total, copied, never a product computed here", () => {
  const catalog = fixture()
  for (const [months, offerId, total] of [
    [3, "offer-supporter-3", 1301],
    [12, "offer-supporter-12", 4207],
  ]) {
    const selection = selectionFor(catalog, "supporter", months, "fresh")
    assert.equal(selection.total, total, `${months} months must be charged the catalog's own number`)
    assert.equal(selection.offer.offerId, offerId, "D6 reads the months back from this offerId")
    assert.equal(selection.price.priceId, SUPPORTER_PRICE_ID)
    // The products a browser-side calculation would have produced instead.
    const {monthPrice} = selection.price
    for (const wrong of [months * monthPrice, (months - 1) * monthPrice, (months / 3) * 2 * monthPrice, monthPrice]) {
      assert.notEqual(selection.total, wrong, "the fixture must make a computed total look different")
    }
  }
})

test("one month has no offer, and is charged the price's own monthPrice", () => {
  const catalog = fixture()
  const selection = selectionFor(catalog, "supporter", MONTHS_WITHOUT_OFFER, "fresh")
  assert.equal(selection.offer, null, "D6 reads exactly one month from a request with no offerId")
  assert.equal(selection.total, 700)
  assert.equal(selection.price.currency, "usd")
})

test("a tier or duration that cannot be sold has no selection at all", () => {
  const payload = catalogPayload()
  payload.prices = payload.prices.filter((p) => p.badgeType !== "legend")
  const catalog = parseCatalog(payload)
  assert.equal(selectionFor(catalog, "legend", 3, "fresh"), null)
  assert.equal(selectionFor(catalog, "legend", MONTHS_WITHOUT_OFFER, "fresh"), null)
  assert.equal(selectionFor(catalog, "investor", 3, "fresh"), null)
  assert.equal(selectionFor(fixture(), "supporter", 6, "fresh"), null)
})

test("the saving is the undiscounted monthly cost less the total, or nothing", () => {
  const catalog = fixture()
  assert.equal(savingOn(selectionFor(catalog, "supporter", 3, "fresh")), 3 * 700 - 1301)
  assert.equal(savingOn(selectionFor(catalog, "supporter", 12, "fresh")), 12 * 700 - 4207)
  assert.equal(savingOn(selectionFor(catalog, "legend", 3, "fresh")), 3 * 7000 - 13001)
  // One month is the monthly price, so there is nothing to compare it to.
  assert.equal(savingOn(selectionFor(catalog, "supporter", MONTHS_WITHOUT_OFFER, "fresh")), null)
})

test("a mispriced offer claims no saving rather than a negative one", () => {
  const payload = catalogPayload()
  offerOf(payload, "offer-supporter-3").total = 2500
  offerOf(payload, "offer-supporter-12").total = 12 * 700
  const catalog = parseCatalog(payload)
  assert.equal(savingOn(selectionFor(catalog, "supporter", 3, "fresh")), null, "2500 costs more than 3 x 700")
  assert.equal(savingOn(selectionFor(catalog, "supporter", 12, "fresh")), null, "a saving of zero is not a saving")
  // The total is still the service's number: it is charged, not corrected.
  assert.equal(selectionFor(catalog, "supporter", 3, "fresh").total, 2500)
})

// -- the fetch --------------------------------------------------------------

function sink() {
  const seen = {catalogs: [], errors: []}
  return {
    seen,
    setCatalog: (catalog) => seen.catalogs.push(catalog),
    showError: (message) => seen.errors.push(message),
  }
}

function response(body, {ok = true, status = 200} = {}) {
  return Promise.resolve({ok, status, json: () => (body instanceof Error ? Promise.reject(body) : Promise.resolve(body))})
}

async function withQuietConsole(run) {
  const logged = []
  const original = console.error
  console.error = (...args) => logged.push(args)
  try {
    await run()
  } finally {
    console.error = original
  }
  return logged
}

test("a served catalog reaches the shell, parsed, from /api/catalog", async () => {
  const target = sink()
  const asked = []
  await loadCatalog((path) => {
    asked.push(path)
    return response(catalogPayload())
  }, target)
  assert.deepEqual(asked, [CATALOG_PATH])
  assert.deepEqual(target.seen.errors, [], "a successful load says nothing in the banner")
  assert.equal(target.seen.catalogs.length, 1)
  assert.equal(target.seen.catalogs[0].prices.length, 2)
  assert.equal(selectionFor(target.seen.catalogs[0], "supporter", 3, "fresh").total, 1301)
})

test("every way the fetch can fail shows the banner, prices nothing, and does not retry", async () => {
  const failures = {
    "a 500 from the service": () => response({}, {ok: false, status: 500}),
    "a 404, in case the route moves": () => response({}, {ok: false, status: 404}),
    "a body that is not a catalog": () => response({prices: "soon"}),
    "a body that is not JSON at all": () => response(new SyntaxError("Unexpected token <")),
    "a network error": () => Promise.reject(new TypeError("Failed to fetch")),
  }
  for (const [what, fetchLike] of Object.entries(failures)) {
    const target = sink()
    let calls = 0
    const logged = await withQuietConsole(() =>
      loadCatalog((path) => {
        calls += 1
        return fetchLike(path)
      }, target)
    )
    assert.equal(calls, 1, `${what}: one attempt, no silent retry`)
    assert.deepEqual(target.seen.catalogs, [], `${what}: nothing may be priced from a failed load`)
    assert.equal(target.seen.errors.length, 1, `${what}: the visitor must be told`)
    assert.match(target.seen.errors[0], /Prices could not be loaded/, what)
    assert.match(target.seen.errors[0], /contact support/, `${what}: the banner points somewhere`)
    assert.equal(logged.length, 1, `${what}: the reason belongs in the console`)
  }
})

// -- the site constants the payload does not carry --------------------------

test("every tier has a perk line and every duration the plan names is offered", () => {
  assert.deepEqual(
    TIERS.map((t) => [t.badgeType, t.label, t.perk]),
    [
      ["supporter", "Supporter", "2 GB files"],
      ["legend", "Legend", "5 GB files"],
    ]
  )
  assert.deepEqual([...DURATIONS], [1, 3, 12])
  assert.equal(MONTHS_WITHOUT_OFFER, 1)
})
