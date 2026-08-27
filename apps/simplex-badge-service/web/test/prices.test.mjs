// D3's screens, rendered from a fixture catalog.
//
// Every assertion here reads the rendered card back — the label and the detail
// LINE a visitor sees — rather than the value some function returned, so the
// copy, the formatter and the selection are all in the path being checked.
// The fixture's totals are deliberately not products of its monthly prices
// (see fixture.mjs), so a site that computed a price instead of copying the
// service's would print a different number here rather than the same one by
// coincidence.

import test from "node:test"
import assert from "node:assert/strict"

import {parseCatalog} from "../dist/catalog.js"
import {optionsOfQuestion, screenView} from "../dist/view.js"
import {optionCards, summaryRows, text} from "./el.mjs"
import {LEGEND_PRICE_ID, SUPPORTER_PRICE_ID, catalogPayload, offerOf, priceOf, seededPayload} from "./fixture.mjs"

const fixture = (edit) => {
  const payload = catalogPayload()
  if (edit) edit(payload)
  return parseCatalog(payload)
}

const cardsOf = (id, answers, catalog) => optionCards(screenView(id, answers, catalog))
const detailsOf = (id, answers, catalog) => Object.fromEntries(cardsOf(id, answers, catalog).map((c) => [c.value, c.detail]))

// -- screen 1: choose your level --------------------------------------------

test("each level shows its own monthly price and its perk line", () => {
  const cards = cardsOf("tier", {}, fixture())
  assert.deepEqual(
    cards.map((c) => [c.value, c.label, c.detail, c.disabled]),
    [
      ["supporter", "Supporter", "$7.00 per month · 2 GB files", false],
      ["legend", "Legend", "$70.00 per month · 5 GB files", false],
    ]
  )
  assert.equal(text(screenView("tier", {}, fixture())).includes("Choose your level"), true)
})

test("a repriced tier shows the new price, since it renders what the payload holds", () => {
  const catalog = fixture((p) => (priceOf(p, SUPPORTER_PRICE_ID).monthPrice = 1234))
  assert.equal(detailsOf("tier", {}, catalog).supporter, "$12.34 per month · 2 GB files")
})

test("a level with no price is disabled, not hidden, and still says what it would give", () => {
  // The step's manual line: "removing the legend price disables that card".
  const catalog = fixture((p) => (p.prices = p.prices.filter((price) => price.priceId !== LEGEND_PRICE_ID)))
  const cards = cardsOf("tier", {}, catalog)
  assert.deepEqual(
    cards.map((c) => c.value),
    ["supporter", "legend"],
    "the legend card must still be on the screen"
  )
  assert.deepEqual(
    cards.map((c) => c.disabled),
    [false, true]
  )
  assert.equal(cards[1].detail, "Unavailable · 5 GB files")
})

test("a deprecated price is not offered as a fresh choice, and is shown once chosen", () => {
  const catalog = fixture((p) => (priceOf(p, LEGEND_PRICE_ID).status = "deprecated"))
  const fresh = cardsOf("tier", {}, catalog)[1]
  assert.equal(fresh.disabled, true, "a withdrawn price must not be a fresh choice")
  // D5 lands here with ?tier=legend, and a walk back through the wizard must
  // not lose the answer or misprice it.
  const chosen = cardsOf("tier", {tier: "legend"}, catalog)[1]
  assert.equal(chosen.disabled, false)
  assert.equal(chosen.checked, true)
  assert.equal(chosen.detail, "$70.00 per month · 5 GB files")
})

// -- screen 2: how long? ----------------------------------------------------

test("the three durations show the served total, and only the offers show a saving", () => {
  const details = detailsOf("months", {tier: "supporter"}, fixture())
  assert.deepEqual(details, {
    // No offer for one month: the price's own monthPrice, and nothing beside
    // it. A saving here would mean an offer had been applied to a row that has
    // none.
    1: "$7.00",
    // 1301, not 3 x 700 = 2100 and not A4's 2 x 700 = 1400.
    3: "$13.01 · you save $7.99",
    // 4207, not 12 x 700 = 8400 and not A4's 6 x 700 = 4200.
    12: "$42.07 · you save $41.93",
  })
})

test("the durations are priced from the chosen tier, not from the first price", () => {
  const details = detailsOf("months", {tier: "legend"}, fixture())
  assert.deepEqual(details, {
    1: "$70.00",
    3: "$130.01 · you save $79.99",
    12: "$420.07 · you save $419.93",
  })
})

test("changing only the served total changes only the price shown", () => {
  // The tightest form of "the browser computes no chargeable amount": nothing
  // about the site changed, one number in the payload did, and the screen
  // follows it.
  const catalog = fixture((p) => (offerOf(p, "offer-supporter-3").total = 111))
  assert.equal(detailsOf("months", {tier: "supporter"}, catalog)[3], "$1.11 · you save $19.89")
})

test("an unpriced or missing offer disables that duration alone", () => {
  const catalog = fixture((p) => {
    offerOf(p, "offer-supporter-3").total = null
    p.offers = p.offers.filter((o) => o.offerId !== "offer-supporter-12")
  })
  const cards = cardsOf("months", {tier: "supporter"}, catalog)
  assert.deepEqual(
    cards.map((c) => [c.value, c.detail, c.disabled]),
    [
      ["1", "$7.00", false],
      ["3", "Unavailable", true],
      ["12", "Unavailable", true],
    ]
  )
})

test("a deprecated offer is honoured for the duration already chosen", () => {
  const catalog = fixture((p) => (offerOf(p, "offer-supporter-12").status = "deprecated"))
  assert.equal(detailsOf("months", {tier: "supporter"}, catalog)[12], "Unavailable")
  const chosen = detailsOf("months", {tier: "supporter", months: "12"}, catalog)
  assert.equal(chosen[12], "$42.07 · you save $41.93")
  assert.equal(chosen[3], "$13.01 · you save $7.99", "the other durations stay fresh choices")
})

test("with no tier chosen, every duration is disabled and says why", () => {
  // Reachable by hand-editing the hash to #/months on a first visit.
  const cards = cardsOf("months", {}, fixture())
  assert.deepEqual(
    cards.map((c) => [c.disabled, c.detail]),
    [
      [true, "Choose your level first"],
      [true, "Choose your level first"],
      [true, "Choose your level first"],
    ]
  )
})

test("the prices this service actually seeds read as the plan's own figures", () => {
  // A4's seeded catalog, priced by its offerTotal: $7 and $70 a month, one
  // free month in three and six in twelve. The fixture elsewhere in this file
  // is deliberately unround to catch arithmetic; this is what a visitor will
  // really see on the day, in the copy they will really see it in.
  const catalog = parseCatalog(seededPayload())
  assert.deepEqual(detailsOf("tier", {}, catalog), {
    supporter: "$7.00 per month · 2 GB files",
    legend: "$70.00 per month · 5 GB files",
  })
  assert.deepEqual(detailsOf("months", {tier: "supporter"}, catalog), {
    1: "$7.00",
    3: "$14.00 · you save $7.00",
    12: "$42.00 · you save $42.00",
  })
  assert.deepEqual(detailsOf("months", {tier: "legend"}, catalog), {
    1: "$70.00",
    3: "$140.00 · you save $70.00",
    12: "$420.00 · you save $420.00",
  })
})

// -- screen 3: how would you like to pay? -----------------------------------

test("the three payment methods are D6's own spellings and need no catalog", () => {
  for (const catalog of [null, fixture()]) {
    assert.deepEqual(
      cardsOf("pay", {}, catalog).map((c) => [c.value, c.label, c.disabled]),
      [
        ["card", "Card", false],
        ["btc", "Bitcoin", false],
        ["xmr", "Monero", false],
      ]
    )
  }
})

// -- screen 4: the summary --------------------------------------------------

test("the summary shows the chosen tier, length, total and method", () => {
  const rows = summaryRows(screenView("checkout", {tier: "supporter", months: "12", pay: "btc"}, fixture()))
  assert.deepEqual(rows, {Level: "Supporter", Length: "12 months", Total: "$42.07", Payment: "Bitcoin"})
})

test("the summary's total is the same number the duration screen showed", () => {
  const catalog = fixture()
  for (const [tier, months] of [
    ["supporter", "1"],
    ["supporter", "3"],
    ["supporter", "12"],
    ["legend", "3"],
  ]) {
    const shown = detailsOf("months", {tier}, catalog)[months].split(" · ")[0]
    const {Total} = summaryRows(screenView("checkout", {tier, months, pay: "card"}, catalog))
    assert.equal(Total, shown, `${tier} for ${months} months`)
  }
})

test("the summary never invents an amount", () => {
  const catalog = fixture()
  const rows = (answers, c = catalog) => summaryRows(screenView("checkout", answers, c))
  assert.equal(rows({}).Total, "Not chosen yet")
  assert.equal(rows({tier: "supporter"}).Total, "Not chosen yet")
  assert.equal(rows({tier: "supporter", months: "12"}, null).Total, "Loading prices…")
  assert.equal(rows({tier: "legend", months: "6"}).Total, "Unavailable", "a duration with no offer has no price")
  assert.equal(rows({tier: "nonsense", months: "12"}).Total, "Unavailable")
  assert.equal(rows({tier: "supporter", months: "12x"}).Total, "Unavailable")
  // An answer that is not a month count is not silently coerced into one.
  assert.equal(rows({tier: "supporter", months: "12x"}).Length, "12x")
})

// -- before the catalog lands -----------------------------------------------

test("with no catalog every option is rendered, and none can be chosen", () => {
  for (const id of ["tier", "months"]) {
    const cards = cardsOf(id, {tier: "supporter"}, null)
    assert.ok(cards.length > 0, `screen ${id} renders nothing without a catalog`)
    for (const card of cards) {
      assert.equal(card.disabled, true, `${id}/${card.value} is choosable before its price is known`)
      assert.equal(card.detail, "Loading prices…")
    }
  }
})

test("no screen renders an empty detail line, in any state", () => {
  const states = [
    [null, {}],
    [fixture(), {}],
    [fixture(), {tier: "supporter", months: "3", pay: "card"}],
    [fixture((p) => (p.prices = [])), {tier: "legend", months: "3"}],
  ]
  for (const [catalog, answers] of states) {
    for (const q of ["tier", "months", "pay"]) {
      for (const option of optionsOfQuestion(q, answers, catalog)) {
        assert.notEqual(option.detail.trim(), "", `${q}/${option.value} has a blank second line`)
        assert.notEqual(option.label.trim(), "", `${q}/${option.value} has no label`)
      }
    }
  }
})
