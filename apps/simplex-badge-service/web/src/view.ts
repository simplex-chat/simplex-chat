// The screens, as pure data.
//
// A screen is described as an element tree and never touches the DOM, so the
// structure a browser will render can be asserted with `node --test` (see
// ../test/static.test.mjs, which counts the <h1> of every screen and checks
// that every radio group is a real fieldset/legend/label, and
// ../test/prices.test.mjs, which drives a fixture catalog through this module
// and reads the rendered prices back). ui.ts turns a tree into elements with
// document.createElement and textContent — there is no innerHTML anywhere, so
// a label that comes from the catalog or from a query parameter (D5) cannot
// become markup.
//
// Every price and every total on these screens is a number the service
// computed and served; see catalog.ts's header for why that matters.

import {FIRST_SCREEN, nextScreen, type ScreenId} from "./router.js"
import {
  DURATIONS,
  MONTHS_WITHOUT_OFFER,
  TIERS,
  formatAmount,
  priceForTier,
  savingOn,
  selectionFor,
  type Catalog,
  type Use,
} from "./catalog.js"

export type Child = El | string

export interface El {
  readonly tag: string
  readonly attrs: Readonly<Record<string, string>>
  readonly children: readonly Child[]
}

/** An element node. An attribute present with an empty value is a boolean attribute. */
export function h(tag: string, attrs: Record<string, string> = {}, children: readonly Child[] = []): El {
  return {tag, attrs, children}
}

/** One radio card. `detail` is the secondary line, as on the app's PeriodCard. */
export interface Option {
  readonly value: string
  readonly label: string
  readonly detail: string
  readonly disabled?: boolean
}

/**
 * The answers gathered so far.
 *
 * `tier` is a badge type and `months` a month count, which is what D5's
 * `?tier=`/`?months=` parameters carry and what the app's hand-off sends. The
 * catalog identifiers D6 wants are resolved from them at the last moment, so a
 * catalog that changes under a visitor reprices their answers instead of
 * carrying a stale id to checkout.
 */
export interface Answers {
  readonly tier?: string
  readonly months?: string
  readonly pay?: string
}

/** The radio group name of a question screen, which is also its answer key. */
export type Question = "tier" | "months" | "pay"

interface QuestionScreen {
  readonly heading: string
  readonly legend: string
}

const QUESTIONS: Readonly<Record<Question, QuestionScreen>> = {
  tier: {heading: "Choose your level", legend: "Badge level"},
  months: {heading: "How long?", legend: "Subscription length"},
  pay: {heading: "How would you like to pay?", legend: "Payment method"},
}

// The only options that are not built from the catalog: a payment method is a
// property of the service's providers, not of a price. D6 takes these three
// spellings as its `method`.
const PAY_OPTIONS: readonly Option[] = [
  {value: "card", label: "Card", detail: "Visa, Mastercard and others"},
  {value: "btc", label: "Bitcoin", detail: "On-chain or Lightning"},
  {value: "xmr", label: "Monero", detail: "On-chain"},
]

const QUESTION_IDS: ReadonlySet<string> = new Set(Object.keys(QUESTIONS))

const LOADING_PRICES = "Loading prices…"
const UNAVAILABLE = "Unavailable"
const CHOOSE_TIER_FIRST = "Choose your level first"
const NOT_CHOSEN = "Not chosen yet"
const SEPARATOR = " · "

/** The question a screen asks, or null for a screen that asks none. */
export function questionOfScreen(id: ScreenId): Question | null {
  return QUESTION_IDS.has(id) ? (id as Question) : null
}

/**
 * The options of a question screen, priced from the catalog.
 *
 * A tier or duration that cannot be sold is rendered DISABLED, not hidden (UX
 * §2.1) — including while the catalog is still on its way, so that nothing can
 * be chosen before its price is known. The same applies to a deprecated row,
 * which is not offered as a fresh choice but is still rendered when it is the
 * answer already held: that is how a `?tier=`/`?months=` parameter for a
 * withdrawn price survives a walk back through the wizard.
 */
export function optionsOfQuestion(q: Question, answers: Answers, catalog: Catalog | null): readonly Option[] {
  switch (q) {
    case "tier":
      return tierOptions(answers, catalog)
    case "months":
      return monthOptions(answers, catalog)
    case "pay":
      return PAY_OPTIONS
  }
}

function tierOptions(answers: Answers, catalog: Catalog | null): readonly Option[] {
  return TIERS.map(({badgeType, label, perk}) => {
    const price = catalog && priceForTier(catalog, badgeType, use(badgeType === answers.tier))
    if (!price) return unavailable(badgeType, label, catalog === null ? LOADING_PRICES : UNAVAILABLE + SEPARATOR + perk)
    return {value: badgeType, label, detail: `${formatAmount(price.monthPrice, price.currency)} per month${SEPARATOR}${perk}`}
  })
}

function monthOptions(answers: Answers, catalog: Catalog | null): readonly Option[] {
  const {tier} = answers
  return DURATIONS.map((months) => {
    const value = String(months)
    const label = monthsLabel(months)
    if (catalog === null) return unavailable(value, label, LOADING_PRICES)
    // Reachable by hand-editing the hash to #/months on a first visit.
    if (tier === undefined) return unavailable(value, label, CHOOSE_TIER_FIRST)
    const selection = selectionFor(catalog, tier, months, use(value === answers.months))
    if (!selection) return unavailable(value, label, UNAVAILABLE)
    const {currency} = selection.price
    // The price shown is the service's own total, verbatim. The saving beside
    // it is a display-only comparison (catalog.ts) and one month has none, so
    // that row shows its monthly price and nothing else.
    const total = formatAmount(selection.total, currency)
    const saving = savingOn(selection)
    return {value, label, detail: saving === null ? total : `${total}${SEPARATOR}you save ${formatAmount(saving, currency)}`}
  })
}

function unavailable(value: string, label: string, detail: string): Option {
  return {value, label, detail, disabled: true}
}

/**
 * What to say when a question is submitted with nothing chosen.
 *
 * Every option is disabled until the catalog lands, so before then the visitor
 * cannot choose one and "choose an option" would be a lie about whose turn it
 * is. Here rather than in ui.ts because it is copy, and because the shell's own
 * branches are the part of the site no test can reach.
 */
export function nothingChosenMessage(catalog: Catalog | null): string {
  return catalog === null ? "Prices are still loading. Please try again in a moment." : "Choose an option to continue."
}

function use(chosen: boolean): Use {
  return chosen ? "chosen" : "fresh"
}

function monthsLabel(months: number): string {
  return months === MONTHS_WITHOUT_OFFER ? `${months} month` : `${months} months`
}

/**
 * The earliest screen whose question has no answer, or `checkout` when every
 * question is answered.
 *
 * This is where a visit starts. With no answers it is FIRST_SCREEN, which is
 * the whole of today's behaviour; D5's prefill seeds answers and this then
 * skips each screen it has already answered, which is what that step asks for.
 * Pure, so the rule is testable without a browser.
 */
export function firstUnansweredScreen(answers: Answers): ScreenId {
  let id: ScreenId = FIRST_SCREEN
  for (;;) {
    const q = questionOfScreen(id)
    if (!q || answers[q] === undefined) return id
    const next = nextScreen(id)
    if (!next) return id
    id = next
  }
}

/**
 * The whole screen, ready to be turned into elements. Exactly one <h1>.
 *
 * The switch is exhaustive over ScreenId with no default, so adding a screen
 * to router.ts fails to compile until this renders it — there is no route the
 * shell can reach that renders nothing. `catalog` is null until the fetch
 * lands, which is a state every screen renders rather than a state it waits
 * for.
 */
export function screenView(id: ScreenId, answers: Answers, catalog: Catalog | null): El {
  switch (id) {
    case "tier":
    case "months":
    case "pay":
      return questionScreen(id, answers, catalog)
    case "checkout":
      return checkoutScreen(answers, catalog)
    case "order":
      return orderScreen()
    case "code":
      return codeScreen()
  }
}

function questionScreen(q: Question, answers: Answers, catalog: Catalog | null): El {
  const {heading, legend} = QUESTIONS[q]
  const options = optionsOfQuestion(q, answers, catalog)
  return section(heading, [
    h("form", {class: "form"}, [
      h("fieldset", {class: "options"}, [h("legend", {class: "options__legend"}, [legend]), ...options.map((o) => optionCard(q, o, answers[q]))]),
      submitButton("Continue"),
    ]),
  ])
}

function optionCard(group: string, option: Option, answer: string | undefined): El {
  const input: Record<string, string> = {class: "option__input", type: "radio", name: group, value: option.value}
  if (option.value === answer) input.checked = ""
  if (option.disabled) input.disabled = ""
  return h("label", {class: "option"}, [
    h("input", input),
    h("span", {class: "option__body"}, [
      h("span", {class: "option__label"}, [option.label]),
      h("span", {class: "option__detail"}, [option.detail]),
    ]),
  ])
}

function checkoutScreen(answers: Answers, catalog: Catalog | null): El {
  return section("Review your order", [
    h("form", {class: "form"}, [
      h("dl", {class: "summary"}, [
        ...summaryRow("Level", tierLabel(answers.tier)),
        ...summaryRow("Length", lengthLabel(answers.months)),
        ...summaryRow("Total", totalLabel(answers, catalog)),
        ...summaryRow("Payment", payLabel(answers.pay)),
      ]),
      // Inert until D7 wires it: ui.ts answers this submit with a banner.
      submitButton("Pay"),
    ]),
  ])
}

function summaryRow(term: string, value: string): readonly El[] {
  return [h("dt", {class: "summary__term"}, [term]), h("dd", {class: "summary__value"}, [value])]
}

function tierLabel(value: string | undefined): string {
  if (value === undefined) return NOT_CHOSEN
  return TIERS.find((t) => t.badgeType === value)?.label ?? value
}

function lengthLabel(value: string | undefined): string {
  if (value === undefined) return NOT_CHOSEN
  const months = monthsOf(value)
  return months === null ? value : monthsLabel(months)
}

function payLabel(value: string | undefined): string {
  if (value === undefined) return NOT_CHOSEN
  return PAY_OPTIONS.find((o) => o.value === value)?.label ?? value
}

// The one place the summary states an amount, and it states the service's.
function totalLabel(answers: Answers, catalog: Catalog | null): string {
  const {tier, months} = answers
  if (tier === undefined || months === undefined) return NOT_CHOSEN
  if (catalog === null) return LOADING_PRICES
  const chosen = monthsOf(months)
  const selection = chosen === null ? null : selectionFor(catalog, tier, chosen, "chosen")
  return selection === null ? UNAVAILABLE : formatAmount(selection.total, selection.price.currency)
}

// An answer is a string, and every path into it is hand-editable: a query
// parameter (D5), a resumed order (E5), the hash. Anything that is not a month
// count is not one.
function monthsOf(value: string): number | null {
  const months = Number(value)
  return Number.isInteger(months) && months > 0 ? months : null
}

// E5 replaces this with the crypto payment screen, E6 with the result screen.
// They exist now so that no path can route to a hash that renders nothing.
function orderScreen(): El {
  return section("Complete your payment", [h("p", {class: "prose"}, ["Payment details will appear here once an order has been created."])])
}

function codeScreen(): El {
  return section("Your redemption code", [h("p", {class: "prose"}, ["Your code will appear here once a payment has been confirmed."])])
}

function section(heading: string, body: readonly Child[]): El {
  return h("section", {class: "screen"}, [h("h1", {class: "screen__heading"}, [heading]), ...body])
}

function submitButton(label: string): El {
  return h("button", {class: "button", type: "submit"}, [label])
}
