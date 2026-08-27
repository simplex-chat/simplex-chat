// The screens, as pure data.
//
// A screen is described as an element tree and never touches the DOM, so the
// structure a browser will render can be asserted with `node --test` (see
// ../test/static.test.mjs, which counts the <h1> of every screen and checks
// that every radio group is a real fieldset/legend/label). ui.ts turns a tree
// into elements with document.createElement and textContent — there is no
// innerHTML anywhere, so a label that later comes from the catalog (D3) or from
// a query parameter (D5) cannot become markup.
//
// The options here are placeholders. D3 builds the first three screens from the
// catalog payload and replaces them.

import type {ScreenId} from "./router.js"

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

/** The answers gathered so far. D3 replaces the values with catalog identifiers. */
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
  readonly options: readonly Option[]
}

const QUESTIONS: Readonly<Record<Question, QuestionScreen>> = {
  tier: {
    heading: "Choose your level",
    legend: "Badge level",
    options: [
      {value: "supporter", label: "Supporter", detail: "2 GB files"},
      {value: "legend", label: "Legend", detail: "5 GB files"},
    ],
  },
  months: {
    heading: "How long?",
    legend: "Subscription length",
    options: [
      {value: "1", label: "1 month", detail: "Billed once"},
      {value: "3", label: "3 months", detail: "Billed once"},
      {value: "12", label: "12 months", detail: "Billed once"},
    ],
  },
  pay: {
    heading: "How would you like to pay?",
    legend: "Payment method",
    options: [
      {value: "card", label: "Card", detail: "Visa, Mastercard and others"},
      {value: "btc", label: "Bitcoin", detail: "On-chain or Lightning"},
      {value: "xmr", label: "Monero", detail: "On-chain"},
    ],
  },
}

const QUESTION_IDS: ReadonlySet<string> = new Set(Object.keys(QUESTIONS))

/** The question a screen asks, or null for a screen that asks none. */
export function questionOfScreen(id: ScreenId): Question | null {
  return QUESTION_IDS.has(id) ? (id as Question) : null
}

/** The placeholder options of a question screen. D3 replaces this with the catalog. */
export function optionsOfQuestion(q: Question): readonly Option[] {
  return QUESTIONS[q].options
}

/**
 * The whole screen, ready to be turned into elements. Exactly one <h1>.
 *
 * The switch is exhaustive over ScreenId with no default, so adding a screen
 * to router.ts fails to compile until this renders it — there is no route the
 * shell can reach that renders nothing.
 */
export function screenView(id: ScreenId, answers: Answers): El {
  switch (id) {
    case "tier":
    case "months":
    case "pay":
      return questionScreen(id, answers[id])
    case "checkout":
      return checkoutScreen(answers)
    case "order":
      return orderScreen()
    case "code":
      return codeScreen()
  }
}

function questionScreen(q: Question, answer: string | undefined): El {
  const {heading, legend, options} = QUESTIONS[q]
  return section(heading, [
    h("form", {class: "form"}, [
      h("fieldset", {class: "options"}, [h("legend", {class: "options__legend"}, [legend]), ...options.map((o) => optionCard(q, o, answer))]),
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

const NOT_CHOSEN = "Not chosen yet"

function checkoutScreen(answers: Answers): El {
  return section("Review your order", [
    h("form", {class: "form"}, [
      h("dl", {class: "summary"}, [
        ...summaryRow("Level", labelOf("tier", answers.tier)),
        ...summaryRow("Length", labelOf("months", answers.months)),
        ...summaryRow("Payment", labelOf("pay", answers.pay)),
      ]),
      submitButton("Pay"),
    ]),
  ])
}

function summaryRow(term: string, value: string): readonly El[] {
  return [h("dt", {class: "summary__term"}, [term]), h("dd", {class: "summary__value"}, [value])]
}

function labelOf(q: Question, value: string | undefined): string {
  if (value === undefined) return NOT_CHOSEN
  return QUESTIONS[q].options.find((o) => o.value === value)?.label ?? value
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
