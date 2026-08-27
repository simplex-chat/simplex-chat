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
import { FIRST_SCREEN, nextScreen } from "./router.js";
import { DURATIONS, MONTHS_WITHOUT_OFFER, TIERS, formatAmount, priceForTier, savingOn, selectionFor, } from "./catalog.js";
/** An element node. An attribute present with an empty value is a boolean attribute. */
export function h(tag, attrs = {}, children = []) {
    return { tag, attrs, children };
}
const QUESTIONS = {
    tier: { heading: "Choose your level", legend: "Badge level" },
    months: { heading: "How long?", legend: "Subscription length" },
    pay: { heading: "How would you like to pay?", legend: "Payment method" },
};
// The only options that are not built from the catalog: a payment method is a
// property of the service's providers, not of a price. D6 takes these three
// spellings as its `method`.
const PAY_OPTIONS = [
    { value: "card", label: "Card", detail: "Visa, Mastercard and others" },
    { value: "btc", label: "Bitcoin", detail: "On-chain or Lightning" },
    { value: "xmr", label: "Monero", detail: "On-chain" },
];
const QUESTION_IDS = new Set(Object.keys(QUESTIONS));
const LOADING_PRICES = "Loading prices…";
const UNAVAILABLE = "Unavailable";
const CHOOSE_TIER_FIRST = "Choose your level first";
const NOT_CHOSEN = "Not chosen yet";
const SEPARATOR = " · ";
/** The question a screen asks, or null for a screen that asks none. */
export function questionOfScreen(id) {
    return QUESTION_IDS.has(id) ? id : null;
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
export function optionsOfQuestion(q, answers, catalog) {
    switch (q) {
        case "tier":
            return tierOptions(answers, catalog);
        case "months":
            return monthOptions(answers, catalog);
        case "pay":
            return PAY_OPTIONS;
    }
}
function tierOptions(answers, catalog) {
    return TIERS.map(({ badgeType, label, perk }) => {
        const price = catalog && priceForTier(catalog, badgeType, use(badgeType === answers.tier));
        if (!price)
            return unavailable(badgeType, label, catalog === null ? LOADING_PRICES : UNAVAILABLE + SEPARATOR + perk);
        return { value: badgeType, label, detail: `${formatAmount(price.monthPrice, price.currency)} per month${SEPARATOR}${perk}` };
    });
}
function monthOptions(answers, catalog) {
    const { tier } = answers;
    return DURATIONS.map((months) => {
        const value = String(months);
        const label = monthsLabel(months);
        if (catalog === null)
            return unavailable(value, label, LOADING_PRICES);
        // Reachable by hand-editing the hash to #/months on a first visit.
        if (tier === undefined)
            return unavailable(value, label, CHOOSE_TIER_FIRST);
        const selection = selectionFor(catalog, tier, months, use(value === answers.months));
        if (!selection)
            return unavailable(value, label, UNAVAILABLE);
        const { currency } = selection.price;
        // The price shown is the service's own total, verbatim. The saving beside
        // it is a display-only comparison (catalog.ts) and one month has none, so
        // that row shows its monthly price and nothing else.
        const total = formatAmount(selection.total, currency);
        const saving = savingOn(selection);
        return { value, label, detail: saving === null ? total : `${total}${SEPARATOR}you save ${formatAmount(saving, currency)}` };
    });
}
function unavailable(value, label, detail) {
    return { value, label, detail, disabled: true };
}
/**
 * What to say when a question is submitted with nothing chosen.
 *
 * Every option is disabled until the catalog lands, so before then the visitor
 * cannot choose one and "choose an option" would be a lie about whose turn it
 * is. Here rather than in ui.ts because it is copy, and because the shell's own
 * branches are the part of the site no test can reach.
 */
export function nothingChosenMessage(catalog) {
    return catalog === null ? "Prices are still loading. Please try again in a moment." : "Choose an option to continue.";
}
function use(chosen) {
    return chosen ? "chosen" : "fresh";
}
function monthsLabel(months) {
    return months === MONTHS_WITHOUT_OFFER ? `${months} month` : `${months} months`;
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
export function firstUnansweredScreen(answers) {
    let id = FIRST_SCREEN;
    for (;;) {
        const q = questionOfScreen(id);
        if (!q || answers[q] === undefined)
            return id;
        const next = nextScreen(id);
        if (!next)
            return id;
        id = next;
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
export function screenView(id, answers, catalog) {
    switch (id) {
        case "tier":
        case "months":
        case "pay":
            return questionScreen(id, answers, catalog);
        case "checkout":
            return checkoutScreen(answers, catalog);
        case "order":
            return orderScreen();
        case "code":
            return codeScreen();
    }
}
function questionScreen(q, answers, catalog) {
    const { heading, legend } = QUESTIONS[q];
    const options = optionsOfQuestion(q, answers, catalog);
    return section(heading, [
        h("form", { class: "form" }, [
            h("fieldset", { class: "options" }, [h("legend", { class: "options__legend" }, [legend]), ...options.map((o) => optionCard(q, o, answers[q]))]),
            submitButton("Continue"),
        ]),
    ]);
}
function optionCard(group, option, answer) {
    const input = { class: "option__input", type: "radio", name: group, value: option.value };
    if (option.value === answer)
        input.checked = "";
    if (option.disabled)
        input.disabled = "";
    return h("label", { class: "option" }, [
        h("input", input),
        h("span", { class: "option__body" }, [
            h("span", { class: "option__label" }, [option.label]),
            h("span", { class: "option__detail" }, [option.detail]),
        ]),
    ]);
}
function checkoutScreen(answers, catalog) {
    return section("Review your order", [
        h("form", { class: "form" }, [
            h("dl", { class: "summary" }, [
                ...summaryRow("Level", tierLabel(answers.tier)),
                ...summaryRow("Length", lengthLabel(answers.months)),
                ...summaryRow("Total", totalLabel(answers, catalog)),
                ...summaryRow("Payment", payLabel(answers.pay)),
            ]),
            // Inert until D7 wires it: ui.ts answers this submit with a banner.
            submitButton("Pay"),
        ]),
    ]);
}
function summaryRow(term, value) {
    return [h("dt", { class: "summary__term" }, [term]), h("dd", { class: "summary__value" }, [value])];
}
function tierLabel(value) {
    if (value === undefined)
        return NOT_CHOSEN;
    return TIERS.find((t) => t.badgeType === value)?.label ?? value;
}
function lengthLabel(value) {
    if (value === undefined)
        return NOT_CHOSEN;
    const months = monthsOf(value);
    return months === null ? value : monthsLabel(months);
}
function payLabel(value) {
    if (value === undefined)
        return NOT_CHOSEN;
    return PAY_OPTIONS.find((o) => o.value === value)?.label ?? value;
}
// The one place the summary states an amount, and it states the service's.
function totalLabel(answers, catalog) {
    const { tier, months } = answers;
    if (tier === undefined || months === undefined)
        return NOT_CHOSEN;
    if (catalog === null)
        return LOADING_PRICES;
    const chosen = monthsOf(months);
    const selection = chosen === null ? null : selectionFor(catalog, tier, chosen, "chosen");
    return selection === null ? UNAVAILABLE : formatAmount(selection.total, selection.price.currency);
}
// An answer is a string, and every path into it is hand-editable: a query
// parameter (D5), a resumed order (E5), the hash. Anything that is not a month
// count is not one.
function monthsOf(value) {
    const months = Number(value);
    return Number.isInteger(months) && months > 0 ? months : null;
}
// E5 replaces this with the crypto payment screen, E6 with the result screen.
// They exist now so that no path can route to a hash that renders nothing.
function orderScreen() {
    return section("Complete your payment", [h("p", { class: "prose" }, ["Payment details will appear here once an order has been created."])]);
}
function codeScreen() {
    return section("Your redemption code", [h("p", { class: "prose" }, ["Your code will appear here once a payment has been confirmed."])]);
}
function section(heading, body) {
    return h("section", { class: "screen" }, [h("h1", { class: "screen__heading" }, [heading]), ...body]);
}
function submitButton(label) {
    return h("button", { class: "button", type: "submit" }, [label]);
}
