// The catalog: the payload `GET /api/catalog` serves, the site constants it
// does not carry, and the one money formatter.
//
// THE BROWSER COMPUTES NO CHARGEABLE AMOUNT. `offerTotal` in
// BadgeService/Catalog.hs is the only implementation of a total in the system;
// D4 serves its result in every offer's `total`; this module copies that number
// into a Selection and view.ts renders it. The one multiplication here is
// `savingOn`, which is a DISPLAY-ONLY comparison and is never sent anywhere —
// see its comment. `POST /api/checkout` (D6) carries `{priceId, offerId?,
// method}` and no amount at all, so the price shown and the price charged
// cannot drift.
//
// Nothing here touches the DOM, so all of it is unit-tested with `node --test`
// (../test/catalog.test.mjs).
const ACTIVE = "active";
const DEPRECATED = "deprecated";
export const TIERS = [
    { badgeType: "supporter", label: "Supporter", perk: "2 GB files" },
    { badgeType: "legend", label: "Legend", perk: "5 GB files" },
];
/** The durations offered, in months. A4 seeds an offer for 3 and 12 only. */
export const DURATIONS = [1, 3, 12];
/** The one duration with no offer: it is charged at the price's `monthPrice`,
 *  and its checkout request carries no `offerId` (D6 reads exactly one month
 *  from that absence). */
export const MONTHS_WITHOUT_OFFER = 1;
/** `CurrencyAmount` is a `Word32` (PaymentService/Types.hs). */
export const MAX_MINOR_UNITS = 4294967295;
const CURRENCY_SYMBOLS = new Map([["usd", "$"]]);
const MINOR_UNITS_IN_MAJOR = 100;
/**
 * A minor-unit amount as text: `700` and `"usd"` render as `$7.00`.
 *
 * This is integer formatting, not arithmetic on a price — the amount is
 * displayed exactly as it arrived. A currency with no symbol renders as its
 * ISO code before the digits (`EUR 12.34`), so an unknown currency is never
 * shown as a bare number that could be read as dollars. The one formatter in
 * the site: no other module formats money.
 */
export function formatAmount(minorUnits, currency) {
    if (!Number.isInteger(minorUnits) || minorUnits < 0 || minorUnits > MAX_MINOR_UNITS) {
        throw new RangeError(`not a minor-unit amount: ${minorUnits}`);
    }
    const major = Math.trunc(minorUnits / MINOR_UNITS_IN_MAJOR);
    const minor = minorUnits % MINOR_UNITS_IN_MAJOR;
    const digits = minor < 10 ? `0${minor}` : `${minor}`;
    const symbol = CURRENCY_SYMBOLS.get(currency.toLowerCase());
    return symbol === undefined ? `${currency.toUpperCase()} ${major}.${digits}` : `${symbol}${major}.${digits}`;
}
/**
 * The price to sell `badgeType` at, or null if there is none to sell.
 *
 * Prefers an `active` row over a `deprecated` one, then the most recently
 * created; a tie keeps the payload's order. Repricing appends a new price and
 * deprecates the old one (UX §3), so a badge type can legitimately have more
 * than one row here and the newest active one is the current price.
 */
export function priceForTier(catalog, badgeType, use) {
    return best(catalog.prices.filter((p) => p.badgeType === badgeType && usable(p.status, use)));
}
/** The offer to sell `months` of `priceId` at, or null if there is none. An
 *  offer the service could not price (`total: null`) is not one. */
export function offerForMonths(catalog, priceId, months, use) {
    return best(catalog.offers.filter((o) => o.priceId === priceId && o.months === months && o.total !== null && usable(o.status, use)));
}
/**
 * The selection for a tier and a duration, or null when that combination
 * cannot be sold.
 *
 * `use` applies to the OFFER. The price is always looked up as `chosen`,
 * because `badgeType` is an answer the visitor has already given — the tier
 * screen is where a deprecated price is kept out of the fresh choices, and by
 * the time a duration is being priced the tier is settled.
 */
export function selectionFor(catalog, badgeType, months, use) {
    const price = priceForTier(catalog, badgeType, "chosen");
    if (!price)
        return null;
    if (months === MONTHS_WITHOUT_OFFER)
        return { price, offer: null, months, total: price.monthPrice };
    const offer = offerForMonths(catalog, price.priceId, months, use);
    // `offer.total` is non-null by offerForMonths' filter; this reads it rather
    // than asserting it, because a total is not a thing to assume.
    return offer && offer.total !== null ? { price, offer, months, total: offer.total } : null;
}
/**
 * DISPLAY ONLY. What the same duration would cost at the monthly price, less
 * what it actually costs — the "you save" line, and the only multiplication in
 * the site.
 *
 * It is a comparison figure, not a price: it is never sent to
 * `/api/checkout`, never stored in a Selection, and never rendered as the
 * amount to pay. `Selection.total` is the amount, and it comes from the
 * service. Null when there is nothing to claim, so a mispriced offer says
 * nothing rather than boasting of a negative saving.
 */
export function savingOn(selection) {
    const undiscounted = selection.months * selection.price.monthPrice;
    return undiscounted > selection.total ? undiscounted - selection.total : null;
}
function usable(status, use) {
    return status === ACTIVE || (use === "chosen" && status === DEPRECATED);
}
// The best of a set of interchangeable rows: active beats deprecated, then
// newest wins, then the payload's order. Total over an empty list.
function best(rows) {
    let chosen = null;
    for (const row of rows)
        if (chosen === null || better(row, chosen))
            chosen = row;
    return chosen;
}
// Ordered by hand rather than by a numeric score: a score combining status and
// a millisecond timestamp needs more than the 53 bits a double holds exactly.
function better(row, than) {
    const active = row.status === ACTIVE;
    if (active !== (than.status === ACTIVE))
        return active;
    // createdAt parses: parseCatalog rejects a row whose timestamp does not.
    return Date.parse(row.createdAt) > Date.parse(than.createdAt);
}
/**
 * The `/api/catalog` payload, validated.
 *
 * A malformed payload is refused whole rather than repaired row by row: a
 * price that cannot be read is not a price to show at a guess. Unknown fields
 * and unknown badge types are ignored, so a newer service can add either
 * without breaking an older site.
 *
 * @throws TypeError naming the field that is wrong.
 */
export function parseCatalog(payload) {
    const root = asObject(payload, "catalog");
    return {
        prices: asArray(root.prices, "prices").map(parsePrice),
        offers: asArray(root.offers, "offers").map(parseOffer),
    };
}
function parsePrice(value, i) {
    const at = `prices[${i}]`;
    const row = asObject(value, at);
    return {
        priceId: asText(row.priceId, `${at}.priceId`),
        badgeType: asText(row.badgeType, `${at}.badgeType`),
        monthPrice: asMinorUnits(row.monthPrice, `${at}.monthPrice`),
        currency: asText(row.currency, `${at}.currency`),
        status: asText(row.status, `${at}.status`),
        createdAt: asTimestamp(row.createdAt, `${at}.createdAt`),
    };
}
function parseOffer(value, i) {
    const at = `offers[${i}]`;
    const row = asObject(value, at);
    return {
        offerId: asText(row.offerId, `${at}.offerId`),
        // Absent means "applies to any price" (A2). B1's getActiveCatalog joins on
        // the price, so the site never sees one; it is read as unpinned, and an
        // unpinned offer matches no priceId and is therefore never selected.
        priceId: row.priceId === undefined || row.priceId === null ? null : asText(row.priceId, `${at}.priceId`),
        months: asMonths(row.months, `${at}.months`),
        status: asText(row.status, `${at}.status`),
        createdAt: asTimestamp(row.createdAt, `${at}.createdAt`),
        total: row.total === undefined || row.total === null ? null : asMinorUnits(row.total, `${at}.total`),
    };
}
function asObject(value, at) {
    if (typeof value !== "object" || value === null || Array.isArray(value))
        throw new TypeError(`${at} is not an object`);
    return value;
}
function asArray(value, at) {
    if (!Array.isArray(value))
        throw new TypeError(`${at} is not an array`);
    return value;
}
function asText(value, at) {
    if (typeof value !== "string" || value === "")
        throw new TypeError(`${at} is not a non-empty string`);
    return value;
}
function asMinorUnits(value, at) {
    if (typeof value !== "number" || !Number.isInteger(value) || value < 0 || value > MAX_MINOR_UNITS) {
        throw new TypeError(`${at} is not a minor-unit amount`);
    }
    return value;
}
// months is a Word8 on the wire, and zero months is not a duration.
const MAX_MONTHS = 255;
function asMonths(value, at) {
    if (typeof value !== "number" || !Number.isInteger(value) || value < 1 || value > MAX_MONTHS) {
        throw new TypeError(`${at} is not a month count`);
    }
    return value;
}
function asTimestamp(value, at) {
    const text = asText(value, at);
    if (Number.isNaN(Date.parse(text)))
        throw new TypeError(`${at} is not a timestamp`);
    return text;
}
export const CATALOG_PATH = "/api/catalog";
const PRICES_UNAVAILABLE = "Prices could not be loaded. Please reload the page, or contact support using the link below.";
/**
 * Fetch the catalog and hand it to the sink, or say so in the banner.
 *
 * One attempt, no retry: a silent retry loop turns a broken service into a
 * page that simply never prices anything, with nothing on screen to say why.
 * Nothing here fails to a blank screen (D2).
 */
export async function loadCatalog(fetchLike, sink) {
    try {
        const response = await fetchLike(CATALOG_PATH);
        if (!response.ok)
            throw new Error(`GET ${CATALOG_PATH} answered ${response.status}`);
        sink.setCatalog(parseCatalog(await response.json()));
    }
    catch (err) {
        // The reason is for whoever opens the console; the banner is for the visitor.
        console.error(err);
        sink.showError(PRICES_UNAVAILABLE);
    }
}
