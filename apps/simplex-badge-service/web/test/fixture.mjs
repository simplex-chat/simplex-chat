// A catalog payload to render against, in the wire shape `GET /api/catalog`
// serves (the RPC BadgeCatalog encoding, A2). Tests parse it with the site's
// own parseCatalog, so a fixture drives the whole path a real payload takes.
//
// THE TOTALS ARE DELIBERATELY NOT PRODUCTS. A4 prices 3 supporter months at
// 2 x 700 = 1400 and 12 at 6 x 700 = 4200, and a site that multiplied months
// by the monthly price itself would land on 1400, 2100, 4200 or 8400 and look
// right. 1301 and 4207 are none of those, so any arithmetic the browser does
// on the price shows up as a wrong number rather than as a coincidence. The
// savings against the undiscounted 3 x 700 and 12 x 700 are 799 and 4193, and
// they are not round either.

const CREATED = "2026-08-01T09:00:00Z"

export const SUPPORTER_PRICE_ID = "price-supporter"
export const LEGEND_PRICE_ID = "price-legend"

/** A fresh, mutable payload: a test may delete a price or edit a total. */
export function catalogPayload() {
  return {
    prices: [
      {
        priceId: SUPPORTER_PRICE_ID,
        badgeType: "supporter",
        monthPrice: 700,
        currency: "usd",
        status: "active",
        createdAt: CREATED,
      },
      {
        priceId: LEGEND_PRICE_ID,
        badgeType: "legend",
        monthPrice: 7000,
        currency: "usd",
        status: "active",
        createdAt: CREATED,
      },
    ],
    offers: [
      offer("offer-supporter-3", SUPPORTER_PRICE_ID, 3, 1301),
      offer("offer-supporter-12", SUPPORTER_PRICE_ID, 12, 4207),
      offer("offer-legend-3", LEGEND_PRICE_ID, 3, 13001),
      offer("offer-legend-12", LEGEND_PRICE_ID, 12, 42007),
    ],
  }
}

/**
 * The catalog A4 actually seeds, with the totals its `offerTotal` computes:
 * $7 and $70 a month, one free month in three and six in twelve (UX §1, §6.12).
 * The fixture above is for proving the site copies what it is served; this one
 * is for reading the shipped prices back in the shipped copy.
 */
export function seededPayload() {
  const payload = catalogPayload()
  const totals = {"offer-supporter-3": 1400, "offer-supporter-12": 4200, "offer-legend-3": 14000, "offer-legend-12": 42000}
  for (const o of payload.offers) o.total = totals[o.offerId]
  return payload
}

export function offer(offerId, priceId, months, total, status = "active", createdAt = CREATED) {
  // `discount` rides along unread: the site prices from `total` alone, and a
  // fixture that omitted the field would not prove that.
  return {offerId, priceId, months, discount: {type: "freeMonths", freeMonths: 1}, status, createdAt, total}
}

export function priceOf(payload, priceId) {
  const price = payload.prices.find((p) => p.priceId === priceId)
  if (!price) throw new Error(`no price ${priceId} in the fixture`)
  return price
}

export function offerOf(payload, offerId) {
  const found = payload.offers.find((o) => o.offerId === offerId)
  if (!found) throw new Error(`no offer ${offerId} in the fixture`)
  return found
}
