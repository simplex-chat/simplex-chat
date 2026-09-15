export type BadgeType = "supporter" | "legend";

export interface Price {
  priceId: string;
  badgeType: BadgeType;
  monthPrice: number; // minor units
  currency: string;
}

export type Discount =
  | { type: "freeMonths"; freeMonths: number }
  | { type: "discount"; discount: number };

export interface Offer {
  offerId: string;
  priceId: string;
  months: number;
  discount: Discount;
}

export interface Catalog {
  prices: readonly Price[];
  offers: readonly Offer[];
}

export const CATALOG: Catalog = {
  prices: [
    { priceId: "price_supporter", badgeType: "supporter", monthPrice: 700, currency: "usd" },
    { priceId: "price_legend", badgeType: "legend", monthPrice: 7000, currency: "usd" },
  ],
  offers: [
    { offerId: "offer_3m", priceId: "price_legend", months: 3, discount: { type: "freeMonths", freeMonths: 1 } },
    { offerId: "offer_12m", priceId: "price_legend", months: 12, discount: { type: "discount", discount: 50 } },
    { offerId: "offer_3m_s", priceId: "price_supporter", months: 3, discount: { type: "freeMonths", freeMonths: 1 } },
    { offerId: "offer_12m_s", priceId: "price_supporter", months: 12, discount: { type: "discount", discount: 50 } },
  ],
};

// This key names no row in CATALOG.offers, and the request must omit the offerId when it is chosen.
export const SINGLE_MONTH = "1m";

export interface Total { months: number; price: number; amount: number }

const MAX_AMOUNT = 100_000_000; // $1,000,000 in minor units

export function offerTotal(monthPrice: number, offer: Offer | undefined): Total | string {
  if (!Number.isInteger(monthPrice) || monthPrice < 0) return "bad month price";
  if (offer === undefined) return charge(1, monthPrice);
  if (!Number.isInteger(offer.months) || offer.months < 0) return "months must be a non-negative integer";
  if (offer.months <= 0) return "zero months";
  const gross = monthPrice * offer.months;
  if (offer.discount.type === "freeMonths") {
    if (!Number.isInteger(offer.discount.freeMonths) || offer.discount.freeMonths < 0) return "free months must be a non-negative integer";
    if (offer.discount.freeMonths >= offer.months) return "free months exceed the term";
    return charge(offer.months, monthPrice * (offer.months - offer.discount.freeMonths), gross);
  }
  if (!Number.isInteger(offer.discount.discount) || offer.discount.discount < 0) return "discount must be a non-negative integer";
  if (offer.discount.discount >= 100) return "discount too large";
  return charge(offer.months, Math.floor((gross * (100 - offer.discount.discount)) / 100), gross);
}

// The full price is checked against the cap as well as the charge, because a total the page shows as sellable would otherwise be refused at checkout.
function charge(months: number, amount: number, price = amount): Total | string {
  if (amount <= 0 || amount > MAX_AMOUNT || price > MAX_AMOUNT) return "amount unsellable";
  return { months, price, amount };
}

export function savingPercent(price: number, amount: number): number {
  if (price <= 0) return 0;
  return Math.round(((price - amount) / price) * 100);
}
