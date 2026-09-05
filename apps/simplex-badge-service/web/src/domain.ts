// The words this app is written in, and the shape of the thing it sells. Nothing here
// knows about HTTP, about localStorage, or about a screen.
import { oneOf } from "./parse.js";

// Each set is written once, as the list; the type is read off it.
export const STEPS = ["tier", "months", "checkout"] as const;
export const THEMES = ["light", "dark", "system"] as const;
const ORDER_STATUSES = ["open", "paid", "expired"] as const;
const METHODS = ["card", "btc", "xmr"] as const;
const CHAINS = ["btc", "xmr"] as const;

export type Step = (typeof STEPS)[number];
export type Theme = (typeof THEMES)[number];
export type OrderStatus = (typeof ORDER_STATUSES)[number];
export type Method = (typeof METHODS)[number];
export type Chain = (typeof CHAINS)[number];

// One reader per set, so the wire and storage are held to the same list.
export const readStep = oneOf(STEPS);
export const readTheme = oneOf(THEMES);
export const readStatus = oneOf(ORDER_STATUSES);
export const readMethod = oneOf(METHODS);
export const readChain = oneOf(CHAINS);

/** The draft of a purchase in progress. The three choices are written with `| undefined`
 * because a patch sets one to undefined to take it back: picking a different tier drops the
 * offer that belonged to the old one. */
export interface SessionRecord {
  step: Step;
  priceId?: string | undefined;
  offerId?: string | undefined;
  method?: Method | undefined;
}

/** Settled when the order is created and never revised. */
export interface OrderIdentity {
  orderId: string;
  badgeType: string;
  months: number;
  createdAt: string;
}

/** What this browser owns: the service never sends these back, so nothing it says can
 * clear them, and a code exists nowhere else. */
export interface OrderLocalState {
  code: string | undefined;
  submitted: boolean | undefined;
  method: Method | undefined;
}

/** Every field the service can send, each a key even when it sends nothing: a builder must
 * decide about all of them, so a withdrawn field cannot survive in the stored record. */
export interface OrderServerState {
  status: OrderStatus;
  amount: number | undefined;
  currency: string | undefined;
  amountPaid: number | undefined;
  cryptoAmountPaid: string | undefined;
  cryptoAmountDue: string | undefined;
  paidInFull: boolean | undefined;
  address: string | undefined;
  cryptoAmount: string | undefined;
  cryptoCurrency: Chain | undefined;
  expiresAt: string | undefined;
}

// The stored form: keys absent rather than undefined, the way JSON gives them back.
type OrderLocalFields = Partial<OrderLocalState>;
type OrderServerFields = Partial<OrderServerState> & { status: OrderStatus };

export type OrderRecord = OrderIdentity & OrderLocalFields & OrderServerFields;

function identityOf(o: OrderIdentity): OrderIdentity {
  const { orderId, badgeType, months, createdAt } = o;
  return { orderId, badgeType, months, createdAt };
}

/** The one cast either side of the complete/sparse boundary lives here. */
export function definedOnly<Complete extends object, Sparse>(all: Complete): Sparse {
  const present = Object.entries(all).filter(([, v]) => v !== undefined);
  return Object.fromEntries(present) as unknown as Sparse;
}

/** Three groups covering every key and none twice, so nothing carries over by accident. */
export function composeOrder(
  identity: OrderIdentity,
  local: OrderLocalState,
  server: OrderServerState,
): OrderRecord {
  return definedOnly({ ...identityOf(identity), ...local, ...server });
}
