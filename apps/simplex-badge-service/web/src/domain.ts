import { oneOf } from "./parse.js";

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

export const readStep = oneOf(STEPS);
export const readTheme = oneOf(THEMES);
export const readStatus = oneOf(ORDER_STATUSES);
export const readMethod = oneOf(METHODS);
export const readChain = oneOf(CHAINS);

/** The optional fields are written `| undefined` so a patch can set one back to undefined to clear it. */
export interface SessionRecord {
  step: Step;
  priceId?: string | undefined;
  offerId?: string | undefined;
  method?: Method | undefined;
}

export interface OrderIdentity {
  orderId: string;
  badgeType: string;
  months: number;
  createdAt: string;
}

/** The service never sends these fields back, so nothing it returns can clear them. */
export interface OrderLocalState {
  code: string | undefined;
  submitted: boolean | undefined;
  canceled: boolean | undefined;
  method: Method | undefined;
}

/** Every field is a required key even when undefined, so a withdrawn field cannot survive in the stored record. */
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

type OrderLocalFields = Partial<OrderLocalState>;
type OrderServerFields = Partial<OrderServerState> & { status: OrderStatus };

export type OrderRecord = OrderIdentity & OrderLocalFields & OrderServerFields;

function identityOf(o: OrderIdentity): OrderIdentity {
  const { orderId, badgeType, months, createdAt } = o;
  return { orderId, badgeType, months, createdAt };
}

export function definedOnly<Complete extends object, Sparse>(all: Complete): Sparse {
  const present = Object.entries(all).filter(([, v]) => v !== undefined);
  return Object.fromEntries(present) as unknown as Sparse;
}

export function composeOrder(
  identity: OrderIdentity,
  local: OrderLocalState,
  server: OrderServerState,
): OrderRecord {
  return definedOnly({ ...identityOf(identity), ...local, ...server });
}
