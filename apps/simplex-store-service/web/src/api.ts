import { readChain, readStatus, type Chain, type Method, type OrderStatus } from "./domain.js";
import { asObject, fieldsInto, filledText, flag, oneOf, positiveInteger, text, nonNegativeInteger } from "./parse.js";

const WIRE_ERROR_CODES = [
  "catalog_changed", "bad_request", "code_conflict",
  "rate_limited", "internal", "provider_unavailable", "not_found",
  "not_open", "funded", "method_not_allowed",
] as const;
const LOCAL_ERROR_CODES = ["invalid_response", "unknown"] as const;

export type ErrorCode = (typeof WIRE_ERROR_CODES)[number] | (typeof LOCAL_ERROR_CODES)[number];

export class ApiError extends Error {
  constructor(readonly code: ErrorCode, readonly status: number, readonly retryAfter?: number) {
    super(`${code} (${status})`);
    this.name = "ApiError";
  }
}

export class AbortedError extends Error {
  constructor() {
    super("the wait was aborted");
    this.name = "AbortedError";
  }
}

const RETRY_AFTER_MAX_SECONDS = 300;

// The five-minute cap keeps a hostile Retry-After from parking the page for hours.
function retryAfterSeconds(res: Response): number | undefined {
  const raw = res.headers.get("retry-after")?.trim();
  if (!raw || !/^\d+$/.test(raw)) return undefined;
  const seconds = Number(raw);
  return seconds > 0 ? Math.min(seconds, RETRY_AFTER_MAX_SECONDS) : undefined;
}

async function drain(res: Response): Promise<void> {
  try {
    await res.text();
  } catch { /* nothing left to reclaim */ }
}

export interface CreateRequest {
  priceId: string;
  offerId?: string;
  method: Method;
  codeHash: string;
}

export interface CreatedInvoice {
  invoiceId: string;
  badgeType: string;
  months: number;
  amount: number;
  currency: string;
  expiresAt: string;
  clientSecret?: string;
  address?: string;
  cryptoAmount?: string;
  cryptoCurrency?: Chain;
}

export interface InvoiceView {
  status: OrderStatus;
  amountPaid?: number;
  cryptoAmountPaid?: string;
  cryptoAmountDue?: string;
  settledAt?: string;
  badgeType?: string;
  months?: number;
  amount?: number;
  currency?: string;
  expiresAt?: string;
  clientSecret?: string;
  address?: string;
  cryptoAmount?: string;
  cryptoCurrency?: Chain;
  paidInFull?: boolean;
  requiredConfirmations?: number;
}

export function inferMethod(view: Pick<InvoiceView, "clientSecret" | "cryptoCurrency">): Method | undefined {
  if (view.clientSecret !== undefined) return "card";
  if (view.cryptoCurrency !== undefined) return view.cryptoCurrency;
  return undefined;
}

// A proxy or CDN in front of the service can return its own 404 body, so the status alone decides not_found.
async function decodeError(res: Response): Promise<never> {
  let code: ErrorCode = "unknown";
  try {
    code = wireError(asObject(await res.json())?.error) ?? "unknown";
  } catch { /* an empty or unparseable body stays "unknown" */ }
  if (res.status === 404) code = "not_found";
  throw new ApiError(code, res.status, retryAfterSeconds(res));
}

const wireError = oneOf(WIRE_ERROR_CODES);

function matchesRequestedMethod(
  method: Method,
  clientSecret: string | undefined,
  address: string | undefined,
  cryptoAmount: string | undefined,
  cryptoCurrency: Chain | undefined,
): boolean {
  if (method === "card") {
    return clientSecret !== undefined && address === undefined && cryptoAmount === undefined && cryptoCurrency === undefined;
  }
  return clientSecret === undefined && address !== undefined && cryptoAmount !== undefined && cryptoCurrency === method;
}

function parseCreatedInvoice(body: unknown, status: number, requestedMethod: Method): CreatedInvoice {
  const b = asObject(body);
  const refuse = (): never => { throw new ApiError("invalid_response", status); };
  if (b === undefined) return refuse();
  const invoiceId = filledText(b.invoiceId);
  const badgeType = filledText(b.badgeType);
  const months = positiveInteger(b.months);
  const amount = positiveInteger(b.amount);
  const currency = filledText(b.currency);
  const expiresAt = filledText(b.expiresAt);
  if (invoiceId === undefined || badgeType === undefined ||
      months === undefined || amount === undefined || currency === undefined || expiresAt === undefined) {
    return refuse();
  }
  const optional: Partial<CreatedInvoice> = {};
  const take = fieldsInto<CreatedInvoice>(b, optional);
  const read =
    take("clientSecret", text) && take("address", text) &&
    take("cryptoAmount", text) && take("cryptoCurrency", readChain);
  if (!read) return refuse();
  const { clientSecret, address, cryptoAmount, cryptoCurrency } = optional;
  if (!matchesRequestedMethod(requestedMethod, clientSecret, address, cryptoAmount, cryptoCurrency)) return refuse();
  return { invoiceId, badgeType, months, amount, currency, expiresAt, ...optional };
}

export function parseInvoiceView(body: unknown): InvoiceView | null {
  const b = asObject(body);
  if (b === undefined) return null;
  const status = readStatus(b.status);
  if (status === undefined) return null;
  const rest: Partial<InvoiceView> = {};
  const take = fieldsInto<InvoiceView>(b, rest);
  const read =
    take("amountPaid", nonNegativeInteger) && take("cryptoAmountPaid", text) && take("cryptoAmountDue", text) && take("settledAt", text) &&
    take("badgeType", text) && take("months", positiveInteger) && take("amount", positiveInteger) &&
    take("currency", text) && take("expiresAt", text) &&
    take("clientSecret", text) && take("address", text) && take("cryptoAmount", text) &&
    take("cryptoCurrency", readChain) && take("paidInFull", flag) &&
    take("requiredConfirmations", nonNegativeInteger);
  return read ? { status, ...rest } : null;
}

export async function createInvoice(req: CreateRequest, f: typeof fetch = fetch): Promise<CreatedInvoice> {
  const body: Record<string, unknown> = { priceId: req.priceId, method: req.method, codeHash: req.codeHash };
  if (req.offerId !== undefined) body.offerId = req.offerId;
  const res = await f("/api/invoice", {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(body),
  });
  if (!res.ok) return decodeError(res);
  let raw: unknown;
  try {
    raw = await res.json();
  } catch {
    throw new ApiError("invalid_response", res.status);
  }
  return parseCreatedInvoice(raw, res.status, req.method);
}

async function readView(res: Response): Promise<InvoiceView> {
  if (!res.ok) return decodeError(res);
  let raw: unknown;
  try {
    raw = await res.json();
  } catch {
    throw new ApiError("invalid_response", res.status);
  }
  const view = parseInvoiceView(raw);
  if (view === null) throw new ApiError("invalid_response", res.status);
  return view;
}

export async function cancelInvoice(orderId: string, f: typeof fetch = fetch): Promise<InvoiceView> {
  return readView(await f(`/api/invoice/${encodeURIComponent(orderId)}/cancel`, { method: "POST" }));
}

export async function readInvoice(invoiceId: string, f: typeof fetch = fetch, signal?: AbortSignal): Promise<InvoiceView> {
  return readView(await f(`/api/invoice/${encodeURIComponent(invoiceId)}`, signal ? { signal } : undefined));
}

export type Sleep = (ms: number, signal?: AbortSignal) => Promise<void>;

export const realSleep: Sleep = (ms, signal) => {
  if (signal?.aborted) return Promise.reject(new AbortedError());
  return new Promise<void>((resolve, reject) => {
    const timer = setTimeout(() => {
      signal?.removeEventListener("abort", onAbort);
      resolve();
    }, ms);
    const onAbort = () => {
      clearTimeout(timer);
      reject(new AbortedError());
    };
    signal?.addEventListener("abort", onAbort, { once: true });
  });
};

export const BACKOFF_START = 1000;
export const BACKOFF_MAX = 30_000;
const FAST_THRESHOLD_MS = 5000;
const FAST_LIMIT = 3;

export interface PaymentMark {
  paid: string | undefined;
  paidInFull: boolean | undefined;
}

export const NOTHING_RECEIVED: PaymentMark = { paid: undefined, paidInFull: undefined };

export function paymentMark(view: InvoiceView): PaymentMark {
  return { paid: view.cryptoAmountPaid, paidInFull: view.paidInFull };
}

function changed(now: PaymentMark, seen: PaymentMark): boolean {
  return (now.paid ?? "") !== (seen.paid ?? "") || (now.paidInFull === true) !== (seen.paidInFull === true);
}

export async function waitForChange(
  invoiceId: string,
  seen: OrderStatus,
  f: typeof fetch = fetch,
  sleep: Sleep = realSleep,
  signal?: AbortSignal,
  now: () => number = Date.now,
  seenPayment: PaymentMark = NOTHING_RECEIVED,
): Promise<InvoiceView> {
  if (seen === "paid") {
    throw new Error("waitForChange: 'paid' is terminal; there is nothing left to wait for");
  }
  let backoff = BACKOFF_START;
  let fastRepeats = 0;
  const backOff = async (): Promise<void> => {
    await sleep(backoff, signal);
    backoff = Math.min(backoff * 2, BACKOFF_MAX);
  };
  for (;;) {
    if (signal?.aborted) throw new AbortedError();
    const startedAt = now();
    let res: Response;
    try {
      const query = `wait=${encodeURIComponent(seen)}`
        + `&seenPaid=${encodeURIComponent(seenPayment.paid ?? "")}`
        + `&seenFull=${seenPayment.paidInFull === true ? "1" : "0"}`;
      res = await f(`/api/invoice/${encodeURIComponent(invoiceId)}?${query}`, signal ? { signal } : undefined);
    } catch {
      await backOff();
      continue;
    }
    if (res.status === 404) return decodeError(res);
    if (res.status === 429) {
      const retryAfter = retryAfterSeconds(res);
      await drain(res);
      if (retryAfter !== undefined) {
        await sleep(retryAfter * 1000, signal);
        continue;
      }
      await backOff();
      continue;
    }
    if (!res.ok) {
      await drain(res);
      await backOff();
      continue;
    }
    let view: InvoiceView;
    try {
      const raw = await res.json();
      const parsed = parseInvoiceView(raw);
      if (!parsed) throw new Error("malformed GET 200 body");
      view = parsed;
    } catch {
      await backOff();
      continue;
    }
    if (view.status !== seen || changed(paymentMark(view), seenPayment)) return view;

    fastRepeats = (now() - startedAt) < FAST_THRESHOLD_MS ? fastRepeats + 1 : 0;
    if (fastRepeats >= FAST_LIMIT) {
      await backOff();
    } else {
      backoff = BACKOFF_START;
    }
  }
}
