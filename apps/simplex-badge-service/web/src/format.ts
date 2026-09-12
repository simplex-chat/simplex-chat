// Turning a number or a timestamp into the words a screen prints. No state, no clock of its
// own: the caller passes the moment, so a rendered screen is a function of what it was given.

const SYMBOLS: Readonly<Record<string, string>> = { usd: "$", eur: "€", gbp: "£" };

export function money(minor: number, currency: string): string {
  const symbol = SYMBOLS[currency.toLowerCase()];
  const figure = (minor / 100).toFixed(2);
  return symbol ? `${symbol}${figure}` : `${figure} ${currency.toUpperCase()}`;
}

export function moneyCompact(minor: number, currency: string): string {
  if (minor % 100 !== 0) return money(minor, currency);
  const symbol = SYMBOLS[currency.toLowerCase()];
  const figure = String(Math.trunc(minor / 100));
  return symbol ? `${symbol}${figure}` : `${figure} ${currency.toUpperCase()}`;
}

export function countdown(expiresAt: string | undefined, nowMs: number): string | null {
  if (expiresAt === undefined) return null;
  const end = Date.parse(expiresAt);
  if (Number.isNaN(end)) return null;
  const left = Math.floor((end - nowMs) / 1000);
  if (left <= 0) return null;
  const s = left % 60;
  const m = Math.floor(left / 60) % 60;
  const h = Math.floor(left / 3600);
  const pad = (n: number) => String(n).padStart(2, "0");
  return h > 0 ? `${h}:${pad(m)}:${pad(s)}` : `${pad(m)}:${pad(s)}`;
}

/** What the provider says is still owed. Three answers because the screen says something
 * different for each: a covered invoice comes back as "0.00000000", and asking a buyer to send
 * zero is worse than asking for nothing at all. */
export type Outstanding =
  | { kind: "owed"; amount: string }
  | { kind: "covered" }
  | { kind: "unknown" };

export function outstanding(due: string | undefined): Outstanding {
  if (due === undefined || !/^\d+(\.\d+)?$/.test(due)) return { kind: "unknown" };
  return /[1-9]/.test(due) ? { kind: "owed", amount: due } : { kind: "covered" };
}

export function startedAgo(createdAt: string, nowMs: number): string | null {
  const began = Date.parse(createdAt);
  if (Number.isNaN(began)) return null;
  const minutes = Math.floor((nowMs - began) / 60_000);
  if (minutes < 1) return "Started less than a minute ago.";
  if (minutes === 1) return "Started 1 minute ago.";
  if (minutes < 60) return `Started ${minutes} minutes ago.`;
  const hours = Math.floor(minutes / 60);
  return hours === 1 ? "Started 1 hour ago." : `Started ${hours} hours ago.`;
}
