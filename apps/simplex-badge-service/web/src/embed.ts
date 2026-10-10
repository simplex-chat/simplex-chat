import { readTheme } from "./domain.js";
import type { Theme } from "./domain.js";

export const THEME_MESSAGE = "simplex-theme";
export const EMBED_READY = "simplex-embed-ready";
export const ROUTE_MESSAGE = "simplex-route";
export const HEIGHT_MESSAGE = "simplex-height";
export const COLORS_MESSAGE = "simplex-colors";
export const NEW_PURCHASE_MESSAGE = "simplex-new-purchase";
export const RETURN_URL_MESSAGE = "simplex-return-url";
export const NAV_MESSAGE = "simplex-nav";

export function isNewPurchaseMessage(data: unknown): boolean {
  return typeof data === "object" && data !== null && (data as { type?: unknown }).type === NEW_PURCHASE_MESSAGE;
}

export function bgFromMessage(data: unknown): string | undefined {
  if (typeof data !== "object" || data === null) return undefined;
  const d = data as { type?: unknown; bg?: unknown };
  if (d.type !== COLORS_MESSAGE || typeof d.bg !== "string") return undefined;
  return /^#[0-9a-fA-F]{3,8}$/.test(d.bg) ? d.bg : undefined;
}

const ROUTE_HASHES: readonly string[] = ["", "/", "#/tier", "#/months", "#/checkout", "#/codes"];

export function routeFromMessage(data: unknown): string | undefined {
  if (typeof data !== "object" || data === null) return undefined;
  const d = data as { type?: unknown; hash?: unknown };
  if (d.type !== ROUTE_MESSAGE || typeof d.hash !== "string") return undefined;
  return ROUTE_HASHES.includes(d.hash) ? d.hash : undefined;
}

export function returnUrlFromMessage(data: unknown): string | undefined {
  if (typeof data !== "object" || data === null) return undefined;
  const d = data as { type?: unknown; url?: unknown };
  if (d.type !== RETURN_URL_MESSAGE || typeof d.url !== "string") return undefined;
  try {
    const u = new URL(d.url);
    return u.protocol === "https:" || u.protocol === "http:" ? d.url : undefined;
  } catch {
    return undefined;
  }
}

/** The dotted-suffix check on `.simplex.chat` turns away `simplex.chat.attacker.com` and `notsimplex.chat`. */
export function trustedHost(origin: string): boolean {
  let u: URL;
  try {
    u = new URL(origin);
  } catch {
    return false;
  }
  return u.protocol === "https:" && (u.hostname === "simplex.chat" || u.hostname.endsWith(".simplex.chat"));
}

export function themeFromMessage(data: unknown): Theme | undefined {
  if (typeof data !== "object" || data === null) return undefined;
  const d = data as { type?: unknown; theme?: unknown };
  if (d.type !== THEME_MESSAGE) return undefined;
  return readTheme(d.theme);
}
