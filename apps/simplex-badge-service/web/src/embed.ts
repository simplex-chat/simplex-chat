// Running inside the site's iframe. The host frames this app full-bleed with its own navbar
// hidden, and owns the theme: it posts the buyer's site-wide choice in, this frame echoes its own
// toggle back, and one switch drives both. A theme name is low-risk, but only the site's own origin
// may drive it: an untrusted framer does not get to touch the page.
import { readTheme } from "./domain.js";
import type { Theme } from "./domain.js";

export const THEME_MESSAGE = "simplex-theme";
export const EMBED_READY = "simplex-embed-ready";
export const ROUTE_MESSAGE = "simplex-route";
export const HEIGHT_MESSAGE = "simplex-height"; // frame -> host: our content height (with `min`, the welcome floor), to size the iframe
export const COLORS_MESSAGE = "simplex-colors"; // host -> frame: the site's page background, to match it
export const NEW_PURCHASE_MESSAGE = "simplex-new-purchase"; // host -> frame: the site's "Buy a code", start fresh
// frame -> host: the frame's current SHAREABLE route, announced after each navigation, for the host
// to persist (in its URL) and hand back on the next load. The contract turns on empty vs non-empty:
//   non-empty ("#/tier", "#/codes") — a wizard step or the codes list: shareable, so the host stores
//     it and restores it as a route next load.
//   empty ("")                     — the landing or an order/payment screen: NOT shareable. An order's
//     state lives in the frame's own store (the `?order=` session), never in a URL the host holds, so
//     the host stores nothing to restore and the frame self-resumes (an open order, else the landing).
// This is what stops a host reload from painting a wizard step over a payment the buyer is mid-way
// through: the frame announces "" on the payment screen, so the host has no route to clobber it with.
export const NAV_MESSAGE = "simplex-nav";

/** True for a well-formed new-purchase message from the host: its "Buy a code" navbar item, which
 * starts a fresh purchase rather than routing to a step, so a reload can be told apart from it. */
export function isNewPurchaseMessage(data: unknown): boolean {
  return typeof data === "object" && data !== null && (data as { type?: unknown }).type === NEW_PURCHASE_MESSAGE;
}

/** The background colour a well-formed colours message carries, so the frame's page matches the
 * site's, or undefined to ignore. Only a background is taken; the app keeps its own accent and ink. */
export function bgFromMessage(data: unknown): string | undefined {
  if (typeof data !== "object" || data === null) return undefined;
  const d = data as { type?: unknown; bg?: unknown };
  if (d.type !== COLORS_MESSAGE || typeof d.bg !== "string") return undefined;
  return /^#[0-9a-fA-F]{3,8}$/.test(d.bg) ? d.bg : undefined;
}

// The routes the host may drive: the wizard's hashes, the codes list, and the landing (empty). A
// hash outside this set is ignored, so the host cannot steer the frame to anything it invents.
const ROUTE_HASHES: readonly string[] = ["", "/", "#/tier", "#/months", "#/checkout", "#/codes"];

/** The hash carried by a well-formed route message, or undefined for anything the frame should
 * ignore: a message of another kind, an unknown hash, or something that is not an object. */
export function routeFromMessage(data: unknown): string | undefined {
  if (typeof data !== "object" || data === null) return undefined;
  const d = data as { type?: unknown; hash?: unknown };
  if (d.type !== ROUTE_MESSAGE || typeof d.hash !== "string") return undefined;
  return ROUTE_HASHES.includes(d.hash) ? d.hash : undefined;
}

/** Only simplex.chat and its subdomains, and only over https, may drive the theme. `endsWith` on a
 * dotted suffix, so `simplex.chat.attacker.com` and `notsimplex.chat` are both turned away. */
export function trustedHost(origin: string): boolean {
  let u: URL;
  try {
    u = new URL(origin);
  } catch {
    return false;
  }
  return u.protocol === "https:" && (u.hostname === "simplex.chat" || u.hostname.endsWith(".simplex.chat"));
}

/** The theme carried by a well-formed theme message, or undefined for anything the frame should
 * ignore: a message of another kind, a bad theme name, or something that is not an object. */
export function themeFromMessage(data: unknown): Theme | undefined {
  if (typeof data !== "object" || data === null) return undefined;
  const d = data as { type?: unknown; theme?: unknown };
  if (d.type !== THEME_MESSAGE) return undefined;
  return readTheme(d.theme);
}
