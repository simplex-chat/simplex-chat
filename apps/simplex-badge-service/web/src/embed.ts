// Running inside the site's iframe. The host frames this app full-bleed with its own navbar
// hidden, and owns the theme: it posts the buyer's site-wide choice in, this frame echoes its own
// toggle back, and one switch drives both. A theme name is low-risk, but only the site's own origin
// may drive it: an untrusted framer does not get to touch the page.
import { readTheme } from "./domain.js";
import type { Theme } from "./domain.js";

export const THEME_MESSAGE = "simplex-theme";
export const EMBED_READY = "simplex-embed-ready";
export const ROUTE_MESSAGE = "simplex-route";
export const HEIGHT_MESSAGE = "simplex-height"; // frame -> host: our content height, so it can size the iframe
export const COLORS_MESSAGE = "simplex-colors"; // host -> frame: the site's page background, to match it

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
