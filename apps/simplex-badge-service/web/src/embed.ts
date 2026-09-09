// Running inside the site's iframe. The host frames this app full-bleed with its own navbar
// hidden, and owns the theme: it posts the buyer's site-wide choice in, this frame echoes its own
// toggle back, and one switch drives both. A theme name is low-risk, but only the site's own origin
// may drive it: an untrusted framer does not get to touch the page.
import { readTheme } from "./domain.js";
import type { Theme } from "./domain.js";

export const THEME_MESSAGE = "simplex-theme";
export const EMBED_READY = "simplex-embed-ready";

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
