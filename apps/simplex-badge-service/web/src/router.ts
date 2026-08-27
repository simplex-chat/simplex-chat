// Hash routing for the site wizard: one question per screen, no page reloads.
//
// Everything here is pure — no DOM, no history, no location — so that the
// mapping a browser depends on can be tested with `node --test` (see
// ../test/router.test.mjs). ui.ts owns pushState, popstate and rendering.

/** Every screen the site can show, in wizard order. */
export const SCREEN_IDS = ["tier", "months", "pay", "checkout", "order", "code"] as const

export type ScreenId = (typeof SCREEN_IDS)[number]

/** The first question of the wizard. Where a visit with no answers starts. */
export const FIRST_SCREEN: ScreenId = "tier"

const SCREENS: ReadonlySet<string> = new Set(SCREEN_IDS)

/**
 * The screen a location hash names, or null when it names none — which
 * includes an absent or empty hash, the ordinary first visit.
 *
 * Accepts `#/tier`, `#tier`, and either with a trailing slash, because a hash
 * is hand-editable and a URL may be pasted with the slash dropped. Null is a
 * real answer, not an error, and deliberately not FIRST_SCREEN: *where to
 * start* is not a property of the hash. The caller decides, and ui.ts starts
 * at the first unanswered screen — which with D5's prefill is not necessarily
 * the first one — rather than leaving a blank page.
 */
export function screenIdForHash(hash: string): ScreenId | null {
  const name = hash.replace(/^#/, "").replace(/^\//, "").replace(/\/$/, "")
  return SCREENS.has(name) ? (name as ScreenId) : null
}

/** The canonical hash for a screen. The inverse of screenIdForHash. */
export function hashForScreen(id: ScreenId): string {
  return `#/${id}`
}

/**
 * The screen after this one, or null at the end of the wizard.
 *
 * `checkout` ends the answered part: `order` and `code` are reached from a
 * checkout response (D7, E5, E6), never by advancing.
 */
export function nextScreen(id: ScreenId): ScreenId | null {
  const wizard: readonly ScreenId[] = ["tier", "months", "pay", "checkout"]
  const i = wizard.indexOf(id)
  return i >= 0 && i + 1 < wizard.length ? wizard[i + 1] : null
}
