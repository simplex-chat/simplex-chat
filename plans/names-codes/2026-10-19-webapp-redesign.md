# Names webapp redesign: simpler, in the simplex.domains look

Status: approved direction (owner, 2026-10-19). Two details were taken as defaults while the owner was away and are easy to change: the "Your names" header link and the checkout block order.

## Goal

The names webapp (`simplex.domains/webapp`) looks like simplex.domains and works like the badges webapp (`simplex-chat/apps/simplex-badge-service/web`): one column, one action per screen, few screens, little code. Source goes from about 8.8k lines to about 5.5k.

## Decisions (owner, 2026-10-19)

| Topic | Decision |
|---|---|
| Visual design | simplex.domains tokens: colours, gradients, fonts, type scale, pill buttons, light and dark |
| Home | The current simplex.domains page minus the countdown, plus the search |
| Motion | The badges model: the purchase steps slide on one CSS track, other screens swap whole; no view transitions, no in-place diffing |
| Purchase flow | Steps a buyer expects, one per screen, each centred and fitting the window without scrolling: Name (search) → Links (optional, Skip) → Your name code (save it; "I have saved it" enables Continue) → Check your order (summary incl. links, Pay with, "I have a code", Pay) → Payment → Registering → "is yours" (owner, 2026-10-19 evening) |
| Prepaid codes | Entered at checkout and in Your names only; no header popup, no applied-code banner |
| Links | Set before paying, in their own optional step; editable later on Edit links |
| Taken after paying | The retarget flow stays |

## 1. Look and frame

Tokens come from `simplex.domains/index.html`. The badges app already maps them onto app components (`badges web/public/styles.css`), and this design follows that mapping.

- Fonts:
  - GT-Walsheim 700 for headings, with the vertical heading gradient (`#53C1FF -50%` → `#0053D0 160%`; cyan in dark).
  - Manrope for text: 18px body; 500 for leads, notes and soft text; 600 for buttons and uppercase labels at .18em tracking.
- Colours:
  - Ground `#F3FAFF` with the landing's two radial gradients.
  - Ink `#0D1B3A`, soft ink at .651.
  - Primary `#0053D0`, accent `#0197FF`.
  - Surface white, surface line `rgba(0,83,208,.14)`.
  - Dark palette as `html.dark` on the landing (ground `#000832`, primary `#70F0F9`, surface `#0A1C44`).
  - Status colours (ok, warn, danger) as in the badges app.
- Components:
  - Buttons: flat pill, radius 999px, padding 16px 32px, 18px/600, hover `brightness(1.06)`, full width up to the column.
  - Cards: 1px surface line, radius 24px, no shadow.
  - The QR card and the name code frame use the same card.
  - Inputs: pills like the landing's email field.
- Frame:
  - The landing's header: logo and theme icon, not sticky, no bar, plus a "Your names" text link next to the theme icon on every page (default).
  - The landing's footer: "Pay with" marks and the links row.
  - Inner pages: one centred 640px column with a Back link above the heading.
- Removed:
  - the sticky header bar and phone menu;
  - the grid background, the violet gradient, gradient-and-glow buttons and card shadows;
  - the "NEW" pill, the how-it-works tiles and their animated scene;
  - the five-step progress bar, skeleton shimmer and the `@property` theme easing.

## 2. Screens and flow

**Home** is the simplex.domains page with the countdown replaced by the search field:
- the headline "Your SimpleX domain" and its subtitle;
- the "Invest to claim your name" button and its note;
- the footer.

The search answer appears under the field in the same column:
- Available: name, price, term, Register.
- Taken: who it points at.
- Reserved, invalid, could not be checked: one line each.
- Rate limited: the countdown line, Search disabled.

**Checkout** (one page), blocks in this order (default):
1. Summary card: name, term, registered until, total.
2. Optional links: contact address and channel, with their previews.
3. Payment method: Card, Bitcoin, Monero.
4. "I have a code": a field that, when valid, makes the total $0.
5. Your name code: the code, Copy, and the "I have saved my name code" checkbox.
6. One Pay button, enabled once the code is saved.

**Payment and registration**:
- Crypto: QR, amount, address with Copy, the rate countdown.
- Card: the Stripe form.
- Then confirming and registering, with the stage tracker and its countdown kept.
- Then "is yours": the name, until when, the name code again, and the next step in the app.

**Order outcomes**: one screen each, in the badges pattern (heading, one notice, one button):
- "Invoice closed" replaces the six expired, cancelled, still-open and paid-late variants. It keeps the refund and "don't send more" wording where the invoice saw a payment.
- "Card payment not confirmed" replaces card-failed and slow-card.
- Taken after paying: the retarget flow, as today.
- Failed registration: Try again, or the code refusal with a way back to search.
- A rate limit is one screen with a countdown.
- Unknown link, not on this device, unreadable: one screen each.

**Your names** is the rows from storage, read once when opened (no follow timers, no staggered rows). It also has:
- Add a code;
- Show code, inline in the row;
- Edit links;
- Restore;
- Forget, which asks on the page itself, not in a `window.confirm`.

## 3. Code

- **Deleted:**
  - `src/motion.ts`, `src/motion-values.ts` and the view-transition and keyframe CSS;
  - the `{sections, update}` screen contract and its helpers (`drawAs`, `changeTo`, `syncList`, `live`, slot carry);
  - the `moves` transition tables and the route-to-motion matrix;
  - focus restoration in `src/focus.ts`: the new screen's heading takes focus, as in badges;
  - the popups (`src/views/codes.ts` as a popup), skeletons, button spinner crossfade, offline-note footer reservation, phone menu, and the step bar.
- **Kept:**
  - `src/screens/*`: the pure reducers and screen-for-state functions, unit-tested, minus the transition tables.
  - Unchanged: `crypto/*`, `api.ts`, `flow.ts`, `store.ts`, `session.ts`, `qr.ts`, `stripe.ts`, `sw/`.
- **Views:**
  - Each screen is a builder from state to nodes, swapped whole with `replaceChildren`.
  - Inputs the user is typing in (search, links, code) keep their node across redraws.
  - The purchase steps sit on one track that slides by CSS transform, as `badges main.ts:265-466`.
- **Target:** about 5.5k lines of source, with `styles.css` at about 600 lines.

## 4. Tests and verification

- **Vitest:** reducers and screen functions keep their tests, minus the transition-table tests.
- **e2e:**
  - Kept: the functional specs (search, codes, paid, taken, robustness, offline, focus basics), updated to the new screens.
  - Deleted: `motion`, `cuts`, `crossing`, `sweep` and the four `inplace` specs.
  - Added: one check that the track slide and screen swaps show no blank frame, run in Chromium and Firefox 158.
- **Review rounds:** run again on the result until every reviewer reports no issues:
  - e2e in Chromium, Firefox 157 and WebKit;
  - chain against the local contract;
  - design against the landing in Chromium and Firefox 158;
  - motion;
  - code.

The round-65 findings that survive the redesign are carried into it:
- a failed or slow code check shows the neutral "Trying again spends nothing more." line;
- a "Not checked" row is read again;
- Monero addresses wrap evenly;
- "name.simplex" is folded in the field.
