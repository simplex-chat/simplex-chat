# Names webapp: pages that change in place — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers-extended-cc:subagent-driven-development (recommended) or superpowers-extended-cc:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Every in-page state change of the names webapp updates only the nodes whose state changed; only a move to another screen is a page transition.

**Architecture:** Today every state change rebuilds the whole page (`page()` with fresh DOM) and `motion.ts` compares old and new pages (signatures, slots, runtime view-transition names, Firefox overlays) to make the swap look partial. That comparison is the source of every flicker reported. The new model: a view computes its state and calls `screen(kind, state, build)`; the screen of the same kind already shown gets `update(state)` and changes its own nodes (class changes animated by CSS transitions, `live()` cross-fades, `rollTo`, keyed list sync with `addTo`/`removeFrom`); another kind is built once and shown by `page()` as a page change. Page changes use view transitions with statically named shared elements (header, footer, step bar, order card, order name) in Chromium and WebKit, and a fade-through in Firefox. The comparison machinery is then deleted.

**Tech Stack:** Vite + TypeScript (vanilla DOM), Web Animations API, CSS transitions, View Transitions API, Playwright e2e (Chromium, Firefox, WebKit), vitest (node, no DOM).

**Spec:** This document (the owner approved the approach in conversation on 2026-10-17: "Proceed with your plan please, make the architecture correct and modern"). Context: `plans/names-codes/2026-10-09-names-web-store-plan.md`.

## Global Constraints

- Code lives in `/project/git/simplex.domains/webapp`; never commit unless the owner asks.
- Follow CLAUDE.md: comments rare, short, no plan references; named constants; no dead code; fix every warning; English only.
- In-page change = same route token family (a `refresh()` redraw or a poll answer) and same screen kind. It MUST NOT call `page()`, MUST NOT start a view transition, and MUST keep the DOM nodes of everything that did not change (same object identity).
- Screen change (another kind, or another route) = one page transition: view transition with types `forward` / `back` / `screen` / `fill` / `theme` in browsers where `transitions()` is true; elsewhere (Firefox, `motion.ts` `VIEW_TRANSITIONS` false) the fade-through in `swapPage(..., "through")`.
- View transitions name only static shared elements: `page-top`, `page-foot`, `offline-note`, `page` (the main content), `page-title` (`.page-head h1`), `page-lead` (`.page-head .lead`), `crumbs`, `step-ring`, transient `step-line-*` / `step-done-*` from `crumbs()`, `order-card` (`.summary`), `page-panel` (the main column's first `.panel`), `order-name`. No runtime pairing, no `part-N` / `gone-N` / `fresh-N` names.
- Reduced motion: every change applies at once (`moving()` false), as today.
- Timers and listeners that belong to a screen stop when the screen is replaced or the route is left (the screen's `AbortSignal`), not when a redraw of the same screen happens.
- Focus: an update keeps focus on the node that had it (the node survives); a screen change focuses the new heading, as today (`focus.focusHeading`).
- Determinism (owner, 2026-10-17: "implement all three, everything should be deterministic"): each view's state is one object changed only by `dispatch(event)` through a pure `reduce(state, event)`; a pure `screenOf(state)` picks the screen; a per-view transition table (`moves`) gives each screen-to-screen move its motion. Reducers and `screenOf` never read the clock, randomness, storage, the network or the DOM: time and outside data arrive inside events. Effects (API calls, storage writes, timers, focus) run in the view's controller, and their results come back as events. A move missing from the table is logged with `console.warn("[screens] <view>: unexpected move <from> → <to>")` and still shown as a page change. The pure modules live in `src/screens/` and are covered by vitest: every `screenOf` branch and every table row.
- e2e must pass in Chromium, Firefox and WebKit (`npx playwright test --project=<browser>`), with the motion checks of `e2e/cuts.ts` (no flash, cut-in, cut-out, spike, jump, shift, scroll-jump).

- Every page-change animation that exists today is preserved (owner, 2026-10-17): header, footer and ground stay still and the footer moves to the new height; the order's name travels from the search result into the order card (`order-name`); the step bar's pills stay while the ring moves on (`step-ring`), the line to the reached step fills (`step-line-*`) and the step left turns to a growing tick (`step-done-*`); the order card stays and morphs across checkout → name code → payment → registering; headings and leads change in place; the main panel morphs; content that differs slides out and in the way the reader goes (`forward`/`back`) or fades within a page; the theme is one cross-fade.

**User decisions (already made):**
- "Proceed with your plan please, make the architecture correct and modern" (2026-10-17): views update in place; page transitions only between screens; delete the redraw/diff machinery.
- Firefox before 158 does not use view transitions (bug 2066829, fixed in 158); it changes pages by the fallback (fade through, shared parts moved by FLIP). Firefox 158 and newer use view transitions like Chromium (owner updated to 158, 2026-10-17: "Yes, proceed").
- Order/registering page first, then checkout, Your names, search.
- Pure screen functions, a transition table per view, and one reducer per view, all unit-tested; everything deterministic (2026-10-17).

---

## File structure

| File | Responsibility after the change |
|---|---|
| `src/views/shell.ts` | Router glue, header/footer, popups, `page()` (screen change only), new `screen()` primitive and the shown screen's lifetime |
| `src/motion.ts` | In-place motion (`live`, `crossFade`, `rollTo`, `addTo`, `removeFrom`, height easing, new `syncList`), page change (`transition`, `swapPage`), no comparison code |
| `src/views/order.ts` | Order page: `follow()` loop computes state and calls `screen()` with kinds `pay-crypto`, `pay-card`, `confirming`, `slow`, `registering`, `registered`, `taken`, `code-issued`, `failed`, `not-on-device`, `status:<id>`; each kind has `build` and, where its state changes in place, `update` |
| `src/views/checkout.ts` | Checkout and name-code screens with `update` for code applied/removed, method, waiting order, link previews |
| `src/views/names.ts` | Your names screen with keyed row sync; edit and restore screens |
| `src/views/search.ts` | Search screen with `update` for applied code, retarget banner, typed name kept |
| `src/styles.css` | Page transition rules for the static names; CSS transitions for step states; no part/box/swap/gone/fresh rules |
| `e2e/motion.spec.ts`, `e2e/cuts.ts` | Motion checks for the new model: in-place updates keep nodes and run no view transition |
| `e2e/inplace.spec.ts` (new) | One test per screen: an in-page state change keeps unchanged nodes, runs no transition, shows no cut |
| `src/screens/machine.ts` (new) | Pure core: `Machine`, `Motion`, `step()` (reduce, pick screen, look up the move, warn on an unexpected one) |
| `src/screens/order.ts`, `checkout.ts`, `names.ts`, `search.ts`, `routes.ts` (new) | Pure per-view state, events, `reduce`, `screenOf`, `moves`; `routes.ts` gives moves between views (search → checkout → name code → order, Your names) |
| `test/machine.test.ts`, `test/screens-*.test.ts` (new) | vitest: `step()`, every `screenOf` branch and every table row per view |

---

### Task 1: The screen primitive

**Goal:** `screen()` in `shell.ts` builds a screen once and routes later states of the same kind to its `update`, with a per-screen `AbortSignal` for its timers.

**Files:**
- Modify: `src/views/shell.ts` (add `screen`, `Built`, the shown-screen state; reset it in `nextRoute` when the route is not a redraw)
- Modify: `src/views/order.ts:27-38` (`every()` takes a signal instead of pushing to `cleanups`)
- Test: `e2e/inplace.spec.ts` (new; first test drives Task 2, so this task's test is the harness helper)

**Acceptance Criteria:**
- [ ] `screen(active, kind, state, build)` calls `build(state, signal)` and `page()` when no screen of `kind` is shown on this route family; otherwise calls the shown screen's `update(state)` and nothing else.
- [ ] A screen without `update` is rebuilt (`page()`) when its kind is called again with a state whose `JSON.stringify` differs from the shown one; with an equal state it is left alone.
- [ ] Leaving the route (`nextRoute` without `redrawing`) aborts the shown screen's signal and forgets it; a `refresh()` keeps it.
- [ ] `e2e/inplace.spec.ts` exists with helpers `markNodes(page, selectors)` (sets `__kept = true` on each matched node) and `keptNodes(page, selectors)` (returns which still carry it), and `takeTransitions` from `e2e/cuts.ts`.
- [ ] `npx tsc --noEmit -p .` clean.

**Verify:** `npx tsc --noEmit -p . && npm test` → no errors, `77 passed`

**Steps:**

- [ ] **Step 1: Add the primitive to `src/views/shell.ts`** (after `refresh()`):

```ts
// A screen of a view is built once and then told each new state of what it shows, changing only what differs; a state
// of another kind is another screen, shown as a page change. What a screen runs, as a countdown, stops with it.
export interface Built<S> { sections: Node[]; update?: (state: S) => void }
interface Shown { kind: string; state: string; update?: (state: unknown) => void; stop: AbortController }
let shown: Shown | null = null

export function screen<S>(active: Active, kind: string, state: S, build: (state: S, signal: AbortSignal) => Built<S>): void {
  const same = shown !== null && shown.kind === kind && isRedraw() && document.querySelector(SHOWN_PAGE) !== null
  if (same && shown!.update) return shown!.update(state)
  if (same && shown!.state === JSON.stringify(state)) return
  shown?.stop.abort()
  const stop = new AbortController()
  const built = build(state, stop.signal)
  shown = { kind, state: JSON.stringify(state), update: built.update as Shown["update"], stop }
  page(active, built.sections)
}
```

In `nextRoute()`, where `focus.drop()` runs for a new page, also run:

```ts
    shown?.stop.abort()
    shown = null
```

`state` passed to screens must be JSON-serialisable data (API views, records, flags), never DOM nodes or functions.

- [ ] **Step 2: Signal-owned timers in `src/views/order.ts`**: replace `cleanups`/`cleanUp`/`every` with

```ts
function every(ms: number, f: () => void, signal: AbortSignal): void {
  const id = setInterval(f, ms)
  signal.addEventListener("abort", () => clearInterval(id), { once: true })
}
```

and give `onTheSecond(left, f, signal)` the same signal (clear its timeout on abort). `cleanups.push(card.destroy)` becomes `signal.addEventListener("abort", card.destroy, { once: true })`.

- [ ] **Step 3: Create `e2e/inplace.spec.ts` helpers**

```ts
import { expect, test, type Page } from "@playwright/test"
import { recordCuts, takeCuts, takeTransitions } from "./cuts.ts"

test.use({ reducedMotion: "no-preference" })
test.beforeEach(async ({ page }) => { await page.addInitScript(recordCuts) })

// marks the nodes a state change must keep, to tell afterwards which are still the same nodes
export const markNodes = (page: Page, selectors: string[]) => page.evaluate((list) => {
  for (const s of list) for (const n of document.querySelectorAll("body > main " + s)) (n as unknown as { __kept: boolean }).__kept = true
}, selectors)
export const keptNodes = (page: Page, selectors: string[]) => page.evaluate((list) => list.filter((s) => {
  const n = document.querySelector("body > main " + s) as unknown as { __kept?: boolean } | null
  return n?.__kept === true
}), selectors)
```

- [ ] **Step 4: Type-check and unit tests**

Run: `npx tsc --noEmit -p . && npm test`
Expected: no type errors, `Tests 77 passed`

```json:metadata
{"files": ["src/views/shell.ts", "src/views/order.ts", "e2e/inplace.spec.ts"], "verifyCommand": "npx tsc --noEmit -p . && npm test", "acceptanceCriteria": ["screen() builds once and routes same-kind states to update", "screen without update rebuilds only on a changed state", "leaving the route aborts the screen signal; refresh keeps it", "inplace.spec.ts helpers exist", "tsc clean"], "modelTier": "frontier"}
```

---

### Task 2: Order page as screens, registering changes in place

**Goal:** The order page's `follow()` loop calls `screen()` with a kind per screen; the registering screen's phases (committing → committed → wait → registering), its countdown, transactions and problem note change in place.

**Files:**
- Modify: `src/views/order.ts` (`draw`, `payment`, `confirming`, `registering`, `track`, `txRows`, `registered`, `taken`, `codeIssued`, `failed`, status screens; remove `carry(note)` uses)
- Modify: `src/styles.css` (`.stage` / `.node` state transitions)
- Test: `e2e/inplace.spec.ts`

**Acceptance Criteria:**
- [ ] Kinds: `pay-crypto`, `pay-card`, `confirming`, `slow`, `registering`, `registered`, `taken`, `code-issued`, `failed`, `not-on-device`, and `status:<name>` for each status page (`paid-late`, `paid-by-invoice`, `still-open`, `card-failed`, `expired-cancelled`, `expired-received`, `expired`, `price-changed`, `unreadable`, `unknown-link`).
- [ ] `registering` update, for a new `RegistrationView` of the same order: each `.stage` gets its new state class (`done`/`active`/`""`), its node text crosses over (live), its title crosses over (live); the wait line fill starts on entering `committed`; the countdown clock and note keep their live behaviour; each tx row's value and chip cross over (live); the problem slot crosses over; heading/lead/crumbs/card are untouched unless their text changes (then live).
- [ ] `pay-crypto` update: partial-payment alert slot, amount label and amount, copy fields, QR (only when the URI changed), "Waiting for (the rest of) payment" and the cancel link slot cross over in place; the rate countdown restarts only if `expiresAt` changed.
- [ ] `confirming` update: the confirmations line crosses over; the slow timer keeps running from the same `cardSubmittedAt`.
- [ ] Stage colours change by CSS transition (`transition: background-color, border-color, color, box-shadow` on `.stage .node` and the line drawn by `.stage::before`), not by a view transition.
- [ ] e2e `registering changes in place`: on the code-order journey, nodes `.track`, `.stage:nth-child(1)`, `.countdown`, `.txs`, `.summary`, `h1` are marked during committing; after the commit is mined and again after the wait ends, all are still kept, `takeTransitions` is `[]`, `takeCuts` is `[]`, and the stage classes are `done active ""` then `done done active`.
- [ ] e2e `a new screen is a page change`: registering → registered runs exactly one transition (or, in Firefox, `.page-leaving` appears once) and the heading takes focus.

**Verify:** `npx playwright test e2e/inplace.spec.ts e2e/paid.spec.ts e2e/taken.spec.ts --project=chromium` → all passed

**Steps:**

- [ ] **Step 1: Write the failing e2e test** in `e2e/inplace.spec.ts`:

```ts
import { control, REGISTRATION_MS, uniqueLabel } from "./helpers.ts"
test("registering changes in place: the steps, countdown and transactions keep their nodes through each phase", async ({ page, request }) => {
  const { code } = await control(request, "code", { minLength: 6 }) as { code: string }
  await page.goto("/")
  await page.getByRole("button", { name: "Check a code" }).click()
  await page.getByRole("textbox", { name: "Code", exact: true }).fill(code)
  await page.getByRole("button", { name: "Use this code" }).click()
  await page.getByRole("textbox", { name: "Name", exact: true }).fill(uniqueLabel("inplace"))
  await page.getByRole("textbox", { name: "Name", exact: true }).press("Enter")
  await page.getByRole("button", { name: "Register with code" }).click()
  await page.getByRole("button", { name: "Continue" }).click()
  await page.getByText("I have saved my code").click()
  await page.getByRole("button", { name: "Register", exact: true }).click()
  await page.locator(".stage.active", { hasText: "Committing" }).waitFor({ timeout: REGISTRATION_MS })
  const kept = [".track", ".stage:nth-child(1)", ".countdown", ".txs", ".summary", "h1"]
  await markNodes(page, kept)
  await takeTransitions(page)
  await page.locator(".stage.active", { hasText: "Waiting" }).waitFor({ timeout: REGISTRATION_MS })
  expect(await keptNodes(page, kept)).toEqual(kept)
  expect(await page.evaluate(() => [...document.querySelectorAll("body > main .stage")].map((s) => s.className))).toEqual(["stage done", "stage active", "stage"])
  await page.locator(".stage.active", { hasText: "Registering" }).waitFor({ timeout: REGISTRATION_MS })
  expect(await keptNodes(page, kept)).toEqual(kept)
  expect(await takeTransitions(page)).toEqual([])
  expect(await takeCuts(page)).toEqual([])
})
```

- [ ] **Step 2: Run it to verify it fails**

Run: `npx playwright test e2e/inplace.spec.ts --project=chromium`
Expected: FAIL at the first `keptNodes` (nodes were rebuilt).

- [ ] **Step 3: Kinds in `draw()`**: `draw(invoice, view, record)` builds the state `{ invoice, view, record }` and calls `screen(null, kindOf(invoice, view, record), state, buildOf(kind))`, where `kindOf` returns the kind strings above using exactly the branch conditions now in `draw()`, `payment()`, `confirming()` (slow when `Date.now() > slowAt`) and `expired()`. Status pages become `status:<name>` kinds with `build` returning the same `status(...)` sections they return today and no `update`.

- [ ] **Step 4: Registering screen** — rewrite `registering()` as `buildRegistering(state, signal): Built<OrderState>`:

```ts
function buildRegistering({ invoice, view, record }: OrderState, signal: AbortSignal): Built<OrderState> {
  const steps = track()
  const clockNode = live(el("b", {}))
  const note = live(el("p", {}))
  const problem = live(el("div", {}))
  const txs = txRows()
  const lead = live(el("span", {}))
  let current = view
  const show = (next: OrderState) => {
    current = next.view
    const states = stagesOf(next.view)
    steps.set(states)
    txs.set(next.view, states, refusedOf(next.view, next.record))
    lead.replaceChildren(leadOf(next.view, next.record))
    problem.replaceChildren(...problemOf(next.view, next.record))
    waitFor(next, signal)
  }
  show({ invoice, view, record })
  onTheSecond(() => waitLeft(current), () => tick(current, record, clockNode, note), signal)
  const sections = [pageHead(headingOf(view, record), lead, crumbs("Registration", { fromApp: openedFromApp(view, record), prepaid: !invoice })),
    twoCols(el("div", { class: "panel" }, el("div", { class: "panel-sec" }, steps.node, el("div", { class: "countdown" }, clockNode, note), problem), txs.node),
      orderCard(labelOf(view, record), view.tld, paidRows(view, invoice)))]
  return { sections, update: show }
}
```

where `track()` returns `{ node, set(states) }`: it builds the three `.stage` elements once (each with a live `.node` span and live `b` title) and `set` assigns `stage.className = \`stage ${st}\`.trim()`, `node.replaceChildren(st === "done" ? "✓" : st === "failed" ? "✕" : n)` only when the text differs, and the title (`Committing`/`Committed`, `Registering`/`Registered`) only when it differs. `txRows()` returns `{ node, set(view, states, refused) }` with live value and chip cells per row, replacing children only when their text differs. `waitFor` starts `fillWait` the first time the phase is `committed` (once per screen). The reveal retry (`revealing`, `revealFailures`) moves into `tick` unchanged.

- [ ] **Step 5: Stage transitions in `src/styles.css`** (after `.stage.failed .node`):

```css
/* a step turning active or done eases to its colours, its node and line changing in place */
.stage::before, .stage .node { transition: var(--moves); }
```

- [ ] **Step 6: Payment and confirming screens** — `buildPayCrypto` keeps references to: the partial-alert slot (`live(el("div", {}))`), amount label, amount, USD line, the three copy fields (each a live value cell), the QR slot (live; replaced only when `paymentUri(...)` differs from the last), the awaiting text (live) and the cancel slot (live). Its `update` recomputes each from the new invoice and replaces only what differs. `buildConfirming` keeps the confirmations line (live) and the slow timer on `signal`.

- [ ] **Step 7: Run the tests**

Run: `npx playwright test e2e/inplace.spec.ts e2e/paid.spec.ts e2e/taken.spec.ts e2e/focus.spec.ts --project=chromium`
Expected: all passed

```json:metadata
{"files": ["src/views/order.ts", "src/styles.css", "e2e/inplace.spec.ts"], "verifyCommand": "npx playwright test e2e/inplace.spec.ts e2e/paid.spec.ts e2e/taken.spec.ts e2e/focus.spec.ts --project=chromium", "acceptanceCriteria": ["order kinds as listed", "registering update keeps nodes and changes only differing ones", "pay-crypto update in place", "confirming update in place", "stage colours by CSS transition", "e2e registering changes in place passes", "e2e new screen is one page change"], "modelTier": "frontier"}
```

---

### Task 3: Checkout and name-code screens change in place

**Goal:** Applying or removing a code, choosing a method, a waiting order and link previews change the checkout's nodes in place; the name-code page's states do too.

**Files:**
- Modify: `src/views/checkout.ts` (`checkoutPage`, `codeEntry`, `ownerCodePage`, `settleWaiting`, `showOpenOrder`; remove `carry(linksSec)`, `linksDraw`)
- Modify: `src/views/codes.ts:169` (calls `refresh()` instead of `checkoutPage()` for the checkout)
- Test: `e2e/inplace.spec.ts`

**Acceptance Criteria:**
- [ ] Kind `checkout`: `update(c)` crosses over the rows (`Paying with` ↔ `Code`), the total, the code area, the link slot (`I have a code` ↔ `Remove the code`), the crumbs (`prepaid` changes the steps: the step bar crosses over as one live part) and the Continue button state; the links section (fields being typed, previews loading) is the same node throughout.
- [ ] Kind `owner-code`: the "I have saved my code" check and Register button keep their nodes when the order is refused for its method and drawn again.
- [ ] Error pages (`price-changed`, `too-many`, `failed`) are `status:<name>` kinds.
- [ ] e2e `checkout changes in place`: mark `.summary`, `.panel`, `.link-field`, `h1`; apply a code, remove it; all kept, no transition, no cut, rows read `Code` then `Paying with`.
- [ ] `src/screens/checkout.ts`: state, events (code applied/removed, method chosen, waiting order seen/settled, hold started/ended, order refused, price changed), pure `reduce`, `screenOf` (`checkout`, `owner-code`, `status:<name>`), `moves`; the views dispatch events and render from state; `test/screens-checkout.test.ts` covers every `screenOf` branch and every `moves` row.

**Verify:** `npx playwright test e2e/inplace.spec.ts e2e/checkout.spec.ts e2e/codes.spec.ts --project=chromium` → all passed

**Steps:**

- [ ] **Step 1: Failing e2e test**

```ts
test("the checkout changes in place: a code applied and removed keeps the card, panel and link fields", async ({ page, request }) => {
  const { code } = await control(request, "code", { minLength: 6 }) as { code: string }
  await page.goto(`/#/?name=${uniqueLabel("inco")}`)
  await page.getByRole("button", { name: "Register" }).click()
  await page.getByRole("heading", { name: "Check your order" }).waitFor()
  const kept = [".summary", ".panel", ".link-field", "h1"]
  await markNodes(page, kept)
  await takeTransitions(page)
  await page.getByText("I have a code").click()
  await page.locator(".code-entry input").fill(code)
  await expect(page.getByText("Remove the code")).toBeVisible()
  expect(await page.locator(".summary .r").allInnerTexts()).toContainEqual(expect.stringContaining("Code"))
  await page.getByText("Remove the code").click()
  await expect(page.getByText("I have a code")).toBeVisible()
  expect(await keptNodes(page, kept)).toEqual(kept)
  expect(await takeTransitions(page)).toEqual([])
  expect(await takeCuts(page)).toEqual([])
})
```

- [ ] **Step 2: Run, expect FAIL** (`npx playwright test e2e/inplace.spec.ts -g checkout --project=chromium`).

- [ ] **Step 3: Restructure `checkoutPage()`** into state (`session.checkout()`, waiting record, held flag, applied code) → `screen("search", "checkout", state, buildCheckout)`. `buildCheckout` creates the links section, method radios, card rows (each value a live cell keyed by row label), total (live), code area (live), link slot (live), the crumbs slot (live, `crumbs("Details", ...)` replaced only when `prepaid` changes) and Continue; `update` recomputes rows/total/slots and replaces only differing ones. The code entry's success path calls `refresh()`; removing the code calls `refresh()`.

- [ ] **Step 4: Owner-code page** as kind `owner-code` with `update` that keeps the check and button and replaces the hold note (live).

- [ ] **Step 5: Run tests**

Run: `npx playwright test e2e/inplace.spec.ts e2e/checkout.spec.ts e2e/codes.spec.ts e2e/focus.spec.ts --project=chromium`
Expected: all passed

```json:metadata
{"files": ["src/views/checkout.ts", "src/views/codes.ts", "e2e/inplace.spec.ts"], "verifyCommand": "npx playwright test e2e/inplace.spec.ts e2e/checkout.spec.ts e2e/codes.spec.ts e2e/focus.spec.ts --project=chromium", "acceptanceCriteria": ["checkout update in place", "owner-code update in place", "error pages are status kinds", "e2e checkout changes in place passes"], "modelTier": "frontier"}
```

---

### Task 4: Your names, edit and restore screens change in place

**Goal:** The Your names list syncs its rows by key (kept, changed, added, removed) instead of being rebuilt; the edit and restore pages' states change in place.

**Files:**
- Modify: `src/motion.ts` (add `syncList`)
- Modify: `src/views/names.ts` (`historyPage`, `confirmForget`, `pendingRow`, `orderRow`, `codeRow`, `editPage`, `restorePage`)
- Test: `e2e/inplace.spec.ts`

**Acceptance Criteria:**
- [ ] `syncList(part, rows)` in `motion.ts`: for each new row with `data-key`, an existing child with the same key is kept when `isEqualNode` holds, else replaced by `removeFrom(part, old, row)` (fades across); keys only in the old list leave by `removeFrom(part, old)`; keys only in the new list come in by `addTo(part, row, nextSibling)`; order follows `rows`.
- [ ] Kind `names`: `update` calls `syncList` for the Names and Codes lists and crosses over the empty-list notes and the forget area; rows that did not change keep their nodes; the entering rise runs only when the screen is built.
- [ ] Kinds `edit`, `edit-unreadable`, `restore`: retry and refusal states cross over in place; `editPage`'s `setTimeout` retry uses the screen signal.
- [ ] e2e `Your names changes in place`: with two orders, mark both rows; add a code in another tab (`storage` event); both rows kept, the code row added, no transition, no cut.
- [ ] `src/screens/names.ts`: state (records, codes, pending, forget question, edit and restore states), events (storage changed, forget asked/kept/confirmed, edit answer, restore answer), pure `reduce`, `screenOf`, `moves`; `test/screens-names.test.ts` covers every branch and row.

**Verify:** `npx playwright test e2e/inplace.spec.ts e2e/names.spec.ts e2e/history.spec.ts --project=chromium` → all passed

**Steps:**

- [ ] **Step 1: Failing e2e test**

```ts
test("Your names changes in place: rows another tab leaves alone keep their nodes", async ({ page, context, request }) => {
  const codeOf = async () => (await control(request, "code", { minLength: 6 }) as { code: string }).code
  const [first, second] = [await codeOf(), await codeOf()]
  const store = (code: string) => localStorage.setItem(`simplex-names:code:${code}`, JSON.stringify({ code, minLength: 6, years: 2, addedAt: new Date().toISOString() }))
  await page.goto("/#/names")
  await page.evaluate(store, first)
  await page.reload()
  await page.locator(".row-item").first().waitFor()
  await expect(page.locator(".row-item.entering")).toHaveCount(0)
  await markNodes(page, [".row-item", "h1"])
  await takeTransitions(page)
  const other = await context.newPage()
  await other.goto("/#/names")
  await other.evaluate(store, second)
  await expect(page.locator(".row-item")).toHaveCount(2)
  expect(await keptNodes(page, [".row-item", "h1"])).toEqual([".row-item", "h1"])
  expect(await takeTransitions(page)).toEqual([])
  expect(await takeCuts(page)).toEqual([])
})
```

- [ ] **Step 2: Run, expect FAIL.**

- [ ] **Step 3: `syncList` in `src/motion.ts`** (after `addTo`):

```ts
// A list drawn again from its rows, each keyed by data-key: a row alike stays as it is, a changed one crosses over in
// its place, one gone fades as what follows slides up, and a new one fades in as what follows slides aside.
export function syncList(part: HTMLElement, rows: HTMLElement[]): void {
  const byKey = new Map([...part.children].filter((n) => !n.classList.contains("outgoing")).map((n) => [(n as HTMLElement).dataset.key, n as HTMLElement]))
  const wanted = new Set(rows.map((r) => r.dataset.key))
  for (const [key, node] of byKey) if (!wanted.has(key)) removeFrom(part, node)
  let before: Element | null = part.firstElementChild
  for (const row of rows) {
    while (before?.classList.contains("outgoing")) before = before.nextElementSibling
    const old = byKey.get(row.dataset.key)
    if (!old) addTo(part, row, before)
    else if (old.isEqualNode(row)) {
      if (old !== before) part.insertBefore(old, before)
      before = old.nextElementSibling
      continue
    } else removeFrom(part, old, row)
    before = row.nextElementSibling
  }
}
```

- [ ] **Step 4: Your names screen** — `historyPage()` computes `{ orders, codes, pending, canPersist }` and calls `screen("names", "names", state, buildNames)`; `buildNames` keeps the two list parts (live) and the forget area; `update` builds rows (`pendingRow`, `orderRow`, `codeRow`, each with `data-key`) and calls `syncList`. The storage listener calls `refresh()` (batched in one frame as now).

- [ ] **Step 5: Edit and restore** — `editPage` state `{ registrationId, view | error }` → kinds; `update` crosses over the result line and fields' notes; the retry `setTimeout` is cleared on `signal` abort.

- [ ] **Step 6: Run tests**

Run: `npx playwright test e2e/inplace.spec.ts e2e/names.spec.ts e2e/history.spec.ts e2e/focus.spec.ts --project=chromium`
Expected: all passed

```json:metadata
{"files": ["src/motion.ts", "src/views/names.ts", "e2e/inplace.spec.ts"], "verifyCommand": "npx playwright test e2e/inplace.spec.ts e2e/names.spec.ts e2e/history.spec.ts e2e/focus.spec.ts --project=chromium", "acceptanceCriteria": ["syncList keeps, crosses, adds and removes rows by key", "names update syncs lists in place", "edit and restore states in place", "e2e Your names changes in place passes"], "modelTier": "frontier"}
```

---

### Task 5: Search screen changes in place

**Goal:** A code applied or removed, a retarget banner corrected and a redraw keep the search box, typed name and results nodes.

**Files:**
- Modify: `src/views/search.ts` (`searchPage`, `removeCode`, `checkRetarget`, the applied-code note; remove `shownInput`, `shownResults`, `carry(results)`)
- Modify: `src/views/codes.ts:109,179` (keep `refresh()`)
- Test: `e2e/inplace.spec.ts`

**Acceptance Criteria:**
- [ ] Kind `search:<fromApp|retarget|plain>`: `update` crosses over the banner slot and the applied-code note; the input, its typed value, the search button and the results part keep their nodes; a new `name` parameter that differs from the searched one runs the search in place.
- [ ] Kind `status:bad-app-link` for the damaged wallet link.
- [ ] e2e `search changes in place`: type a name, apply a code from the popup; `.search`, `.search input`, `.results`, `h1` kept, input value unchanged, no transition, no cut.
- [ ] `src/screens/search.ts`: state (name, typed, answer, applied code, retarget, app wallet, pause), events (typed, searched, answered, code applied/removed, retarget read), pure `reduce`, `screenOf`, `moves`; `test/screens-search.test.ts` covers every branch and row.

**Verify:** `npx playwright test e2e/inplace.spec.ts e2e/search.spec.ts e2e/codes.spec.ts --project=chromium` → all passed

**Steps:**

- [ ] **Step 1: Failing e2e test**

```ts
test("the search changes in place: a code applied from the popup keeps the box, the typed name and the results", async ({ page, request }) => {
  const { code } = await control(request, "code", { minLength: 6 }) as { code: string }
  await page.goto("/")
  await page.getByRole("textbox", { name: "Name", exact: true }).fill("typedname")
  const kept = [".search", ".search input", ".results", "h1"]
  await markNodes(page, kept)
  await takeTransitions(page)
  await page.getByRole("button", { name: "Check a code" }).click()
  await page.getByRole("textbox", { name: "Code", exact: true }).fill(code)
  await page.getByRole("button", { name: "Use this code" }).click()
  await expect(page.getByText(/^Code applied/)).toBeVisible()
  expect(await keptNodes(page, kept)).toEqual(kept)
  await expect(page.getByRole("textbox", { name: "Name", exact: true })).toHaveValue("typedname")
  expect(await takeTransitions(page)).toEqual([])
  expect(await takeCuts(page)).toEqual([])
})
```

- [ ] **Step 2: Run, expect FAIL.**

- [ ] **Step 3: Restructure `searchPage()`** into state `{ name, fromApp, retargetId, applied }` → `screen("search", kind, state, buildSearch)`; `buildSearch` creates hero, input, box, results (live), banner slot (live), applied-note slot (live), bento; `update` updates the banner and applied note, and runs `submit()` when `state.name` differs from the last searched name. `removeCode` calls `refresh()` then focuses the same input (it survives).

- [ ] **Step 4: Run tests**

Run: `npx playwright test e2e/inplace.spec.ts e2e/search.spec.ts e2e/codes.spec.ts e2e/focus.spec.ts --project=chromium`
Expected: all passed

```json:metadata
{"files": ["src/views/search.ts", "src/views/codes.ts", "e2e/inplace.spec.ts"], "verifyCommand": "npx playwright test e2e/inplace.spec.ts e2e/search.spec.ts e2e/codes.spec.ts e2e/focus.spec.ts --project=chromium", "acceptanceCriteria": ["search update in place keeps box, input, results", "bad app link is a status kind", "e2e search changes in place passes"], "modelTier": "frontier"}
```

---

### Task 6: Page changes with static names; delete the comparison machinery

**Goal:** `page()` shows a new screen with one transition and no comparison; every runtime pairing path and its CSS is deleted.

**Files:**
- Modify: `src/views/shell.ts` (`page()`: no `compare`, no identical-redraw path, no `carry`; change type `forward`/`back`/`screen`/`fill`)
- Modify: `src/motion.ts` (delete `compare`, `Match`, `Comparison`, `signatures`, `keysOf`, `slotsOf`, `pairsOf`, `freshOf`, `paints`, `leaf`, `empty`, `carry`, `placeCarried`, stand-ins, `MOST_MORPHED`, `kindOf`, `anchorOf`, `WHOLE`, `MOVING`, `changeOnly`, `leaveOver`'s "over" mode, the pair-naming loop in `begin`, `unname`'s pair names; keep `transition`, `transitionsIdle`, `dropWaitingPage`, `finishMotion`, `settled`, transient names, `swapPage` with `"at-once" | "through"`)
- Modify: `src/styles.css` (delete `.part`, `.box`, `.swap`, `.gone`, `.fresh`, `.step`, `.tick`, `.name` pair rules and `vt-moving`; add page rules; static names `order-card`)
- Modify: `src/views/order.ts`, `src/views/checkout.ts` (`.summary` gets `view-transition-name: order-card` via CSS only), remove `data-whole`
- Test: `e2e/motion.spec.ts`, `e2e/cuts.ts`

**Acceptance Criteria:**
- [ ] `rg -n "compare\(|pairsOf|freshOf|carry\(|data-whole|part-\$\{|gone-|fresh-|vt-moving" src` returns nothing.
- [ ] A page change in Chromium/WebKit runs one view transition whose groups are a subset of `root, page, page-top, page-foot, offline-note, step-ring, step-line-*, step-done-*, order-card, order-name`.
- [ ] `page` group: old fades out in `--axis-out-ms`, new fades in over `--axis-in-ms` after it, both sliding `--axis` the way the reader goes for `forward`/`back`; `screen` and `fill` without the slide.
- [ ] Firefox: page changes use `swapPage(old, main, ease, "through")`; no `.page-leaving` remains after `PAGE_OUT` time.
- [ ] `e2e/motion.spec.ts` updated: tests asserting `part-`/`gone-`/`fresh-` groups now assert the static groups above; in-place assertions moved to `e2e/inplace.spec.ts`.
- [ ] Each preserved animation has an e2e check in `e2e/motion.spec.ts` (groups and pseudo animations from `takeTransitions` in Chromium/WebKit): search → checkout has `order-name`; checkout → name code has `order-card`, `page-title`, `crumbs`, `step-ring`, `step-line-name-code` (`fill-across`), `step-done-details` (`tick-in`); forward/back page changes slide `page` (`axis-out`/`axis-in`); the theme change is one root cross-fade.
- [ ] `motion.ts` `VIEW_TRANSITIONS` admits Gecko from major version 158 (`FIREFOX_FIXED = 158`, read from the user agent's `Firefox/<n>`), with the bug cited in one short comment; older Gecko and any browser without transition types take the fallback.
- [ ] Firefox before 158 (no view transitions) keeps the shared parts' motion by FLIP: on a page change, each element of `order-name`, `order-card`, `crumbs`, `page-title`, `page-panel` present on both pages is hidden in the leaving page and the new one moves (translate, and height for `order-card`/`page-panel`) from the old one's place over `--axis-ms`; the rest fades through. e2e in Firefox: the name's new element starts at the result's place (its first animation's `translate` keyframe equals the old offset) and the card does not fade.
- [ ] `src/screens/routes.ts`: moves between views (search, checkout, name code, order, Your names, restore, edit, trademarks) with their motion; `page()` takes the motion from `step()`/`routes.ts` instead of `wayTo()`'s crumb and nav heuristics (browser Back/Forward still give `back`/`forward`); `test/screens-routes.test.ts` covers every row.
- [ ] `npx tsc --noEmit -p .` clean; `npm test` passes (77 plus the new screen tests).

**Verify:** `npx tsc --noEmit -p . && npm test && npx playwright test e2e/motion.spec.ts e2e/inplace.spec.ts --project=chromium` → all passed

**Steps:**

- [ ] **Step 1: Update `e2e/motion.spec.ts` expectations first** — in "a page opened slides the way the reader goes…", replace the group assertion with

```ts
    expect(opened.groups.filter((g) => !/^step-(line|done)-/.test(g)).sort()).toEqual(["page", "page-foot", "page-top", "root"])
```

and delete the `redraws` part (in-place redraws no longer transition; covered in `inplace.spec.ts`). In "the next step of an order…", keep the `step-ring` / `step-line-name-code` / `step-done-details` assertions, replace the root and `gone-` assertions with

```ts
  expect(next.groups).toContain("page")
  expect(next.groups).toContain("order-card")
```

- [ ] **Step 2: Run, expect FAIL** (`npx playwright test e2e/motion.spec.ts --project=chromium`).

- [ ] **Step 3: CSS** — replace the view-transition section between `/* Every change of the page is compared…` and the theme rules with:

```css
/* A page change moves only the shared parts, each by its name: the header and footer stay, the step bar's ring goes over,
   the order's card and name go to their new places; the rest of the page fades through, the way the reader goes. */
.summary { view-transition-name: order-card; }
body > main:not(.pending) { view-transition-name: page; }
::view-transition-group(page), ::view-transition-group(order-card), ::view-transition-group(.name), ::view-transition-group(step-ring), ::view-transition-group(page-foot) { animation-duration: var(--axis-ms); animation-timing-function: var(--emphasized); }
::view-transition-old(page) { animation: var(--axis-out-ms) linear both fade-out; }
::view-transition-new(page) { animation: var(--axis-in-ms) var(--emphasized) var(--axis-out-ms) both appear; }
html:active-view-transition-type(forward, back)::view-transition-old(page) { animation: var(--axis-ms) var(--emphasized) both axis-out, var(--axis-out-ms) linear both fade-out; }
html:active-view-transition-type(forward, back)::view-transition-new(page) { animation: var(--axis-ms) var(--emphasized) both axis-in, var(--axis-in-ms) var(--emphasized) var(--axis-out-ms) both appear; }
```

keeping the existing `page-top`/`page-foot`/`offline-note` rules (types now `forward, back, screen, fill`), the `step-*` rules, `.order-name` rules and keyframes.

- [ ] **Step 4: `page()`** — delete `compare`, `filled`/`arrived`/`unseen` comparison branches that depend on it, the `!comparison.differs` path; `change` is `samePage ? (filled ? "fill" : "screen") : wayTo(...)`; `transition(change, swap, () => finishMotion(old))`; `swap` calls `swapPage(old, main, !scrollTo, transitions() ? "at-once" : "through")`.

- [ ] **Step 5: `motion.ts`** — delete the listed functions and types; `transition(change, update, prepare)` drops the `comparison` parameter; `begin` becomes: `prepare(); unname(); start the view transition with the update; on finish unname and run the queue`.

- [ ] **Step 6: Run tests**

Run: `npx tsc --noEmit -p . && npm test && npx playwright test e2e/motion.spec.ts e2e/inplace.spec.ts --project=chromium`
Expected: no type errors, 77 passed, all passed

```json:metadata
{"files": ["src/views/shell.ts", "src/motion.ts", "src/styles.css", "src/views/order.ts", "src/views/checkout.ts", "e2e/motion.spec.ts", "e2e/cuts.ts"], "verifyCommand": "npx tsc --noEmit -p . && npm test && npx playwright test e2e/motion.spec.ts e2e/inplace.spec.ts --project=chromium", "acceptanceCriteria": ["no comparison code remains (rg empty)", "page change groups are static names only", "page group fade-through with axis slide", "Firefox fades through", "motion.spec updated", "tsc clean, unit tests pass"], "modelTier": "frontier"}
```

---

### Task 7: Verify across browsers, recordings and review

**Goal:** The whole suite passes in three browsers, frame recordings show no flicker, and review subagents report no issues.

> **USER-ORDERED GATE — NON-SKIPPABLE.** This task was requested by the user in the current conversation. It MUST NOT be closed by walking around it, by declaring it "verified inline", or by substituting a cheaper check. Close only after every item in `acceptanceCriteria` has been re-validated independently, with output captured.

**Files:**
- Test: whole `e2e/`, scratchpad recordings (`tools/journey/plain-journey.mjs`, `tools/static-spikes.mjs`)

**Acceptance Criteria:**
- [ ] `npx playwright test --project=chromium`, `--project=firefox`, `--project=webkit` each report 0 failed (2 offline tests skipped without `BASE_URL`).
- [ ] Firefox and Chromium recordings of the `code-order` and `checkout-edits` journeys: `static-spikes.mjs` reports 0 one-frame spikes in header and ground; frame sheets of each in-page change show only the changed parts moving.
- [ ] A review round of subagents (code, motion with frame recordings in Chromium and Firefox, design desktop and phone) reports no issues; findings are fixed and the round repeated until it does.

**Verify:** `for b in chromium firefox webkit; do npx playwright test --project=$b; done` → `0 failed` each

**Steps:**

- [ ] **Step 1: Full e2e in each browser** (one at a time; Firefox and WebKit with the scratchpad env scripts).
- [ ] **Step 2: Recordings** — `node plain-journey.mjs firefox code-order <dir>` and `chromium`, then `node static-spikes.mjs <dir>`; cut and read frame sheets of each change.
- [ ] **Step 3: Review round** — dispatch reviewers (no edits to the repos, scratchpad only, no polling waits), fix findings, repeat until none.

```json:metadata
{"files": [], "verifyCommand": "for b in chromium firefox webkit; do npx playwright test --project=$b; done", "acceptanceCriteria": ["0 failed in chromium, firefox, webkit", "0 one-frame spikes in recordings; only changed parts move", "review subagents report no issues"], "modelTier": "frontier", "userGate": true, "tags": ["user-gate"]}
```

---

### Task 8: The state machine core

**Goal:** A pure `src/screens/machine.ts` with `Machine`, `Motion` and `step()`, unit-tested, for every view to run its state through.

**Files:**
- Create: `src/screens/machine.ts`
- Test: `test/machine.test.ts`

**Acceptance Criteria:**
- [ ] `step(machine, state, shown, event, warn)` returns `{ state, screen, motion, expected }`: `state = machine.reduce(state, event)`, `screen = machine.screenOf(state)`; `shown === null` → motion `"screen"`, expected; `screen === shown` → `"in-place"`, expected; a move listed in `machine.moves[shown][screen]` → that motion, expected; otherwise `warn("[screens] <name>: unexpected move <shown> → <screen>")`, motion `"screen"`, `expected: false`.
- [ ] No DOM, clock, storage or network use in `src/screens/` (`rg -n "document|window|Date\.now|localStorage|fetch" src/screens` empty).
- [ ] vitest covers the four branches with a small test machine (counter-like states and two screens).

**Verify:** `npx tsc --noEmit -p . && npx vitest run test/machine.test.ts` → passed

**Steps:**

- [ ] **Step 1: Failing test** `test/machine.test.ts`:

```ts
import { describe, expect, it, vi } from "vitest"
import { type Machine, step } from "../src/screens/machine.ts"

type S = { n: number }
type E = { kind: "add"; by: number }
type K = "low" | "high" | "odd"
const machine: Machine<S, E, K> = {
  name: "test",
  reduce: (s, e) => ({ n: s.n + e.by }),
  screenOf: (s) => (s.n % 2 ? "odd" : s.n < 10 ? "low" : "high"),
  moves: { low: { high: "forward" }, high: { low: "back" }, odd: {} },
}

describe("step", () => {
  it("draws the first screen as a page change", () => {
    expect(step(machine, { n: 0 }, null, { kind: "add", by: 2 }, vi.fn())).toEqual({ state: { n: 2 }, screen: "low", motion: "screen", expected: true })
  })
  it("keeps a screen of the same kind in place", () => {
    expect(step(machine, { n: 2 }, "low", { kind: "add", by: 2 }, vi.fn()).motion).toBe("in-place")
  })
  it("takes a listed move's motion", () => {
    expect(step(machine, { n: 8 }, "low", { kind: "add", by: 2 }, vi.fn())).toMatchObject({ screen: "high", motion: "forward", expected: true })
  })
  it("logs a move missing from the table and still shows it as a page change", () => {
    const warn = vi.fn()
    expect(step(machine, { n: 2 }, "low", { kind: "add", by: 1 }, warn)).toMatchObject({ screen: "odd", motion: "screen", expected: false })
    expect(warn).toHaveBeenCalledWith("[screens] test: unexpected move low → odd")
  })
})
```

- [ ] **Step 2: Run, expect FAIL** (`npx vitest run test/machine.test.ts`: module not found).

- [ ] **Step 3: Implement** `src/screens/machine.ts`:

```ts
// A view's behaviour as data: its state changes only by events through reduce, the screen shown follows from the state
// alone, and each move from one screen to another has its motion; a move the table lacks is logged, and still shown.
export type Motion = "in-place" | "forward" | "back" | "screen"
export interface Machine<S, E, K extends string> {
  name: string
  reduce: (state: S, event: E) => S
  screenOf: (state: S) => K
  moves: { readonly [F in K]: Partial<Record<K, Exclude<Motion, "in-place">>> }
}
export interface Step<S, K> { state: S; screen: K; motion: Motion; expected: boolean }

export function step<S, E, K extends string>(machine: Machine<S, E, K>, state: S, shown: K | null, event: E, warn: (message: string) => void = console.warn): Step<S, K> {
  const next = machine.reduce(state, event)
  const screen = machine.screenOf(next)
  if (shown === null) return { state: next, screen, motion: "screen", expected: true }
  if (screen === shown) return { state: next, screen, motion: "in-place", expected: true }
  const motion = machine.moves[shown][screen]
  if (motion) return { state: next, screen, motion, expected: true }
  warn(`[screens] ${machine.name}: unexpected move ${shown} → ${screen}`)
  return { state: next, screen, motion: "screen", expected: false }
}
```

- [ ] **Step 4: Run, expect PASS**; `npx tsc --noEmit -p .` clean.

```json:metadata
{"files": ["src/screens/machine.ts", "test/machine.test.ts"], "verifyCommand": "npx tsc --noEmit -p . && npx vitest run test/machine.test.ts", "acceptanceCriteria": ["step() covers first draw, in place, listed move, unexpected move with warn", "src/screens has no DOM/clock/storage/network use", "vitest covers the four branches"], "modelTier": "mechanical"}
```

---

### Task 9: The order page as a state machine

**Goal:** The order page's state lives in one object changed by events through a pure reducer in `src/screens/order.ts`; its screen comes from a pure `screenOf`, its moves from a table, all unit-tested; `order.ts` becomes the controller (polls, timers, API calls, storage) that dispatches events and renders.

**Files:**
- Create: `src/screens/order.ts`
- Modify: `src/views/order.ts` (follow loop, timers and actions dispatch events; `kindOf` replaced by `screenOf`; screens render from state)
- Test: `test/screens-order.test.ts`

**Acceptance Criteria:**
- [ ] `OrderState` holds everything the screens read: invoice, registration view, record, card submitted time, reveal attempts and refusals, action notes, hold, `now` (service time of the last event).
- [ ] Events: `answered` (invoice and/or view with service time), `record-changed`, `tick` (service time), `card-submitted`, `card-failed`, `reveal-sent`, `reveal-refused`, `reveal-failed`, `cancel-started`, `cancel-answered`, `cancel-failed`, `reorder-started`, `reorder-failed`, `hold-started`, `hold-ended`; reducer pure (no clock, storage, network, DOM).
- [ ] `screenOf` returns every kind of Task 2 and nothing else; `moves` lists the order lifecycle (pay-crypto → confirming → registering → registered | taken | failed; pay-crypto → expired | still-open | expired-cancelled; pay-card → confirming | card-failed | slow; confirming → slow | registering; registering → registered | taken | failed | not-on-device; etc.) with motion `screen`.
- [ ] `test/screens-order.test.ts`: one test per `screenOf` branch (state → kind), one per `moves` row (a state of the from-screen and an event leading to the to-screen: `step()` returns the row's motion, `expected: true`), the reducer's handling of each event, and a race test: two `answered` events applied in either order (older service time second) end in the same state (an older answer never replaces a newer one).
- [ ] The e2e of Task 2 still pass.

**Verify:** `npx tsc --noEmit -p . && npm test && npx playwright test e2e/inplace.spec.ts e2e/paid.spec.ts e2e/taken.spec.ts --project=chromium` → all passed

```json:metadata
{"files": ["src/screens/order.ts", "src/views/order.ts", "test/screens-order.test.ts"], "verifyCommand": "npx tsc --noEmit -p . && npm test && npx playwright test e2e/inplace.spec.ts e2e/paid.spec.ts e2e/taken.spec.ts --project=chromium", "acceptanceCriteria": ["OrderState holds all screen inputs", "events as listed, reducer pure", "screenOf returns Task 2 kinds; moves list the lifecycle", "vitest covers every branch, row, event and the answer race", "Task 2 e2e still pass"], "modelTier": "frontier"}
```

---

## Self-review

- Spec coverage: in-place updates per view (Tasks 2–5), page transitions with static names and machinery removal (Task 6), verification (Task 7), primitive (Task 1).
- Types: `Built<S>`, `screen()`, `syncList()`, `every(ms, f, signal)`, `onTheSecond(left, f, signal)` are defined in Tasks 1 and 4 and used with the same signatures later.
- Order: Task 6 depends on Tasks 2–5 (no view may still rely on `compare`).
