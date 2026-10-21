# Names webapp redesign Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers-extended-cc:subagent-driven-development (recommended) or superpowers-extended-cc:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rebuild the names webapp (`simplex.domains/webapp`) in the simplex.domains look with the badges webapp's working model: one column, one action per screen, whole-screen builders, a sliding purchase track, and about 5.5k lines of source instead of 8.8k.

**Architecture:** Pure reducers and `screenOf` functions in `src/screens/*` stay the source of truth. Every view becomes a set of builders `(state, signal) => HTMLElement` that `shell.draw()` swaps in whole with `replaceChildren`. Search, checkout and payment sit on one CSS track (`src/views/track.ts`, adapted from badges `main.ts:265-466`) that slides by `transform`; every other screen swaps whole. Inputs being typed in are kept nodes (`shell.kept()`) that move into each new screen. No view transitions, no in-place diffing, no focus restoration machinery.

**Tech Stack:** Vite 6, TypeScript 5.9 (strict, `noUnusedLocals`), plain DOM, Vitest 3, Playwright 1.64 (Chromium, Firefox 157, WebKit, Firefox 158 via `moz-firefox`), dev backend `dev/start.sh` (anvil + PR #36 contracts + mock service).

**Spec:** plans/names-codes/2026-10-19-webapp-redesign.md

## Global Constraints

- **Repository.** All work is in `/project/git/simplex.domains/webapp` (git root `/project/git/simplex.domains`), branch `sh/names-webapp`, starting at `7ed55cf`. Paths below are relative to `webapp/` unless absolute. Visual source: `/project/git/simplex.domains/index.html` (cited as `landing:LINE`). Working-model source: `/project/git/simplex-chat-4/apps/simplex-badge-service/web` (cited as `badges main.ts:LINE`, `badges styles.css:LINE`, `badges screens.ts:LINE`).
- **Code rules (CLAUDE.md).** Named constants, no magic values; no dead code (an export nothing imports is deleted in the same task); no duplication; no shims kept past the task that replaces them; comments only for traps, ordering, sources of values and invariants, one or two lines, no em-dash asides; fix every warning (`npx tsc --noEmit` prints nothing).
- **Determinism.** `src/screens/*` stay pure: no `Date.now`, `Math.random`, DOM, storage or network. Time enters as event fields (`now`). Every reducer change gets Vitest cases.
- **No new dependencies.** `package.json` `dependencies` and `devDependencies` stay as at `7ed55cf`.
- **Builders.** A builder returns one `<section class="screen">` (or `class="home"`) with exactly one `h1`. Callbacks inside a kept node read the view's module-level `state`, never the state the builder was called with.
- **Copy.** User-facing sentences stay as at `7ed55cf` unless a task gives the new words. The refund wording, "Do not send anything more to the address", "Nothing was charged" and the reference lines are kept wherever the invoice saw a payment.
- **Motion.** Only the track's `transform`/`height` transition (320 ms, `cubic-bezier(.22, .61, .36, 1)`, badges `styles.css:434-443`), 0 ms under `prefers-reduced-motion: reduce`. No `@keyframes`, no `view-transition-*`, no `@property`, no Web Animations calls, no theme easing.
- **Dev backend.** Start it once per session in the background (it runs until stopped), then wait for readiness with curl's own retry, never a sleep loop:
  ```bash
  S=/tmp/claude-1000/-project-git-simplex-chat-4/5b3db9f9-78f1-4006-b413-b087b96f64ef/scratchpad
  cd /project/git/simplex.domains/webapp
  SLOTS=3 MIN_COMMITMENT_AGE=5 MOCK_RESEND_MS=1000 MOCK_RETRY_AFTER_SECONDS=2 ENS_CONTRACTS_DIR=$S/nsc/ens-contracts ANVIL=$S/foundry/anvil dev/start.sh   # run_in_background
  curl -sf --retry 90 --retry-connrefused --retry-delay 2 -o /dev/null http://127.0.0.1:8101/control/state   # foreground, exits 0 once slot 2's mock answers
  ```
  E2E then runs as `SLOTS=3 npx playwright test …`. A mock change needs a backend restart.
- **Browsers.** Before any Playwright run: `ulimit -c 0` and `source $S/tools/webkit-env.sh` (it also sources `$S/firefox-env/env.sh`). Firefox 158 is `/nix/store/fzqp8k7z4y915fsdssb6x16b7iiqg2dm-firefox-158.0/bin/firefox`, passed as `FIREFOX_158=…`, launched with Playwright channel `"moz-firefox"`.
- **Agents.** No polling waits anywhere (`sleep N; tail`, `while …; do sleep`): run long commands in the foreground with a timeout, or with `run_in_background` and wait for the exit notification. Every agent prompt dispatched for this plan states this rule. Review agents are read-only: they never edit `src/`, `e2e/`, `test/` and never run gradle.
- **Commits.** One commit per task, message `webapp: <subject>` (≤ 50 chars, imperative, no period, no body, no co-author), staging explicit paths with `git -C /project/git/simplex.domains add <paths>`. Every commit passes `npx tsc --noEmit` and `npx vitest run`.
- **E2E between tasks.** Each screen task updates the existing e2e tests whose selectors or flows it changes (named in its steps) and runs them; Task 10 sweeps the rest; Task 11 is the full gate.
- **Unused exports check** (used in several tasks; expected output: nothing):
  ```bash
  cd /project/git/simplex.domains/webapp && for n in $(rg -o --no-filename '^export (?:async )?(?:const|function|type|interface|class) (\w+)' -r '$1' src); do [ "$(rg -lw "$n" src test e2e dev | wc -l)" -lt 2 ] && echo "unused export: $n"; done
  ```

**User decisions (already made):**
- "Visual design: simplex.domains tokens: colours, gradients, fonts, type scale, pill buttons, light and dark."
- "Home: the current simplex.domains page minus the countdown, plus the search."
- "Motion: the badges model: the purchase steps slide on one CSS track, other screens swap whole; no view transitions, no in-place diffing."
- "Checkout: one page: summary, optional links, payment method, name code, Pay."
- "Prepaid codes: entered at checkout and in Your names only; no header popup, no applied-code banner."
- "Links: stay optional at checkout."
- "Taken after paying: the retarget flow stays."
- "The 'Your names' header link and the checkout block order are defaults the owner may change."

---

### Task 1: Tokens and frame

**Goal:** The page wears the landing's tokens, fonts, type scale, pill buttons, cards and fields in light and dark, inside the landing's header (logo, "Your names", theme icon) and footer ("Pay with" marks, links row), with the grid background, sticky bar, phone menu and `@property` theme easing gone.

**Files:**
- Modify: `src/styles.css` (lines 1-230 rewritten: fonts, tokens, base, frame, shared components; lines 650-660 motion variables kept for Task 2; lines 807-840 theme easing and phone menu deleted; remaining rules re-pointed to the new token names)
- Modify: `src/views/shell.ts:140-323` (header, theme, phone menu, footer, frame)
- Modify: `src/dom.ts:111-113` (`alertBox` emits `.notice` with tones `ok | warn | danger | info`) and its callers in `src/views/search.ts`, `src/views/checkout.ts`, `src/views/codes.ts`, `src/views/names.ts`, `src/views/order.ts`, `src/views/slots.ts`
- Modify: `src/icons.ts` (delete `menu`, `moon`, `sun` line icons)
- Modify: `index.html` (static header, theme-color, font preloads)
- Modify: `README.md` (CSP script hashes)
- Delete: `public/fonts/Manrope/Manrope-ExtraBold.woff2`, `public/fonts/Manrope/Manrope-ExtraBold-latin.woff2`
- Modify: `e2e/search.spec.ts` (delete the phone-menu tests at lines 118, 142, 202), `e2e/focus.spec.ts` (delete tests at lines 212, 264, 281)

**Acceptance Criteria:**
- [ ] `getComputedStyle(document.documentElement).getPropertyValue("--page-color")` is `#F3FAFF` in light and `#000832` with `html.dark`; `--primary` is `#0053D0` / `#70F0F9`.
- [ ] `rg -n "@property|grid-line|menu-btn|menu-open|theme-easing|--glow|--button-glow|box-shadow: var\(--shadow" src/styles.css` prints nothing.
- [ ] On every route the header shows the logo image, a "Your names" link to `/#/names` and one theme button; no element in the page has `position: sticky`.
- [ ] The footer shows "Pay with" and the Card, Bitcoin and Monero marks, and the links Wefunder, Trademark owners, Contact.
- [ ] `.btn` computes `border-radius: 999px`, `padding: 16px 32px`, `font-size: 18px`, `font-weight: 600`, no `box-shadow`.
- [ ] The README CSP hashes equal the output of the hash command in Step 6.
- [ ] `npx tsc --noEmit` prints nothing; `npx vitest run` passes.

**Verify:**
```bash
cd /project/git/simplex.domains/webapp && npx tsc --noEmit && npx vitest run   # → no tsc output; "Test Files … passed"
rg -n "@property|grid-line|menu-btn|theme-easing" src/styles.css               # → no output
```

**Steps:**

- [ ] **Step 1: Write the font faces and the token block** at the top of `src/styles.css`, replacing lines 1-163. Keep the existing latin/rest subset pairs and fallback faces for GT-Walsheim 700 and Manrope 500, 600, 700 (lines 5-8, 10-13, 16-19 of the old file); drop the 800 pair (lines 9, 15). Body text is Manrope 500: every visible text on the landing sets 500 or 600 (`landing:153,166,221-222`), so no Regular face is added.

```css
/* simplex.domains' tokens (landing index.html:30-83); status colours from the badges webapp (public/styles.css:33-45, 97-105) */
:root {
  color-scheme: light;
  --display: "GT-Walsheim", "GT-Walsheim fallback", Gilroy, Helvetica, sans-serif;
  --body: "Manrope", "Manrope fallback", sans-serif;
  --mono: ui-monospace, "SF Mono", Menlo, Consolas, "Liberation Mono", "DejaVu Sans Mono", monospace;

  --fs-h1: clamp(28px, 5vw, 48px);
  --fs-h2: clamp(20px, 2.3vw, 24px);
  --fs-lead: clamp(19px, 2.3vw, 24px);
  --fs-body: 18px;
  --fs-small: clamp(14px, 1.5vw, 16px);
  --fs-label: clamp(12px, 1.6vw, 16px);
  --frame-pad: clamp(18px, 3.5vw, 44px);
  --column: 640px;

  --page-color: #F3FAFF;
  --page-image:
    radial-gradient(95% 62% at 85% 4%, oklch(0.90 0.07 255 / 0.45), transparent 62%),
    radial-gradient(135% 108% at 50% 122%, oklch(0.92 0.075 255) 0%, oklch(0.965 0.035 253) 46%, oklch(0.995 0.008 250) 100%);
  --heading: linear-gradient(to bottom, #53C1FF -50%, #0053D0 160%);
  --ink: #0D1B3A;
  --ink-soft: rgba(13, 27, 58, .651);
  --primary: #0053D0;
  --accent: #0197FF;
  --toggle: #001796;
  --sym-back: #023789;
  --line-strong: rgba(0, 83, 208, .4);
  --chip: rgba(0, 83, 208, .06);
  --chip-line: rgba(0, 83, 208, .14);
  --primary-ink: #ffffff;
  --surface: #ffffff;
  --surface-line: rgba(0, 83, 208, .14);
  --backdrop: rgba(6, 20, 56, .38);
  --disabled: rgba(13, 27, 58, .07);
  --on-disabled: rgba(13, 27, 58, .50);

  --ok-fg: #1c7c3f;     --ok-bg: #e8f5ec;     --ok-line: #bfe3cc;
  --warn-fg: #8a6100;   --warn-bg: #fff8e6;   --warn-line: #f0dcae;
  --danger-fg: #b3261e; --danger-bg: #fdecec; --danger-line: #f3c9c6;
}
html.dark {
  color-scheme: dark;
  --page-color: #000832;
  --page-image:
    radial-gradient(92% 62% at 82% -2%, oklch(0.44 0.135 256 / 0.38), transparent 60%),
    radial-gradient(140% 118% at 50% -12%, oklch(0.30 0.105 258) 0%, oklch(0.19 0.088 260) 46%, oklch(0.135 0.075 263) 100%);
  --heading: linear-gradient(to bottom, #70F0F9 0%, #70F0F9 33%, #50D8F1 100%);
  --ink: #EAF2FF;
  --ink-soft: rgba(234, 242, 255, .735);
  --primary: #70F0F9;
  --accent: #02C0FF;
  --toggle: #ffffff;
  --sym-back: #ffffff;
  --line-strong: rgba(112, 240, 249, .42);
  --chip: rgba(112, 240, 249, .08);
  --chip-line: rgba(112, 240, 249, .2);
  --primary-ink: #041038;
  --surface: #0A1C44;
  --surface-line: rgba(112, 240, 249, .18);
  --backdrop: rgba(2, 8, 26, .58);
  --disabled: rgba(255, 255, 255, .09);
  --on-disabled: rgba(255, 255, 255, .45);

  --ok-fg: #7FE3A6;     --ok-bg: rgba(127, 227, 166, .12);     --ok-line: rgba(127, 227, 166, .28);
  --warn-fg: #F5C86A;   --warn-bg: rgba(245, 200, 106, .12);   --warn-line: rgba(245, 200, 106, .28);
  --danger-fg: #FF9F98; --danger-bg: rgba(255, 159, 152, .12); --danger-line: rgba(255, 159, 152, .28);
}
```

- [ ] **Step 2: Write the base, frame and shared components** directly after the tokens (replacing old lines 165-260: base, grid `body::before`, glow `body::after`, sticky `.top`, buttons, chips, labels).

```css
* { box-sizing: border-box; margin: 0; padding: 0; }
[hidden] { display: none !important; }
.sr-only { position: absolute; width: 1px; height: 1px; overflow: hidden; clip-path: inset(50%); white-space: nowrap; }
html { background: var(--page-color); }
body { min-height: 100dvh; display: flex; flex-direction: column; font: 500 var(--fs-body)/1.5 var(--body); color: var(--ink);
  background: var(--page-image), var(--page-color); -webkit-font-smoothing: antialiased; text-rendering: optimizeLegibility; text-wrap: pretty; }
a { color: var(--primary); text-decoration: none; font-weight: 600; }
a:hover { text-decoration: underline; text-underline-offset: 3px; }
:focus-visible { outline: 2px solid var(--accent); outline-offset: 3px; border-radius: 4px; }
svg.i { width: 18px; height: 18px; fill: none; stroke: currentColor; stroke-width: 2; stroke-linecap: round; stroke-linejoin: round; flex: none; }
.mono { font-family: var(--mono); }
.nowrap { white-space: nowrap; }
h1 { font: 700 var(--fs-h1)/1.05 var(--display); letter-spacing: -.025em; background: var(--heading); -webkit-background-clip: text;
  background-clip: text; -webkit-text-fill-color: transparent; color: transparent; padding-bottom: .04em; text-wrap: balance; }
h2 { font: 700 var(--fs-h2)/1.15 var(--display); letter-spacing: -.015em; }
.lead { font-size: var(--fs-lead); font-weight: 500; color: var(--ink-soft); }
.note { font-size: var(--fs-small); font-weight: 500; color: var(--ink-soft); }
.label { font-size: var(--fs-label); font-weight: 600; letter-spacing: .18em; text-transform: uppercase; color: var(--ink-soft); }

/* the landing's header (landing:117-129): not sticky, no bar */
.topbar { display: flex; align-items: center; justify-content: space-between; gap: 16px; padding: var(--frame-pad); }
.brand { display: inline-flex; }
.brand img { height: clamp(36px, 4.4vw, 52px); width: auto; display: block; }
.brand .logo-dark, html.dark .brand .logo-light { display: none; }
html.dark .brand .logo-dark { display: block; }
.top-links { display: flex; align-items: center; gap: clamp(14px, 2vw, 24px); font-size: var(--fs-small); }
.theme-switch { display: inline-flex; background: none; border: 0; cursor: pointer; padding: 6px; margin: -6px; }
.theme-switch svg { width: 22px; height: auto; display: block; overflow: visible; }
.theme-switch svg path { fill: var(--toggle); }
.theme-switch .sun, html.dark .theme-switch .moon { display: none; }
html.dark .theme-switch .sun { display: block; }
body > main { flex: 1 0 auto; }

/* the landing's footer (landing:170-194) */
.footer { display: flex; flex-direction: column; align-items: center; gap: 10px; padding: var(--frame-pad); text-align: center; }
.pay { display: flex; flex-wrap: wrap; align-items: center; justify-content: center; gap: 8px 14px; font-size: var(--fs-small); color: var(--ink-soft); }
.pay span { display: inline-flex; align-items: center; gap: 7px; }
.pay svg.mark { width: 21px; height: 21px; }
.footer-links { display: flex; flex-wrap: wrap; justify-content: center; gap: 8px 14px; font-size: var(--fs-small); }
.footer-links a { font-weight: 500; }
.footer-links .sep { color: var(--line-strong); font-weight: 400; }

/* flat pill buttons (landing:133-141) */
.btn { display: inline-flex; align-items: center; justify-content: center; gap: 8px; padding: 16px 32px; border-radius: 999px;
  border: 1px solid transparent; background: var(--primary); color: var(--primary-ink); font: 600 18px/1.2 var(--body);
  letter-spacing: .01em; white-space: nowrap; cursor: pointer; transition: filter .15s ease; }
.btn:not([disabled]):hover { filter: brightness(1.06); text-decoration: none; }
.btn:not([disabled]):active { transform: translateY(1px); }
.btn.block { display: flex; width: 100%; }
.btn.quiet { background: none; color: var(--primary); border-color: var(--surface-line); }
.btn.danger { background: var(--danger-fg); color: var(--page-color); }
.btn[disabled] { background: var(--disabled); color: var(--on-disabled); border-color: transparent; cursor: default; }
.btn[aria-busy="true"] { cursor: progress; }

/* cards: 1px line, radius 24, no shadow */
.card { background: var(--surface); border: 1px solid var(--surface-line); border-radius: 24px; padding: 24px; }
/* pill fields, as the landing's email field (landing:231-233) */
.field { display: flex; align-items: center; gap: 10px; padding: 16px 24px; border: 1px solid var(--surface-line); border-radius: 999px;
  background: var(--chip); transition: border-color .15s ease; }
.field:focus-within { border-color: var(--primary); }
.field input { flex: 1; min-width: 0; border: 0; background: none; outline: none; font: 500 var(--fs-body) var(--body); color: var(--ink); }
.field input::placeholder { color: var(--ink-soft); }
.field.bad { border-color: var(--danger-fg); }
.field.ok { border-color: var(--ok-fg); }
.notice { display: flex; gap: 12px; padding: 14px 18px; border-radius: 16px; border: 1px solid; font-size: var(--fs-small); text-align: left; }
.notice.ok { color: var(--ok-fg); background: var(--ok-bg); border-color: var(--ok-line); }
.notice.warn { color: var(--warn-fg); background: var(--warn-bg); border-color: var(--warn-line); }
.notice.danger { color: var(--danger-fg); background: var(--danger-bg); border-color: var(--danger-line); }
.notice.info { color: var(--ink); background: var(--chip); border-color: var(--chip-line); }
.chip { display: inline-flex; align-items: center; gap: 6px; padding: 3px 10px; border-radius: 999px; font-size: 14px; font-weight: 600;
  background: var(--chip); color: var(--primary); }
.chip.ok { background: var(--ok-bg); color: var(--ok-fg); }
.chip.warn { background: var(--warn-bg); color: var(--warn-fg); }
.chip.bad { background: var(--danger-bg); color: var(--danger-fg); }
```

- [ ] **Step 3: Re-point the remaining old rules** (old lines 260-940) with this mapping, then delete every declaration whose token has no row (shadows, glows, rings, sheens): `--bg`→`--page-color`, `--ink-2`/`--ink-3`→`--ink-soft`, `--line`/`--line-2`→`--surface-line`, `--surface-2`/`--brand-soft`/`--glass`→`--chip`, `--brand`/`--heading-1`→`--primary`, `--brand-2`→`--accent`, `--brand-line`→`--chip-line`, `--on-brand`→`--primary-ink`, `--ok`→`--ok-fg`, `--ok-soft`→`--ok-bg`, `--bad`→`--danger-fg`, `--bad-soft`→`--danger-bg`, `--warn`→`--warn-fg`, `--warn-soft`→`--warn-bg`, `--scrim`→`--backdrop`. Rename `.alert` rules to `.notice` and `alertBox()` in `src/dom.ts:111-113` to emit `class: "notice <tone>"` with tones `ok | warn | danger | info` (callers passing `"bad"` pass `"danger"`; `rg -n 'alertBox\("bad"' src` → none after the edit). `.panel` keeps working until Task 8 by sharing the `.card` rule: change its selector line 335 to `.panel { background: var(--surface); border: 1px solid var(--surface-line); border-radius: 24px; }`. Delete old lines 807-840 (theme easing, phone menu, `:root::after` scrim), `.top*`, `.icon-btn`, `.ghost-btn`, `.foot*`.

- [ ] **Step 4: Static header in `index.html`.** Replace the body's header with the landing's, plus the link; update theme colours and the preload face list (drop ExtraBold):

```html
<meta name="theme-color" content="#F3FAFF" media="(prefers-color-scheme: light)">
<meta name="theme-color" content="#000832" media="(prefers-color-scheme: dark)">
…faces=[["GT-Walsheim/GT-Walsheim-LC-Bold","GT-Walsheim",700],["Manrope/Manrope-Medium","Manrope",500],["Manrope/Manrope-SemiBold","Manrope",600],["Manrope/Manrope-Bold","Manrope",700]]…
<body>
<header class="topbar">
  <a class="brand" href="/#/" aria-label="SimpleX names"><img class="logo-light" src="/img/logo-light.png" alt="SimpleX" width="400" height="99"><img class="logo-dark" src="/img/logo-dark.png" alt="SimpleX" width="401" height="101"></a>
  <nav class="top-links"><a href="/#/names">Your names</a><button class="theme-switch" type="button" aria-label="Switch light or dark theme"><!-- landing:267-280 sun and moon SVGs, verbatim, with classes "sun" and "moon" --></button></nav>
</header>
<main></main>
</body>
```

- [ ] **Step 5: Frame in `src/views/shell.ts`.** Delete `brand()`, `themeIcons()`, `header()`, `fadeMenu()`, `turnIn()`, `closeMenu()` and its two document listeners, `MENU_MS`, `PHONE_MENU`, `MENU_SHUT`, `MENU_WHOLE`, `THEME_EASE_MS`, `themeEased` (lines 140-267). Keep `isDark()`. Replace `paintBars()`, `toggleTheme()`, `footer()` and `frame()` with:

```ts
const PAY_MARKS: Method[] = ["card", "btc", "xmr"]

function paintBars(): void {
  const ground = getComputedStyle(document.documentElement).getPropertyValue("--page-color").trim()
  for (const meta of document.querySelectorAll<HTMLMetaElement>('meta[name="theme-color"]')) meta.content = ground
}

function toggleTheme(): void {
  const dark = !isDark()
  store.setTheme(dark ? "dark" : "light")
  document.documentElement.classList.toggle("dark", dark)
  paintBars()
}

function footer(): HTMLElement {
  const sep = () => el("span", { class: "sep", "aria-hidden": "true" }, "·")
  return el("footer", { class: "footer" },
    el("div", { class: "pay" }, el("span", {}, "Pay with"), ...PAY_MARKS.map((m) => el("span", {}, methodMark(m), methodName(m)))),
    el("div", { class: "footer-links" },
      el("a", { href: WEFUNDER_URL, target: "_blank", rel: "noopener" }, "SimpleX crowdfunding (Wefunder)"), sep(),
      el("a", { href: "/#/trademarks" }, "Trademark owners"), sep(),
      el("a", { href: `mailto:${CONTACT_EMAIL}` }, "Contact")))
}

// index.html draws the header; the script adds its theme switch and the footer once
function frame(): HTMLElement {
  const main = document.querySelector<HTMLElement>("body > main")!
  if (document.body.dataset.framed) return main
  document.body.dataset.framed = "1"
  document.querySelector(".theme-switch")!.addEventListener("click", toggleTheme)
  document.body.append(footer())
  paintBars()
  return main
}
```
`page()` calls `frame()` where it called the old one (it uses only `main`; the `top` nav highlighting at line 366 and `closeMenu` calls at 350 go). Remove the "Check a code" header entries; the popup stays reachable from the home page's investors tile until Task 4. Delete `menu`, `moon`, `sun` from `LINE_ICONS` in `src/icons.ts`.

- [ ] **Step 6: Update the README's CSP hashes** (README "Hosting", `script-src`) with the output of:
```bash
node -e 'const fs=require("fs"),c=require("crypto");for(const m of fs.readFileSync("index.html","utf8").matchAll(/<script>([\s\S]*?)<\/script>/g))console.log("sha256-"+c.createHash("sha256").update(m[1]).digest("base64"))'
```

- [ ] **Step 7: Delete the menu e2e tests** (`e2e/search.spec.ts` tests at lines 118, 142, 202; `e2e/focus.spec.ts` tests at lines 212, 264, 281) and the ExtraBold font files. Run the Verify commands.

- [ ] **Step 8: Commit**
```bash
git -C /project/git/simplex.domains add webapp/src/styles.css webapp/src/views/shell.ts webapp/src/icons.ts webapp/src/dom.ts webapp/src/views webapp/index.html webapp/README.md webapp/public/fonts webapp/e2e/search.spec.ts webapp/e2e/focus.spec.ts
git -C /project/git/simplex.domains commit -m "webapp: landing tokens, header and footer"
```

### Task 2: Whole-screen builders

**Goal:** Every view draws whole screens from state through `shell.draw()`, with typed-in inputs kept as nodes, and the motion, view-transition, slot, transition-table and focus-restoration code is gone.

**Files:**
- Delete: `src/motion.ts`, `src/motion-values.ts`, `src/focus.ts`, `src/views/slots.ts`, `test/motion-values.test.ts`, `test/machine.test.ts`, `e2e/motion.spec.ts`, `e2e/cuts.spec.ts`, `e2e/cuts.ts`, `e2e/crossing.spec.ts`, `e2e/sweep.ts`, `e2e/inplace.spec.ts`, `e2e/inplace-checkout.spec.ts`, `e2e/inplace-names.spec.ts`, `e2e/inplace-search.spec.ts`
- Rename: `src/screens/machine.ts` → `src/screens/hold.ts` (keeps only `Hold` and `NO_LEAD`)
- Modify: `src/screens/search.ts:121-128`, `src/screens/checkout.ts:89-100`, `src/screens/order.ts:194-223`, `src/screens/names.ts:87,182,319-330`, `src/screens/routes.ts:18-29` (delete `moves`, `*Machine`, `routes`)
- Modify: `src/views/shell.ts` (new contract), `src/views/effects.ts` (delete `oneAtATime`), `src/views/hold.ts` (gains `holdNotice`, `secondsLeft`), `src/dom.ts`, `src/icons.ts` (adds `back`), `src/views/search.ts`, `src/views/checkout.ts`, `src/views/codes.ts`, `src/views/names.ts`, `src/views/order.ts`, `src/main.ts`, `src/styles.css`
- Modify: `test/effects.test.ts`, `test/screens-search.test.ts`, `test/screens-checkout.test.ts`, `test/screens-order.test.ts`, `test/screens-names.test.ts`, `test/screens-routes.test.ts`, `test/hold.test.ts`
- Modify: `e2e/helpers.ts`, `playwright.config.ts`, `e2e/robustness.spec.ts:524` (drop the `@motion` tag)

**Acceptance Criteria:**
- [ ] `fd 'motion|focus|slots' src` prints nothing; `rg -n "view-transition|@keyframes|startViewTransition|\.animate\(|getAnimations" src` prints nothing.
- [ ] `rg -n "moves|Machine|headingFocus|routes\b" src/screens test` prints nothing.
- [ ] `rg -n "Built<|Build<|unchanging|changeTo|drawAs|live\(|syncList|screens<" src` prints nothing.
- [ ] Typing in the search field, then a redraw (search answer arriving) leaves the same `<input>` node focused with its caret where it was (e2e `robustness.spec.ts` "a name typed with .simplex…" passes in Chromium).
- [ ] A screen of another kind moves focus to its `h1` (`tabindex="-1"`) unless focus is in the header or footer; a redraw of the same kind returns focus to the control with the same `data-focus` key.
- [ ] `npx tsc --noEmit` prints nothing; `npx vitest run` passes.

**Verify:**
```bash
cd /project/git/simplex.domains/webapp && npx tsc --noEmit && npx vitest run    # → no tsc output; all test files pass
ulimit -c 0 && source $S/tools/webkit-env.sh && SLOTS=3 npx playwright test e2e/search.spec.ts e2e/paid.spec.ts --project=chromium   # → "passed", 0 failed
```

**Steps:**

- [ ] **Step 1: Shrink the screens' shared module.** `git mv src/screens/machine.ts src/screens/hold.ts`; its content becomes:

```ts
// a wait the service asked for: when it ends, on the service's clock, and the words said before the countdown
export interface Hold { until: number; lead: string }
export const NO_LEAD = ""
```
Update every `from "./machine.ts"` / `"../screens/machine.ts"` import. Delete `moves` and `searchMachine` (`screens/search.ts:121-128`), `to`, `REFUSALS`, `moves`, `checkoutMachine` (`screens/checkout.ts:89-100`), `to`, `REGISTRATION`, `CLOSED`, `GONE`, `moves`, `orderMachine` (`screens/order.ts:194-223`), `listMachine`, `restoreMachine`, `to`, `editMoves`, `editMachine` (`screens/names.ts:87,182,319-330`), the `ListKind`/`RestoreKind` types if unused, and `routes`, `FORWARD`, `BACK` (`screens/routes.ts:18-29`). Delete the `describe("moves")` blocks and the `headingFocus` block from the screen tests, `describe("routes")` from `test/screens-routes.test.ts`, `test/machine.test.ts`, `test/motion-values.test.ts`, and the `oneAtATime` block from `test/effects.test.ts` with `oneAtATime` itself from `src/views/effects.ts`.

- [ ] **Step 2: The screen contract in `src/views/shell.ts`.** Delete `screens()`, `Built`, `Build`, `page()`, `placeholderSoon()`, `placeholder()`, `withBody()`, `twoCols()`, `crumbs()`, `crumbsTo()`, `lookOf()`, `transient`, `nameTab()`, `waitingPage`, `SHOWN_PAGE`, `drawnView`, `routeView`, the `noteRoom` observer and the motion imports. `pageHead()` becomes `screen()`, `status()` becomes `outcome()`. Keep the route token, history position and scroll memory (`lines 26-118`), `popup()`/`closePopup()` without animation (Task 8 deletes them), the offline note without `overlayIn/Out` and without `--offline-room`. Add:

```ts
import type { Kid } from "../dom.ts"

export type Builder<S> = (state: S, signal: AbortSignal) => HTMLElement
export interface ScreenHead { title: Kid; lead?: Kid; back?: string }
export type Tone = "ok" | "warn" | "danger" | "info"
export interface Outcome { tone: Tone; icon: IconName; title: string; text: Kid; action?: Node | null; reference?: string; extra?: Kid }

let shownKind: string | null = null
let shownRoute = -1
let screenStop: AbortController | null = null
// nodes a reader types into, kept for the route so a redraw moves them rather than rebuilding them
const keptNodes = new Map<string, HTMLElement>()

export function kept<T extends HTMLElement>(key: string, make: () => T): T {
  const known = keptNodes.get(key)
  if (known) return known as T
  const made = make()
  keptNodes.set(key, made)
  return made
}

/** Builds the screen for `kind` and swaps it in whole; the last screen's timers end with its signal. */
export function draw(kind: string, build: (signal: AbortSignal) => HTMLElement): void {
  const main = frame()
  const active = document.activeElement instanceof HTMLElement ? document.activeElement : null
  const caret = active instanceof HTMLInputElement ? [active.selectionStart, active.selectionEnd] as const : null
  const focusKey = active?.dataset.focus ?? null
  const first = shownKind === null
  const changed = kind !== shownKind || shownRoute !== route
  screenStop?.abort()
  screenStop = new AbortController()
  const next = build(screenStop.signal)
  main.replaceChildren(next)
  main.inert = false
  shownKind = kind
  shownRoute = route
  document.title = titleOf(next)
  if (!first) settleFocus(next, active, caret, changed ? null : focusKey)
  if (scrollTarget !== null) {
    scrollTo(0, scrollTarget)
    scrollTarget = null
  }
}

function settleFocus(next: HTMLElement, active: HTMLElement | null, caret: readonly [number | null, number | null] | null, focusKey: string | null): void {
  if (active?.isConnected && next.contains(active)) {
    active.focus({ preventScroll: true })
    if (caret && active instanceof HTMLInputElement) active.setSelectionRange(caret[0], caret[1])
    return
  }
  const same = focusKey ? next.querySelector<HTMLElement>(`[data-focus="${CSS.escape(focusKey)}"]:not([disabled])`) : null
  if (same) return same.focus({ preventScroll: true })
  // focus on the header or footer stays there; focus that was in main, even in a slide now inert, goes to the new heading
  if (active && active !== document.body && active.isConnected && !active.closest("body > main")) return
  const heading = next.querySelector<HTMLElement>("h1")
  if (!heading) return
  heading.tabIndex = -1
  heading.focus({ preventScroll: true })
}

const SITE_TITLE = "SimpleX names"
const titleOf = (next: HTMLElement) => {
  const heading = next.querySelector("h1")?.textContent?.trim()
  return heading ? `${heading} · ${SITE_TITLE}` : SITE_TITLE
}

export function screen(head: ScreenHead, ...body: Kid[]): HTMLElement {
  return el("section", { class: "screen" },
    head.back ? el("a", { class: "back", href: head.back }, icon("back"), "Back") : null,
    el("h1", {}, head.title),
    head.lead ? el("p", { class: "lead" }, head.lead) : null,
    ...body)
}

// the badges pattern: a heading, one notice, one action
export function outcome(o: Outcome): HTMLElement {
  return el("section", { class: "screen outcome" },
    el("h1", {}, o.title),
    alertBox(o.tone, o.icon, null, o.text),
    o.extra ?? null,
    o.action ?? null,
    o.reference ? el("p", { class: "note ref" }, "Reference ", el("span", { class: "mono" }, o.reference)) : null)
}

// shown only in a document that has drawn nothing yet, while its first read is on its way
export const loadingScreen = (title: string) => el("section", { class: "screen" }, el("h1", {}, title))
```
`nextRoute()` clears `keptNodes` unless the call is a redraw (`redrawing && drawnRoute === route`), sets `main.inert = true` when the route is new so the last screen stays visible but inactive until the new route draws, and no longer calls `focus.drop()`. Add `back: "M15 18l-6-6 6-6"` to `LINE_ICONS` in `src/icons.ts` (badges `chevronLeft`). `setRenderer(render)` loses its popup argument.

- [ ] **Step 3: `src/dom.ts`.** Export `Kid`. Delete `working`, `idle`, `release`, `SETTLE_MS`, `settling`, the `focus.keep` call in `disabled`, `rollTo` (in `waitOut` the count is written with `count.textContent = String(left)`). `liveRegion(cls)` returns the plain element. Move `setAction` here from `src/views/slots.ts:32-40`, rewritten:

```ts
export function setAction(button: HTMLButtonElement, busy: boolean, held: boolean): HTMLButtonElement {
  button.disabled = busy || held
  if (busy) button.setAttribute("aria-busy", "true")
  else button.removeAttribute("aria-busy")
  return button
}
```
Move `holdNotice` and `secondsLeft` from `src/views/slots.ts:23-30` to `src/views/hold.ts`; delete `slots.ts`.

- [ ] **Step 4: Convert each view mechanically** (no visual redesign yet). In every `buildX(s, signal): Built<S>` the `update` closure goes: what it set becomes direct construction from `s`. Rules: `live(x)` → `x`; `changeTo(part, ...kids)` → build `part` with `kids`; `drawAs(part, key, draw)` → `draw()` inline; `unchanging([...])` → the node; `sections: [a, b]` → `el("section", { class: "screen" }, a, b)` until the screen's own task restyles it; `focus.*` calls are deleted; `transitionsIdle().then(f)` → `f()`; `moving()` → `true` where it chose smooth scrolling, deleted elsewhere. Each view keeps a module `state` and one `render()`:

```ts
// src/views/order.ts (search, checkout, names follow the same shape)
const BUILDERS: Record<OrderKind, Builder<OrderState>> = { … }
function dispatch(event: OrderEvent): void {
  const before = state
  state = reduce(state, event)
  if (!isRoute(pageRoute)) return
  const kind = screenOf(state)
  draw(kind, (signal) => BUILDERS[kind](state, signal))
  if (revealDue(state)) reveal(state.view!, state.record!)
  // as old views/order.ts:83: keepCodes dispatches record-changed itself, so it runs only when the screen or record moved
  if (kind !== screenOf(before) || state.record !== before.record) keepCodes(state)
  wakeOnTime()
}
```
Kept nodes, each created with `kept(key, make)` inside its builder: the search field box (`"search-field"`, `views/search.ts:238-260`), the checkout link fields (`"link-contact"`, `"link-channel"`, `views/checkout.ts:258-259`), the checkout code entry (`"checkout-code"`, `views/checkout.ts:343-362`), the Stripe form container (`"stripe-form"`, `views/order.ts:478`; it mounts once per route and is destroyed in `whenLeaving`, not by the screen signal), the restore field (`"restore-code"`, `views/names.ts:458-460`), the edit link fields (`"edit-contact"`, `"edit-channel"`, `views/names.ts:637,639`). The search reducer's `typed` event updates `state` without calling `draw` (the field already shows what was typed; its input listener clears `.bad` on the box itself). Controls that must keep focus over a same-kind redraw carry `data-focus`: the method radios (`data-focus="method-<m>"`), Continue/Pay (`"pay"`), the saved checkbox (`"saved"`), Search (`"search"`), Restore (`"restore"`), Save links (`"save"`), Forget/Keep (`"forget"`, `"keep"`), row actions (`"<rowKey>-<action>"`). Order and edit pages draw `loadingScreen("Opening the order…")` / `loadingScreen("Reading the name…")` only when `main` has no children yet.

- [ ] **Step 5: CSS.** Delete from `src/styles.css` every view-transition rule (old 713-775), every `@keyframes` (690-694, 788 and the `scene`'s 306), the motion variables (650-660), `.working`/`.settling`/spinner (777-786), skeleton/shimmer (699-702), `.pending`, `.outgoing`, `.entering` and the offline room (704-712). Add the column that `screen()` and `outcome()` draw into:

```css
/* inner pages: one centred column with Back above the heading (badges screens.ts:35-43) */
.screen { width: 100%; max-width: calc(var(--column) + 32px); margin-inline: auto; padding: 12px 16px 48px; display: flex; flex-direction: column; gap: 22px; }
.back { display: inline-flex; align-items: center; gap: 4px; font-size: var(--fs-small); align-self: flex-start; }
.outcome { text-align: center; }
.outcome .btn { align-self: center; }
```

- [ ] **Step 6: e2e support.** In `e2e/helpers.ts` delete the `motion-values` import, `fades`, `settle`, `pressInPlace`, `markNodes`, `keptNodes` and the `unexpectedMoves` fixture (`test` becomes `base.extend<{}>({ baseURL: … })`). Delete the nine motion spec/helper files listed above. In `playwright.config.ts` delete `MOTION`, `WEBKIT_MOTION_WORKERS`, `SAFARI_MOTION`, `GENERAL`, `FIREFOX_158_MOTION` and the four `*-motion` projects, leaving `chromium`, `firefox`, `webkit`; keep `use.reducedMotion: "reduce"`. Remove `{ tag: "@motion" }` from `e2e/robustness.spec.ts:524`.

- [ ] **Step 7: Run Verify, then commit**
```bash
git -C /project/git/simplex.domains add -A webapp/src webapp/test webapp/e2e webapp/playwright.config.ts
git -C /project/git/simplex.domains commit -m "webapp: draw whole screens, drop motion layer"
```

### Task 3: Purchase track

**Goal:** Search, checkout and the payment screens sit on one sliding CSS track adapted from badges `main.ts:265-466`; every other screen swaps whole.

**Files:**
- Create: `src/views/track.ts`
- Create: `e2e/track.spec.ts`
- Modify: `src/views/shell.ts` (`draw` gains a `step` argument)
- Modify: `src/views/search.ts`, `src/views/checkout.ts`, `src/views/order.ts` (pass their step)
- Modify: `src/styles.css` (track rules; the stage tracker's `.track` becomes `.stages`), `src/views/order.ts:766-779` (`class: "stages"`)

**Acceptance Criteria:**
- [ ] On search, checkout and pay screens `main > .track > .rail` exists with three `.slide` children; exactly one is not `inert`.
- [ ] After Register the rail's computed transform is `translateX(-W)` where W is the track's width (shift −1); after Back it is 0; on a pay screen it is −2.
- [ ] Your names, restore, edit, trademarks and every order screen other than `pay-crypto`/`pay-card` render without `.track`.
- [ ] Moves between non-adjacent steps, first mounts and reduced motion set `--slide:0ms`.
- [ ] The track's height follows the slide in view when its content grows (ResizeObserver), so nothing is clipped.
- [ ] `e2e/track.spec.ts` passes in Chromium; `npx tsc --noEmit` prints nothing; `npx vitest run` passes.

**Verify:**
```bash
cd /project/git/simplex.domains/webapp && npx tsc --noEmit && npx vitest run
ulimit -c 0 && source $S/tools/webkit-env.sh && SLOTS=3 npx playwright test e2e/track.spec.ts --project=chromium --project=firefox   # → 2 passed per project
```

**Steps:**

- [ ] **Step 1: Write the failing e2e test `e2e/track.spec.ts`**

```ts
import { expect, type Page } from "@playwright/test"
import { search, startOrder, test, uniqueLabel } from "./helpers.ts"

// the rail's shift in slides: its translateX over the track's width
const shift = (page: Page) => page.locator(".rail").evaluate((rail) => {
  const x = new DOMMatrix(getComputedStyle(rail).transform).m41
  return Math.round(x / rail.parentElement!.getBoundingClientRect().width)
})
const liveSlides = (page: Page) => page.locator(".track > .rail > .slide:not([inert])")

test("search, checkout and payment move along one track a step at a time, the other steps inert", async ({ page }) => {
  await search(page, uniqueLabel("track"))
  await expect(page.locator(".rail > .slide")).toHaveCount(3)
  expect(await shift(page)).toBe(0)
  await page.getByRole("button", { name: "Register" }).click()
  await expect(page.getByRole("heading", { name: "Check your order" })).toBeFocused()
  expect(await shift(page)).toBe(-1)
  await expect(liveSlides(page)).toHaveCount(1)
  await page.goBack()
  await expect(page.getByLabel("Name", { exact: true })).toBeVisible()
  expect(await shift(page)).toBe(0)
})

test("a payment screen is the track's third step, and Your names swaps the whole page", async ({ page }) => {
  await startOrder(page, uniqueLabel("trackpay"), { method: "btc" })
  await expect(page.getByRole("heading", { name: "Pay with Bitcoin" })).toBeVisible()
  expect(await shift(page)).toBe(-2)
  await page.getByRole("link", { name: "Your names" }).click()
  await expect(page.getByRole("heading", { name: "Your names" })).toBeVisible()
  await expect(page.locator(".track")).toHaveCount(0)
})
```
Run it: `SLOTS=3 npx playwright test e2e/track.spec.ts --project=chromium` → FAIL (`.rail` not found).

- [ ] **Step 2: Write `src/views/track.ts`** (adapted from badges `main.ts:265-268` track/rail/panels, `412-436` inert, reduced motion, `SLIDE_MS`, panel height and `moveTrack`, `446-466` `showIndex`):

```ts
import { el } from "../dom.ts"

export type TrackStep = 0 | 1 | 2
export const SEARCH_STEP: TrackStep = 0
export const CHECKOUT_STEP: TrackStep = 1
export const PAYMENT_STEP: TrackStep = 2
const STEPS = 3
// the badges wizard's slide (simplex-badge-service/web/src/main.ts:423)
const SLIDE_MS = 320

const slides = Array.from({ length: STEPS }, () => el("div", { class: "slide" }))
const rail = el("div", { class: "rail" }, ...slides)
const track = el("div", { class: "track" }, rail)
let index: TrackStep = SEARCH_STEP

const reducedMotion = () => matchMedia("(prefers-reduced-motion: reduce)").matches
const slideMs = (smooth: boolean) => smooth && !reducedMotion() ? SLIDE_MS : 0

function fitHeight(ms: number): void {
  const height = Math.ceil(slides[index].getBoundingClientRect().height)
  track.setAttribute("style", height > 0 ? `--slide:${ms}ms;height:${height}px` : `--slide:${ms}ms`)
}

// a slide whose content grows or shrinks after it was placed (a link preview, an answer) resizes the track at once
const resized = new ResizeObserver(() => fitHeight(0))

function moveTrack(smooth: boolean): void {
  const ms = slideMs(smooth)
  rail.setAttribute("style", `--slide:${ms}ms;transform:translateX(-${index * 100}%)`)
  fitHeight(ms)
}

/** Puts a purchase screen on its step; a move to the next or previous step slides, any other move is at once. */
export function placeOnTrack(main: HTMLElement, step: TrackStep, screen: HTMLElement): void {
  const mounted = main.firstChild === track
  const adjacent = Math.abs(step - index) === 1
  if (!mounted) {
    for (const s of slides) s.replaceChildren()
    main.replaceChildren(track)
  }
  slides[step].replaceChildren(screen)
  resized.disconnect()
  index = step
  slides.forEach((s, i) => { s.inert = i !== index })
  moveTrack(mounted && adjacent)
  resized.observe(slides[index])
}

export function leaveTrack(main: HTMLElement, screen: HTMLElement): void {
  resized.disconnect()
  main.replaceChildren(screen)
}
```

- [ ] **Step 3: Track CSS** (badges `styles.css:434-449`), appended to `src/styles.css`; rename the stage tracker's old `.track` rules (old line 376 onward) to `.stages` and its builder's class in `src/views/order.ts`:

```css
/* .track clips and takes the height of the slide in view; .rail is the row of three slides and is what moves */
.track { overflow: hidden; transition: height var(--slide, 320ms) cubic-bezier(.22, .61, .36, 1); }
.rail { display: flex; align-items: flex-start; width: 100%; transition: transform var(--slide, 320ms) cubic-bezier(.22, .61, .36, 1); }
.slide { flex: 0 0 100%; min-width: 0; }
```

- [ ] **Step 4: `draw` takes the step.** In `src/views/shell.ts` change the signature to `draw(kind: string, build: (signal: AbortSignal) => HTMLElement, step: TrackStep | null = null)` and replace `main.replaceChildren(next)` with `if (step === null) leaveTrack(main, next); else placeOnTrack(main, step, next)`. Search passes `SEARCH_STEP` for `search:*` kinds (`status:bad-app-link` passes `null`); checkout passes `CHECKOUT_STEP` for every checkout kind including its refusal screens (badges `main.ts:513-536` replaces the checkout panel the same way); order passes `PAYMENT_STEP` for `pay-crypto` and `pay-card`, `null` otherwise. When a step is entered from above the viewport (the track's top is above `scrollY`), `draw` scrolls to the track's top at once before placing the screen.

- [ ] **Step 5: Run Verify; commit**
```bash
git -C /project/git/simplex.domains add webapp/src/views/track.ts webapp/src/views/shell.ts webapp/src/views/search.ts webapp/src/views/checkout.ts webapp/src/views/order.ts webapp/src/styles.css webapp/e2e/track.spec.ts
git -C /project/git/simplex.domains commit -m "webapp: slide purchase steps on one track"
```

### Task 4: Home with search

**Goal:** The home page is the simplex.domains page with the countdown replaced by the search field, and every search answer appears under the field as one card or one line.

**Files:**
- Modify: `src/views/search.ts` (rewrite: ~547 → ~300 lines)
- Modify: `src/screens/search.ts` (drop `applied`, `code-removed`, `codeFor`, `card-due`, `searching.card`)
- Modify: `test/screens-search.test.ts`
- Modify: `src/icons.ts` (delete `wefunderMark` and icons left unused), `src/views/shell.ts` (delete `INVEST_LINE`, `INVEST_AMOUNT`, `BADGES_URL` if unused), `src/styles.css` (home rules; delete `.hero`, `.bento`, `.tile`, `.scene`, `.showcase`, `.invest`, `.result*`, `.banner*`, `.under-search`, `.hint`)
- Modify: `e2e/search.spec.ts`, `e2e/codes.spec.ts`, `e2e/focus.spec.ts`, `e2e/robustness.spec.ts`, `e2e/helpers.ts`

**Acceptance Criteria:**
- [ ] The home shows, top to bottom: `h1` "Your SimpleX domain", the sub line "One name for your public channel and contact address", the search field with ".simplex" and a Search button, the answer area, the "Invest to claim your name" pill (link to `WEFUNDER_URL`, new tab) with the note "Invest in SimpleX Chat to get a name for 5 years", then the footer.
- [ ] No countdown, "NEW" pill, how-it-works tiles, animated scene or code banner exists (`rg -n "bento|scene|playScene|NEW|applied" src/views/search.ts` prints nothing).
- [ ] Available: one card with the name, its price, its term ("2 years + 30 days") and Register. Taken: the name, its chip and the entities it points at. Reserved, rule, could-not-check: one line each. Rate limited: one countdown line and Search disabled.
- [ ] Opened from the app, from a retarget, or after a move: one notice line above the field; no invest block.
- [ ] Vitest `screens-search` covers: `searched` sets `searching: { label }`; `answered` clears it; no `applied` field remains.
- [ ] e2e `search.spec.ts` "search answers every kind of name", `robustness.spec.ts` "a slow answer…" and "a name typed with .simplex…" pass in Chromium.

**Verify:**
```bash
cd /project/git/simplex.domains/webapp && npx tsc --noEmit && npx vitest run test/screens-search.test.ts   # → passes
ulimit -c 0 && source $S/tools/webkit-env.sh && SLOTS=3 npx playwright test e2e/search.spec.ts e2e/robustness.spec.ts --project=chromium   # → 0 failed
```

**Steps:**

- [ ] **Step 1: Reducer tests first.** In `test/screens-search.test.ts` delete the `codeFor` block and the `code-removed`/`card-due` cases; add:

```ts
describe("reduce: searching", () => {
  it("a search under way keeps the last answer on screen until the new one comes", () => {
    const found = reduce(reduce(initialSearch(), { type: "searched", label: "bakery", hold: null }), { type: "answered", seq: 1, label: "bakery", answer: AVAILABLE, mine: null, now: 0 })
    const next = reduce({ ...found, typed: "harbour" }, { type: "searched", label: "harbour", hold: null })
    expect(next.searching).toEqual({ label: "harbour" })
    expect(next.answer).toEqual(found.answer)
  })
  it("the opened event carries no code", () => {
    expect(Object.keys(initialSearch())).not.toContain("applied")
  })
})
```
(`AVAILABLE` is the file's existing available-answer fixture.) Run → FAIL.

- [ ] **Step 2: Reducer.** In `src/screens/search.ts`: `searching: { label: string } | null`; delete `applied` from `SearchState`, `initialSearch`, the `opened` event and `opened()`; delete `codeFor`, the `card-due` and `code-removed` events and cases. Run the test → PASS.

- [ ] **Step 3: The home builder** in `src/views/search.ts` (layout of `landing:284-313` with the clock block replaced):

```ts
const HEADLINE = "Your SimpleX domain"
const SUB = "One name for your public channel and contact address"
const INVEST_CTA = "Invest to claim your name"
const INVEST_NOTE = "Invest in SimpleX Chat to get a name for 5 years"

function homeScreen(s: SearchState, signal: AbortSignal): HTMLElement {
  return el("section", { class: "home" },
    el("div", { class: "vspace" }),
    el("div", { class: "headline" }, el("h1", {}, HEADLINE), el("p", { class: "sub" }, SUB)),
    el("div", { class: "vspace cap" }),
    openingLine(s),
    searchBox(s),
    el("div", { class: "answer", role: "status", "aria-live": "polite" }, answerOf(s, signal)),
    el("div", { class: "vspace cap" }),
    s.opening === "plain" ? el("div", { class: "cta" },
      el("a", { class: "btn", href: WEFUNDER_URL, target: "_blank", rel: "noopener" }, INVEST_CTA),
      el("p", { class: "note" }, INVEST_NOTE)) : null,
    el("div", { class: "vspace" }))
}
```
`searchBox(s)` returns the kept `"search-field"` node: `el("div", { class: "search-row" }, el("label", { class: "field" }, input, el("span", { class: "tld" }, ".simplex")), searchButton)`; the builder sets `searchButton.disabled = s.paused` and `aria-busy` while `s.searching`. `openingLine(s)` is one `alertBox("info", …)` from the old `bannerOf`/`movedBanner` text (`views/search.ts:300-320`) or `null`. `answerOf(s, signal)` returns, in this order: the pause line (`waitOut` "Searching is paused: try again in N seconds.") when `s.paused && s.hold`; the rule line (`ruleLine`, lines 344-347); the unchecked line "This name could not be checked. " plus `link("Try again", retry)`; for a found answer the available card (name via `nameTitle`, `"${letters} · ${termText(years)}"`, `dollars(price)`, Register or "Register with your payment" with the register note under it, from `availableResult`/`registerNoteOf`, lines 393-431, minus every code branch) or the taken card (`registeredResult`, lines 374-391, with `takenResult` as a `.card`) or the reserved line "Reserved names are not sold here. If it is yours to use, write to " + mailto `CONTACT_EMAIL` + "."; else nothing. Delete `teamPopup`, `bento`, `playWhenSeen`, `playScene`, `SCENE_*`, `PREVIEW_*`, `SEARCH_CARD_AFTER_MS`, `removeCode`, `coveredBy`, `card-due` scheduling, `shownSearch`. `register()` builds the checkout with `code: null`.

- [ ] **Step 4: Home CSS** (landing `:146-168` minus the clock and the `rise` animation):

```css
/* the header's and footer's height on a desktop, so the home fills the first screen as the landing's hero does */
:root { --frame-h: 260px; }
.home { display: flex; flex-direction: column; align-items: center; text-align: center; padding-inline: 16px;
  min-height: calc(100dvh - var(--frame-h)); }
.home .vspace { flex: 1; }
.home .vspace.cap { min-height: 18px; max-height: 108px; }
.headline { display: flex; flex-direction: column; align-items: center; width: 100%; max-width: 600px; container-type: inline-size; }
.headline h1 { font-size: clamp(28px, 10.3cqw, 66px); line-height: 1.02; white-space: nowrap; }
.headline .sub { font-size: clamp(11px, 3.92cqw, 26px); margin-top: 3cqw; white-space: nowrap; color: var(--ink-soft); }
.search-row { display: flex; gap: 10px; width: 100%; max-width: var(--column); }
.search-row .field { flex: 1; }
.search-row .tld { color: var(--ink-soft); }
.answer { width: 100%; max-width: var(--column); margin-top: 18px; text-align: left; }
.answer .card { display: grid; gap: 12px; }
.cta { display: flex; flex-direction: column; align-items: center; gap: clamp(14px, 1.8vh, 20px); width: 100%; }
.cta .btn { width: 100%; max-width: 384px; }
@media (max-width: 480px) { .search-row { flex-direction: column; } }
```

- [ ] **Step 5: e2e.** Delete the tests that use the popup or tiles: `search.spec.ts` "the home page's investors tile…", "on a touch phone the investors tile…"; `codes.spec.ts` "a prepaid code is checked, applied on search…" (rewritten in Task 5), "a code used on an order page leads to search…", "a code applied from the header popup…", "removing an applied code…"; `focus.spec.ts` "a name typed but not searched stays when a code is applied…", "removing the code from a result…"; `robustness.spec.ts` "a code checked from the header applies…", "a popup stays open…", "a link still being checked is kept when a code is applied…". Update `search.spec.ts` "search answers every kind of name" to the new texts (reserved line, card selectors `.answer .card`, entities `.answer .entity`), and `robustness.spec.ts:289` to `.answer .entity`. Run Verify.

- [ ] **Step 6: Commit**
```bash
git -C /project/git/simplex.domains add webapp/src webapp/test/screens-search.test.ts webapp/e2e
git -C /project/git/simplex.domains commit -m "webapp: landing home with search answers"
```

### Task 5: One-page checkout

**Goal:** Checkout is one page in this order: summary card, optional links with previews, payment method, "I have a code", the name code with Copy and the saved checkbox, and one Pay button; the separate name-code page and every code popup are gone.

**Files:**
- Modify: `src/views/checkout.ts` (rewrite: ~640 → ~420 lines)
- Modify: `src/views/codes.ts` (rewrite: popup → `codeField`, ~181 → ~90 lines)
- Modify: `src/screens/checkout.ts` (drop `page`, `entry`, `entry-opened`, `CheckoutPage`)
- Modify: `src/screens/routes.ts` (drop `"name-code"`), `src/main.ts` (drop `ownerCodePage`, `codePopup`)
- Modify: `src/session.ts` (delete `APPLIED`, `appliedCode`, `setAppliedCode`)
- Modify: `src/views/names.ts:180-185,389-412` (no `setAppliedCode`; code rows lose Use; Add a code uses `codeField`), `src/views/order.ts:990-1006` (taken-with-code action becomes "Search another name")
- Modify: `test/screens-checkout.test.ts`, `test/screens-routes.test.ts`, `src/styles.css` (checkout rules; delete `.code-entry`, `.covered*`, `.sub-links`, `.why`, `.cols`, `.summary`, `.mini-invest`, `.crumb*`)
- Modify: `e2e/helpers.ts` (`startOrder`, `savedCode`), `e2e/codes.spec.ts`, `e2e/search.spec.ts`, `e2e/robustness.spec.ts`, `e2e/focus.spec.ts`, `e2e/app.spec.ts`

**Acceptance Criteria:**
- [ ] `/#/checkout` shows blocks in DOM order: `.summary-card`, `.links` (absent for an app order), `.methods` (or the covered line, or the waiting-order notice), `.code-entry` labelled "I have a code" (absent for an app order), `.name-code` with `data-testid="owner-code"`, Copy and the checkbox "I have saved my name code" (absent for an app order), then one button whose name starts with "Pay" (or "Register" with a code, "Continue" for an app order).
- [ ] The Pay button is disabled until the checkbox is ticked, while the order is sending, while a card order waits and during a hold.
- [ ] A valid prepaid code typed into "I have a code" makes the total `$0.00` with the old price struck through and hides the methods; a code for longer names shows "This code covers names of N or more letters; <name> has M." under the field.
- [ ] `/#/checkout/code` routes to search; `rg -n "codePopup|applyCode|useCode|appliedCode|ownerCodePage|owner-code\"" src` prints only the `data-testid`.
- [ ] Opening the checkout (not from the app) stores the name code as a pending code, so Your names lists it before any order exists (e2e `codes.spec.ts` "an owner code shown at checkout is kept in Your names…").
- [ ] e2e `search.spec.ts` "the checkout explains every refusal…", `codes.spec.ts` code-at-checkout tests and `paid.spec.ts` "a Bitcoin order pays in two parts…" pass in Chromium.

**Verify:**
```bash
cd /project/git/simplex.domains/webapp && npx tsc --noEmit && npx vitest run    # → passes
ulimit -c 0 && source $S/tools/webkit-env.sh && SLOTS=3 npx playwright test e2e/search.spec.ts e2e/codes.spec.ts e2e/paid.spec.ts e2e/app.spec.ts --project=chromium   # → 0 failed
```

**Steps:**

- [ ] **Step 1: Reducer tests first.** In `test/screens-checkout.test.ts` replace every `initialState("checkout", c)` / `initialState("owner-code", c)` with `initialState(c)`, delete the `entry-opened` cases, and add:

```ts
describe("screenOf", () => {
  it("is the one checkout page until the service refuses the order", () => {
    expect(screenOf(initialState(CHECKOUT))).toBe("checkout")
    expect(screenOf(reduce(initialState(CHECKOUT), { type: "order-refused", reason: "failed" }))).toBe("status:failed")
    expect(screenOf(reduce(initialState(CHECKOUT), { type: "hold-started", until: 5 }))).toBe("status:too-many")
  })
  it("a code applied clears the code's last refusal and stays on the page", () => {
    const refused = reduce(initialState(CHECKOUT), { type: "order-refused", reason: "code", error: "This code was already used." })
    const applied = reduce(refused, { type: "code-applied", code: CODE })
    expect(applied.checkout.codeError).toBeUndefined()
    expect(screenOf(applied)).toBe("checkout")
  })
})
```
(`CHECKOUT` and `CODE` are the file's fixtures.) In `test/screens-routes.test.ts` change the `/checkout/code` case to expect `"search"`. Run → FAIL.

- [ ] **Step 2: Reducer and route.** `src/screens/checkout.ts`: delete `CheckoutPage`, `page`, `entry`, `entry-opened`; `export type CheckoutKind = "checkout" | \`status:${Refusal}\``; `initialState(checkout: Checkout): CheckoutState`; `screenOf = (s) => s.refusal ? \`status:${s.refusal}\` : "checkout"`. `src/screens/routes.ts`: delete `"name-code"` from `View` and `viewOf`. Run tests → PASS.

- [ ] **Step 3: `src/views/codes.ts`** keeps `CodeVerdict`, `STATE_LINES`, `CODE_ERRORS`, `covers`, `MISTYPED`, `judgeCode`, `JUDGE_PAUSE_MS`, `watchCode` (lines 13-85) and replaces `verdictNode`, `codePopup`, `applyCode`, `useCode` with:

```ts
/** A kept code field with its verdict line under it; `judged` hears each verdict, null while there is nothing to judge. */
export function codeField(key: string, label: string, judged: (v: CodeVerdict | null) => void): HTMLElement {
  return kept(key, () => {
    const input = el("input", { type: "text", "aria-label": label, spellcheck: "false", placeholder: "SN6-…", autocomplete: "off" })
    const box = el("label", { class: "field" }, icon("key"), input)
    const line = liveRegion("verdict")
    watchCode(input, (v) => {
      box.className = `field${v === null ? "" : v.ok ? " ok" : " bad"}`
      line.replaceChildren(v === null ? "" : v.ok ? v.line : `${v.title}. ${v.line}`)
      judged(v)
    })
    return el("div", { class: "code-entry" }, el("span", { class: "label" }, label), box, line)
  })
}
```

- [ ] **Step 4: The checkout page structure** (`src/views/checkout.ts`). `checkoutPage()` creates the name code and the pending record on open for a non-app checkout (moved from `ownerCodePage`, old lines 51-58), then draws; `ownerCodePage`, `orderRows`, `totalLine`, `labelAs`, `focusOnceShown`, `subLink`, `sideCard`, `buildOwnerCode`, `answer()` are deleted (refusals are always `act(event)` on this page):

```ts
const PAY_LABEL = (c: session.Checkout) => c.code ? "Register" : c.appOwner ? "Continue" : `Pay ${dollars(c.price, true)} with ${methodName(c.method)}`
const SAVED_LABEL = "I have saved my name code"

function checkoutScreen(s: CheckoutState, signal: AbortSignal): HTMLElement {
  const c = s.checkout
  const fromApp = c.appOwner !== null
  const blocked = blockedBy(s)
  return screen({ title: "Check your order", back: searchLink(c.label, c.appOwner) },
    storageWarning(fromApp),
    summaryCard(c, blocked !== null),
    fromApp ? null : linksBlock(c, signal),
    c.code ? coveredLine(c.code) : blocked ? waitingNotice(blocked) : methodsBlock(s),
    fromApp ? null : codeBlock(c),
    fromApp ? null : nameCodeBlock(c),
    payBlock(s, blocked !== null))
}

// name, term, registered until, total; a code strikes the price through
function summaryCard(c: session.Checkout, blocked: boolean): HTMLElement
// "Point it at your SimpleX" + "Optional", the two kept link fields with their previews
function linksBlock(c: session.Checkout, signal: AbortSignal): HTMLElement
// the radio group of old methodsPart (lines 210-249) built from state, with the unavailable and open-order notices above it
function methodsBlock(s: CheckoutState): HTMLElement
// codeField("checkout-code", "I have a code", onVerdict) plus the service's last refusal (c.codeError) as one line
function codeBlock(c: session.Checkout): HTMLElement
// "Your name code", codeDisplay(c.ownerCode!, "owner-code") with its Copy, and the saved checkbox (data-focus="saved")
function nameCodeBlock(c: session.Checkout): HTMLElement
// the Pay button (data-focus="pay"), the hold notice under it, and the fine line about the wallet
function payBlock(s: CheckoutState, blocked: boolean): HTMLElement
```
`onVerdict` (module function): `v?.ok && state.checkout.label.length < v.code.minLength` → the field's line says "This code covers names of N or more letters; NAME has M." and nothing is applied; `v?.ok` → `act({ type: "code-applied", code: v.code })`; anything else while a code is applied → `act({ type: "code-removed" })`. Pay's click: a `.link-field` with `data-state` `bad` or `checking` takes focus instead; otherwise `void submitOrder()`. `submitOrder`/`sendOrder`/`openInvoice`/`orderError` stay (old lines 541-631) minus `setAppliedCode` and the `/#/checkout/code` branches. The refusal screens (`price-changed`, `too-many`, `failed`) stay as builders (old lines 397-435) and draw on `CHECKOUT_STEP`.

- [ ] **Step 5: Callers.** `src/main.ts`: drop `ownerCodePage`, `codePopup`, the `"name-code"` case and the popup argument of `setRenderer`. `src/session.ts`: delete `APPLIED`, `appliedCode`, `setAppliedCode`. `src/views/names.ts`: `forgetEverything` drops `setAppliedCode`; the code row drops Use (`useButton`, lines 392-409); "Add a code" becomes `codeField("add-code", "Add a code", …)` plus an "Add" button enabled for an `ok` verdict not yet listed (same `store.addCode` call as the old popup, line 101). `src/views/order.ts` `buildTakenWithCode`: the action is `btn("Search another name", "", () => go("/#/"))`.

- [ ] **Step 6: Checkout CSS.**
```css
.summary-card .rows { display: grid; gap: 10px; }
.summary-card .row { display: flex; justify-content: space-between; gap: 16px; color: var(--ink-soft); }
.summary-card .row b { color: var(--ink); font-weight: 600; }
.summary-card .total { display: flex; justify-content: space-between; border-top: 1px solid var(--surface-line); padding-top: 14px; font-weight: 600; font-size: 20px; }
.methods { display: grid; grid-template-columns: repeat(3, 1fr); gap: 14px; }
.method { display: flex; flex-direction: column; align-items: center; gap: 8px; padding: 20px 12px; border: 1px solid var(--surface-line); border-radius: 20px; background: var(--surface); cursor: pointer; }
.method[aria-checked="true"] { border-color: var(--primary); color: var(--primary); }
.method[aria-disabled="true"] { color: var(--ink-soft); background: var(--chip); cursor: default; }
.code-display { display: flex; align-items: center; gap: 12px; font: 700 19px var(--mono); text-wrap: balance; }
.check { display: flex; align-items: center; gap: 10px; font-weight: 600; }
@media (max-width: 480px) { .methods { grid-template-columns: 1fr; } }
```
(`.code-display` sits inside a `.card`; the QR card and the name code frame share `.card`.)

- [ ] **Step 7: e2e.** `e2e/helpers.ts`:

```ts
export async function startOrder(page: Page, label: string, opts: OrderOptions = {}): Promise<{ invoiceId: string; ownerCode: string }> {
  await search(page, label)
  await page.getByRole("button", { name: "Register" }).click()
  await expect(page.getByRole("heading", { name: "Check your order" })).toBeVisible()
  if (opts.contact) {
    await page.getByLabel("Address").fill(opts.contact)
    await expect(page.locator(".link-field").first().locator(".entity")).toBeVisible()
  }
  if (opts.channel) {
    await page.getByLabel("Channel").fill(opts.channel)
    await expect(page.locator(".link-field").nth(1).locator(".entity")).toBeVisible()
  }
  await page.getByRole("radio", { name: METHOD_NAMES[opts.method ?? "btc"] }).click()
  const ownerCode = (await page.getByTestId("owner-code").locator(".code-text").textContent())!.trim()
  const pay = page.getByRole("button", { name: /^Pay / })
  await expect(pay).toBeDisabled()
  await savedCode(page)
  if (opts.shots) await shot(page, `${opts.shots}-checkout`)
  await pay.click()
  await page.waitForURL(/\?order=/)
  return { invoiceId: new URL(page.url()).searchParams.get("order")!, ownerCode }
}

export async function savedCode(page: Page): Promise<void> {
  const box = page.getByRole("checkbox", { name: "I have saved my name code" })
  if (!(await box.isChecked())) await box.check()
}
```
Rewrite `codes.spec.ts:58` as "a prepaid code entered at checkout registers a name with nothing to pay" (fill "I have a code", expect `.summary-card .total` to read `$0.00`, press Register, wait registered) and `codes.spec.ts:102` as "the checkout's code field refuses a code for longer names, typos and owner codes"; `codes.spec.ts:124` adds the code through Your names' field, lists it, and forgets it (no Use). Delete `focus.spec.ts` tests at 123, 224 (popup), `robustness.spec.ts:419` (Details gone back to from the name code), `search.spec.ts:277`'s name-code step. Replace "Continue"/"Continue to payment"/"Save your name code" expectations with the Pay button everywhere (`rg -n "Continue to payment|Save your name code|I have saved my code" e2e` → none). Run Verify.

- [ ] **Step 8: Commit**
```bash
git -C /project/git/simplex.domains add webapp/src webapp/test webapp/e2e
git -C /project/git/simplex.domains commit -m "webapp: one-page checkout with code field"
```

### Task 6: Payment, registration and "is yours"

**Goal:** The pay, confirming, registering and registered screens are single columns in the new look, keeping the QR, amount, address with Copy, rate countdown, Stripe form, stage tracker and wait countdown.

**Files:**
- Modify: `src/views/order.ts:363-526,633-654,759-988` (pay crypto, pay card, confirming, stages, registering, registered)
- Modify: `src/styles.css` (pay, stages, countdown, registered rules; delete `.pay-head`, `.pay-body`, `.meter`, `.celebrate`, `.tick`, `.next`, `.item`, `.txs` grid leftovers)
- Modify: `e2e/paid.spec.ts`, `e2e/app.spec.ts`, `e2e/codes.spec.ts:306`

**Acceptance Criteria:**
- [ ] Pay with Bitcoin/Monero: one column with `h1` "Pay with Bitcoin", the lead, the QR in a `.card`, the Amount, address and Reference copy fields, "Rate held m:ss" counting down each second, the waiting line and "Cancel this invoice".
- [ ] A part payment shows one warn notice above the QR and "Send the rest"; "Cancel this invoice" is gone then.
- [ ] Pay by card: `h1` "Pay by card", the kept Stripe container (`.stripe`), errors under it, one "Pay $X" button; a redraw while confirming does not remount Stripe (one `mountCard` call per route).
- [ ] Registering: `h1` "Registering <name>", the three stages (`.stages > .stage` with `done`/`active`/`failed`), the wait countdown `m:ss` with its note, the transactions rows.
- [ ] Registered: `h1` "<name>.simplex is yours", the line "Registered for … until …", the name code in a `.card` with Copy, and one next-step card with the code's QR, "Scan or enter the code in SimpleX: the name moves to the app's wallet." and an "Edit links" link; an app order shows "Back to SimpleX" instead.
- [ ] e2e `paid.spec.ts` "a Bitcoin order pays in two parts…", "a card order waits for Stripe…", "Cancel this invoice reads as working…" and `codes.spec.ts:306` pass in Chromium.

**Verify:**
```bash
cd /project/git/simplex.domains/webapp && npx tsc --noEmit && npx vitest run
ulimit -c 0 && source $S/tools/webkit-env.sh && SLOTS=3 npx playwright test e2e/paid.spec.ts e2e/app.spec.ts e2e/codes.spec.ts --project=chromium   # → 0 failed
```

**Steps:**

- [ ] **Step 1: Pay crypto.** Replace `buildPayCrypto` (old 399-471) with a builder of this structure; the rate countdown keeps `countdown()` (old 390-397) on the screen signal, writing `textContent`:

```ts
function payCryptoScreen(s: OrderState, signal: AbortSignal): HTMLElement {
  const inv = s.invoice!
  const coin = coinOf(inv)!
  const partial = Number(inv.cryptoAmountPaid ?? "0") > 0
  const due = partial ? inv.cryptoAmountDue! : inv.cryptoAmount!
  const uri = paymentUri(coin, inv.address!, due)
  return screen({ title: `Pay with ${methodName(coin)}`, lead: "Bookmark this page: the address and the countdown live on its link." },
    partial ? alertBox("warn", "alert", "Part of the payment arrived", partSentence(inv, coin)) : null,
    orderLine(s),
    uri ? el("div", { class: "card qr" }, qrSvg(uri, `${methodName(coin)} payment`)) : null,
    copyField(partial ? "Send the rest" : "Amount", `${due} ${TICKER[coin]}`, due),
    copyField(`${methodName(coin)} address`, inv.address!),
    copyField("Reference", inv.invoiceId, false),
    rateLine(inv, signal),
    el("p", { class: "note waiting" }, partial ? "Waiting for the rest of the payment" : "Waiting for payment", partial ? null : cancelLink(s)),
    actionNote(s, !partial))
}
```
`TICKER` is `const TICKER: Record<"btc" | "xmr", string> = { btc: "BTC", xmr: "XMR" }` (replacing the inline `"BTC" : "XMR"` choices at old 404, 644, 692). `partSentence` is the old part-payment sentence (436). `rateLine(inv, signal)` is a `.note` "Rate held m:ss" from the old `countdown` (390-397). `cancelLink(s)` is the old cancel link (421-431) without `takeTurns`: its text is "Cancelling…" with `aria-disabled` while `s.busy === "cancel"`, else "Cancel this invoice". `copyField(label, value, copy = value)` is the old `copyField` (377-388) returning the node directly; its one-word value keeps a `<wbr>` between letters inside `span.word` (the Monero wrap fix, Task 9). `orderLine(s)` is one `.note` line "NAME.simplex · TERM · $TOTAL". `actionNote(s, holds)` is the old `showNote` (329-332) as a builder.

- [ ] **Step 2: Pay card.** `payCardScreen` keeps the kept `"stripe-form"` container and mounts once per route: `mount()` (old 493-508) runs only when the container has no `data-mounted`, sets it, and registers `card.destroy` with `whenLeaving`. The Pay button's busy state comes from `s.busy === "card"`. `cardStandIn` stays for development.

- [ ] **Step 3: Confirming** (old 633-654) becomes `screen({ title: card ? "Paying by card" : "Confirming your payment" }, orderLine(s), alertBox("info", "clock", confirmationsLine, sentence), referenceLine)` with no meter.

- [ ] **Step 4: Registering.** `stages(states)` (old `track`, 766-779) returns `el("ol", { class: "stages" }, …)` built from states; delete `fillWait`, `waitsSeen`'s animation use and `--done`. The countdown keeps `onTheSecond` (912-925) on the screen signal, writing the clock with `textContent`; `txRows` (792-806) builds from state.

- [ ] **Step 5: Registered.**

```ts
function registeredScreen(s: OrderState): HTMLElement {
  const view = s.view!
  const label = labelOf(view, s.record)
  const code = s.record?.ownerCode ?? null
  return screen({ title: [label ? gradName(label, view.tld) : "The name", " is yours"], lead: registeredLead(view, s.record) },
    code ? el("div", { class: "card name-code" }, el("span", { class: "label" }, "Your name code"), codeDisplay(code, "owner-code"),
      store.canPersist() ? alertBox("warn", "shield", null, "Whoever has this code controls the name. Nobody else has a copy, not even us.")
        : alertBox("danger", "alert", "This code could not be saved in this browser", "The copy you saved before paying is the only one.")) : null,
    code ? el("div", { class: "card next-step" }, el("div", { class: "qr" }, qrSvg(code, "name code")),
      el("div", {}, el("h2", {}, "Redeem it in the app"), el("p", { class: "note" }, "Scan or enter the code in SimpleX: the name moves to the app's wallet."),
        el("a", { href: `/#/edit/${view.id}` }, "Edit links")))
      : s.record ? btn("Back to SimpleX", "block", () => { location.href = "simplex:/" }) : withoutRecord(view, "quiet block", "Restore with your name code"))
}
```
`registeredLead` stays (old 954-960).

- [ ] **Step 6: CSS** for `.qr` (centred, white ground: `qrSvg` draws its own frame), `.copy-field` (label above, value row with Copy at right, `font: 600 15px var(--mono)`, `text-wrap: balance`, `.word { word-break: break-all; }`), `.stages` (three columns, a 4px bar per stage in `--surface-line`, `done` in `--ok-fg`, `active` in `--primary`, `failed` in `--danger-fg`, a 30px node), `.countdown` (clock in `--display` 700 at `--fs-h2`), `.next-step` (QR and text side by side, stacked under 480px).

- [ ] **Step 7: e2e.** Update `paid.spec.ts` and `app.spec.ts` selectors to the new structure (`.card.qr svg`, `.copy-field`, `.stages .stage.done`, heading names unchanged). Run Verify.

- [ ] **Step 8: Commit**
```bash
git -C /project/git/simplex.domains add webapp/src/views/order.ts webapp/src/styles.css webapp/e2e
git -C /project/git/simplex.domains commit -m "webapp: payment and registration screens"
```

### Task 7: Order outcomes merged

**Goal:** Each order ending is one screen in the badges pattern: "Invoice closed" replaces the expired, cancelled, still-open and paid-late variants; "Card payment not confirmed" replaces card-failed and slow; taken keeps the retarget flow; failed offers Try again or the way back; a checkout rate limit is one countdown screen.

**Files:**
- Modify: `src/screens/order.ts` (kinds, `closedCase`, `paymentKind`)
- Modify: `test/screens-order.test.ts`
- Modify: `src/views/order.ts:334-357,536-564,656-722,927-931,990-1120` (outcome builders)
- Modify: `src/views/shell.ts` (add `limitScreen`), `src/views/checkout.ts:402-425` (too-many uses it)
- Modify: `src/styles.css` (`.outcome`; delete `.status*`, `.orb`, `.options`, `.option`, `.ref`)
- Modify: `e2e/paid.spec.ts`, `e2e/robustness.spec.ts`, `e2e/taken.spec.ts`, `e2e/search.spec.ts:178`

**Acceptance Criteria:**
- [ ] `OrderKind` is exactly `"pay-crypto" | "pay-card" | "confirming" | "registering" | "registered" | "taken" | "code-issued" | "failed" | "not-on-device" | "status:closed" | "status:card-unconfirmed" | "status:paid-by-invoice" | "status:unreadable" | "status:unknown-link" | "status:price-changed"`.
- [ ] Vitest: `closedCase` returns `paid-late`, `still-open`, `cancelled`, `received`, `expired` and `null` for the six fixtures in Step 1; `screenOf` maps all five closed cases to `status:closed` and card-failed and slow to `status:card-unconfirmed`.
- [ ] Every closed screen has `h1` "Invoice closed", one notice, at most one button and the reference; a payment-holding invoice's notice keeps the refund line ("Write to chat@simplex.chat with the reference below…" or "Support refunds it: quote the reference below."), a cancel the service still shows open keeps "Do not send anything more to the address."
- [ ] "Card payment not confirmed" has one notice and at most one button ("Load the card form again" after a form failure, "Enter the card again" when Stripe has seen nothing, none when it has).
- [ ] The checkout's rate limit is one screen: `h1` "Too many attempts", a countdown line, "Try again" disabled until it reaches zero, and Back.
- [ ] e2e `paid.spec.ts` "a cancelled invoice leads back…", "a card form that does not load…", `robustness.spec.ts` "Get a new invoice keeps…", "a payment that arrives after expiry…", and `taken.spec.ts` pass in Chromium.

**Verify:**
```bash
cd /project/git/simplex.domains/webapp && npx tsc --noEmit && npx vitest run test/screens-order.test.ts   # → passes
ulimit -c 0 && source $S/tools/webkit-env.sh && SLOTS=3 npx playwright test e2e/paid.spec.ts e2e/robustness.spec.ts e2e/taken.spec.ts --project=chromium   # → 0 failed
```

**Steps:**

- [ ] **Step 1: Failing tests** in `test/screens-order.test.ts` (fixtures `INVOICE`, `VIEW`, `RECORD` as the file defines them):

```ts
describe("closedCase", () => {
  const at = (invoice: Partial<InvoiceView>, record: Partial<OrderRecord> | null = RECORD) =>
    ({ ...initialState("inv", null), view: VIEW, invoice: { ...INVOICE, ...invoice }, record: record && { ...RECORD, ...record } })
  it.each([
    ["paid-late", at({ status: "paid" })],
    ["still-open", at({ status: "open" }, { cancelled: true })],
    ["cancelled", at({ status: "expired" }, { cancelled: true })],
    ["received", at({ status: "expired", cryptoAmountPaid: "0.0001" })],
    ["expired", at({ status: "expired" })],
  ] as const)("%s", (expected, s) => {
    expect(closedCase(s)).toBe(expected)
    expect(screenOf(s)).toBe("status:closed")
  })
  it("an open invoice nobody cancelled is not closed", () => {
    expect(closedCase(at({ status: "open" }))).toBeNull()
  })
})

describe("card not confirmed", () => {
  const card = { ...INVOICE, cryptoCurrency: undefined, status: "open" as const }
  it("a form that failed and a confirmation past CARD_SLOW_MS are one screen", () => {
    const failed = reduce({ ...initialState("inv", null), view: VIEW, invoice: card, record: RECORD }, { type: "card-failed" })
    expect(screenOf(failed)).toBe("status:card-unconfirmed")
    const slow = { ...initialState("inv", null), view: VIEW, invoice: card, record: RECORD, cardSubmittedAt: new Date(0).toISOString(), now: CARD_SLOW_MS }
    expect(screenOf(slow)).toBe("status:card-unconfirmed")
  })
})
```
Run → FAIL.

- [ ] **Step 2: Reducer.** In `src/screens/order.ts`:

```ts
export type ClosedCase = "paid-late" | "still-open" | "cancelled" | "received" | "expired"

// an unpaid order's invoice that took no more payment: closed, or cancelled while the service still shows it open
export function closedCase({ invoice, view, record }: OrderState): ClosedCase | null {
  if (!invoice || view?.phase !== "unpaid") return null
  if (invoice.status === "paid") return "paid-late"
  if (invoice.status === "open") return record?.cancelled && !invoice.paidInFull ? "still-open" : null
  if (record?.cancelled && !holdsPayment(invoice)) return "cancelled"
  return holdsPayment(invoice) ? "received" : "expired"
}
```
`screenOf`: after the `!view` line, `if (closedCase(s)) return "status:closed"`; `paymentKind` loses the still-open line and returns `"status:card-unconfirmed"` for both the failed form and `slow`. Update the old `screenOf` expectations for `status:expired*`, `status:paid-late`, `status:still-open`, `status:card-failed`, `slow`. Run → PASS.

- [ ] **Step 3: Builders.** Delete `buildPaidLate`, `buildStillOpen`, `buildCardFailed`, `buildSlow`, `buildExpired`; add, built on `outcome()`:

```ts
const CLOSED_TITLE = "Invoice closed"
function closedScreen(s: OrderState): HTMLElement {
  const inv = s.invoice!
  const [tone, text] = closedWords(closedCase(s)!, inv)
  return outcome({ tone, icon: "alert", title: CLOSED_TITLE, text, reference: inv.invoiceId,
    extra: actionNote(s, s.record !== null && !s.record.replacedBy), action: closedAction(s) })
}
// the old pages' sentences: paid-late (old 347), still-open (cancelStillOpen + partly paid, 542), cancelled (682),
// received (693-694 with the mailto), expired (707)
function closedWords(c: ClosedCase, inv: api.InvoiceView): [Tone, Kid]
// replacedBy → "Open the new invoice"; cancelled → "Order the name again"; paid-late or still-open → "Search a name";
// received or expired → "Get a new invoice" with a record (busy and held from state), else withoutRecord(...)
function closedAction(s: OrderState): HTMLElement

function cardUnconfirmedScreen(s: OrderState): HTMLElement {
  const inv = s.invoice!
  const action = s.cardFailed ? btn("Load the card form again", "", () => dispatch({ type: "card-retried" }))
    : inv.amountPaid ? null : btn("Enter the card again", "", enterCardAgain)
  return outcome({ tone: "warn", icon: "alert", title: "Card payment not confirmed", reference: inv.invoiceId, action,
    text: s.cardFailed ? "Stripe's form was blocked or failed to load. Nothing was charged."
      : "The payment has not been confirmed. This page keeps checking: come back to it later, or quote the reference below." })
}
```
`buildUnreadable`, `buildUnknownLink`, `buildPaidByInvoice`, `buildNotOnDevice`, `buildPriceChanged`, `buildCodeIssued`, `buildTakenWithCode` become `outcome()` calls with their existing words. `buildTaken` keeps both options ("Choose another name" to `/#/?retarget=<id>`, "Take a code") as a `screen()` with the failed stages, one sentence and the two buttons stacked. `buildFailed` keeps Try again or, once the code is refused, "Search a name". `reorderAfterCancel` is deleted with the card-failed page's "Get a new invoice".

- [ ] **Step 4: One rate-limit screen** in `src/views/shell.ts`, used by the checkout's `status:too-many`:

```ts
/** The tab's wait as one screen: Try again turns on when the count reaches zero. */
export function limitScreen(hold: Hold | null, again: () => void, back: string): HTMLElement {
  const line = el("span", {})
  const tryAgain = btn("Try again", "block", again)
  tryAgain.disabled = hold !== null
  if (hold) waitOut(line, Math.max(1, secondsLeft(hold)), (left) => `Too many orders were tried from here. Try again in ${left}.`, () => { tryAgain.disabled = false })
  else line.textContent = "Too many orders were tried from here. Nothing was charged."
  return outcome({ tone: "warn", icon: "clock", title: "Too many attempts", text: line, action: tryAgain,
    extra: el("a", { class: "back", href: back }, "Back") })
}
```
(`waitOut` stops by itself once its node has left the page, `dom.ts:46-47`, so the next screen ends the count.)

- [ ] **Step 5: e2e.** Update heading expectations: "Invoice expired"/"Invoice cancelled"/"Paid after the invoice closed" → "Invoice closed" plus the notice text; "The card form did not load"/"This is taking longer than expected" → "Card payment not confirmed"; `paid.spec.ts:509` expects "Load the card form again" (no new invoice). Run Verify.

- [ ] **Step 6: Commit**
```bash
git -C /project/git/simplex.domains add webapp/src webapp/test/screens-order.test.ts webapp/e2e
git -C /project/git/simplex.domains commit -m "webapp: merge order outcome screens"
```

### Task 8: Your names, restore, edit and trademarks

**Goal:** Your names lists the stored rows read once on open, with Add a code, Show code inline, Edit links, Restore and Forget asked on the page; restore, edit links and trademarks are single columns; the last popup goes.

**Files:**
- Modify: `src/views/names.ts` (rewrite: ~722 → ~450 lines)
- Modify: `src/screens/names.ts` (`shownCode`, `code-toggled`)
- Modify: `test/screens-names.test.ts`
- Modify: `src/views/shell.ts` (delete `popup`, `closePopup`, `shut`, `popupKeys`, `opener`, `behindPopup`, `OVERLAY`, `POPUP_MS`, `popupRoute`)
- Modify: `src/styles.css` (list rows; delete `.modal*`, `html.modal-open`, `--backdrop` from both token blocks, `.panel`, `.panel-sec`, `.row-item`, `.toolbar`, `.list-title`)
- Modify: `e2e/codes.spec.ts`, `e2e/focus.spec.ts`, `e2e/robustness.spec.ts`, `e2e/paid.spec.ts:526`

**Acceptance Criteria:**
- [ ] Opening `/#/names` reads each order and each code once; a row whose read failed shows "Not checked" and is read again once after `api.retryDelay(e, UNREAD_RETRY_MS)`; no other timer runs (`rg -n "FOLLOW_MS|MOST_STAGGERED|entering|storage\"" src/views/names.ts` prints nothing).
- [ ] The page shows, in order: `h1` "Your names", its lead, the Add a code field with an Add button, a "Restore with a name code" link, the Names card (pending rows, then order rows), the Codes card, and "Forget everything in this browser".
- [ ] "Show code" on a registered row expands that row with the name code (Copy) and its QR; pressing it again ("Hide code") collapses it; only one row is expanded at a time.
- [ ] "Forget everything in this browser" turns into the question with "Forget everything" and "Keep them" on the page, focus on "Keep them"; no `window.confirm` (`rg -n "confirm\(" src` prints nothing).
- [ ] Restore, edit links and trademarks are `.screen` columns with Back to Your names (trademarks: Back to home).
- [ ] `rg -n "popup|modal|\"panel" src` prints nothing; the unused-exports check prints nothing.
- [ ] e2e `codes.spec.ts` Your names tests, `robustness.spec.ts:299` "a row Your names could not read is read again…" and `paid.spec.ts:526` pass in Chromium.

**Verify:**
```bash
cd /project/git/simplex.domains/webapp && npx tsc --noEmit && npx vitest run test/screens-names.test.ts   # → passes
ulimit -c 0 && source $S/tools/webkit-env.sh && SLOTS=3 npx playwright test e2e/codes.spec.ts e2e/focus.spec.ts e2e/robustness.spec.ts e2e/paid.spec.ts --project=chromium   # → 0 failed
```

**Steps:**

- [ ] **Step 1: Failing reducer tests** in `test/screens-names.test.ts`:

```ts
describe("show code", () => {
  const stored = { orders: [ORDER, { ...ORDER, registrationId: "reg-2" }], pending: [], codes: [], canPersist: true }
  const list = () => initialList(stored, null, {}, {}, 0)
  it("expands one row at a time and collapses it when toggled again", () => {
    const one = reduceList(list(), { type: "code-toggled", id: ORDER.registrationId })
    expect(one.shownCode).toBe(ORDER.registrationId)
    expect(reduceList(one, { type: "code-toggled", id: "reg-2" }).shownCode).toBe("reg-2")
    expect(reduceList(one, { type: "code-toggled", id: ORDER.registrationId }).shownCode).toBeNull()
  })
  it("a row no longer stored is not shown", () => {
    const one = reduceList(list(), { type: "code-toggled", id: "reg-2" })
    expect(reduceList(one, { type: "stored", ...stored, orders: [ORDER], hold: null }).shownCode).toBeNull()
  })
  it("forgetting everything collapses the row", () => {
    const one = reduceList(list(), { type: "code-toggled", id: ORDER.registrationId })
    expect(reduceList(one, { type: "forget-confirmed" }).shownCode).toBeNull()
  })
})
```
(`ORDER` is the file's order fixture.) Run → FAIL.

- [ ] **Step 2: Reducer.** `ListState` gains `shownCode: string | null` (initial `null`); `ListEvent` gains `{ type: "code-toggled"; id: string }` handled as `{ ...s, shownCode: s.shownCode === e.id ? null : ordered(e.id) ? e.id : s.shownCode }`; `stored` keeps it only if the id is still among `stored.orders`; `forget-confirmed` sets it `null`. Run → PASS.

- [ ] **Step 3: The list view.** `historyPage()` builds the state from `storedNow()` once and reads rows once (`readRows`, old 100-114, without the `readFor` record comparison: each id is read once per opening); `readOrder` (old 116-151) drops `follows`/`FOLLOW_MS` and on failure schedules one `after(api.retryDelay(e, UNREAD_RETRY_MS) ?? UNREAD_RETRY_MS, () => void readOrder(id, gen), listStop)` where `listStop` is aborted by `whenLeaving`. Delete the storage listener, `rowFor`/`ListRow`/`emptyNote`/staggered entry. The builder:

```ts
function namesScreen(s: ListState): HTMLElement {
  return screen({ title: "Your names", lead: leadOf(s.canPersist), back: "/#/" },
    addCodeBlock(),
    el("a", { class: "btn quiet", href: "/#/restore" }, icon("refresh"), "Restore with a name code"),
    el("div", { class: "card list" }, el("h2", {}, "Names"),
      ...(s.pending.length || listedOrders(s).length
        ? [...s.pending.map((p) => pendingRow(p, s)), ...listedOrders(s).map((o) => orderRow(o, s))]
        : [el("p", { class: "note" }, "No names in this browser yet.")])),
    el("div", { class: "card list" }, el("h2", {}, "Codes"),
      ...(s.codes.length ? s.codes.map((c) => codeRow(c, s)) : [el("p", { class: "note" }, "No codes in this browser yet.")])),
    hasRecords(s) ? forgetBlock(s) : null)
}
```
`orderRow(o, s)` returns `el("div", { class: "list-row", "data-key": id }, mark, main (name, sub line from subLine old 299-318), chip (chipOf, old 333-338), actions)`; actions are Open, or for a registered row with a name code "Show code"/"Hide code" (`data-focus="<id>-code"`, dispatches `code-toggled`) and "Edit links" when editable; when `s.shownCode === id` the row appends `el("div", { class: "row-code" }, codeDisplay(o.ownerCode!), el("div", { class: "qr" }, qrSvg(o.ownerCode!, "name code")))`. `pendingRow` and `codeRow` are the old builders (357-412) built from state (code rows: chip and Copy). `forgetBlock(s)`: `s.asking` → the question notice with "Forget everything" (`danger`, `data-focus="forget"`) and "Keep them" (`data-focus="keep"`, focused when asking starts); else the link. `addCodeBlock()` is `codeField("add-code", "Add a code", …)` plus an Add button enabled for an `ok` verdict whose code is not yet in `store.codes()` ("Already in your codes. It is listed under Codes below." otherwise).

- [ ] **Step 4: Restore, edit, trademarks** become `screen()` builders with `back: "/#/names"` (trademarks `"/#/"`): restore keeps its kept field, Restore button (`data-focus="restore"`), refusal/hold line and the three "How it works" sentences as one `.note` list; edit keeps its kept link fields, Save links (`data-focus="save"`) and the result notice (`resultNode`, old 614-627); its status kinds are `outcome()`-style notices inside `screen()`.

- [ ] **Step 5: Delete the popup** from `src/views/shell.ts` and the modal CSS, `--backdrop` (both token blocks), `.panel`/`.panel-sec`. Run the unused-exports check (Global Constraints) → no output.

- [ ] **Step 6: e2e.** Update the Your names tests: rows are `.list-row`; Show code expands in place (`.list-row .row-code .code-text`); `paid.spec.ts:526` reopens `/#/names` after the other tab saves instead of expecting a live update; delete `focus.spec.ts:50` ("keeps focus in its row when another tab adds a row") and `codes.spec.ts:416` ("keeps keyboard focus when another tab saves"); `focus.spec.ts:188` expects focus on "Keep them". Run Verify.

- [ ] **Step 7: Commit**
```bash
git -C /project/git/simplex.domains add webapp/src webapp/test/screens-names.test.ts webapp/e2e
git -C /project/git/simplex.domains commit -m "webapp: Your names read once, codes inline"
```

### Task 9: Round-65 carry-over fixes

**Goal:** The four round-65 findings that survive the redesign hold in the new code and are pinned by tests: the neutral failed lead while the code check fails or is slow, the re-read of a "Not checked" row, the even wrap of a Monero address, and "name.simplex" folded in the field.

**Files:**
- Modify: `src/screens/order.ts` (gains `failedLead`), `src/views/order.ts` (uses it; old 1089-1091 deleted)
- Modify: `test/screens-order.test.ts`, `test/rules.test.ts`
- Modify: `e2e/paid.spec.ts:371`, `e2e/robustness.spec.ts:289,299`, `e2e/codes.spec.ts:234`

**Acceptance Criteria:**
- [ ] `failedLead` is a pure export of `src/screens/order.ts`; Vitest: invoice → "Your payment is kept. Trying again charges nothing."; code refused → "Your code can no longer pay for it, so trying again cannot register it."; prepaid with `codeKept` → "Your code is kept. Trying again spends nothing more."; prepaid without an answer yet (slow or failed check) and no prepaid code → "Trying again spends nothing more.".
- [ ] Vitest: `foldLabel("Bakery.simplex")` is `"bakery"` and `foldLabel("​bakery ")` is `"bakery"`.
- [ ] e2e `codes.spec.ts:234` "a failed code order is drawn before its code is checked…", `robustness.spec.ts:299` "a row Your names could not read is read again…", `paid.spec.ts:371` "a long address wraps into even lines…" (line-width ratio ≥ `EVEN_LINES` at 1280 and 390 px) and `robustness.spec.ts:289` "a name typed with .simplex…" pass in Chromium, Firefox and WebKit.

**Verify:**
```bash
cd /project/git/simplex.domains/webapp && npx tsc --noEmit && npx vitest run test/screens-order.test.ts test/rules.test.ts   # → passes
ulimit -c 0 && source $S/tools/webkit-env.sh && SLOTS=3 npx playwright test e2e/codes.spec.ts e2e/robustness.spec.ts e2e/paid.spec.ts -g "drawn before its code|read again|even lines|typed with .simplex" --project=chromium --project=firefox --project=webkit   # → 12 passed
```

**Steps:**

- [ ] **Step 1: Failing tests** in `test/screens-order.test.ts`:

```ts
describe("failedLead", () => {
  const failed = (change: Partial<OrderState>) => ({ ...initialState("reg", null), view: { ...VIEW, phase: "failed" as const }, ...change })
  it("an invoice's payment is kept", () => {
    expect(failedLead(failed({ invoice: INVOICE }))).toBe("Your payment is kept. Trying again charges nothing.")
  })
  it("a refused code says it can no longer pay", () => {
    expect(failedLead(failed({ codeRefused: true }))).toBe("Your code can no longer pay for it, so trying again cannot register it.")
  })
  it("a code is said to be kept only once the service says so", () => {
    const coded = { record: { ...RECORD, prepaidCode: "SN6-AAAA" } }
    expect(failedLead(failed({ ...coded, codeKept: true }))).toBe("Your code is kept. Trying again spends nothing more.")
    expect(failedLead(failed(coded))).toBe("Trying again spends nothing more.")
  })
  it("without a code or an invoice the lead is neutral", () => {
    expect(failedLead(failed({}))).toBe("Trying again spends nothing more.")
  })
})
```
and in `test/rules.test.ts`:
```ts
it("a name typed with .simplex, upper case or invisible characters folds to the name", () => {
  expect(foldLabel("Bakery.simplex")).toBe("bakery")
  expect(foldLabel("​bakery ")).toBe("bakery")
})
```
Run → the `failedLead` block FAILS (not exported).

- [ ] **Step 2: Move `failedLead`** from `src/views/order.ts` (old 1089-1091) into `src/screens/order.ts` unchanged and import it in the view. Run → PASS.

- [ ] **Step 3: Check the wrap and re-read in the new markup.** `copyField` keeps a `<wbr>` between letters of a one-word value inside `span.word`, and `.copy-field .val` keeps `text-wrap: balance` with `.word { word-break: break-all; }` (WebKit balances only between given breaks, Firefox only in a word that breaks anywhere). The names view's failed-read re-read is the one scheduled in Task 8 Step 3. Update the e2e selectors (`.copy-field` value span, `.list-row .chip`, `.answer .entity`) and run Verify.

- [ ] **Step 4: Commit**
```bash
git -C /project/git/simplex.domains add webapp/src/screens/order.ts webapp/src/views/order.ts webapp/test webapp/e2e
git -C /project/git/simplex.domains commit -m "webapp: pin round-65 fixes in tests"
```

### Task 10: Tests, frame check and README

**Goal:** Every remaining e2e spec matches the new screens, one Playwright check proves the track slide and screen swaps never show a near-empty frame in Chromium and Firefox 158, and the README describes the new layout and runs.

**Files:**
- Create: `e2e/frames.spec.ts`
- Modify: `playwright.config.ts` (projects `chromium`, `firefox`, `webkit`, `firefox158`)
- Modify: every `e2e/*.spec.ts` still failing on old selectors; `e2e/helpers.ts`
- Modify: `README.md` (Layout, Run, Test sections)

**Acceptance Criteria:**
- [ ] `e2e/frames.spec.ts` passes in the `chromium` and `firefox158` projects and is excluded from `firefox` and `webkit`.
- [ ] With a blank frame injected into `draw()` (Step 3), the frames spec fails in Chromium; with it reverted, it passes.
- [ ] `rg -n "motion|crossing|inplace|cuts|sweep|popup|Check a code|Continue to payment" e2e playwright.config.ts` prints nothing.
- [ ] `SLOTS=3 npx playwright test --project=chromium --project=firefox --project=webkit` reports 0 failed.
- [ ] README's Test section gives `ulimit -c 0`, the `SLOTS=3 … dev/start.sh` line, `SLOTS=3 npx playwright test` and the `FIREFOX_158=… --project=firefox158` line, and no motion projects.

**Verify:**
```bash
cd /project/git/simplex.domains/webapp && npx tsc --noEmit && npx vitest run
ulimit -c 0 && source $S/tools/webkit-env.sh && SLOTS=3 npx playwright test --project=chromium --project=firefox --project=webkit   # → "N passed", 0 failed
FIREFOX_158=/nix/store/fzqp8k7z4y915fsdssb6x16b7iiqg2dm-firefox-158.0/bin/firefox SLOTS=3 npx playwright test --project=firefox158   # → 1 passed
```

**Steps:**

- [ ] **Step 1: Write `e2e/frames.spec.ts`**

```ts
import { expect, type Page } from "@playwright/test"
import { control, savedCode, search, test, uniqueLabel } from "./helpers.ts"

// the slide runs as a reader who has not asked for less motion sees it
test.use({ reducedMotion: "no-preference" })

// The main column is sampled on a grid each frame. A frame whose drawn points fall below this share of the fewer of its
// two ends is near empty.
const COLUMNS = 12
const ROWS = 24
const NEAR_EMPTY_SHARE = 0.4

async function startRecording(page: Page): Promise<void> {
  await page.evaluate(([columns, rows]) => {
    const w = window as unknown as { drawnFrames: number[]; recording: boolean }
    w.drawnFrames = []
    w.recording = true
    const main = document.querySelector("body > main")!
    const drawn = (x: number, y: number) => {
      const hit = document.elementFromPoint(x, y)
      return hit !== null && main.contains(hit) && !hit.matches("main, .track, .rail, .slide, .screen, .home")
    }
    const look = () => {
      if (!w.recording) return
      const box = main.getBoundingClientRect()
      const top = Math.max(box.top, 0)
      const bottom = Math.min(box.bottom, innerHeight)
      let hits = 0
      for (let i = 0; i < columns; i++) {
        for (let j = 0; j < rows; j++) if (drawn(box.left + (i + 0.5) * box.width / columns, top + (j + 0.5) * (bottom - top) / rows)) hits++
      }
      w.drawnFrames.push(hits)
      requestAnimationFrame(look)
    }
    requestAnimationFrame(look)
  }, [COLUMNS, ROWS] as const)
}

async function stopRecording(page: Page): Promise<number[]> {
  // the slide is a pair of CSS transitions, which the document lists among its animations until they end
  await page.evaluate(() => Promise.all(document.getAnimations().map((a) => a.finished.catch(() => undefined))))
  await page.evaluate(() => new Promise((done) => requestAnimationFrame(() => requestAnimationFrame(done))))
  return page.evaluate(() => {
    const w = window as unknown as { drawnFrames: number[]; recording: boolean }
    w.recording = false
    return w.drawnFrames
  })
}

function expectNoEmptyFrame(frames: number[], change: string): void {
  expect(frames.length, `${change}: frames recorded`).toBeGreaterThan(1)
  const floor = Math.min(frames[0], frames[frames.length - 1]) * NEAR_EMPTY_SHARE
  expect(Math.min(...frames), `${change}: drawn points per frame ${frames.join(",")}`).toBeGreaterThanOrEqual(floor)
}

test("the purchase track slides and other screens swap without a near-empty frame", async ({ page, request }) => {
  await search(page, uniqueLabel("frames"))
  await expect(page.getByRole("button", { name: "Register" })).toBeVisible()

  await startRecording(page)
  await page.getByRole("button", { name: "Register" }).click()
  await expect(page.getByRole("heading", { name: "Check your order" })).toBeVisible()
  expectNoEmptyFrame(await stopRecording(page), "search to checkout")

  await startRecording(page)
  await page.goBack()
  await expect(page.getByRole("heading", { name: "Your SimpleX domain" })).toBeVisible()
  expectNoEmptyFrame(await stopRecording(page), "checkout back to search")

  await page.goForward()
  await expect(page.getByRole("heading", { name: "Check your order" })).toBeVisible()
  await savedCode(page)
  await startRecording(page)
  await page.getByRole("button", { name: /^Pay / }).click()
  await expect(page.getByRole("heading", { name: "Pay with Bitcoin" })).toBeVisible()
  expectNoEmptyFrame(await stopRecording(page), "checkout to payment")

  const invoiceId = new URL(page.url()).searchParams.get("order")!
  await startRecording(page)
  await control(request, `confirming/${invoiceId}`)
  await expect(page.getByRole("heading", { name: "Confirming your payment" })).toBeVisible()
  expectNoEmptyFrame(await stopRecording(page), "payment to confirming")

  await startRecording(page)
  await page.getByRole("link", { name: "Your names" }).click()
  await expect(page.getByRole("heading", { name: "Your names" })).toBeVisible()
  expectNoEmptyFrame(await stopRecording(page), "order to Your names")
})
```

- [ ] **Step 2: Projects** in `playwright.config.ts`:

```ts
const FRAMES = /frames\.spec\.ts$/
// A Firefox 158 or later, named by FIREFOX_158. It says its own version rather than the one Playwright's device names,
// and finds its own libraries, which the library path set for Playwright's own browsers would replace.
const FIREFOX_158 = process.env.FIREFOX_158
const FIREFOX_158_PROJECT = FIREFOX_158
  ? [{ name: "firefox158", testMatch: FRAMES, use: { ...FIREFOX, userAgent: undefined, launchOptions: { channel: "moz-firefox", executablePath: FIREFOX_158, env: { ...process.env, LD_LIBRARY_PATH: "" } } } }]
  : []
// …
projects: [
  { name: "chromium", use: CHROME },
  { name: "firefox", testIgnore: FRAMES, use: FIREFOX },
  { name: "webkit", testIgnore: FRAMES, use: SAFARI },
  ...FIREFOX_158_PROJECT,
],
```

- [ ] **Step 3: Prove the check catches a blank frame.** Temporarily replace `slides[step].replaceChildren(screen)` in `placeOnTrack` (`src/views/track.ts`) with `slides.forEach((s) => s.replaceChildren()); requestAnimationFrame(() => slides[step].replaceChildren(screen))`, which leaves every slide empty for one frame, run `SLOTS=3 npx playwright test e2e/frames.spec.ts --project=chromium` → FAIL naming "search to checkout"; revert with `git -C /project/git/simplex.domains checkout -- webapp/src/views` and run again → PASS. The sabotage is never committed.

- [ ] **Step 4: Sweep the remaining specs.** Run the full suite in the three projects; fix each failure by updating its selectors and texts to the screens of Tasks 4-8 (never by weakening what it asserts). `app.spec.ts:164` (offline note room) is deleted with the room it tested; `app.spec.ts:113` keeps checking that no preload goes unused with the four faces.

- [ ] **Step 5: README.** Layout: `src/views/` lists `shell.ts` (frame, `draw`, `kept`, `screen`, `outcome`), `track.ts` (the purchase track), and the five views. Run and Test sections:

```
ulimit -c 0
SLOTS=3 MIN_COMMITMENT_AGE=5 MOCK_RESEND_MS=1000 MOCK_RETRY_AFTER_SECONDS=2 ENS_CONTRACTS_DIR=<ens-contracts> ANVIL=<anvil> dev/start.sh
SLOTS=3 npx playwright test
FIREFOX_158=<Firefox 158 executable> SLOTS=3 npx playwright test --project=firefox158
```
with one paragraph: the tests run with reduced motion except `e2e/frames.spec.ts`, which samples each frame of the track's slide and of whole-screen swaps and fails on a near-empty one; it runs in Chromium and, with `FIREFOX_158`, in Firefox 158. Delete the motion, crossing, cuts and in-place paragraph.

- [ ] **Step 6: Run Verify; commit**
```bash
git -C /project/git/simplex.domains add webapp/e2e webapp/playwright.config.ts webapp/README.md
git -C /project/git/simplex.domains commit -m "webapp: frame check, e2e and README for redesign"
```

### Task 11: Verification gate

**Goal:** The redesign is proven by two clean full e2e runs in Chromium, Firefox 157, WebKit and Firefox 158, and by review rounds (chain, design against the landing, motion, code) repeated until every reviewer reports no issues, with source at about 5.5k lines.

> **USER-ORDERED GATE — NON-SKIPPABLE.** This task was requested by the user in the current conversation. It MUST NOT be closed by walking around it, by declaring it "verified inline", or by substituting a cheaper check. Close only after every item in `acceptanceCriteria` has been re-validated independently, with output captured.

**Files:**
- Modify: only files a review finding requires, each fix as its own `webapp: <subject>` commit

**Acceptance Criteria:**
- [ ] Run 1 and run 2 of `SLOTS=3 npx playwright test --project=chromium --project=firefox --project=webkit` each end with "0 failed" (no "failed" or "flaky" line), output captured.
- [ ] Run 1 and run 2 of `FIREFOX_158=… SLOTS=3 npx playwright test --project=firefox158` each report 1 passed, output captured.
- [ ] Chain review: a reviewer drives a Bitcoin order, a card order, a code order, a taken-then-retarget order and a link edit against the locally deployed PR #36 contract and reports, from `GET /control/state` and anvil reads, that every registration's owner, expiry and records match what the page showed: "no issues".
- [ ] Design review: a reviewer compares `SHOTS=1` screenshots of every screen (desktop 1280×900 and phone 390×844, light and dark, Chromium and Firefox 158) with `/project/git/simplex.domains/index.html` rendered at the same sizes and with the token block, and reports "no issues".
- [ ] Motion review: a reviewer records video of search → checkout → payment → confirming → registered and Back, in Chromium and Firefox 158, and reports no blank frame, no flash and no layout jump: "no issues".
- [ ] Code review: a reviewer checks the diff `7ed55cf..HEAD` against the Global Constraints (dead code, duplication, magic values, comments, determinism, warnings) and reports "no issues"; `npx tsc --noEmit` prints nothing; the unused-exports check prints nothing.
- [ ] Rounds repeat after each fix until one round has all four reviewers reporting "no issues".
- [ ] `fd -e ts . src | xargs wc -l | tail -1` reports a total ≤ 6,000 TS lines (ruling 2026-10-19), and `wc -l < src/styles.css` ≤ 650.

**Verify:**
```bash
cd /project/git/simplex.domains/webapp && npx tsc --noEmit && npx vitest run && fd -e ts . src | xargs wc -l | tail -1 && wc -l < src/styles.css   # → total ≤ 6000; styles ≤ 650
ulimit -c 0 && source $S/tools/webkit-env.sh && SLOTS=3 npx playwright test --project=chromium --project=firefox --project=webkit 2>&1 | tee /tmp/gate-run1.log | tail -5   # → "0 failed"; repeat as run2
FIREFOX_158=/nix/store/fzqp8k7z4y915fsdssb6x16b7iiqg2dm-firefox-158.0/bin/firefox SLOTS=3 npx playwright test --project=firefox158 2>&1 | tail -3   # → "1 passed"
```

**Steps:**

- [ ] **Step 1: Fresh backend.** Stop any running backend, start `SLOTS=3 … dev/start.sh` in the background as in Global Constraints, wait with the curl readiness command.
- [ ] **Step 2: Line counts and static checks** (first Verify line); record the numbers.
- [ ] **Step 3: Run 1 and run 2** of both e2e commands, capturing each log; any failure is fixed (own commit), then both runs start over.
- [ ] **Step 4: Review round.** Dispatch four read-only reviewers in parallel (chain, design, motion, code) with the criteria above. Each prompt states: read-only (no edits to `src/`, `e2e/`, `test/`, no gradle), no polling waits (`run_in_background` and wait for the exit notification), the not-a-defect list (decisions in this plan's header and "Spec gaps" accepted by the owner), and asks for findings with file:line or screenshot path, or the words "no issues". Wait until all four report.
- [ ] **Step 5: Fix and repeat.** Each finding gets a fix commit; then repeat Steps 3-4 until one round has four "no issues" and both runs are clean after the last fix.
- [ ] **Step 6: Close** with the captured logs, line counts and the four reviewers' final reports attached to the task.

## Spec coverage

| Spec item | Task |
|---|---|
| §1 fonts, colours, dark palette, status colours | 1 |
| §1 buttons, cards, inputs, QR and name-code card | 1, 5, 6 |
| §1 header with "Your names", footer, 640px column with Back | 1, 2 (`screen` and its CSS) |
| §1 removed: sticky bar, phone menu, grid, violet gradient, glow buttons, shadows, @property easing | 1 |
| §1 removed: NEW pill, tiles, scene | 4 |
| §1 removed: step bar, skeleton shimmer | 2 |
| §2 Home and search answers | 4 |
| §2 Checkout blocks 1-6 | 5 |
| §2 Payment and registration, stage tracker and countdown | 6 |
| §2 Order outcomes, rate-limit screen, retarget, failed | 7 |
| §2 Your names: read once, Add a code, Show code inline, Edit links, Restore, Forget on the page | 8 |
| §3 deletions (motion, contract, moves, focus restoration, popups, spinner, offline room) | 2, 5, 8 |
| §3 builders, kept inputs, track | 2, 3 |
| §3 target lines | 11 |
| §4 Vitest minus transition tables | 2, 4, 5, 7, 8, 9 |
| §4 e2e kept, deleted, added frame check | 2, 10 |
| §4 review rounds | 11 |
| Round-65 carry-overs | 9 |

## Rulings on the plan's open points (2026-10-19)

These override the task text where they differ.

1. **Focus:** the new screen's heading takes focus on each screen change, for accessibility, even though badges doesn't do this.
2. **Applied code:** `session.ts` loses `appliedCode`, `setAppliedCode` and `APPLIED`, as the plan says.
3. **Your names refresh:** the list is read when opened. Rows whose read failed are read again once, on a timer. The cross-tab listener goes.
4. **Closed invoices:** the closed-invoice kinds merge as the plan counts them. "Paid by invoice" stays its own screen.
5. **Card form failed:** the one button is the way forward ("Get a new invoice"). No outcome screen loses its only way forward.
6. **Rate limits:** a rate limit is one screen at checkout and inline countdown lines elsewhere, as the plan says.
7. **Using codes:** there is no "Use" button on code rows, and no "Use it for another name". The retarget flow after a taken name stays.
8. **Home:** the simplex.domains page exactly, with the countdown replaced by the search. Keep its invest button, its popup (modal) and its copy. Task 4 ports the landing's modal markup and CSS.
9. **Footer:** exactly the landing's, including the "In-app" mark and its links.
10. **Font weight:** body text at 500, as the plan says.
11. **Size limits:** `src` at most 6,000 TS lines (raised from 5,600 after the owner added the Links and name-code steps) and `styles.css` at most 650.
12. **Backend paths:** the dev backend's contracts and anvil paths in the old scratchpad are read-only resources and stay usable.

### Task 7b: Badges-like checkout flow

Added on 2026-10-19 after the owner found the one-page checkout too cluttered. It replaces the checkout part of Task 5.

**Goal:** The purchase reads like the badges webapp, one concern per step:
1. Search → Register.
2. "Check your order":
   - the name, then one line of term and expiry, then the total;
   - "Pay with" Card, Bitcoin or Monero, as icon and name only;
   - an "I have a code" link that opens the code field only when clicked;
   - one Pay button.
3. Payment.
4. Registering / "is yours":
   - the name code card, with Copy and the "I have saved my name code" checkbox;
   - a "Set links" action that opens Edit links.

Removed from checkout:
- the links block and its previews;
- the name code card and its checkbox gating;
- the method sub-labels;
- "You pay on the next page";
- the wallet note.

**Files:**
- Modify: `src/views/checkout.ts`, `src/screens/checkout.ts`, `src/views/order.ts`, `src/screens/order.ts`, `src/styles.css`, `src/session.ts` (links no longer part of the checkout), and `src/flow.ts` if the order request carries links.
- Tests: `test/screens-checkout.test.ts`, `test/screens-order.test.ts`.
- e2e: specs that fill links or tick the saved box at checkout (`helpers.ts` `startOrder`, `savedCode`; paid, codes, robustness, app, focus, track).

**Acceptance Criteria:**
- [ ] The checkout DOM holds, in order: h1 "Check your order"; a summary card (name, one term/expiry line, Total); "Pay with" and three method cards; an "I have a code" link; one Pay button. It holds no link field, no name-code card, no checkbox and no wallet note.
- [ ] Pay is enabled as soon as a method is chosen (or a valid code applies). Nothing else gates it except sending or a hold.
- [ ] The owner code is still created and stored in this browser before the order is sent, as today.
- [ ] Clicking "I have a code" reveals the code field and focuses it. A valid code makes the total $0.00 and hides the methods, as before.
- [ ] The registering screen and the "is yours" screen show the name code card with Copy and the "I have saved my name code" checkbox. The checkbox is remembered per order.
- [ ] "is yours" offers "Set links", which opens Edit links for that name. An app order keeps "Back to SimpleX".
- [ ] An order opened from the app keeps its links from the app request, as today.
- [ ] The code refusal at order time still empties the field, explains why in its live line and focuses the field, with its e2e test kept.
- [ ] `npx tsc --noEmit` is clean; vitest passes; the Chromium e2e suite is green.

**Verify:** `cd /project/git/simplex.domains/webapp && npx tsc --noEmit && npx vitest run && SLOTS=3 npx playwright test --project=chromium` → 0 failed

### Task 7c: Purchase flow a buyer expects, no scrolling

Added 2026-10-19, evening. It replaces the purchase steps from Tasks 5 and 7b. The owner's words: "Flow is awful… we should edit links beforehand… why show 'I saved the code' if it doesn't do anything… contain everything in the center, so there's no scrolling… like any human would expect it to work."

**Goal:** Buying a name follows the order a person expects: one step per screen, each step centred and fitting the window with no scrolling.
1. **Name.** Home search, then "Register" on the available name.
2. **Links** (optional).
   - h1: "What should &lt;name&gt; open?"
   - Fields: the contact address and the channel field, each with its preview.
   - Buttons: Continue, and a quiet "Skip for now".
   - Links can be changed later on Edit links.
3. **Your name code.**
   - h1: "Save your name code"
   - One short line: it is the only key to the name; if it is lost, nothing can recover it.
   - The code, with Copy.
   - "I have saved my name code" enables Continue. This is the only thing the checkbox does.
4. **Check your order.**
   - Summary card: the name, the term and expiry line, the links if any (one line each, with "Change" going back to step 2), and Total.
   - "Pay with" Card, Bitcoin or Monero.
   - An "I have a code" link.
   - One Pay button.
5. **Payment** (crypto QR or card form): a compact layout that fits the window.
6. **Registering:** the stages and the wait.
7. **"&lt;name&gt; is yours":**
   - Line: registered until &lt;date&gt;.
   - Card: "Redeem it in the app", with the QR.
   - Buttons: "Your names", and "Edit links" (or "Back to SimpleX" for an app order).
   - No name code card and no checkbox: the code was saved before paying, and Your names "Show code" still shows it.

Steps 1–5 sit on the track, as adjacent slides. Back moves one step back. The Links and name-code steps can be revisited from "Check your order".

**Files:**
- Modify:
  - `src/screens/checkout.ts` (steps: `links`, `code`, `review`; events)
  - `src/views/checkout.ts`
  - `src/views/track.ts` (`STEPS`)
  - `src/views/order.ts` (pay, registering and registered screens: compact; drop the paid-screen code card and checkbox; drop `codeSaved` if nothing else reads it)
  - `src/session.ts` (checkout carries the links again)
  - `src/store.ts`
  - `src/styles.css` (centred, no-scroll step layout)
  - `src/views/names.ts` (`linkField` is shared again by checkout and Edit links, with no duplication)
- Tests:
  - `test/screens-checkout.test.ts` and any other test for a changed reducer
  - every e2e spec and helper the flow touches (`startOrder`, `openCode`, `checkoutOwnerCode`, …)

**Acceptance Criteria:**
- [ ] Register on an available name opens the Links step. Continue or "Skip for now" opens the name-code step. "I have saved my name code" enables Continue there, and Continue opens "Check your order". Pay opens payment. Back walks the same steps in reverse.
- [ ] Links typed in step 2 are sent with the order, appear on "Check your order", and are on chain after registration. Skipping sends none.
- [ ] The owner code is created and stored in this browser when step 3 opens. It is the same code that owns the registered name.
- [ ] No checkbox exists anywhere it doesn't gate an action. The paid screens have no name-code checkbox.
- [ ] At 1280×800 and at 390×844, in light and dark, these screens fit without vertical scrolling (`document.scrollingElement.scrollHeight <= innerHeight`): home, Links, name code, Check your order (with and without the code field open), payment (Bitcoin and card), registering and "is yours". The step content is vertically centred between the header and the footer. An e2e test asserts this at both sizes.
- [ ] Prepaid code path: "I have a code" on Check your order applies the code, gives $0.00 and turns Pay into Register. The order-time code refusal still empties the field, gives the reason and focuses the field.
- [ ] App orders, opened from the app, skip the Links and name-code steps, as the app holds the wallet. They keep "Back to SimpleX".
- [ ] `npx tsc --noEmit` is clean. Vitest passes. The full Chromium e2e suite is green.

**Verify:** `cd /project/git/simplex.domains/webapp && npx tsc --noEmit && npx vitest run && SLOTS=3 npx playwright test --project=chromium` → 0 failed
