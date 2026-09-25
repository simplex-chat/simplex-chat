# Badge codes web client — implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers-extended-cc:subagent-driven-development (recommended) or superpowers-extended-cc:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the badge-codes web client from the design, with a mock API server so the whole purchase flow runs and is tested end to end without Stripe, BTCPay or the Haskell service.

**Architecture:** TypeScript compiled to ES modules by `tsc` alone — no bundler, no framework, no runtime dependency. One module (`main.ts`) touches the DOM; everything else is pure and tested headlessly under `node:test`. A Python mock server (stdlib only) serves the two endpoints and a control surface, standing in for the Haskell service **and** for Stripe and BTCPay, so the whole browser flow runs without any of them.

**Tech Stack:** TypeScript 6, `tsc`, `node:test`, Web Crypto (`crypto.subtle`, `crypto.getRandomValues`), Service Worker, `localStorage`. The mock is Python 3 stdlib (`ThreadingHTTPServer`).

**Spec:** `plans/badges-codes/2026-08-27-badge-codes.md` — read it alongside this plan. Section references below (§4.2, §7.1) point into it.

## Global Constraints

- **No Haskell changes.** This plan creates files under `apps/simplex-badge-service/web/` only. No `.hs` file, no migration, no cabal change. The service side is already designed (§5, §6) and is not built here.
- **No runtime dependencies.** `package.json` may carry dev dependencies only (`typescript`, `@types/node`). Nothing is installed at runtime and nothing is bundled. The mock uses the Python standard library and nothing else.
- **No framework.** No React, no build step beyond `tsc`. Markup is never assigned from a string (`innerHTML` is banned); build DOM with `document.createElement` and `textContent`.
- **Only `main.ts` and `screens.ts` may touch `document` or `window`.** Every other module takes its dependencies as arguments so it can be tested in Node.
- **Exact values from the spec, verbatim:**
  - alphabet `23456789ABCDEFGHJKMNPQRSTUVWXYZ` (31 characters, §4.2)
  - 19 body characters + 1 check character, displayed as `SXB-` and four groups of five
  - check character: number the body from the **left** starting at 1, sum `position × alphabetIndex`, check value `(31 − (sum mod 31)) mod 31`
  - rejection sampling: draw a byte, discard if `>= 248`, else `byte % 31`
  - storage keys `sxb.session.v1` and `sxb.orders.v1`; orders cap 50
  - invoice statuses `open` | `paid` | `expired`
  - long-poll hold 30 s
- **The known-good code vector** is `SXB-YDC8A-YGQTM-PUYZ9-2TUXP`: body `YDC8AYGQTMPUYZ92TUX`, weighted sum 3793, `3793 mod 31 = 11`, check `(31−11) mod 31 = 20` = `P`. Every task that touches code generation must keep this passing.
- **A code is never rendered while its order is unpaid** (§7.2), including QR and clipboard.

**User decisions (already made):**
- "Stripe will be widget embedded in our website" — card fields are in-page, no redirect.
- "Yes, no emails" — `ui_mode: elements` with the Payment Element; the client renders no email field.
- "codes - client-side random codes + hash" — the browser generates the code and sends only its `SHA-256`.
- "ALL state in localstorage" including wizard step, unpaid invoices, and history.
- "If customer reload page with unpaid invoice - show that invoice with button 'new invoice'".
- "Make our webapp with full offline support."
- "Desktop from left to right (scroll), responsive, back button."

---

## File Structure

```
apps/simplex-badge-service/web/
  package.json          dev-only deps, scripts: build, test, mock
  tsconfig.json         ES2022 modules, strict, outDir dist/
  .gitignore            dist/ and node_modules/, with !public/
  src/
    codes.ts            alphabet, check character, generation, hashing
    catalog.ts          the compiled-in catalog type and totals
    store.ts            session + orders over an injectable Storage
    routing.ts          URL and store -> which screen, pure
    api.ts              fetch client for the two endpoints, and the wait loop
    screens.ts          DOM builders, one function per screen
    main.ts             wiring; the only entry point
  public/
    index.html          the shell, with the wizard track
    styles.css          the visual system from §7.3
    sw.js               service worker, precache + network-only /api/*
  mock/
    server.py           static files + the two endpoints + /control/* (stdlib only)
  test/
    codes.test.ts
    catalog.test.ts
    store.test.ts
    routing.test.ts
    flow.test.ts        end to end against mock/server.py
```

`src/screens.ts` and `src/main.ts` are the DOM boundary. `codes`, `catalog`, `store`, `routing` and `api` are pure or dependency-injected, and carry the tests.

---

### Task 1: Scaffold, build and test harness

**Goal:** `npm run build` compiles TypeScript to `dist/`, and `npm test` runs an empty but real test suite.

**Files:**
- Create: `apps/simplex-badge-service/web/package.json`
- Create: `apps/simplex-badge-service/web/tsconfig.json`
- Create: `apps/simplex-badge-service/web/.gitignore`
- Create: `apps/simplex-badge-service/web/test/smoke.test.ts`

**Acceptance Criteria:**
- [ ] `npm run build` emits `dist/` and exits 0
- [ ] `npm test` compiles then runs `node --test`, and reports 1 passing test
- [ ] `dist/` and `node_modules/` are ignored by git; `public/` is not
- [ ] `package.json` has **no** `dependencies`, only `devDependencies`

**Verify:** `cd apps/simplex-badge-service/web && npm install && npm run build && npm test` → `pass 1`

**Steps:**

- [ ] **Step 1: `package.json`**

```json
{
  "name": "simplex-badge-web",
  "private": true,
  "type": "module",
  "scripts": {
    "build": "tsc",
    "test": "tsc && node --test dist/test/",
    "mock": "python3 mock/server.py"
  },
  "devDependencies": {
    "typescript": "^6.0.0",
    "@types/node": "^24.0.0"
  }
}
```

- [ ] **Step 2: `tsconfig.json`**

```json
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "ES2022",
    "moduleResolution": "bundler",
    "lib": ["ES2022", "DOM"],
    "strict": true,
    "noUncheckedIndexedAccess": true,
    "outDir": "dist",
    "rootDir": ".",
    "declaration": false,
    "sourceMap": true
  },
  "include": ["src/**/*.ts", "test/**/*.ts"]
}
```

- [ ] **Step 3: `.gitignore`**

```
node_modules/
dist/
!public/
```

- [ ] **Step 4: a real smoke test**

```typescript
// test/smoke.test.ts
import { test } from "node:test";
import assert from "node:assert/strict";

test("the toolchain runs ES modules under node:test", () => {
  assert.equal(typeof crypto.subtle.digest, "function");
});
```

- [ ] **Step 5: run it**

Run: `npm install && npm run build && npm test`
Expected: `pass 1`, `fail 0`

- [ ] **Step 6: commit**

```bash
git add apps/simplex-badge-service/web
git commit -m "web: scaffold typescript build and test harness"
```

---

### Task 2: Code generation and the check character

**Goal:** `src/codes.ts` generates a spec-conformant code, verifies one, and hashes it — with the exhaustive properties from §4.2 proved by test.

**Files:**
- Create: `apps/simplex-badge-service/web/src/codes.ts`
- Create: `apps/simplex-badge-service/web/test/codes.test.ts`

**Acceptance Criteria:**
- [ ] `checkChar("YDC8AYGQTMPUYZ92TUX")` is `"P"`, and `display` of that body is `SXB-YDC8A-YGQTM-PUYZ9-2TUXP`
- [ ] Over 2000 random codes: **zero** undetected single-character substitutions
- [ ] Over 2000 random codes: **zero** undetected transpositions of two body characters, adjacent or not
- [ ] `normalise` accepts lowercase, hyphens and spaces, and rejects any character outside the alphabet — including `0`, `1`, `I`, `L`, `O`
- [ ] `generate` returns 20 characters, all in the alphabet, and its own check character verifies
- [ ] `hash` returns base64url of `SHA-256` over the ASCII of the normalised code

**Verify:** `npm test -- --test-name-pattern=codes` → all pass

**Steps:**

- [ ] **Step 1: write the failing tests**

```typescript
// test/codes.test.ts
import { test } from "node:test";
import assert from "node:assert/strict";
import { ALPHABET, checkChar, display, normalise, generate, hash } from "../src/codes.js";

const VECTOR_BODY = "YDC8AYGQTMPUYZ92TUX";
const VECTOR = "SXB-YDC8A-YGQTM-PUYZ9-2TUXP";

test("codes: the known vector's check character is P", () => {
  assert.equal(checkChar(VECTOR_BODY), "P");
  assert.equal(display(VECTOR_BODY + "P"), VECTOR);
});

test("codes: alphabet is the 31 unambiguous characters", () => {
  assert.equal(ALPHABET.length, 31);
  for (const bad of "01ILO") assert.ok(!ALPHABET.includes(bad), `${bad} must not be in the alphabet`);
});

function randomBody(): string {
  let s = "";
  for (let i = 0; i < 19; i++) s += ALPHABET[Math.floor(Math.random() * 31)]!;
  return s;
}

test("codes: every single-character substitution is detected", () => {
  let undetected = 0;
  for (let n = 0; n < 2000; n++) {
    const full = randomBody() + checkChar(randomBody());
    const body = full.slice(0, 19);
    const code = body + checkChar(body);
    for (let i = 0; i < 20; i++) {
      for (const c of ALPHABET) {
        if (c === code[i]) continue;
        const g = code.slice(0, i) + c + code.slice(i + 1);
        if (checkChar(g.slice(0, 19)) === g[19]) undetected++;
      }
    }
  }
  assert.equal(undetected, 0);
});

test("codes: every transposition of two body characters is detected", () => {
  let undetected = 0;
  for (let n = 0; n < 2000; n++) {
    const body = randomBody();
    const code = body + checkChar(body);
    for (let i = 0; i < 19; i++) {
      for (let j = i + 1; j < 19; j++) {
        if (code[i] === code[j]) continue;
        const a = code.split("");
        [a[i], a[j]] = [a[j]!, a[i]!];
        const g = a.join("");
        if (checkChar(g.slice(0, 19)) === g[19]) undetected++;
      }
    }
  }
  assert.equal(undetected, 0);
});

test("codes: normalise strips formatting and rejects ambiguous characters", () => {
  assert.equal(normalise("sxb-ydc8a-ygqtm-puyz9-2tuxp"), VECTOR_BODY + "P");
  assert.equal(normalise(" SXB YDC8A YGQTM PUYZ9 2TUXP "), VECTOR_BODY + "P");
  for (const bad of "01ILO") assert.equal(normalise("SXB-" + bad.repeat(20)), null);
  assert.equal(normalise("SXB-TOOSHORT"), null);
});

test("codes: generate produces a valid code", async () => {
  for (let n = 0; n < 200; n++) {
    const c = generate();
    assert.equal(c.length, 20);
    for (const ch of c) assert.ok(ALPHABET.includes(ch));
    assert.equal(checkChar(c.slice(0, 19)), c[19]);
  }
});

test("codes: hash is base64url sha-256 of the normalised code", async () => {
  const h = await hash(VECTOR_BODY + "P");
  assert.match(h, /^[A-Za-z0-9_-]{43}$/);
  assert.equal(h, await hash(normalise(VECTOR)!));
});
```

- [ ] **Step 2: run, expect failure**

Run: `npm test`
Expected: FAIL — `Cannot find module '../src/codes.js'`

- [ ] **Step 3: implement**

```typescript
// src/codes.ts
// The code format of the design's §4.2. 31 characters, none of them ambiguous:
// 0, 1, I, L and O are absent, so there is nothing to fold on input.
export const ALPHABET = "23456789ABCDEFGHJKMNPQRSTUVWXYZ";
const N = ALPHABET.length; // 31, prime — see checkChar
const BODY = 19;
const PREFIX = "SXB-";

const INDEX: ReadonlyMap<string, number> = new Map(
  [...ALPHABET].map((c, i) => [c, i] as const),
);

/**
 * Weighted sum modulo 31, positions numbered from the left starting at 1.
 * 31 is prime and the weights 1..19 are distinct modulo 31, which is what
 * detects every single-character error and every transposition within the body.
 * Luhn is not used: it needs an even modulus, and over 32 characters it misses 0<->Z.
 */
export function checkChar(body: string): string {
  let sum = 0;
  for (let i = 0; i < body.length; i++) {
    const idx = INDEX.get(body[i]!);
    if (idx === undefined) throw new Error(`character outside the alphabet: ${body[i]}`);
    sum += (i + 1) * idx;
  }
  return ALPHABET[(N - (sum % N)) % N]!;
}

/** `SXB-` and four groups of five. Display only: not hashed, not sent. */
export function display(code: string): string {
  const g = code.match(/.{1,5}/g) ?? [];
  return PREFIX + g.join("-");
}

/**
 * Uppercase, drop the prefix and any separators, then validate.
 * Returns null for anything that is not 20 alphabet characters with a good check.
 */
export function normalise(input: string): string | null {
  const s = input.toUpperCase().replace(/[\s-]+/g, "").replace(/^SXB/, "");
  if (s.length !== BODY + 1) return null;
  for (const c of s) if (!INDEX.has(c)) return null;
  if (checkChar(s.slice(0, BODY)) !== s[BODY]) return null;
  return s;
}

/**
 * 19 characters of CSPRNG, plus the check character.
 * Rejection sampling: 256 is not a multiple of 31, so bytes at or above
 * 248 (= 31 * 8) are discarded rather than folded, which would bias the
 * first eight characters of the alphabet.
 */
export function generate(): string {
  let body = "";
  const buf = new Uint8Array(32);
  while (body.length < BODY) {
    crypto.getRandomValues(buf);
    for (const b of buf) {
      if (b >= 248) continue;
      body += ALPHABET[b % N]!;
      if (body.length === BODY) break;
    }
  }
  return body + checkChar(body);
}

/** base64url of SHA-256 over the ASCII bytes of the normalised code. */
export async function hash(normalised: string): Promise<string> {
  const bytes = new TextEncoder().encode(normalised);
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return btoa(String.fromCharCode(...new Uint8Array(digest)))
    .replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}
```

- [ ] **Step 4: run, expect pass**

Run: `npm test`
Expected: all `codes:` tests pass, including the two exhaustive-property tests

- [ ] **Step 5: commit**

```bash
git add apps/simplex-badge-service/web/src/codes.ts apps/simplex-badge-service/web/test/codes.test.ts
git commit -m "web: code generation, check character and hashing"
```

---

### Task 3: Catalog and totals

**Goal:** `src/catalog.ts` holds the compiled-in catalog and computes months, price and amount, with the guards of §5.1.

**Files:**
- Create: `apps/simplex-badge-service/web/src/catalog.ts`
- Create: `apps/simplex-badge-service/web/test/catalog.test.ts`

**Acceptance Criteria:**
- [ ] `offerTotal` returns `{months, price, amount}` in minor units, integers only
- [ ] `freeMonths` charges for `months - free` and delivers `months`: 3 months at $70/mo with 1 free is `{months: 3, price: 21000, amount: 14000}`
- [ ] Every guard returns a reason rather than a number: zero months, free months at or above the term, discount ≥ 100, zero or unsellable amount
- [ ] `savingPercent` is display-only and never leaves the module
- [ ] The catalog exports at least `supporter` at 700 and `legend` at 7000 per month, matching the screens

**Verify:** `npm test -- --test-name-pattern=catalog` → all pass

**Steps:**

- [ ] **Step 1: write the failing tests**

```typescript
// test/catalog.test.ts
import { test } from "node:test";
import assert from "node:assert/strict";
import { CATALOG, offerTotal, savingPercent } from "../src/catalog.js";

test("catalog: one month at the month price", () => {
  assert.deepEqual(offerTotal(7000, undefined), { months: 1, price: 7000, amount: 7000 });
});

test("catalog: free months charge for the difference and deliver the term", () => {
  const offer = { offerId: "o", priceId: "p", months: 3, discount: { type: "freeMonths", freeMonths: 1 } } as const;
  assert.deepEqual(offerTotal(7000, offer), { months: 3, price: 21000, amount: 14000 });
});

test("catalog: percentage discount truncates in the buyer's favour", () => {
  const offer = { offerId: "o", priceId: "p", months: 3, discount: { type: "discount", discount: 33 } } as const;
  // 21000 * 67 / 100 = 14070
  assert.deepEqual(offerTotal(7000, offer), { months: 3, price: 21000, amount: 14070 });
});

test("catalog: every guard returns a reason, never a number", () => {
  const bad = [
    { offerId: "a", priceId: "p", months: 0, discount: { type: "discount", discount: 10 } },
    { offerId: "b", priceId: "p", months: 3, discount: { type: "freeMonths", freeMonths: 3 } },
    { offerId: "c", priceId: "p", months: 3, discount: { type: "discount", discount: 100 } },
  ] as const;
  for (const o of bad) assert.equal(typeof offerTotal(7000, o), "string", `${o.offerId} must be refused`);
  assert.equal(typeof offerTotal(0, undefined), "string", "a zero month price is unsellable");
});

test("catalog: the compiled-in catalog matches the screens", () => {
  const s = CATALOG.prices.find((p) => p.badgeType === "supporter");
  const l = CATALOG.prices.find((p) => p.badgeType === "legend");
  assert.equal(s?.monthPrice, 700);
  assert.equal(l?.monthPrice, 7000);
});

test("catalog: the saving percentage is display-only arithmetic", () => {
  assert.equal(savingPercent(21000, 14000), 33);
  assert.equal(savingPercent(7000, 7000), 0);
});
```

- [ ] **Step 2: run, expect failure**

Run: `npm test`
Expected: FAIL — module not found

- [ ] **Step 3: implement**

```typescript
// src/catalog.ts
// The catalog is compiled into the page at build time (§2.1). Choosing a level
// and a duration therefore reaches no server; the price is re-checked at invoice
// creation, which is what makes a stale build safe (it lands on B4c).

export type BadgeType = "supporter" | "legend";

export interface Price {
  priceId: string;
  badgeType: BadgeType;
  monthPrice: number; // minor units
  currency: string;
}

export type Discount =
  | { type: "freeMonths"; freeMonths: number }
  | { type: "discount"; discount: number };

export interface Offer {
  offerId: string;
  priceId: string;
  months: number;
  discount: Discount;
}

export interface Catalog {
  prices: readonly Price[];
  offers: readonly Offer[];
}

/** Regenerated at build time from badge_prices and badge_offers. */
export const CATALOG: Catalog = {
  prices: [
    { priceId: "price_supporter", badgeType: "supporter", monthPrice: 700, currency: "usd" },
    { priceId: "price_legend", badgeType: "legend", monthPrice: 7000, currency: "usd" },
  ],
  offers: [
    { offerId: "offer_3m", priceId: "price_legend", months: 3, discount: { type: "freeMonths", freeMonths: 1 } },
    { offerId: "offer_12m", priceId: "price_legend", months: 12, discount: { type: "discount", discount: 50 } },
    { offerId: "offer_3m_s", priceId: "price_supporter", months: 3, discount: { type: "freeMonths", freeMonths: 1 } },
    { offerId: "offer_12m_s", priceId: "price_supporter", months: 12, discount: { type: "discount", discount: 50 } },
  ],
};

export interface Total { months: number; price: number; amount: number }

const MAX_AMOUNT = 100_000_000; // $1,000,000 in minor units

/**
 * Months delivered, gross price, and amount charged — all integers.
 * The gross is formed first and the division is last, so nothing intermediate
 * is rounded, and the truncation goes to the buyer.
 * Returns a reason string when the pair cannot be priced; every such pair is
 * left out of the rendered catalog and refused at invoice creation.
 */
export function offerTotal(monthPrice: number, offer: Offer | undefined): Total | string {
  if (!Number.isInteger(monthPrice) || monthPrice < 0) return "bad month price";
  if (offer === undefined) return charge(1, monthPrice * 1);
  if (offer.months <= 0) return "zero months";
  const gross = monthPrice * offer.months;
  if (offer.discount.type === "freeMonths") {
    if (offer.discount.freeMonths >= offer.months) return "free months exceed the term";
    return charge(offer.months, monthPrice * (offer.months - offer.discount.freeMonths), gross);
  }
  if (offer.discount.discount >= 100) return "discount too large";
  return charge(offer.months, Math.floor((gross * (100 - offer.discount.discount)) / 100), gross);
}

function charge(months: number, amount: number, price = amount): Total | string {
  if (amount <= 0 || amount > MAX_AMOUNT) return "amount unsellable";
  return { months, price, amount };
}

/** Display only. Never sent anywhere, never used to compute a charge. */
export function savingPercent(price: number, amount: number): number {
  if (price <= 0) return 0;
  return Math.round(((price - amount) / price) * 100);
}
```

- [ ] **Step 4: run, expect pass**

Run: `npm test`
Expected: all `catalog:` tests pass

- [ ] **Step 5: commit**

```bash
git add apps/simplex-badge-service/web/src/catalog.ts apps/simplex-badge-service/web/test/catalog.test.ts
git commit -m "web: compiled-in catalog and totals"
```

---

### Task 4: Local store — session and orders

**Goal:** `src/store.ts` implements the two keys of §7.2 over an injectable `Storage`, with the bounds and failure behaviour the spec requires.

**Files:**
- Create: `apps/simplex-badge-service/web/src/store.ts`
- Create: `apps/simplex-badge-service/web/test/store.test.ts`

**Acceptance Criteria:**
- [ ] Two keys: `sxb.session.v1` (step and draft) and `sxb.orders.v1` (array, newest first)
- [ ] `saveOrder` upserts by `orderId`; a later record without a code never clears a stored code
- [ ] The cap is 50; the entry dropped is the **oldest holding no code**; when every entry holds a code the new one is not stored
- [ ] A storage that throws on `setItem` leaves every function working and returns `false` from writes
- [ ] Unparseable JSON is treated as corruption: reads return empty, the next write replaces it
- [ ] `newestOpen` returns the newest `open` order, or undefined

**Verify:** `npm test -- --test-name-pattern=store` → all pass

**Steps:**

- [ ] **Step 1: write the failing tests**

```typescript
// test/store.test.ts
import { test } from "node:test";
import assert from "node:assert/strict";
import { Store, type OrderRecord } from "../src/store.js";

class MemoryStorage {
  map = new Map<string, string>();
  failWrites = false;
  getItem(k: string) { return this.map.get(k) ?? null; }
  setItem(k: string, v: string) { if (this.failWrites) throw new Error("QuotaExceeded"); this.map.set(k, v); }
  removeItem(k: string) { this.map.delete(k); }
}

const order = (id: string, over: Partial<OrderRecord> = {}): OrderRecord => ({
  orderId: id, supportRef: "K7M2Q", badgeType: "legend", months: 12,
  createdAt: new Date(Date.parse("2026-08-24T11:02:19Z") + Number(id)).toISOString(),
  status: "open", ...over,
});

test("store: session round-trips the step and the draft", () => {
  const s = new Store(new MemoryStorage());
  assert.equal(s.session().step, "tier");
  s.saveSession({ step: "months", priceId: "price_legend" });
  assert.equal(s.session().step, "months");
  assert.equal(s.session().priceId, "price_legend");
  s.clearSession();
  assert.equal(s.session().priceId, undefined);
});

test("store: orders upsert by id and keep a stored code", () => {
  const s = new Store(new MemoryStorage());
  s.saveOrder(order("1", { code: "SXB-CODE" }));
  s.saveOrder(order("1", { status: "paid" }));
  const got = s.orders()[0]!;
  assert.equal(got.status, "paid");
  assert.equal(got.code, "SXB-CODE", "a later record must not clear the code");
});

test("store: the cap drops the oldest entry holding no code", () => {
  const s = new Store(new MemoryStorage());
  for (let i = 0; i < 50; i++) s.saveOrder(order(String(i), i < 10 ? {} : { code: "C" + i }));
  s.saveOrder(order("999", { code: "NEW" }));
  const ids = s.orders().map((o) => o.orderId);
  assert.equal(ids.length, 50);
  assert.ok(!ids.includes("0"), "the oldest codeless entry is dropped");
  assert.ok(ids.includes("999"));
});

test("store: with every entry holding a code the new one is not stored", () => {
  const s = new Store(new MemoryStorage());
  for (let i = 0; i < 50; i++) s.saveOrder(order(String(i), { code: "C" + i }));
  assert.equal(s.saveOrder(order("999", { code: "NEW" })), false);
  assert.equal(s.orders().length, 50);
});

test("store: a failing storage degrades without throwing", () => {
  const mem = new MemoryStorage();
  const s = new Store(mem);
  mem.failWrites = true;
  assert.equal(s.saveOrder(order("1", { code: "C" })), false);
  assert.equal(s.saveSession({ step: "months" }), false);
  assert.deepEqual(s.orders(), []);
  assert.equal(s.session().step, "tier");
});

test("store: corruption is replaced, not parsed", () => {
  const mem = new MemoryStorage();
  mem.map.set("sxb.orders.v1", "{not json");
  const s = new Store(mem);
  assert.deepEqual(s.orders(), []);
  assert.equal(s.saveOrder(order("1")), true);
  assert.equal(s.orders().length, 1);
});

test("store: newestOpen finds the resumable order", () => {
  const s = new Store(new MemoryStorage());
  s.saveOrder(order("1", { status: "expired" }));
  s.saveOrder(order("2", { status: "open" }));
  s.saveOrder(order("3", { status: "paid", code: "C" }));
  assert.equal(s.newestOpen()?.orderId, "2");
});
```

- [ ] **Step 2: run, expect failure**

Run: `npm test`
Expected: FAIL — module not found

- [ ] **Step 3: implement**

```typescript
// src/store.ts
// Everything the page knows lives here (§7.2). Two keys, because they have
// different lifetimes: losing the session costs a buyer their place, losing
// the orders costs them their codes, which nothing can recover.

export type Step = "tier" | "months" | "checkout";
export type OrderStatus = "open" | "paid" | "expired";
export type Method = "card" | "btc" | "xmr";

export interface SessionRecord {
  step: Step;
  priceId?: string;
  offerId?: string;
  method?: Method;
  /** Set when the card form reported a successful confirm, before the status moves. */
  submitted?: boolean;
}

export interface OrderRecord {
  orderId: string;
  supportRef: string;
  badgeType: string;
  months: number;
  createdAt: string;
  status: OrderStatus;
  code?: string;
}

/** The subset of Storage this needs, so tests can supply their own. */
export interface StorageLike {
  getItem(key: string): string | null;
  setItem(key: string, value: string): void;
  removeItem(key: string): void;
}

const SESSION_KEY = "sxb.session.v1";
const ORDERS_KEY = "sxb.orders.v1";
const CAP = 50;

export class Store {
  constructor(private readonly storage: StorageLike) {}

  private read<T>(key: string, fallback: T): T {
    try {
      const raw = this.storage.getItem(key);
      if (raw === null) return fallback;
      return JSON.parse(raw) as T;
    } catch {
      // Corruption rather than a newer format: the next write replaces it.
      return fallback;
    }
  }

  private write(key: string, value: unknown): boolean {
    try {
      this.storage.setItem(key, JSON.stringify(value));
      return true;
    } catch {
      // Storage disabled, private browsing, or quota. The flow continues in
      // memory; §11.10 argues the page should refuse to start a payment here.
      return false;
    }
  }

  session(): SessionRecord {
    const s = this.read<SessionRecord | null>(SESSION_KEY, null);
    return s && typeof s === "object" ? s : { step: "tier" };
  }

  saveSession(patch: Partial<SessionRecord>): boolean {
    return this.write(SESSION_KEY, { ...this.session(), ...patch });
  }

  clearSession(): void {
    try { this.storage.removeItem(SESSION_KEY); } catch { /* nothing to do */ }
  }

  orders(): OrderRecord[] {
    const list = this.read<OrderRecord[]>(ORDERS_KEY, []);
    return Array.isArray(list) ? list : [];
  }

  /** Upsert by orderId. Never clears a stored code. Returns false if not stored. */
  saveOrder(record: OrderRecord): boolean {
    const list = this.orders();
    const at = list.findIndex((o) => o.orderId === record.orderId);
    if (at >= 0) {
      const kept = list[at]!;
      list[at] = { ...kept, ...record, code: record.code ?? kept.code };
    } else {
      if (list.length >= CAP) {
        // Drop the oldest entry holding no code. An entry with a code is never
        // dropped: the server cannot produce it again.
        let victim = -1;
        for (let i = list.length - 1; i >= 0; i--) if (!list[i]!.code) { victim = i; break; }
        if (victim < 0) return false;
        list.splice(victim, 1);
      }
      list.unshift(record);
    }
    list.sort((a, b) => b.createdAt.localeCompare(a.createdAt));
    return this.write(ORDERS_KEY, list);
  }

  order(orderId: string): OrderRecord | undefined {
    return this.orders().find((o) => o.orderId === orderId);
  }

  newestOpen(): OrderRecord | undefined {
    return this.orders().find((o) => o.status === "open");
  }

  forgetEverything(): void {
    try { this.storage.removeItem(ORDERS_KEY); this.storage.removeItem(SESSION_KEY); } catch { /* nothing */ }
  }
}
```

- [ ] **Step 4: run, expect pass**

Run: `npm test`
Expected: all `store:` tests pass

- [ ] **Step 5: commit**

```bash
git add apps/simplex-badge-service/web/src/store.ts apps/simplex-badge-service/web/test/store.test.ts
git commit -m "web: local store for session and orders"
```

---

### Task 5: Routing and resume

**Goal:** `src/routing.ts` decides which screen a load renders, from the URL and the store, exactly as §7.1 specifies.

**Files:**
- Create: `apps/simplex-badge-service/web/src/routing.ts`
- Create: `apps/simplex-badge-service/web/test/routing.test.ts`

**Acceptance Criteria:**
- [ ] `?order=` always wins over the hash
- [ ] A bare load with an `open` order resumes that order's payment screen
- [ ] A bare load with no open order but a session step renders that step
- [ ] A bare load with neither renders B1
- [ ] `screenForOrder` is total over status × method × submitted: `paid` with a local code is B6, `paid` without is B6b, `expired` is B5c, `open` crypto is B5, `open` card with `submitted` is B5b, `open` card otherwise remounts the card form
- [ ] Every branch is covered by a test, and the function has no default case that swallows an unknown status

**Verify:** `npm test -- --test-name-pattern=routing` → all pass

**Steps:**

- [ ] **Step 1: write the failing tests**

```typescript
// test/routing.test.ts
import { test } from "node:test";
import assert from "node:assert/strict";
import { resolveLoad, screenForOrder } from "../src/routing.js";
import type { OrderRecord } from "../src/store.js";

const o = (over: Partial<OrderRecord>): OrderRecord => ({
  orderId: "abc", supportRef: "K7M2Q", badgeType: "legend", months: 12,
  createdAt: "2026-08-24T11:02:19Z", status: "open", ...over,
});

test("routing: ?order= wins over the hash and the store", () => {
  const r = resolveLoad({ search: "?order=abc", hash: "#/months" }, { step: "checkout" }, o({}));
  assert.deepEqual(r, { kind: "order", orderId: "abc" });
});

test("routing: a bare load resumes the newest open order", () => {
  const r = resolveLoad({ search: "", hash: "" }, { step: "tier" }, o({ orderId: "z" }));
  assert.deepEqual(r, { kind: "order", orderId: "z", resumed: true });
});

test("routing: with no open order the session step is restored", () => {
  const r = resolveLoad({ search: "", hash: "" }, { step: "months" }, undefined);
  assert.deepEqual(r, { kind: "step", step: "months" });
});

test("routing: with neither, B1", () => {
  const r = resolveLoad({ search: "", hash: "" }, { step: "tier" }, undefined);
  assert.deepEqual(r, { kind: "step", step: "tier" });
});

test("routing: screenForOrder is total over the six cases", () => {
  assert.equal(screenForOrder(o({ status: "paid", code: "C" }), "card", false), "B6");
  assert.equal(screenForOrder(o({ status: "paid" }), "card", false), "B6b");
  assert.equal(screenForOrder(o({ status: "expired" }), "xmr", false), "B5c");
  assert.equal(screenForOrder(o({ status: "open" }), "xmr", false), "B5");
  assert.equal(screenForOrder(o({ status: "open" }), "card", true), "B5b");
  assert.equal(screenForOrder(o({ status: "open" }), "card", false), "cardForm");
});
```

- [ ] **Step 2: run, expect failure**

Run: `npm test`
Expected: FAIL — module not found

- [ ] **Step 3: implement**

```typescript
// src/routing.ts
// All page state is in localStorage; the URL only says which order is being
// looked at (§7.1). A load reads the URL, then the store, and renders.

import type { Method, OrderRecord, SessionRecord, Step } from "./store.js";

export type Screen = "B5" | "B5b" | "B5c" | "B6" | "B6b" | "cardForm";

export type Load =
  | { kind: "order"; orderId: string; resumed?: true }
  | { kind: "step"; step: Step };

export interface UrlParts { search: string; hash: string }

/**
 * `?order=` always wins: it names the order to look at, whatever the store says.
 * Otherwise a still-open order is resumed — someone who sent a payment and closed
 * the tab comes back to it rather than to the landing page — and failing that the
 * wizard resumes where it was.
 */
export function resolveLoad(
  url: UrlParts,
  session: SessionRecord,
  newestOpen: OrderRecord | undefined,
): Load {
  const orderId = new URLSearchParams(url.search).get("order");
  if (orderId) return { kind: "order", orderId };
  if (newestOpen) return { kind: "order", orderId: newestOpen.orderId, resumed: true };
  return { kind: "step", step: session.step ?? "tier" };
}

/**
 * Total over status, method and the local `submitted` flag. There is deliberately
 * no default arm: a status this does not know is a compile error, not a blank page.
 */
export function screenForOrder(order: OrderRecord, method: Method, submitted: boolean): Screen {
  switch (order.status) {
    case "paid":
      // The code lives only in the browser; without it nothing can produce one.
      return order.code ? "B6" : "B6b";
    case "expired":
      return "B5c";
    case "open":
      if (method !== "card") return "B5";
      // `submitted` is the browser's own note that confirm() succeeded. A hint,
      // not evidence — it selects a screen and nothing else.
      return submitted ? "B5b" : "cardForm";
  }
}
```

- [ ] **Step 4: run, expect pass**

Run: `npm test`
Expected: all `routing:` tests pass

- [ ] **Step 5: commit**

```bash
git add apps/simplex-badge-service/web/src/routing.ts apps/simplex-badge-service/web/test/routing.test.ts
git commit -m "web: load resolution and screen routing"
```

---

### Task 6: Mock API server

**Goal:** `mock/server.py` serves `public/` and `dist/`, implements the two endpoints including the long poll, and exposes a control surface standing in for Stripe and BTCPay so a test can settle, part-pay or expire an invoice.

**Files:**
- Create: `apps/simplex-badge-service/web/mock/server.py`

**Acceptance Criteria:**
- [ ] `POST /api/invoice` validates `priceId`, `offerId`, `method`, `codeHash`; returns `invoiceId`, `supportRef`, `badgeType`, `months`, `amount`, `currency`, `expiresAt`, and `clientSecret` for card or `address`/`cryptoAmount`/`cryptoCurrency` for crypto
- [ ] A repeated `codeHash` returns `409 {"error":"code_conflict"}`
- [ ] An unknown `priceId` returns `400 {"error":"catalog_changed"}`
- [ ] `GET /api/invoice/:id` answers immediately; `?wait=<status>` holds until the status differs or 30 s elapse, and answers at once if it already differs
- [ ] `POST /control/settle/:id` and `/control/expire/:id` change the status and release every waiter for that invoice within 50 ms
- [ ] No response ever contains a code or a code hash
- [ ] `GET /api/invoice/:unknown` is `404 {"error":"not_found"}`

**Verify:** `python3 mock/server.py --port 8099 &` then `curl -s -XPOST localhost:8099/api/invoice -d '{"priceId":"price_legend","offerId":"offer_12m","method":"xmr","codeHash":"abc"}' -H 'content-type: application/json'` → JSON with `invoiceId` and `address`, no `code` field

**Steps:**

- [ ] **Step 1: write the server**

```python
# mock/server.py
"""Stands in for the Haskell service AND for Stripe and BTCPay, so the whole
browser flow can be driven without any of them. A test fixture: no signatures,
no persistence, no money, and not a specification of the real service.

Standard library only. Threaded, because the wait endpoint holds a connection.
"""
import json, os, secrets, sys, threading, time
from datetime import datetime, timedelta, timezone
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import urlparse, parse_qs

ROOT = Path(__file__).resolve().parent.parent
HOLD_SECONDS = float(os.environ.get("MOCK_HOLD_SECONDS", "30"))

CATALOG = {
    "price_supporter": {"badgeType": "supporter", "monthPrice": 700},
    "price_legend": {"badgeType": "legend", "monthPrice": 7000},
}
OFFERS = {
    "offer_3m": {"months": 3, "free": 1},
    "offer_12m": {"months": 12, "discount": 50},
    "offer_3m_s": {"months": 3, "free": 1},
    "offer_12m_s": {"months": 12, "discount": 50},
}
MIME = {".html": "text/html", ".css": "text/css", ".js": "text/javascript",
        ".svg": "image/svg+xml", ".json": "application/json", ".webmanifest": "application/manifest+json"}

LOCK = threading.Lock()
INVOICES = {}          # invoiceId -> dict
HASHES = {}            # codeHash -> invoiceId, mirroring the real primary key
EVENTS = {}            # invoiceId -> threading.Event, replacing the STM waiters


def now_iso():
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def total(price_id, offer_id):
    price = CATALOG.get(price_id)
    if price is None:
        return None
    if not offer_id:
        return {"months": 1, "amount": price["monthPrice"]}
    offer = OFFERS.get(offer_id)
    if offer is None:
        return None
    gross = price["monthPrice"] * offer["months"]
    if "free" in offer:
        amount = price["monthPrice"] * (offer["months"] - offer["free"])
    else:
        amount = (gross * (100 - offer["discount"])) // 100
    return {"months": offer["months"], "amount": amount}


def public_view(inv):
    """What the browser may see. Note what is absent: no code, no code hash.
    The service never has the code (design §4.2)."""
    view = {
        "status": inv["status"], "badgeType": inv["badgeType"], "months": inv["months"],
        "amount": inv["amount"], "currency": inv["currency"],
        "expiresAt": inv["expiresAt"], "supportRef": inv["supportRef"],
    }
    for k in ("amountPaid", "cryptoAmountPaid", "settledAt"):
        if inv.get(k) is not None:
            view[k] = inv[k]
    if inv["method"] == "card":
        view["clientSecret"] = inv["clientSecret"]
    else:
        view["address"] = inv["address"]
        view["cryptoAmount"] = inv["cryptoAmount"]
        view["cryptoCurrency"] = inv["method"]
    return view


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, *args):
        pass  # quiet under tests

    def _send(self, status, payload, ctype="application/json"):
        body = payload if isinstance(payload, bytes) else json.dumps(payload).encode()
        self.send_response(status)
        self.send_header("content-type", ctype)
        self.send_header("content-length", str(len(body)))
        self.send_header("cache-control", "no-store")
        self.end_headers()
        self.wfile.write(body)

    def _read_json(self):
        length = int(self.headers.get("content-length") or 0)
        try:
            return json.loads(self.rfile.read(length) or b"{}")
        except Exception:
            return None

    def do_POST(self):
        path = urlparse(self.path).path

        # --- control surface: what Stripe or BTCPay would tell us, and what the
        # poller would then read. This is how a test moves money.
        if path.startswith("/control/"):
            parts = path.strip("/").split("/")
            if len(parts) != 3:
                return self._send(400, {"error": "bad_request"})
            _, action, invoice_id = parts
            with LOCK:
                inv = INVOICES.get(invoice_id)
                if inv is None:
                    return self._send(404, {"error": "not_found"})
                if action == "settle":
                    inv["status"] = "paid"
                    inv["amountPaid"] = inv["amount"]
                    inv["settledAt"] = now_iso()
                    if inv["method"] != "card":
                        inv["cryptoAmountPaid"] = inv["cryptoAmount"]
                elif action == "expire":
                    inv["status"] = "expired"
                elif action == "partial":
                    inv["amountPaid"] = inv["amount"] // 2
                    inv["cryptoAmountPaid"] = "0.734"
                else:
                    return self._send(400, {"error": "bad_request"})
                event = EVENTS.get(invoice_id)
                status = inv["status"]
            if event is not None:
                event.set()   # wake every held request for this invoice
            return self._send(200, {"ok": True, "status": status})

        if path == "/api/invoice":
            body = self._read_json()
            if not body or not isinstance(body.get("codeHash"), str) or not body["codeHash"]:
                return self._send(400, {"error": "bad_request"})
            if body.get("method") not in ("card", "btc", "xmr"):
                return self._send(400, {"error": "bad_request"})
            with LOCK:
                if body["codeHash"] in HASHES:
                    # The real code_hash primary key: a duplicate is refused, never reused.
                    return self._send(409, {"error": "code_conflict"})
                t = total(body.get("priceId"), body.get("offerId"))
                if t is None:
                    return self._send(400, {"error": "catalog_changed"})
                invoice_id = secrets.token_urlsafe(16)
                is_card = body["method"] == "card"
                inv = {
                    "invoiceId": invoice_id, "method": body["method"], "status": "open",
                    "badgeType": CATALOG[body["priceId"]]["badgeType"], "months": t["months"],
                    "amount": t["amount"], "currency": "usd",
                    "supportRef": secrets.token_hex(3).upper()[:5],
                    "expiresAt": (datetime.now(timezone.utc) + timedelta(hours=1))
                        .replace(microsecond=0).isoformat().replace("+00:00", "Z"),
                    "clientSecret": f"cs_test_{secrets.token_hex(12)}" if is_card else None,
                    "address": None if is_card else "48HqK2XmVexampleAddress9fRtWc",
                    "cryptoAmount": None if is_card else "1.482",
                }
                INVOICES[invoice_id] = inv
                HASHES[body["codeHash"]] = invoice_id
                EVENTS[invoice_id] = threading.Event()
                payload = {"invoiceId": invoice_id, **public_view(inv)}
            return self._send(200, payload)

        return self._send(405, {"error": "bad_request"})

    def do_GET(self):
        parsed = urlparse(self.path)
        path, query = parsed.path, parse_qs(parsed.query)

        if path.startswith("/api/invoice/"):
            invoice_id = path[len("/api/invoice/"):]
            with LOCK:
                inv = INVOICES.get(invoice_id)
                if inv is None:
                    return self._send(404, {"error": "not_found"})
                current = inv["status"]
                event = EVENTS.get(invoice_id)
            wait = (query.get("wait") or [None])[0]
            if wait is not None and wait == current and event is not None:
                # Hold until settlement sets the event, or the hold expires.
                # Nothing here polls the record on a timer.
                event.wait(timeout=HOLD_SECONDS)
                event.clear()
            with LOCK:
                inv = INVOICES[invoice_id]
                payload = {"invoiceId": invoice_id, **public_view(inv)}
            return self._send(200, payload)

        # static: public/ first, then dist/ for the compiled modules
        rel = "index.html" if path == "/" else path.lstrip("/")
        for base in ("public", "dist"):
            candidate = (ROOT / base / rel).resolve()
            if not str(candidate).startswith(str((ROOT / base).resolve())):
                break  # refuse traversal
            if candidate.is_file():
                ctype = MIME.get(candidate.suffix, "application/octet-stream")
                return self._send(200, candidate.read_bytes(), ctype)
        return self._send(404, {"error": "not_found"})


def main():
    port = 8099
    if "--port" in sys.argv:
        port = int(sys.argv[sys.argv.index("--port") + 1])
    server = ThreadingHTTPServer(("127.0.0.1", port), Handler)
    server.daemon_threads = True
    print(f"mock badge service on http://localhost:{port}", flush=True)
    server.serve_forever()


if __name__ == "__main__":
    main()
```

- [ ] **Step 2: verify by hand**

Run:
```bash
cd apps/simplex-badge-service/web
python3 mock/server.py --port 8099 &
sleep 0.5
curl -s -XPOST localhost:8099/api/invoice -H 'content-type: application/json' \
  -d '{"priceId":"price_legend","offerId":"offer_12m","method":"xmr","codeHash":"h1"}'
```
Expected: JSON containing `"invoiceId"`, `"address"`, `"amount":42000`, and **no** `code` field

Run: the same request again
Expected: `{"error":"code_conflict"}` with status 409

- [ ] **Step 3: stop the server and commit**

```bash
kill %1
git add apps/simplex-badge-service/web/mock/server.py
git commit -m "web: python mock for the service and both providers"
```

---

### Task 7: API client and the waiting loop

**Goal:** `src/api.ts` creates invoices and waits for payment, with the loop rules of §7.1 — reissue at once, back off only on network error, stop on `paid`.

**Files:**
- Create: `apps/simplex-badge-service/web/src/api.ts`
- Create: `apps/simplex-badge-service/web/test/flow.test.ts` (first half; the end-to-end half lands in Task 10)

**Acceptance Criteria:**
- [ ] `createInvoice` sends `priceId`, `offerId`, `method`, `codeHash` and nothing else
- [ ] `createInvoice` maps 400/409/429/503 to typed errors rather than throwing strings
- [ ] `waitForChange` passes `?wait=<current status>` and resolves when the status differs
- [ ] A network failure backs off (1 s, doubling, capped at 30 s) and retries; a 404 stops the loop
- [ ] The loop stops on `paid` and keeps waiting on `expired`
- [ ] Backoff delays are injected, so tests do not sleep

**Verify:** `npm test -- --test-name-pattern=api` → all pass

**Steps:**

- [ ] **Step 1: write the failing tests**

```typescript
// test/flow.test.ts
import { test } from "node:test";
import assert from "node:assert/strict";
import { createInvoice, waitForChange, ApiError } from "../src/api.js";

function fetchReturning(...responses: Array<{ status: number; body: unknown } | Error>) {
  let i = 0;
  const calls: Array<{ url: string; init?: RequestInit }> = [];
  const fn = async (url: string, init?: RequestInit) => {
    calls.push({ url, init });
    const r = responses[Math.min(i++, responses.length - 1)]!;
    if (r instanceof Error) throw r;
    return { ok: r.status < 400, status: r.status, json: async () => r.body } as Response;
  };
  return { fn: fn as unknown as typeof fetch, calls };
}

test("api: createInvoice sends exactly the four fields", async () => {
  const { fn, calls } = fetchReturning({ status: 200, body: { invoiceId: "i1", status: "open" } });
  await createInvoice({ priceId: "p", offerId: "o", method: "xmr", codeHash: "h" }, fn);
  assert.equal(calls[0]!.url, "/api/invoice");
  assert.deepEqual(JSON.parse(String(calls[0]!.init!.body)), { priceId: "p", offerId: "o", method: "xmr", codeHash: "h" });
});

test("api: createInvoice maps error codes to typed errors", async () => {
  for (const [status, code] of [[409, "code_conflict"], [400, "catalog_changed"], [429, "rate_limited"], [503, "provider_unavailable"]] as const) {
    const { fn } = fetchReturning({ status, body: { error: code } });
    await assert.rejects(
      () => createInvoice({ priceId: "p", method: "card", codeHash: "h" }, fn),
      (e: unknown) => e instanceof ApiError && e.code === code,
    );
  }
});

test("api: waitForChange passes the current status and resolves on a change", async () => {
  const { fn, calls } = fetchReturning({ status: 200, body: { invoiceId: "i1", status: "paid" } });
  const got = await waitForChange("i1", "open", fn, async () => {});
  assert.ok(calls[0]!.url.includes("wait=open"));
  assert.equal(got.status, "paid");
});

test("api: a network error backs off and retries, and the delays double", async () => {
  const delays: number[] = [];
  const { fn } = fetchReturning(new Error("offline"), new Error("offline"), { status: 200, body: { invoiceId: "i", status: "paid" } });
  const got = await waitForChange("i", "open", fn, async (ms) => { delays.push(ms); });
  assert.deepEqual(delays, [1000, 2000]);
  assert.equal(got.status, "paid");
});

test("api: a 404 stops the loop", async () => {
  const { fn } = fetchReturning({ status: 404, body: { error: "not_found" } });
  await assert.rejects(
    () => waitForChange("gone", "open", fn, async () => {}),
    (e: unknown) => e instanceof ApiError && e.code === "not_found",
  );
});
```

- [ ] **Step 2: run, expect failure**

Run: `npm test`
Expected: FAIL — module not found

- [ ] **Step 3: implement**

```typescript
// src/api.ts
// The two endpoints of §5, and the waiting loop of §7.1. `fetch` and the sleep
// function are injected so the loop is testable without a network or a clock.

import type { Method, OrderStatus } from "./store.js";

export type ErrorCode =
  | "catalog_changed" | "bad_request" | "code_conflict"
  | "rate_limited" | "internal" | "provider_unavailable" | "not_found" | "unknown";

export class ApiError extends Error {
  constructor(readonly code: ErrorCode, readonly status: number) {
    super(`${code} (${status})`);
    this.name = "ApiError";
  }
}

export interface CreateRequest {
  priceId: string;
  offerId?: string;
  method: Method;
  codeHash: string;
}

export interface InvoiceView {
  invoiceId: string;
  status: OrderStatus;
  badgeType?: string;
  months?: number;
  amount?: number;
  currency?: string;
  expiresAt?: string;
  supportRef?: string;
  amountPaid?: number;
  cryptoAmountPaid?: string;
  settledAt?: string;
  clientSecret?: string;
  address?: string;
  cryptoAmount?: string;
  cryptoCurrency?: string;
}

async function decode(res: Response): Promise<never> {
  let code: ErrorCode = "unknown";
  try {
    const body = (await res.json()) as { error?: string };
    if (body?.error) code = body.error as ErrorCode;
  } catch { /* an empty or unparseable body stays "unknown" */ }
  throw new ApiError(code, res.status);
}

/** The request carries four fields. Everything else is derived server-side. */
export async function createInvoice(req: CreateRequest, f: typeof fetch = fetch): Promise<InvoiceView> {
  const body: Record<string, unknown> = { priceId: req.priceId, method: req.method, codeHash: req.codeHash };
  if (req.offerId !== undefined) body.offerId = req.offerId;
  const res = await f("/api/invoice", {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(body),
  });
  if (!res.ok) return decode(res);
  return (await res.json()) as InvoiceView;
}

export type Sleep = (ms: number) => Promise<void>;

const BACKOFF_START = 1000;
const BACKOFF_MAX = 30_000;

/**
 * One pass of the waiting loop: hold until the status differs from `seen`.
 * A network error backs off and retries — that is the only delay anywhere.
 * A 404 is terminal and propagates; the caller renders the unknown-order screen.
 */
export async function waitForChange(
  invoiceId: string,
  seen: OrderStatus,
  f: typeof fetch = fetch,
  sleep: Sleep = (ms) => new Promise((r) => setTimeout(r, ms)),
): Promise<InvoiceView> {
  let backoff = BACKOFF_START;
  for (;;) {
    let res: Response;
    try {
      res = await f(`/api/invoice/${encodeURIComponent(invoiceId)}?wait=${encodeURIComponent(seen)}`);
    } catch {
      await sleep(backoff);
      backoff = Math.min(backoff * 2, BACKOFF_MAX);
      continue;
    }
    if (res.status === 404) return decode(res);
    if (!res.ok) {
      await sleep(backoff);
      backoff = Math.min(backoff * 2, BACKOFF_MAX);
      continue;
    }
    const view = (await res.json()) as InvoiceView;
    if (view.status !== seen) return view;
    // The hold timed out with nothing changed: reissue at once, no delay.
    backoff = BACKOFF_START;
  }
}
```

- [ ] **Step 4: run, expect pass**

Run: `npm test`
Expected: all `api:` tests pass

- [ ] **Step 5: commit**

```bash
git add apps/simplex-badge-service/web/src/api.ts apps/simplex-badge-service/web/test/flow.test.ts
git commit -m "web: api client and waiting loop"
```

---

### Task 8: The shell, the wizard track and the screens

**Goal:** `public/index.html`, `public/styles.css`, `src/screens.ts` and `src/main.ts` render every screen from the design, with the horizontal track and Back.

**Files:**
- Create: `apps/simplex-badge-service/web/public/index.html`
- Create: `apps/simplex-badge-service/web/public/styles.css`
- Create: `apps/simplex-badge-service/web/src/screens.ts`
- Create: `apps/simplex-badge-service/web/src/main.ts`
- Reference: `plans/badges-codes/screens/*.svg` for layout and copy

**Acceptance Criteria:**
- [ ] B1–B4 are four panels of one horizontal track; Continue scrolls right, **[ ← Back ]** scrolls left and calls `history.back()`
- [ ] The track is not free-scrolling: unreached panels are `inert` and not scrollable to
- [ ] `prefers-reduced-motion: reduce` disables smooth scrolling
- [ ] Copy matches the SVG screens for B1, B2, B3, B4, B4b, B4c, B4d, B5, B5b, B5c, B6, B6b, and B7 is built from §8.4
- [ ] Below 560 px each panel is viewport width — the same track, no second layout
- [ ] **No `innerHTML` anywhere**: `grep -rn 'innerHTML' src/ public/` returns nothing
- [ ] A code is rendered only on B6; B5, B5b, B5c and B7's non-paid rows never show one
- [ ] Colour tokens on bare `:root`, redefined under `prefers-color-scheme: dark`

**Verify:** `npm run build && python3 mock/server.py --port 8099` then open `http://localhost:8099` → B1 renders, Continue advances, Back returns

**Steps:**

- [ ] **Step 1: the shell**

`public/index.html` carries the track and nothing else — every screen is built in `screens.ts`:

```html
<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Support SimpleX</title>
<link rel="stylesheet" href="/styles.css">
</head>
<body>
  <main id="app" aria-live="polite"></main>
  <footer><a href="https://simplex.chat/contact">simplex.chat/contact</a></footer>
  <script type="module" src="/src/main.js"></script>
</body>
</html>
```

- [ ] **Step 2: the visual system**

`public/styles.css` implements §7.3: one centred column at `max-width: 560px`, the accent `#0053D0`, colour tokens on bare `:root` and redefined under `@media (prefers-color-scheme: dark)`, and the track:

```css
:root { --bg:#fff; --fg:#0D0E12; --muted:#6B7785; --line:#e8e8ed; --accent:#0053D0; }
@media (prefers-color-scheme: dark) { :root { --bg:#0D0E12; --fg:#f5f5f7; --muted:#9aa3ad; --line:#2a2d33; } }
body { margin:0; background:var(--bg); color:var(--fg);
       font-family:-apple-system,BlinkMacSystemFont,'Helvetica Neue',Helvetica,Arial,sans-serif; }
#app { max-width:560px; margin:0 auto; overflow-x:hidden; }
.track { display:flex; scroll-behavior:smooth; overflow-x:hidden; }
.panel { flex:0 0 100%; padding:24px; box-sizing:border-box; }
@media (prefers-reduced-motion: reduce) { .track { scroll-behavior:auto; } }
@media (max-width:560px) { .panel { padding:16px; } }
.back { color:var(--accent); background:none; border:0; font-size:16px; cursor:pointer; padding:0 0 12px; }
.primary { display:block; width:100%; padding:16px; border:0; border-radius:8px;
           background:var(--accent); color:#fff; font-size:17px; cursor:pointer; }
.primary[disabled] { opacity:.5; cursor:default; }
.warn { background:#FFF1F0; color:#B3261E; padding:12px; border-radius:8px; }
.code { font-family:ui-monospace,SFMono-Regular,Menlo,monospace; font-size:20px;
        background:#F3FAFF; padding:16px; border-radius:8px; text-align:center; }
```

- [ ] **Step 3: screens as DOM builders**

Every screen is a function returning an element. Nothing is assigned from a string:

```typescript
// src/screens.ts  (excerpt — the same shape for every screen)
export function el(tag: string, attrs: Record<string, string> = {}, ...kids: Array<Node | string>): HTMLElement {
  const node = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) node.setAttribute(k, v);
  for (const kid of kids) node.append(typeof kid === "string" ? document.createTextNode(kid) : kid);
  return node;
}

export function b1(onStart: () => void): HTMLElement {
  return el("section", { class: "panel" },
    el("h1", {}, "Support SimpleX"),
    el("p", {}, "SimpleX has no ads, no user accounts and nothing to sell."),
    el("p", {}, "A supporter badge helps pay for the people who build it."),
    button("Choose your level", onStart),
    el("p", { class: "muted" }, "The badge shows on your profile. Nothing renews by itself, and no account is created."),
  );
}

export function b6(displayCode: string, savedLocally: boolean): HTMLElement {
  const warning = savedLocally
    ? "This is the only copy. It is saved in this browser and nowhere else — not in any account, and not on our side. Anyone using this browser can read it, and clearing the browser loses it."
    : "This code could not be saved in this browser. Copy it now: it is shown here and nowhere else.";
  return el("section", { class: "panel" },
    el("h1", {}, "Paid. Here is your code."),
    el("div", { class: "code" }, displayCode),
    button("Copy code", () => navigator.clipboard.writeText(displayCode)),
    el("p", { class: "muted" }, "REDEEM IT IN THE APP — Settings → Supporter perks → Redeem code"),
    el("p", { class: "warn" }, warning),
  );
}

function button(label: string, onClick: () => void): HTMLElement {
  const b = el("button", { class: "primary", type: "button" }, label);
  b.addEventListener("click", onClick);
  return b;
}
```

Build the remaining screens the same way, taking copy from `plans/badges-codes/screens/*.svg`: `b2`, `b3`, `b4`, `b4b`, `b4c`, `b4d`, `b5`, `b5b`, `b5c`, `b6b`, `b7`, and the two failure screens of §8.2.

- [ ] **Step 4: wiring**

`src/main.ts` is the only module that reads `location`, `localStorage` or `history`. On load it calls `resolveLoad` (Task 5), renders, and for an order starts the waiting loop (Task 7). Back is `history.back()`; the invoice response replaces the entry with `history.replaceState`.

- [ ] **Step 5: check the ban on string markup**

Run: `grep -rn 'innerHTML\|outerHTML\|insertAdjacentHTML' src/ public/`
Expected: no output

- [ ] **Step 6: commit**

```bash
git add apps/simplex-badge-service/web/public apps/simplex-badge-service/web/src/screens.ts apps/simplex-badge-service/web/src/main.ts
git commit -m "web: shell, wizard track and screens"
```

---

### Task 9: Service worker and offline

**Goal:** `public/sw.js` precaches the build and serves it cache-first, while `/api/*` is network-only.

**Files:**
- Create: `apps/simplex-badge-service/web/public/sw.js`
- Modify: `apps/simplex-badge-service/web/src/main.ts` (register the worker)

**Acceptance Criteria:**
- [ ] The precache list is explicit URLs, never the worker's own navigation response — an Anubis challenge page must not be cached as the shell (§7.4)
- [ ] **`/api/*` is never cached** and never entered into the Cache API
- [ ] Activation deletes every cache whose name is not the current build
- [ ] Registration happens only after a load that produced the real shell
- [ ] `js.stripe.com` is never cached: Stripe forbids it (§6.2)

**Verify:** `grep -n 'api/' public/sw.js` shows the exclusion before any cache write

**Steps:**

- [ ] **Step 1: the worker**

```javascript
// public/sw.js
// Offline support for everything except the two endpoints (§7.4). The buyer's
// code lives only in this browser, so reading it back must not need a network.
const BUILD = "sxb-v1";
const PRECACHE = ["/", "/styles.css", "/src/main.js", "/src/screens.js",
  "/src/store.js", "/src/routing.js", "/src/api.js", "/src/codes.js", "/src/catalog.js"];

self.addEventListener("install", (e) => {
  // Explicit URLs only. Caching whatever "/" returns could enshrine an Anubis
  // challenge page as the shell, permanently, for this visitor.
  e.waitUntil(caches.open(BUILD).then((c) => c.addAll(PRECACHE)));
});

self.addEventListener("activate", (e) => {
  e.waitUntil(caches.keys().then((names) =>
    Promise.all(names.filter((n) => n !== BUILD).map((n) => caches.delete(n)))));
});

self.addEventListener("fetch", (e) => {
  const url = new URL(e.request.url);
  // Never cache the API. A stale "paid" would show a code for an order that
  // later failed; a stale "open" would hide a settled one.
  if (url.pathname.startsWith("/api/") || url.origin !== self.location.origin) return;
  e.respondWith(caches.match(e.request).then((hit) => hit ?? fetch(e.request)));
});
```

- [ ] **Step 2: register after a good load**

In `src/main.ts`, at the end of the first successful render:

```typescript
if ("serviceWorker" in navigator) {
  window.addEventListener("load", () => {
    void navigator.serviceWorker.register("/sw.js");
  });
}
```

- [ ] **Step 3: verify the exclusion**

Run: `grep -n 'api/' apps/simplex-badge-service/web/public/sw.js`
Expected: the `startsWith("/api/")` early return appears before any `caches` write

- [ ] **Step 4: commit**

```bash
git add apps/simplex-badge-service/web/public/sw.js apps/simplex-badge-service/web/src/main.ts
git commit -m "web: service worker with api excluded from cache"
```

---

### Task 10: End-to-end flow against the mock server

**Goal:** One test drives a whole purchase against the Python mock — draw a code, create an invoice, wait, settle it through the control surface, and see the status change — proving the parts fit.

> **USER-ORDERED GATE — NON-SKIPPABLE.** This task was requested by the user in the current conversation. It MUST NOT be closed by walking around it, by declaring it "verified inline", or by substituting a cheaper check. Close only after every item in `acceptanceCriteria` has been re-validated independently, with output captured.

**Files:**
- Modify: `apps/simplex-badge-service/web/test/flow.test.ts` (add the end-to-end half)

**Acceptance Criteria:**
- [ ] The test starts `mock/server.py` on a free port and stops it afterwards
- [ ] A generated code hashes, creates an invoice, and the response carries **no** code or code hash
- [ ] `waitForChange` is pending while the invoice is `open`, and resolves within 500 ms of `POST /control/settle/:id` — proving the long poll is woken rather than polled
- [ ] A second invoice with the same `codeHash` returns `code_conflict`
- [ ] An expired invoice with a partial payment reports `amountPaid` and `cryptoAmountPaid`
- [ ] The store, driven by the same responses, ends with one `paid` order holding the code

**Verify:** `npm test -- --test-name-pattern=flow` → all pass

**Steps:**

- [ ] **Step 1: write the end-to-end test**

```typescript
// test/flow.test.ts  (appended)
import { test } from "node:test";
import assert from "node:assert/strict";
import { spawn, type ChildProcess } from "node:child_process";
import { generate, display, hash } from "../src/codes.js";
import { createInvoice, waitForChange, ApiError } from "../src/api.js";
import { Store } from "../src/store.js";

const PORT = 8123;
const BASE = `http://localhost:${PORT}`;
const at = (p: string) => `${BASE}${p}`;
const netFetch: typeof fetch = (input, init) =>
  fetch(typeof input === "string" && input.startsWith("/") ? at(input) : input, init);

async function withServer(fn: () => Promise<void>) {
  const proc: ChildProcess = spawn("python3", ["mock/server.py", "--port", String(PORT)],
    { stdio: "ignore", env: { ...process.env, MOCK_HOLD_SECONDS: "5" } });
  try {
    for (let i = 0; i < 50; i++) {
      try { await fetch(at("/api/invoice/none")); break; } catch { await new Promise((r) => setTimeout(r, 100)); }
    }
    await fn();
  } finally { proc.kill(); }
}

class Mem {
  m = new Map<string, string>();
  getItem(k: string) { return this.m.get(k) ?? null; }
  setItem(k: string, v: string) { this.m.set(k, v); }
  removeItem(k: string) { this.m.delete(k); }
}

test("flow: a purchase runs end to end and settlement wakes the wait", async () => {
  await withServer(async () => {
    const store = new Store(new Mem());
    const code = generate();
    const codeHash = await hash(code);

    // The browser saves the code BEFORE the request: it is the only copy.
    const created = await createInvoice(
      { priceId: "price_legend", offerId: "offer_12m", method: "xmr", codeHash }, netFetch);

    assert.equal(created.status, "open");
    assert.equal(created.amount, 42000);
    const raw = JSON.stringify(created);
    assert.ok(!raw.includes(code), "no response may carry the code");
    assert.ok(!raw.includes(codeHash), "no response may carry the code hash");

    store.saveOrder({
      orderId: created.invoiceId, supportRef: created.supportRef!, badgeType: created.badgeType!,
      months: created.months!, createdAt: new Date().toISOString(), status: "open", code: display(code),
    });

    // Hold, then settle through the control surface and time the wake.
    const started = Date.now();
    const waiting = waitForChange(created.invoiceId, "open", netFetch);
    await new Promise((r) => setTimeout(r, 100));
    await fetch(at(`/control/settle/${created.invoiceId}`), { method: "POST" });
    const settled = await waiting;
    const elapsed = Date.now() - started;

    assert.equal(settled.status, "paid");
    assert.ok(elapsed < 500, `the wait must be woken, not polled (took ${elapsed}ms)`);

    store.saveOrder({ ...store.order(created.invoiceId)!, status: "paid" });
    const final = store.orders();
    assert.equal(final.length, 1);
    assert.equal(final[0]!.status, "paid");
    assert.equal(final[0]!.code, display(code), "settlement must not clear the stored code");
  });
});

test("flow: a repeated code hash is refused before any invoice exists", async () => {
  await withServer(async () => {
    const codeHash = await hash(generate());
    const req = { priceId: "price_legend", method: "card", codeHash } as const;
    await createInvoice(req, netFetch);
    await assert.rejects(() => createInvoice(req, netFetch),
      (e: unknown) => e instanceof ApiError && e.code === "code_conflict");
  });
});

test("flow: an expired invoice reports what arrived", async () => {
  await withServer(async () => {
    const codeHash = await hash(generate());
    const inv = await createInvoice(
      { priceId: "price_supporter", method: "xmr", codeHash }, netFetch);
    await fetch(at(`/control/partial/${inv.invoiceId}`), { method: "POST" });
    await fetch(at(`/control/expire/${inv.invoiceId}`), { method: "POST" });
    const view = await waitForChange(inv.invoiceId, "open", netFetch);
    assert.equal(view.status, "expired");
    assert.equal(view.amountPaid, 350);
    assert.equal(view.cryptoAmountPaid, "0.734");
  });
});
```

- [ ] **Step 2: run, expect failure first**

Run: `npm test`
Expected: the end-to-end tests fail until Tasks 6 and 7 are both in place; then all pass

- [ ] **Step 3: run the whole suite**

Run: `npm test`
Expected: `pass` for every test in `codes`, `catalog`, `store`, `routing`, `api` and `flow`; `fail 0`

- [ ] **Step 4: commit**

```bash
git add apps/simplex-badge-service/web/test/flow.test.ts
git commit -m "web: end-to-end flow test against the mock server"
```

---

### Task 11: README and the run instructions

**Goal:** Someone who has never seen this can build it, run it, and drive a whole purchase by hand.

**Files:**
- Create: `apps/simplex-badge-service/web/README.md`

**Acceptance Criteria:**
- [ ] States the three commands: `npm install`, `npm run build`, `npm test`
- [ ] Explains how to run the mock (`python3 mock/server.py`) and drive a purchase: create, then `curl -XPOST /control/settle/:id`
- [ ] Names what the mock is **not**: no signatures, no persistence, no real money, and not a specification of the Haskell service — it stands in for the service and for Stripe and BTCPay so the browser can be finished first
- [ ] Points at `plans/badges-codes/2026-08-27-badge-codes.md` as the design and this file as the plan

**Verify:** Follow the README from a clean checkout: `npm install && npm run build && npm test && npm run mock` all succeed

**Steps:**

- [ ] **Step 1: write it**, covering build, test, the mock's control surface, and the design pointer.

- [ ] **Step 2: follow it literally** from a clean `node_modules`, and fix anything that does not work as written.

- [ ] **Step 3: commit**

```bash
git add apps/simplex-badge-service/web/README.md
git commit -m "web: readme for build, test and the mock server"
```

---

## Self-review

**Spec coverage.** §4.2 code format → Task 2. §5.1 totals and guards → Task 3. §5.1/§5.2 endpoints → Tasks 6 and 7. §7.1 routing, resume and the wait loop → Tasks 5 and 7. §7.2 the two keys and their bounds → Task 4. §7.3 visual system and the track → Task 8. §7.4 offline → Task 9. §8 screens → Task 8. End-to-end proof → Task 10.

**Deliberately out of scope**, and why: the real Stripe and BTCPay integrations, the Haskell service, the poller, webhook verification, settlement and retention (§6, §9, §10) — all server-side, and the mock stands in for them so the browser flow can be finished and tested first. The Payment Element is stubbed by `clientSecret` in the mock; mounting the real one needs live Stripe keys and belongs with the server work.

**Known gaps carried forward:** the screens in Task 8 are built from the SVG copy by hand, so they need a visual check against `plans/badges-codes/screens/*.svg`; and there is no real-browser test, so the track, Back and the service worker are verified by inspection rather than automation. Both are called out in the acceptance criteria rather than assumed.
