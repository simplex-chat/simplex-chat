// Unit tests for the pure routing functions, run against the built modules
// with `npm test` (node's built-in runner — no dependency, decision 7).
//
// These run against dist/, not src/, so they test what a browser is served.
// Run `npm run build` first; `npm test` does not compile.

import test from "node:test"
import assert from "node:assert/strict"

import {FIRST_SCREEN, SCREEN_IDS, hashForScreen, nextScreen, screenIdForHash} from "../dist/router.js"

test("every screen in the plan has a route", () => {
  assert.deepEqual([...SCREEN_IDS], ["tier", "months", "pay", "checkout", "order", "code"])
})

test("screenIdForHash resolves the canonical hash of every screen", () => {
  for (const id of SCREEN_IDS) {
    assert.equal(screenIdForHash(hashForScreen(id)), id, `#/${id} must route to ${id}`)
  }
})

test("hashForScreen is the inverse of screenIdForHash", () => {
  for (const id of SCREEN_IDS) assert.equal(hashForScreen(id), `#/${id}`)
})

test("a hand-edited hash is tolerated: no slash, or a trailing one", () => {
  assert.equal(screenIdForHash("#tier"), "tier")
  assert.equal(screenIdForHash("#/tier/"), "tier")
  assert.equal(screenIdForHash("#checkout/"), "checkout")
})

test("an absent or empty hash names no screen; where to start is not its business", () => {
  // Deliberately null and not FIRST_SCREEN: with D5's prefill seeded, the
  // first visit does not necessarily start at the first screen, and that
  // decision belongs to the caller.
  assert.equal(screenIdForHash(""), null)
  assert.equal(screenIdForHash("#"), null)
  assert.equal(screenIdForHash("#/"), null)
  assert.equal(FIRST_SCREEN, "tier")
})

test("an unknown hash is null, not a screen and not a throw", () => {
  for (const hash of ["#/nonsense", "#/TIER", "#/tier/extra", "#//tier", "#/order?id=1"]) {
    assert.equal(screenIdForHash(hash), null, `${hash} must not resolve to a screen`)
  }
})

test("nextScreen walks the wizard and stops at checkout", () => {
  assert.equal(nextScreen("tier"), "months")
  assert.equal(nextScreen("months"), "pay")
  assert.equal(nextScreen("pay"), "checkout")
  assert.equal(nextScreen("checkout"), null)
})

test("the two screens reached from a checkout response are not advanced into", () => {
  assert.equal(nextScreen("order"), null)
  assert.equal(nextScreen("code"), null)
})
