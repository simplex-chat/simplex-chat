import { test } from "node:test";
import { timedTest } from "./boot.js";
import assert from "node:assert/strict";
import { once } from "node:events";
import { spawn, type ChildProcess } from "node:child_process";
import { createServer, connect } from "node:net";
import { fileURLToPath } from "node:url";
import { createInvoice, waitForChange, inferMethod, parseInvoiceView, ApiError, AbortedError } from "../src/api.js";
import { generate, display, normalise, checkChar, hash } from "../src/codes.js";
import { Store } from "../src/store.js";

// Every test here resolves in well under a millisecond or is waiting on an abort, so a bounded loop turned
// unbounded fails fast rather than hanging the suite: a 404-retry mutation once ran to the outer job's 88s
// limit instead of failing.
const apiTest = timedTest(2000);

// The backoff, spent at once, but through a macrotask: an `async () => {}` sleep resolves as a
// microtask, and a loop of those starves the timer above, so a regression that stopped
// `waitForChange` returning would hang this file rather than fail one test in it.
const instantSleep = (): Promise<void> => new Promise((resolve) => { setImmediate(resolve); });

type MockResponse = { status: number; body?: unknown; headers?: Record<string, string> | undefined; badJson?: true } | Error;

function fetchReturning(...responses: MockResponse[]) {
  let i = 0;
  const calls: Array<{ url: string; init: RequestInit | undefined }> = [];
  const consumed: boolean[] = [];
  const fn = async (url: string, init?: RequestInit) => {
    const idx = calls.length;
    calls.push({ url, init });
    consumed.push(false);
    const r = responses[Math.min(i++, responses.length - 1)]!;
    if (r instanceof Error) throw r;
    const headerMap = r.headers ?? {};
    const headers = {
      get: (name: string) => {
        const key = Object.keys(headerMap).find((k) => k.toLowerCase() === name.toLowerCase());
        return key ? headerMap[key]! : null;
      },
    };
    const json = async () => {
      if (r.badJson) throw new SyntaxError("Unexpected end of JSON input");
      return r.body;
    };
    const text = async () => { consumed[idx] = true; return JSON.stringify(r.body ?? {}); };
    return { ok: r.status < 400, status: r.status, json, text, headers } as unknown as Response;
  };
  return { fn: fn as unknown as typeof fetch, calls, consumed };
}

// Full, valid POST /api/invoice 200 bodies: every required field present with a distinct, checkable value,
// so a stubbed or half-built return cannot match one by accident, and shaped consistently with the method
// that would request it, so the method/shape cross-check below does not reject them for the wrong reason.
const fullCreated = {
  invoiceId: "inv_9f3a", badgeType: "legend", months: 12,
  amount: 4200, currency: "usd", expiresAt: "2026-08-28T12:00:00Z",
  address: "48HqK2XmVexampleAddress9fRtWc", cryptoAmount: "1.482", cryptoCurrency: "xmr" as const,
};
const cardCreated = {
  invoiceId: "inv_card1", badgeType: "supporter", months: 1,
  amount: 700, currency: "usd", expiresAt: "2026-08-28T12:00:00Z",
  clientSecret: "cs_test_abc123",
};

apiTest("api: createInvoice sends exactly the four fields, as a POST", async () => {
  const { fn, calls } = fetchReturning({ status: 200, body: fullCreated });
  await createInvoice({ priceId: "p", offerId: "o", method: "xmr", codeHash: "h" }, fn);
  assert.equal(calls[0]!.url, "/api/invoice");
  assert.equal(calls[0]!.init!.method, "POST");
  const parsed = JSON.parse(String(calls[0]!.init!.body));
  assert.deepEqual(parsed, { priceId: "p", offerId: "o", method: "xmr", codeHash: "h" });
  // deepEqual alone can pass for the wrong reason if a stray field happened to
  // match by coincidence; assert the key set directly so an extra field, not
  // just a wrong value, is caught.
  assert.deepEqual(Object.keys(parsed).sort(), ["codeHash", "method", "offerId", "priceId"]);
});

apiTest("api: createInvoice omits offerId entirely when absent, rather than sending it as undefined/null", async () => {
  const { fn, calls } = fetchReturning({ status: 200, body: fullCreated });
  await createInvoice({ priceId: "p", method: "xmr", codeHash: "h" }, fn);
  const parsed = JSON.parse(String(calls[0]!.init!.body));
  assert.deepEqual(Object.keys(parsed).sort(), ["codeHash", "method", "priceId"]);
});

apiTest("api: createInvoice returns the 200 body parsed field by field, not a stub", async () => {
  const { fn } = fetchReturning({ status: 200, body: fullCreated });
  const got = await createInvoice({ priceId: "p", method: "xmr", codeHash: "h" }, fn);
  // Field by field, against distinct values, so a hardcoded stand-in like
  // { invoiceId: "", status: "open" } cannot pass by coincidence.
  assert.deepEqual(got, fullCreated);
});

apiTest("api: createInvoice accepts a card-shaped 200 for a card request", async () => {
  const { fn } = fetchReturning({ status: 200, body: cardCreated });
  const got = await createInvoice({ priceId: "p", method: "card", codeHash: "h" }, fn);
  assert.deepEqual(got, cardCreated);
});

apiTest("api: createInvoice maps error codes to typed errors", async () => {
  for (const [status, code] of [[409, "code_conflict"], [400, "catalog_changed"], [429, "rate_limited"], [503, "provider_unavailable"]] as const) {
    const { fn } = fetchReturning({ status, body: { error: code } });
    await assert.rejects(
      () => createInvoice({ priceId: "p", method: "card", codeHash: "h" }, fn),
      (e: unknown) => e instanceof ApiError && e.code === code,
    );
  }
});

apiTest("api: an unrecognised server error code maps to 'unknown', not passed through verbatim", async () => {
  const { fn } = fetchReturning({ status: 400, body: { error: "a_code_this_client_predates" } });
  await assert.rejects(
    () => createInvoice({ priceId: "p", method: "card", codeHash: "h" }, fn),
    (e: unknown) => e instanceof ApiError && e.code === "unknown",
  );
});

apiTest("api: createInvoice rejects a 200 missing a required field, rather than returning it half-built", async () => {
  const { badgeType: _drop, ...missingBadgeType } = fullCreated;
  const { fn } = fetchReturning({ status: 200, body: missingBadgeType });
  await assert.rejects(
    () => createInvoice({ priceId: "p", method: "xmr", codeHash: "h" }, fn),
    (e: unknown) => e instanceof ApiError && e.code === "invalid_response",
  );
});

apiTest("api: createInvoice rejects a 200 whose required field has the wrong type", async () => {
  const { fn } = fetchReturning({ status: 200, body: { ...fullCreated, months: "12" } });
  await assert.rejects(
    () => createInvoice({ priceId: "p", method: "xmr", codeHash: "h" }, fn),
    (e: unknown) => e instanceof ApiError && e.code === "invalid_response",
  );
});

apiTest("api: createInvoice rejects a 200 whose months or amount is zero, negative, or fractional", async () => {
  for (const bad of [{ months: 0 }, { months: -1 }, { months: 1.5 }, { amount: 0 }, { amount: -100 }, { amount: 12.5 }]) {
    const { fn } = fetchReturning({ status: 200, body: { ...fullCreated, ...bad } });
    await assert.rejects(
      () => createInvoice({ priceId: "p", method: "xmr", codeHash: "h" }, fn),
      (e: unknown) => e instanceof ApiError && e.code === "invalid_response",
      `bad=${JSON.stringify(bad)}`,
    );
  }
});

apiTest("api: createInvoice rejects a 200 with an unrecognised cryptoCurrency", async () => {
  const { fn } = fetchReturning({ status: 200, body: { ...fullCreated, cryptoCurrency: "eth" } });
  await assert.rejects(
    () => createInvoice({ priceId: "p", method: "xmr", codeHash: "h" }, fn),
    (e: unknown) => e instanceof ApiError && e.code === "invalid_response",
  );
});

apiTest("api: a read refuses a field of the wrong type rather than passing it on", () => {
  // These were cast, not checked, so a currency of 42 reached the screen and threw on
  // `toLowerCase`, a blank page from one bad field.
  const wrong = [
    { currency: 42 }, { amount: "42000" }, { amountPaid: "not-a-number" }, { months: "12" },
    { months: 0 }, { amount: Number.NaN }, { expiresAt: {} },
    { paidInFull: "true" }, { cryptoAmountPaid: 1.482 }, { settledAt: 0 },
    { requiredConfirmations: "6" }, { cryptoCurrency: "eth" }, { badgeType: false },
    // minor units and a confirmation count: a fraction or a negative is a wrong answer, and
    // rendering it gives "$-12.50" or "2.5 confirmations on the Monero blockchain"
    { amount: -1250 }, { amount: 42000.5 }, { amountPaid: -1 }, { requiredConfirmations: 2.5 },
    // the create path has always refused a zero amount; the read path reads the same wire field
    { amount: 0 },
  ];
  for (const bad of wrong) {
    assert.equal(parseInvoiceView({ status: "open", ...bad }), null, `must refuse ${JSON.stringify(bad)}`);
  }
  assert.equal(parseInvoiceView([1, 2, 3]), null, "an array is not a body");
  assert.equal(parseInvoiceView("open"), null);
  assert.equal(parseInvoiceView(null), null);
});

apiTest("api: a read keeps every well-typed field and reads null as not sent", () => {
  const full = {
    status: "paid", amountPaid: 42000, cryptoAmountPaid: "1.482", settledAt: "2026-08-28T12:05:00Z",
    badgeType: "legend", months: 12, amount: 42000, currency: "usd", expiresAt: "2026-08-28T13:00:00Z",
    address: "48HqK2Xm", cryptoAmount: "1.482", cryptoCurrency: "xmr",
    paidInFull: true, requiredConfirmations: 6,
  };
  assert.deepEqual(parseInvoiceView(full), full, "nothing well-typed may be dropped on the way in");
  assert.deepEqual(parseInvoiceView({ status: "open", amount: null, currency: null }), { status: "open" },
    "Aeson encodes an absent Maybe as null, which is not a malformed field");
});

apiTest("api: createInvoice rejects a 200 whose body is not valid JSON", async () => {
  const { fn } = fetchReturning({ status: 200, badJson: true });
  await assert.rejects(
    () => createInvoice({ priceId: "p", method: "xmr", codeHash: "h" }, fn),
    (e: unknown) => e instanceof ApiError && e.code === "invalid_response",
  );
});

apiTest("api: createInvoice rejects a 200 whose payment-method shape contradicts the requested method", async () => {
  // Requested card, but the body is crypto-shaped (no clientSecret).
  const cryptoForCard = fetchReturning({ status: 200, body: fullCreated });
  await assert.rejects(
    () => createInvoice({ priceId: "p", method: "card", codeHash: "h" }, cryptoForCard.fn),
    (e: unknown) => e instanceof ApiError && e.code === "invalid_response",
  );
  // Requested btc, but the body is card-shaped (has clientSecret).
  const cardForCrypto = fetchReturning({ status: 200, body: cardCreated });
  await assert.rejects(
    () => createInvoice({ priceId: "p", method: "btc", codeHash: "h" }, cardForCrypto.fn),
    (e: unknown) => e instanceof ApiError && e.code === "invalid_response",
  );
  // Requested btc, body is crypto-shaped but for the wrong crypto.
  const wrongCrypto = fetchReturning({ status: 200, body: fullCreated }); // fullCreated is xmr
  await assert.rejects(
    () => createInvoice({ priceId: "p", method: "btc", codeHash: "h" }, wrongCrypto.fn),
    (e: unknown) => e instanceof ApiError && e.code === "invalid_response",
  );
});

apiTest("api: createInvoice treats a null optional field the same as an absent one (Aeson's default Maybe encoding)", async () => {
  // the create endpoint specifies these fields only as `?`, without saying how absence is
  // encoded; Aeson's default for a `Maybe` field is `null`, not omission.
  // A card-shaped body with the crypto fields explicitly `null` (rather than
  // omitted) must be accepted identically to `cardCreated` itself.
  const { fn } = fetchReturning({ status: 200, body: { ...cardCreated, address: null, cryptoAmount: null, cryptoCurrency: null } });
  const got = await createInvoice({ priceId: "p", method: "card", codeHash: "h" }, fn);
  assert.deepEqual(got, cardCreated);
});

apiTest("api: inferMethod reads the wire's own signal, not a session the second device never had", () => {
  assert.equal(inferMethod({ clientSecret: "cs_test_x" }), "card");
  assert.equal(inferMethod({ cryptoCurrency: "btc" }), "btc");
  assert.equal(inferMethod({ cryptoCurrency: "xmr" }), "xmr");
  assert.equal(inferMethod({}), undefined);
});

apiTest("api: waitForChange passes the current status and resolves on a change", async () => {
  const { fn, calls } = fetchReturning({ status: 200, body: { status: "paid" } });
  const got = await waitForChange("i1", "open", fn, instantSleep);
  assert.ok(calls[0]!.url.includes("wait=open"));
  assert.equal(got.status, "paid");
});

apiTest("api: waitForChange sends whatever status is on screen, not a hardcoded 'open' — resuming after expired", async () => {
  // Every other test in this file happens to wait on "open", so a version
  // hardcoding `?wait=open` would still pass all of them; this is the one
  // fixture that catches it.
  const { fn, calls } = fetchReturning({ status: 200, body: { status: "paid" } });
  const got = await waitForChange("i1", "expired", fn, instantSleep);
  assert.ok(calls[0]!.url.includes("wait=expired"), calls[0]!.url);
  assert.equal(got.status, "paid");
});

apiTest("api: waitForChange returns a payment that left the status alone", async () => {
  // BTCPay reports Processing before it confirms, and that leaves the invoice open. Held
  // for a status change alone, this body was parsed and thrown away every pass, so the
  // page sat on "waiting for the payment" until it was reloaded by hand.
  const body = { status: "open", cryptoAmountPaid: "1.482", amountPaid: 42000 };
  const { fn, calls } = fetchReturning({ status: 200, body });
  const got = await waitForChange("i1", "open", fn, instantSleep, undefined, Date.now, undefined);
  assert.equal(calls.length, 1, "it must not have gone round again");
  assert.equal(got.cryptoAmountPaid, "1.482");
});

apiTest("api: waitForChange keeps waiting while the same payment is reported", async () => {
  // and once the page HAS that payment on screen, the same body is not a change: without
  // this it would spin, re-rendering the same screen on every pass
  const body = { status: "open", cryptoAmountPaid: "1.482", amountPaid: 42000 };
  let served = 0;
  const fn = (async (_url: string) => {
    served += 1;
    const next = served >= 3 ? { status: "paid", cryptoAmountPaid: "1.482" } : body;
    return { ok: true, status: 200, json: async () => next, headers: { get: () => null } };
  }) as unknown as typeof fetch;
  const got = await waitForChange("i1", "open", fn, instantSleep, undefined, Date.now, { paid: "1.482", paidInFull: undefined });
  assert.equal(served, 3);
  assert.equal(got.status, "paid");
});

apiTest("api: the provider's verdict is a change on its own, with no figure to go with it", async () => {
  // Monero reports an invoice as confirming while its figures are still zero. The verdict is
  // the whole difference between the payment screen and the confirming one, so a body that
  // carries only that must come back rather than being polled over forever.
  const fn = (async () => ({
    ok: true, status: 200, headers: { get: () => null },
    json: async () => ({ status: "open", paidInFull: true }),
  })) as unknown as typeof fetch;
  const got = await waitForChange("i1", "open", fn, instantSleep, undefined, Date.now,
    { paid: undefined, paidInFull: undefined });
  assert.equal(got.paidInFull, true, "the confirming screen is unreachable without this");
});

apiTest("api: waitForChange refuses seen: 'paid' outright and issues no request", async () => {
  const { fn, calls } = fetchReturning({ status: 200, body: { status: "paid" } });
  await assert.rejects(() => waitForChange("i1", "paid", fn, instantSleep));
  assert.equal(calls.length, 0);
});

apiTest("api: a network error backs off and retries, and the delays double", async () => {
  const delays: number[] = [];
  const { fn } = fetchReturning(new Error("offline"), new Error("offline"), { status: 200, body: { status: "paid" } });
  const got = await waitForChange("i", "open", fn, async (ms) => { delays.push(ms); });
  // Asserting the exact sequence, not just its length: [1000, 1000] (no
  // doubling) or [2000, 4000] (wrong start) would both satisfy a length-2
  // check but neither is the schedule the watch loop specifies.
  assert.deepEqual(delays, [1000, 2000]);
  assert.equal(got.status, "paid");
});

apiTest("api: backoff reaches the 30s cap and holds there across further failures", async () => {
  const delays: number[] = [];
  // 8 consecutive failures: 1000, 2000, 4000, 8000, 16000, then capped at
  // 30000 for the remaining three, proving the cap is both reached and
  // held, not just that some single delay happens to be <= 30000.
  const { fn } = fetchReturning(
    ...Array.from({ length: 8 }, () => new Error("offline")),
    { status: 200, body: { status: "paid" } },
  );
  const got = await waitForChange("i", "open", fn, async (ms) => { delays.push(ms); });
  assert.deepEqual(delays, [1000, 2000, 4000, 8000, 16000, 30000, 30000, 30000]);
  assert.equal(got.status, "paid");
});

apiTest("api: a truncated 200 body backs off and retries rather than throwing a raw parse error", async () => {
  const delays: number[] = [];
  const { fn, calls } = fetchReturning(
    { status: 200, badJson: true },
    { status: 200, body: { status: "paid" } },
  );
  const got = await waitForChange("i", "open", fn, async (ms) => { delays.push(ms); });
  assert.deepEqual(delays, [1000]);
  assert.equal(calls.length, 2);
  assert.equal(got.status, "paid");
});

apiTest("api: a GET 200 with a null body backs off and retries rather than throwing a raw TypeError", async () => {
  const delays: number[] = [];
  const { fn, calls } = fetchReturning(
    { status: 200, body: null },
    { status: 200, body: { status: "paid" } },
  );
  const got = await waitForChange("i", "open", fn, async (ms) => { delays.push(ms); });
  assert.deepEqual(delays, [1000]);
  assert.equal(calls.length, 2);
  assert.equal(got.status, "paid");
});

apiTest("api: a GET 200 with an empty object backs off and retries, never reporting a spurious status change", async () => {
  const delays: number[] = [];
  const { fn, calls } = fetchReturning(
    { status: 200, body: {} },
    { status: 200, body: { status: "paid" } },
  );
  const got = await waitForChange("i", "open", fn, async (ms) => { delays.push(ms); });
  assert.deepEqual(delays, [1000]);
  assert.equal(calls.length, 2);
  assert.equal(got.status, "paid");
});

apiTest("api: a GET 200 with an unrecognised cryptoCurrency backs off and retries", async () => {
  const delays: number[] = [];
  const { fn, calls } = fetchReturning(
    { status: 200, body: { status: "open", cryptoCurrency: "eth" } },
    { status: 200, body: { status: "paid" } },
  );
  const got = await waitForChange("i", "open", fn, async (ms) => { delays.push(ms); });
  assert.deepEqual(delays, [1000]);
  assert.equal(calls.length, 2);
  assert.equal(got.status, "paid");
});

apiTest("api: a same-status 200 (the hold timing out) reissues immediately, with no sleep at all", async () => {
  const { fn, calls } = fetchReturning(
    { status: 200, body: { status: "open" } },  // the hold timed out, nothing changed
    { status: 200, body: { status: "paid" } },
  );
  let sleepCalls = 0;
  const got = await waitForChange("i", "open", fn, async () => { sleepCalls++; });
  // Not "resolved eventually": the reissue must be immediate. A version that
  // slept between passes would still resolve, so the sleep call count is the
  // only thing that distinguishes the two.
  assert.equal(sleepCalls, 0);
  assert.equal(calls.length, 2);
  assert.equal(got.status, "paid");
});

apiTest("api: waitForChange resolves, rather than throws, when the status becomes expired", async () => {
  const { fn } = fetchReturning({ status: 200, body: { status: "expired" } });
  const got = await waitForChange("i", "open", fn, instantSleep);
  assert.equal(got.status, "expired");
});

apiTest("api: EVERY 404 stops the loop, whatever body a proxy or a CDN put on it", async () => {
  // the unknown-order screen defines the unknown-order screen by the status, and the guarantee
  // that the body carries `{"error":"not_found"}` binds this service, not whatever sits in front
  // of it. Keying on the body left the payment screen up with a live address, a Copy button and a
  // dead loop.
  const shapes: Array<[string, MockResponse]> = [
    ["the service's own body", { status: 404, body: { error: "not_found" } }],
    ["a proxy's HTML page", { status: 404, badJson: true }],
    ["an empty body", { status: 404 }],
    ["unrelated JSON", { status: 404, body: { message: "no route matched" } }],
    ["a body naming another code", { status: 404, body: { error: "internal" } }],
  ];
  for (const [what, reply] of shapes) {
    const { fn, calls } = fetchReturning(reply);
    await assert.rejects(
      () => waitForChange("gone", "open", fn, instantSleep),
      (e: unknown) => e instanceof ApiError && e.code === "not_found" && e.status === 404, what,
    );
    assert.equal(calls.length, 1, `${what}: a 404 is terminal, not a failure to retry`);
  }
});

apiTest("api: this client's own error words are never adopted FROM the wire", async () => {
  // `invalid_response` and `unknown` describe the browser refusing a body. A
  // service naming one would be claiming the code a refused body already gets.
  for (const named of ["invalid_response", "unknown", "teapot"]) {
    const { fn } = fetchReturning({ status: 400, body: { error: named } });
    await assert.rejects(
      () => createInvoice({ priceId: "p", method: "xmr", codeHash: "h" }, fn),
      (e: unknown) => e instanceof ApiError && e.code === "unknown" && e.status === 400, named,
    );
  }
  // And a code the create endpoint does define still comes through.
  const { fn } = fetchReturning({ status: 400, body: { error: "bad_request" } });
  await assert.rejects(
    () => createInvoice({ priceId: "p", method: "xmr", codeHash: "h" }, fn),
    (e: unknown) => e instanceof ApiError && e.code === "bad_request",
  );
});

apiTest("api: a non-404 error response's body is drained rather than left unconsumed", async () => {
  const { fn, consumed } = fetchReturning(
    { status: 500, body: { error: "internal" } },
    { status: 200, body: { status: "paid" } },
  );
  await waitForChange("i", "open", fn, instantSleep);
  assert.equal(consumed[0], true);
});

apiTest("api: a 429 waits out the exact interval named by Retry-After, not the 1s backoff start", async () => {
  const delays: number[] = [];
  const { fn } = fetchReturning(
    { status: 429, body: { error: "rate_limited" }, headers: { "retry-after": "5" } },
    { status: 200, body: { status: "paid" } },
  );
  const got = await waitForChange("i", "open", fn, async (ms) => { delays.push(ms); });
  // Exactly [5000]: a version that fell back to the network-error backoff
  // ladder would produce [1000] here, which is a plausible-looking delay
  // but not the interval the (fake) server actually asked for.
  assert.deepEqual(delays, [5000]);
  assert.equal(got.status, "paid");
});

apiTest("api: a 429's wait does not consume or advance the backoff ladder", async () => {
  const delays: number[] = [];
  const { fn } = fetchReturning(
    { status: 429, body: { error: "rate_limited" }, headers: { "retry-after": "5" } },
    new Error("offline"),
    { status: 200, body: { status: "paid" } },
  );
  const got = await waitForChange("i", "open", fn, async (ms) => { delays.push(ms); });
  // The network error after the 429 must back off from the ladder's start
  // (1000), not from 2000 (ladder advanced by the 429) or 5000 (ladder
  // seeded from the Retry-After value).
  assert.deepEqual(delays, [5000, 1000]);
  assert.equal(got.status, "paid");
});

apiTest("api: a 429 with a missing or unusable Retry-After falls back to the network-error backoff", async () => {
  const cases: Array<Record<string, string> | undefined> = [
    undefined, {}, { "retry-after": "" }, { "retry-after": "soon" }, { "retry-after": "0" }, { "retry-after": "-5" },
  ];
  for (const headers of cases) {
    const delays: number[] = [];
    const { fn } = fetchReturning(
      { status: 429, body: { error: "rate_limited" }, headers },
      { status: 200, body: { status: "paid" } },
    );
    const got = await waitForChange("i", "open", fn, async (ms) => { delays.push(ms); });
    assert.deepEqual(delays, [1000], `headers=${JSON.stringify(headers)}`);
    assert.equal(got.status, "paid");
  }
});

apiTest("api: Retry-After is clamped to 300s in the waiting loop, not honoured verbatim", async () => {
  const delays: number[] = [];
  const { fn } = fetchReturning(
    { status: 429, body: { error: "rate_limited" }, headers: { "retry-after": "86400" } },
    { status: 200, body: { status: "paid" } },
  );
  const got = await waitForChange("i", "open", fn, async (ms) => { delays.push(ms); });
  assert.deepEqual(delays, [300_000]);
  assert.equal(got.status, "paid");
});

apiTest("api: createInvoice's ApiError carries retryAfter from a 429's Retry-After, clamped to 300s, and omits it when absent", async () => {
  const withHeader = fetchReturning({ status: 429, body: { error: "rate_limited" }, headers: { "retry-after": "60" } });
  await assert.rejects(
    () => createInvoice({ priceId: "p", method: "card", codeHash: "h" }, withHeader.fn),
    (e: unknown) => e instanceof ApiError && e.code === "rate_limited" && e.retryAfter === 60,
  );

  const excessive = fetchReturning({ status: 429, body: { error: "rate_limited" }, headers: { "retry-after": "86400" } });
  await assert.rejects(
    () => createInvoice({ priceId: "p", method: "card", codeHash: "h" }, excessive.fn),
    (e: unknown) => e instanceof ApiError && e.retryAfter === 300,
  );

  const withoutHeader = fetchReturning({ status: 429, body: { error: "rate_limited" } });
  await assert.rejects(
    () => createInvoice({ priceId: "p", method: "card", codeHash: "h" }, withoutHeader.fn),
    (e: unknown) => e instanceof ApiError && e.code === "rate_limited" && e.retryAfter === undefined,
  );
});

apiTest("api: three consecutive fast (<5s) same-status answers trip the anti-spin floor", async () => {
  const delays: number[] = [];
  const { fn, calls } = fetchReturning(
    { status: 200, body: { status: "open" } },
    { status: 200, body: { status: "open" } },
    { status: 200, body: { status: "open" } },
    { status: 200, body: { status: "paid" } },
  );
  const now = () => 0; // every response looks instantaneous
  const got = await waitForChange("i", "open", fn, async (ms) => { delays.push(ms); }, undefined, now);
  assert.deepEqual(delays, [1000]);
  assert.equal(calls.length, 4);
  assert.equal(got.status, "paid");
});

apiTest("api: a 1.1s hold-ignoring proxy still trips the anti-spin floor (the threshold is 5s, not 1s)", async () => {
  // Below the old 1s threshold this would never have been counted as
  // "fast", yet answering in 1.1s forever is still ~54 requests/minute,
  // just under the 60/min limit, spinning unthrottled.
  const delays: number[] = [];
  const { fn, calls } = fetchReturning(
    { status: 200, body: { status: "open" } },
    { status: 200, body: { status: "open" } },
    { status: 200, body: { status: "open" } },
    { status: 200, body: { status: "paid" } },
  );
  let t = 0;
  const now = () => { t += 1100; return t; };
  const got = await waitForChange("i", "open", fn, async (ms) => { delays.push(ms); }, undefined, now);
  assert.deepEqual(delays, [1000]);
  assert.equal(calls.length, 4);
  assert.equal(got.status, "paid");
});

apiTest("api: a genuine ~30s same-status hold never trips the anti-spin floor", async () => {
  const delays: number[] = [];
  const { fn, calls } = fetchReturning(
    { status: 200, body: { status: "open" } },
    { status: 200, body: { status: "open" } },
    { status: 200, body: { status: "open" } },
    { status: 200, body: { status: "open" } },
    { status: 200, body: { status: "open" } },
    { status: 200, body: { status: "paid" } },
  );
  let t = 0;
  const now = () => { t += 30_000; return t; }; // each hold genuinely takes ~30s
  const got = await waitForChange("i", "open", fn, async (ms) => { delays.push(ms); }, undefined, now);
  // Five consecutive same-status answers, none of them fast: never once
  // backed off. A version that ignored elapsed time and counted every
  // same-status answer as "fast" would trip after the third.
  assert.deepEqual(delays, []);
  assert.equal(calls.length, 6);
  assert.equal(got.status, "paid");
});

apiTest("api: waitForChange rejects at once with AbortedError when the signal is already aborted, issuing no fetch", async () => {
  const controller = new AbortController();
  controller.abort();
  const { fn, calls } = fetchReturning({ status: 200, body: { status: "paid" } });
  await assert.rejects(
    () => waitForChange("i", "open", fn, instantSleep, controller.signal),
    (e: unknown) => e instanceof AbortedError,
  );
  assert.equal(calls.length, 0);
});

apiTest("api: an abort mid-flight rejects with AbortedError and issues no further fetch", async () => {
  const controller = new AbortController();
  let callCount = 0;
  const fn = ((_url: string, init?: RequestInit) => new Promise((_resolve, reject) => {
    callCount++;
    init?.signal?.addEventListener("abort", () => reject(Object.assign(new Error("aborted"), { name: "AbortError" })));
  })) as unknown as typeof fetch;
  const promise = waitForChange("i", "open", fn, instantSleep, controller.signal);
  controller.abort();
  await assert.rejects(() => promise, (e: unknown) => e instanceof AbortedError);
  assert.equal(callCount, 1);
});

apiTest("api: an abort mid-sleep clears its pending timer — setTimeout/clearTimeout counts balance", async () => {
  const realSetTimeout = globalThis.setTimeout;
  const realClearTimeout = globalThis.clearTimeout;
  let setCount = 0;
  let clearCount = 0;
  globalThis.setTimeout = ((...args: Parameters<typeof setTimeout>) => {
    setCount++;
    return realSetTimeout(...args);
  }) as typeof setTimeout;
  globalThis.clearTimeout = ((...args: Parameters<typeof clearTimeout>) => {
    clearCount++;
    return realClearTimeout(...args);
  }) as typeof clearTimeout;
  try {
    const controller = new AbortController();
    const { fn, calls } = fetchReturning(new Error("offline"));
    // No injected sleep: exercise the real default, since a test double is
    // the one thing that never owns a real `setTimeout` handle to leak.
    const promise = waitForChange("i", "open", fn, undefined, controller.signal);
    await new Promise((r) => setImmediate(r)); // let the rejected fetch reach the pending sleep
    controller.abort();
    await assert.rejects(() => promise, (e: unknown) => e instanceof AbortedError);
    assert.equal(calls.length, 1);
    // Not just "it rejected": a version that raced the abort without ever
    // clearing the real timer would still reject here, leaving a handle
    // alive in the background. The counts must balance: one timer started,
    // the same one cleared.
    assert.equal(setCount, 1);
    assert.equal(clearCount, 1);
  } finally {
    globalThis.setTimeout = realSetTimeout;
    globalThis.clearTimeout = realClearTimeout;
  }
});

// ===========================================================================

const SERVER = fileURLToPath(new URL("../../mock/server.py", import.meta.url));

/** A spawn, a readiness poll and three round trips; a spin regression must still fail rather than hang. */
const E2E_TIMEOUT_MS = 30_000;
/** The server-side hold: long enough that the wait below is unambiguously parked on it when settlement
 * arrives, short enough that a failed test releases the socket well inside `E2E_TIMEOUT_MS`. */
const HOLD_SECONDS = "5";
const READY_TIMEOUT_MS = 10_000;
const READY_POLL_MS = 25;
/** How long the wait is left parked before settlement, to show it really is pending. */
const PRE_SETTLE_MS = 150;
/** From `POST /control/settle` to `waitForChange` resolving. A hold that is not real cannot come in under
 * this: three sub-5s same-status answers trip the anti-spin floor in `waitForChange`, which then sleeps a
 * full `BACKOFF_START` (1000 ms) before it would see the change. */
const WAKE_LIMIT_MS = 500;

function e2eTest(name: string, fn: () => Promise<void>): void {
  test(name, { timeout: E2E_TIMEOUT_MS }, fn);
}

const delay = (ms: number) => new Promise((r) => setTimeout(r, ms));

/** localStorage, in memory. Local to this file, as in `store.test.ts` and `routing.test.ts`. */
class MemoryStorage {
  map = new Map<string, string>();
  getItem(k: string) { return this.map.get(k) ?? null; }
  setItem(k: string, v: string) { this.map.set(k, v); }
  removeItem(k: string) { this.map.delete(k); }
}

/** A port the kernel has just told us is free, rather than a constant: a fixed one collides with a stray
 * server left by an earlier run and with any concurrent one, and the failure mode is a test that passes
 * green against the wrong server. */
function freePort(): Promise<number> {
  return new Promise((resolve, reject) => {
    const probe = createServer();
    probe.once("error", reject);
    probe.listen(0, "127.0.0.1", () => {
      const address = probe.address();
      if (address === null || typeof address === "string") {
        probe.close();
        reject(new Error("freePort: the probe socket reported no port"));
        return;
      }
      const { port } = address;
      probe.close(() => resolve(port));
    });
  });
}

/** Polls rather than sleeping a guessed interval. The probe is `GET /api/invoice/<unknown>`, and only this
 * mock's own 404 body counts as ready, so a connection accepted by something else on the port keeps the
 * loop going until the deadline instead of being mistaken for ours. */
async function ready(base: string, proc: ChildProcess): Promise<void> {
  const deadline = Date.now() + READY_TIMEOUT_MS;
  let last = "no attempt completed";
  for (;;) {
    if (proc.exitCode !== null || proc.signalCode !== null) {
      throw new Error(`mock server exited before answering (code ${proc.exitCode}, signal ${proc.signalCode})`);
    }
    try {
      const res = await fetch(`${base}/api/invoice/no-such-invoice`);
      const body = (await res.json()) as { error?: string };
      if (res.status === 404 && body.error === "not_found") return;
      last = `answered ${res.status} ${JSON.stringify(body)}`;
    } catch (e) {
      last = String(e);
    }
    if (Date.now() >= deadline) throw new Error(`mock server never became ready on ${base}: ${last}`);
    await delay(READY_POLL_MS);
  }
}

/**
 * `createInvoice` and `waitForChange` both call `fetch` with a root-relative
 * path, exactly as the browser would; this only supplies the origin.
 */
function viaNetwork(base: string): typeof fetch {
  return ((input: Parameters<typeof fetch>[0], init?: Parameters<typeof fetch>[1]) => {
    const url = typeof input === "string" && input.startsWith("/") ? `${base}${input}` : input;
    // no keep-alive: the mock is SIGKILLed at the end of each server, and a pooled socket to a
    // process that is gone keeps this file's event loop open long after its last assertion
    const headers = { ...(init?.headers as Record<string, string> | undefined), connection: "close" };
    return fetch(url, { ...init, headers });
  }) as typeof fetch;
}

/** Keeps the raw bytes of every response, before any parsing. `parseCreatedInvoice` copies out named fields
 * and drops the rest, so stringifying its return proves nothing about what the wire carried: a `code` field
 * in the response would vanish into the parser and the assertion would pass. */
function recording(f: typeof fetch): { f: typeof fetch; bodies: string[] } {
  const bodies: string[] = [];
  const wrapped = (async (input: Parameters<typeof fetch>[0], init?: Parameters<typeof fetch>[1]) => {
    const res = await f(input, init);
    bodies.push(await res.clone().text());
    return res;
  }) as typeof fetch;
  return { f: wrapped, bodies };
}

/** The control surface: what a payment provider's webhook would have told the service. */
async function control(f: typeof fetch, action: string, invoiceId: string): Promise<{ status: number; body: unknown }> {
  const res = await f(`/control/${action}/${encodeURIComponent(invoiceId)}`, { method: "POST" });
  return { status: res.status, body: await res.json() };
}

async function withServer(fn: (f: typeof fetch, bodies: string[]) => Promise<void>): Promise<void> {
  const port = await freePort();
  const base = `http://127.0.0.1:${port}`;
  const proc = spawn("python3", [SERVER, "--port", String(port)], {
    stdio: "ignore",
    env: { ...process.env, MOCK_HOLD_SECONDS: process.env.MOCK_HOLD_SECONDS ?? HOLD_SECONDS },
  });
  try {
    await ready(base, proc);
    const { f, bodies } = recording(viaNetwork(base));
    await fn(f, bodies);
  } finally {
    // SIGKILL, not SIGTERM: a thread parked in `event.wait(HOLD_SECONDS)` would
    // otherwise keep the process alive past the test, and the port with it.
    proc.kill("SIGKILL");
    if (proc.exitCode === null && proc.signalCode === null) await once(proc, "exit");
  }
}

/**
 * A buyer who reloads, navigates away or closes the tab during a `?wait=` hold
 * drops the socket while the server thread is parked. Settlement then wakes it
 */
e2eTest("e2e: a client that hangs up mid-hold is not an error", async () => {
  const port = await freePort();
  const base = `http://127.0.0.1:${port}`;
  const proc = spawn("python3", [SERVER, "--port", String(port)], {
    stdio: ["ignore", "ignore", "pipe"],
    env: { ...process.env, MOCK_HOLD_SECONDS: process.env.MOCK_HOLD_SECONDS ?? HOLD_SECONDS },
  });
  let stderr = "";
  proc.stderr?.on("data", (chunk) => { stderr += String(chunk); });
  try {
    await ready(base, proc);
    const f = viaNetwork(base);
    const created = await createInvoice(
      { priceId: "price_supporter", method: "btc", codeHash: await hash(generate()) }, f);

    // Park a hold on a raw socket, then vanish without reading the answer.
    const sock = connect(created.invoiceId ? port : port, "127.0.0.1");
    await once(sock, "connect");
    sock.write(`GET /api/invoice/${created.invoiceId}?wait=open HTTP/1.1\r\nHost: x\r\n\r\n`);
    await delay(PRE_SETTLE_MS);
    sock.destroy();
    await delay(100);

    // Settlement wakes the parked thread, which now writes to nothing.
    assert.equal((await control(f, "settle", created.invoiceId)).status, 200);
    await delay(400);

    // The server is still serving, and said nothing about it.
    const after = await fetch(`${base}/api/invoice/${created.invoiceId}`);
    assert.equal(after.status, 200);
    assert.equal(((await after.json()) as { status: string }).status, "paid");
    assert.ok(!/Traceback|BrokenPipeError|ConnectionResetError/.test(stderr),
      `a dropped client was reported as a server error:\n${stderr.slice(0, 600)}`);
  } finally {
    proc.kill("SIGKILL");
    if (proc.exitCode === null && proc.signalCode === null) await once(proc, "exit");
  }
});

e2eTest("e2e: a purchase runs end to end, and settlement wakes the wait", async () => {
  await withServer(async (f, bodies) => {
    const store = new Store(new MemoryStorage());
    const code = generate();
    const codeHash = await hash(code);

    // The record needs the invoice id, so the code is saved once the answer is in.
    const created = await createInvoice(
      { priceId: "price_legend", offerId: "offer_12m", method: "xmr", codeHash }, f);

    // 7000/month, twelve months, half off: the amount is derived server-side.
    assert.equal(created.amount, 42_000);
    assert.equal(created.months, 12);
    assert.equal(created.badgeType, "legend");
    assert.equal(created.currency, "usd");
    assert.equal(created.cryptoCurrency, "xmr");
    assert.equal(created.cryptoAmount, "1.482");
    assert.equal(created.clientSecret, undefined, "an xmr invoice carries no card secret");
    assert.ok(created.invoiceId.length > 0);

    store.saveOrder({
      orderId: created.invoiceId, badgeType: created.badgeType,
      months: created.months, createdAt: new Date().toISOString(), status: "open", code: display(code),
    });

    // Park on the hold, prove it really is parked, then settle and time the wake.
    const waiting = waitForChange(created.invoiceId, "open", f);
    let woken = false;
    waiting.then(() => { woken = true; }, () => { woken = true; });
    await delay(PRE_SETTLE_MS);
    assert.equal(woken, false,
      `the wait must still be pending while the invoice is open (it returned inside ${PRE_SETTLE_MS}ms)`);

    const settleAt = Date.now();
    const settle = await control(f, "settle", created.invoiceId);
    assert.equal(settle.status, 200);
    const settled = await waiting;
    const wakeMs = Date.now() - settleAt;

    assert.equal(settled.status, "paid");
    assert.equal(settled.amountPaid, 42_000);
    assert.equal(settled.cryptoAmountPaid, "1.482");
    assert.ok(wakeMs < WAKE_LIMIT_MS,
      `settlement must wake the hold rather than be found by the next poll (took ${wakeMs}ms)`);

    store.saveOrder({ ...store.order(created.invoiceId)!, status: "paid" });
    const orders = store.orders();
    assert.equal(orders.length, 1);
    const only = orders[0]!;
    assert.equal(only.orderId, created.invoiceId);
    assert.equal(only.status, "paid");
    assert.equal(only.code, display(code), "settlement must not clear the stored code");

    // The code that comes back out is the one that went in, character for
    // character, and still carries a good check character.
    const recovered = normalise(only.code!);
    assert.equal(recovered, code, "the stored code round-trips byte-identical");
    assert.equal(checkChar(recovered!.slice(0, -1)), recovered!.slice(-1), "and still validates");

    // Every byte of every response, not the parsed shapes: the service never
    // has the code, and must never echo the hash it was keyed by.
    assert.ok(bodies.length >= 3, `expected the whole exchange to be recorded, saw ${bodies.length} responses`);
    for (const body of bodies) {
      assert.ok(!body.includes(code), `a response carried the code: ${body}`);
      assert.ok(!body.includes(display(code)), `a response carried the displayed code: ${body}`);
      assert.ok(!body.includes(codeHash), `a response carried the code hash: ${body}`);
    }
  });
});

e2eTest("e2e: a repeated code hash is refused with code_conflict, not merely refused", async () => {
  await withServer(async (f) => {
    const codeHash = await hash(generate());
    const req = { priceId: "price_legend", method: "card", codeHash } as const;
    const first = await createInvoice(req, f);
    assert.ok(first.clientSecret!.startsWith("cs_test_"), "the first one is created normally");
    await assert.rejects(
      () => createInvoice(req, f),
      (e: unknown) => {
        // Asserted rather than returned as a boolean: "some error was thrown"
        // is not the claim. The code and the status are, and a mismatch has to
        // say which one it was.
        assert.ok(e instanceof ApiError, `expected an ApiError, got ${String(e)}`);
        assert.equal(e.code, "code_conflict");
        assert.equal(e.status, 409);
        return true;
      });
  });
});

e2eTest("e2e: an expired invoice reports what arrived before it closed", async () => {
  await withServer(async (f) => {
    const inv = await createInvoice(
      { priceId: "price_supporter", method: "xmr", codeHash: await hash(generate()) }, f);
    assert.equal(inv.amount, 700);
    assert.equal((await control(f, "partial", inv.invoiceId)).status, 200);
    assert.equal((await control(f, "expire", inv.invoiceId)).status, 200);

    // The status already differs from `open`, so this returns without holding.
    const view = await waitForChange(inv.invoiceId, "open", f);
    assert.equal(view.status, "expired");
    assert.equal(view.amountPaid, 350, "half of 700 arrived");
    assert.equal(view.cryptoAmountPaid, "0.734");
    assert.equal(view.settledAt, undefined, "nothing settled");
  });
});
