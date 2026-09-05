import { timedTest } from "./boot.js";
import assert from "node:assert/strict";
import { ALPHABET, PAYLOAD, canonical, checkChar, display, normalise, generate, hash } from "../src/codes.js";

// The two exhaustive sweeps below take a few hundred milliseconds each; every
// other test here is instant. The timeout is what makes a regression that turns
// something bounded into something unbounded FAIL rather than hang.
const codeTest = timedTest(5000);

// From src/Simplex/Chat/Badges/Code.hs: parseBadgeCode reads this code, its canonical form is the string below,
// and badgeCodeHash gives that digest. Disagreeing with any of the three sells codes the service cannot redeem,
// as shipped before with a 31-character alphabet, a mod-31 check character and a hash missing the prefix.
const VECTOR_BODY = "4RT6E8YBMW74Q8DK9DKR";
const VECTOR = "SXB-4RT6E-8YBMW-74Q8D-K9DKR";
const VECTOR_CANONICAL = "SXB4RT6E8YBMW74Q8DK9DKR";
const VECTOR_HASH = "3d_WN-5f2kzgJl49HWgHiLYBDgedGOBVkFD1UsHSE8Y";

codeTest("codes: the vector agrees with parseBadgeCode's canonical form and hash", async () => {
  assert.equal(checkChar(VECTOR_BODY.slice(0, 19)), VECTOR_BODY[19]);
  assert.equal(display(VECTOR_BODY), VECTOR);
  assert.equal(canonical(VECTOR_BODY), VECTOR_CANONICAL);
  assert.equal(await hash(VECTOR_BODY), VECTOR_HASH);
});

codeTest("codes: alphabet is Crockford base32", () => {
  assert.equal(ALPHABET, "0123456789ABCDEFGHJKMNPQRSTVWXYZ");
  assert.equal(ALPHABET.length, 32);
  // the four Crockford omits, U included, which the old 31-character alphabet allowed and
  // the service has never been able to read
  for (const bad of "ILOU") assert.ok(!ALPHABET.includes(bad), `${bad} must not be in the alphabet`);
});

function randomBody(): string {
  let s = "";
  for (let i = 0; i < 19; i++) s += ALPHABET[Math.floor(Math.random() * ALPHABET.length)]!;
  return s;
}

codeTest("codes: every single-character substitution is detected", () => {
  let undetected = 0;
  for (let n = 0; n < 1000; n++) {
    const body = randomBody();
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

// Luhn mod N detects ADJACENT transpositions, not arbitrary ones - that is weaker than the
// mod-31 weighted sum the checkout used to carry, and it is the price of agreeing with
// Code.hs. Its documented blind spot is swapping the values 0 and N-1, so "0" beside "Z".
codeTest("codes: every adjacent transposition is detected but Luhn's 0/Z blind spot", () => {
  let undetected = 0;
  let blindSpot = 0;
  for (let n = 0; n < 2000; n++) {
    const body = randomBody();
    const code = body + checkChar(body);
    for (let i = 0; i < 18; i++) {
      const j = i + 1;
      if (code[i] === code[j]) continue;
      const a = code.split("");
      [a[i], a[j]] = [a[j]!, a[i]!];
      const g = a.join("");
      if (checkChar(g.slice(0, 19)) !== g[19]) continue;
      if ([code[i], code[j]].sort().join("") === "0Z") blindSpot++;
      else undetected++;
    }
  }
  assert.equal(undetected, 0);
  assert.ok(blindSpot > 0, "the 0/Z pair should have turned up in two thousand codes");
});

codeTest("codes: normalise folds I, L and O, and requires the prefix", () => {
  assert.equal(normalise("sxb-4rt6e-8ybmw-74q8d-k9dkr"), VECTOR_BODY);
  assert.equal(normalise(" SXB 4RT6E 8YBMW 74Q8D K9DKR "), VECTOR_BODY);
  // I and L read as 1, O as 0, so a code copied by hand still verifies
  const folded = normalise(display("1".repeat(19) + checkChar("1".repeat(19))).replace(/1/g, "I"));
  assert.equal(folded, "1".repeat(19) + checkChar("1".repeat(19)));
  // U is not in the alphabet and folds onto nothing
  assert.equal(normalise("SXB-UUUUU-UUUUU-UUUUU-UUUUU"), null);
  assert.equal(normalise("SXB-TOOSHORT"), null);
  // parseBadgeCode strips the prefix and fails without it, so this must too
  assert.equal(normalise("4RT6E8YBMW74Q8DK9DKR"), null);
  // a wrong check character is not a code
  assert.equal(normalise(display(VECTOR_BODY.slice(0, 19) + (VECTOR_BODY[19] === "0" ? "1" : "0"))), null);
});

codeTest("codes: generate produces a valid code", () => {
  for (let n = 0; n < 200; n++) {
    const c = generate();
    assert.equal(c.length, 20);
    for (const ch of c) assert.ok(ALPHABET.includes(ch));
    assert.equal(checkChar(c.slice(0, 19)), c[19]);
    assert.equal(normalise(display(c)), c);
  }
});

codeTest("codes: hash is base64url sha-256 over the canonical form, prefix included", async () => {
  const h = await hash(VECTOR_BODY);
  assert.match(h, /^[A-Za-z0-9_-]{43}$/);
  assert.equal(h, await hash(normalise(VECTOR)!));
  // the prefix is part of what is hashed: dropping it was the third divergence
  const bytes = new TextEncoder().encode(VECTOR_BODY);
  const bare = await crypto.subtle.digest("SHA-256", bytes);
  const bareB64 = btoa(String.fromCharCode(...new Uint8Array(bare)))
    .replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
  assert.notEqual(h, bareB64);
});

codeTest("codes: every code drawn is a different one, and the draw covers the alphabet", () => {
  // Nothing else pins the randomness. With a constant generator the first sale succeeds and
  // every later checkout retries code_conflict five times and fails, with the suite green.
  const drawn = new Set<string>();
  const symbols = new Set<string>();
  // per position, because the check character alone ranges over all 32 values: measuring the code
  // as a whole would be satisfied by a payload drawn from a fraction of the alphabet. This bounds
  // the alphabet each position draws from, and nothing more. The keyspace rests on the source of
  // the bytes, which the test below pins.
  const perPosition = Array.from({ length: PAYLOAD }, () => new Set<string>());
  for (let i = 0; i < 5000; i++) {
    const code = generate();
    drawn.add(code);
    for (const c of code) symbols.add(c);
    for (let at = 0; at < PAYLOAD; at++) perPosition[at]!.add(code[at]!);
  }
  assert.equal(drawn.size, 5000, "two buyers must never be handed the same code");
  assert.equal(symbols.size, ALPHABET.length, `the draw reached ${symbols.size} of ${ALPHABET.length} symbols`);
  for (const [at, seen] of perPosition.entries()) {
    assert.equal(seen.size, ALPHABET.length,
      `payload position ${at} drew ${seen.size} of ${ALPHABET.length} symbols, so the alphabet is narrowed`);
  }
});

codeTest("codes: stripping is Unicode, the way parseBadgeCode's isAlphaNum is", () => {
  // An ASCII-only strip would drop an Arabic-Indic digit and read the rest as a valid code that
  // the service, filtering with `isAlphaNum`, would refuse.
  assert.equal(normalise("SXB\u0663-4RT6E-8YBMW-74Q8D-K9DKR"), null);
  assert.equal(normalise("SXB-4RT6E-8YBMW-74Q8D-K9DKR"), VECTOR_BODY, "and the separators still go");
});

codeTest("codes: the payload comes from the CSPRNG, one byte per character", () => {
  // Per-position coverage cannot see this: `Math.random()` is uniform per position too, and its
  // 128-bit state is recoverable from a handful of outputs, so one buyer's code would predict the
  // next. What the keyspace rests on is where the bytes come from and how many are drawn.
  const real = globalThis.crypto.getRandomValues.bind(globalThis.crypto);
  const asked: number[] = [];
  globalThis.crypto.getRandomValues = ((buf: ArrayBufferView) => {
    asked.push(buf.byteLength);
    return real(buf as Uint8Array<ArrayBuffer>);
  }) as typeof globalThis.crypto.getRandomValues;
  try {
    const code = generate();
    assert.equal(code.length, PAYLOAD + 1, "19 drawn characters and the check character");
    assert.deepEqual(asked, [PAYLOAD], "one draw from the CSPRNG, of one byte per payload character");
  } finally {
    globalThis.crypto.getRandomValues = real;
  }
});
