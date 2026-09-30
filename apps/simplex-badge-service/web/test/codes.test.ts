import { timedTest } from "./boot.js";
import assert from "node:assert/strict";
import { ALPHABET, PAYLOAD, canonical, checkChar, display, normalise, generate, hash } from "../src/codes.js";

const codeTest = timedTest(5000);

// The golden vector's canonical form and hash come from parseBadgeCode; a divergence would sell codes the service cannot redeem.
const VECTOR_BODY = "4RT6E8YBMW74Q8DK9DKR";
const VECTOR = "SB-4RT6E-8YBMW-74Q8D-K9DKR";
const VECTOR_CANONICAL = "SB4RT6E8YBMW74Q8DK9DKR";
const VECTOR_HASH = "Lyr52PVy843AXApBOwdq8hJKCfkpE4zuR_Xm_50SDQg";

codeTest("codes: the vector agrees with parseBadgeCode's canonical form and hash", async () => {
  assert.equal(checkChar(VECTOR_BODY.slice(0, 19)), VECTOR_BODY[19]);
  assert.equal(display(VECTOR_BODY), VECTOR);
  assert.equal(canonical(VECTOR_BODY), VECTOR_CANONICAL);
  assert.equal(await hash(VECTOR_BODY), VECTOR_HASH);
});

codeTest("codes: alphabet is Crockford base32", () => {
  assert.equal(ALPHABET, "0123456789ABCDEFGHJKMNPQRSTVWXYZ");
  assert.equal(ALPHABET.length, 32);
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

// Luhn mod N detects adjacent transpositions but not a swap of 0 and Z, which this test allows for.
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
  assert.equal(normalise("sb-4rt6e-8ybmw-74q8d-k9dkr"), VECTOR_BODY);
  assert.equal(normalise(" SB 4RT6E 8YBMW 74Q8D K9DKR "), VECTOR_BODY);
  const folded = normalise(display("1".repeat(19) + checkChar("1".repeat(19))).replace(/1/g, "I"));
  assert.equal(folded, "1".repeat(19) + checkChar("1".repeat(19)));
  assert.equal(normalise("SB-UUUUU-UUUUU-UUUUU-UUUUU"), null);
  assert.equal(normalise("SB-TOOSHORT"), null);
  assert.equal(normalise("4RT6E8YBMW74Q8DK9DKR"), null);
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
  const bytes = new TextEncoder().encode(VECTOR_BODY);
  const bare = await crypto.subtle.digest("SHA-256", bytes);
  const bareB64 = btoa(String.fromCharCode(...new Uint8Array(bare)))
    .replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
  assert.notEqual(h, bareB64);
});

codeTest("codes: every code drawn is a different one, and the draw covers the alphabet", () => {
  const drawn = new Set<string>();
  const symbols = new Set<string>();
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
  // An ASCII-only strip would drop an Arabic-Indic digit and read the rest as a valid code the service refuses.
  assert.equal(normalise("SB\u0663-4RT6E-8YBMW-74Q8D-K9DKR"), null);
  assert.equal(normalise("SB-4RT6E-8YBMW-74Q8D-K9DKR"), VECTOR_BODY, "and the separators still go");
});

codeTest("codes: the payload comes from the CSPRNG, one byte per character", () => {
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
