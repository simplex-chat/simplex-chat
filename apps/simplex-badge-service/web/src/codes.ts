// The scheme is defined by src/Simplex/Chat/Badges/Code.hs, which the service and the app
// both use. Every rule below mirrors it: diverge on any of them and a code sold here can
// never be redeemed.
// Nothing in the page parses a code the buyer types; `normalise` and `checkChar` are the reader
// half of the format, kept so the tests can check this file against Code.hs in both directions.

// Crockford base32: the digits and the upper-case letters except I, L, O and U.
export const ALPHABET = "0123456789ABCDEFGHJKMNPQRSTVWXYZ";
const BASE = ALPHABET.length; // 32
const CODE_LENGTH = 20;       // 19 payload characters and a check character
export const PAYLOAD = CODE_LENGTH - 1;
const GROUP = 5;
const GROUPS = new RegExp(`.{1,${GROUP}}`, "g");
const PREFIX = "SXB";

// I and L are read as 1, O as 0, so a code copied by hand still verifies.
function charValue(c: string): number | undefined {
  const u = c.toUpperCase();
  if (u === "I" || u === "L") return 1;
  if (u === "O") return 0;
  const at = ALPHABET.indexOf(u);
  return at < 0 ? undefined : at;
}

// Luhn mod N with N = 32, over the payload values, folded from the right because the check
// character sits to the right of the payload.
function checkValue(payload: readonly number[]): number {
  let sum = 0;
  let factor = 2;
  for (let i = payload.length - 1; i >= 0; i--) {
    const addend = factor * payload[i]!;
    sum += Math.floor(addend / BASE) + (addend % BASE);
    factor = factor === 2 ? 1 : 2;
  }
  return (BASE - (sum % BASE)) % BASE;
}

export function checkChar(body: string): string {
  const values = [...body].map((c) => {
    const v = charValue(c);
    if (v === undefined) throw new Error(`character outside the alphabet: ${c}`);
    return v;
  });
  return ALPHABET[checkValue(values)]!;
}

/** The canonical form the service hashes: the prefix and 20 characters, no separators. */
export function canonical(code: string): string {
  return PREFIX + code;
}

export function display(code: string): string {
  return [PREFIX, ...(code.match(GROUPS) ?? [])].join("-");
}

/** Any case, separators optional, ambiguous characters folded. The prefix is required. */
export function normalise(input: string): string | null {
  // `parseBadgeCode` filters with `isAlphaNum`, which is Unicode: stripping only ASCII here would
  // read a code this browser accepts and the service does not
  const cleaned = input.replace(/[^\p{L}\p{N}]/gu, "").toUpperCase();
  if (!cleaned.startsWith(PREFIX)) return null;
  const body = cleaned.slice(PREFIX.length);
  if (body.length !== CODE_LENGTH) return null;
  const values: number[] = [];
  for (const c of body) {
    const v = charValue(c);
    if (v === undefined) return null;
    values.push(v);
  }
  if (values[PAYLOAD] !== checkValue(values.slice(0, PAYLOAD))) return null;
  // rebuilt from the values, not from the input: that is what folds I/L/O
  return values.map((v) => ALPHABET[v]!).join("");
}

/** 256 is a multiple of 32, so a byte reduces without bias and nothing is redrawn. */
export function generate(): string {
  const buf = new Uint8Array(PAYLOAD);
  crypto.getRandomValues(buf);
  const payload = [...buf].map((b) => b % BASE);
  return [...payload, checkValue(payload)].map((v) => ALPHABET[v]!).join("");
}

export async function hash(normalised: string): Promise<string> {
  const bytes = new TextEncoder().encode(canonical(normalised));
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return btoa(String.fromCharCode(...new Uint8Array(digest)))
    .replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}
