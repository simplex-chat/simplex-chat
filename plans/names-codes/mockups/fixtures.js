// The board's made-up data, shared by the names screens (domains.js) and the other frames (screens.js).

// the name every order screen shows
export const NAME = "dynamis";
export const ORDER_ID = "q7ZpL2dN9xWc4KfR8tYb1A";
// a year on chain is 365 days, so 730 days from 10 October 2026 cross 29 February 2028 and end on the 9th
export const UNTIL = "9 October 2028";
export const CODE_UNTIL = "9 October 2031";
export const UNTIL_SHORT = UNTIL.replace("October", "Oct");
export const CODE_UNTIL_SHORT = CODE_UNTIL.replace("October", "Oct");
// account 0 of OWNER_CODE below: 0x014f9c09CcEeDEF1B713110d04D3a07d5B5D9b73
export const OWNER_SHORT = "0x014f…9b73";
export const APP_OWNER_SHORT = "0x2F0b…94aE";
// the name code the page makes for every order: "SN-" and 30 characters, its 128-bit wallet entropy and
// the first 22 bits of its SHA-256; this one checks, for entropy 6f2d1c9a4b7e3f80d15a2c6e9b04f7a3
export const OWNER_CODE = "SN-DWPHS-6JBFR-ZR1MA-T5HQ9-P17QM-F5GM5";
// a placeholder amount for names; crowdfunding.ts holds the real badge amounts
export const NAME_INVEST = "$500";
export const COMMIT_TX = "0x8f3a…c21d";
export const REGISTER_TX = "0x2b71…9e04";

// The service's answer for a name; the page shows only this price, never a list.
const PRICES = { 6: 40000, 7: 20000, 8: 10000 };
export function priceOf(label) {
  return PRICES[Math.min(label.length, 8)];
}
export function dollars(minor, cents = false) {
  return `$${(minor / 100).toFixed(cents ? 2 : 0)}`;
}

export const BAKERY = [
  { initials: "AB", hue: 28, name: "Alice's Bakery", kind: "Contact address", link: "https://smp8.simplex.im/a#Y8m3qT0vLw2…" },
  { initials: "BN", hue: 200, name: "Bakery news", kind: "Channel", link: "https://smp9.simplex.im/c#Rk27xN4pDe9…" },
];

const ALPHABET = "0123456789ABCDEFGHJKMNPQRSTVWXYZ";
const BASE = ALPHABET.length;
const PAYLOAD = 19;
// the MINSTD generator, a fixed sequence for stable mockup codes
const LCG_MULTIPLIER = 48271;
const LCG_MODULUS = 2147483647;

function checkValue(values) {
  let sum = 0;
  let factor = 2;
  for (let i = values.length - 1; i >= 0; i--) {
    const addend = factor * values[i];
    sum += Math.floor(addend / BASE) + (addend % BASE);
    factor = factor === 2 ? 1 : 2;
  }
  return (BASE - (sum % BASE)) % BASE;
}

// A prepaid code: "SN", the shortest name it covers (a convention: the service's table decides),
// then 19 payload characters and a check character over the length digits and the payload.
export function prepaidCode(minLength, seed) {
  const len = String(minLength);
  const params = [...len].map((c) => ALPHABET.indexOf(c));
  let x = seed;
  const payload = Array.from({ length: PAYLOAD }, () => {
    x = (x * LCG_MULTIPLIER) % LCG_MODULUS;
    return x % BASE;
  });
  const body = [...payload, checkValue([...params, ...payload])].map((v) => ALPHABET[v]).join("");
  return `SN${len}-${body.match(/.{5}/g).join("-")}`;
}

// an investor code: 5 years, as simplex.domains offers early investors; 6 or more letters is this design's choice
export const APPLIED = prepaidCode(6, 101);
export const APPLIED_SHORT = `${APPLIED.slice(0, 9)}…`;
export const APPLIED_YEARS = 5;
