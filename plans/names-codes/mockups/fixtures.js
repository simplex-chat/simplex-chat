// The board's made-up data, shared by the names screens (domains.js) and the other frames (screens.js).

export const NOW = Date.parse("2026-10-10T12:00:00Z");
export const ORDER_ID = "q7ZpL2dN9xWc4KfR8tYb1A";
// a year on chain is 365 days, so 730 days from 10 October 2026 cross 29 February 2028 and end on the 9th
export const UNTIL = "9 October 2028";
export const CODE_UNTIL = "8 October 2033";
export const OWNER_SHORT = "0x5aE1…7cD3";
export const APP_OWNER_SHORT = "0x2F0b…94aE";
// a stand-in: the page makes a fresh phrase, with its BIP-39 checksum, for every order
export const PHRASE = ["orbit", "velvet", "canyon", "ripple", "harbor", "tiny", "lemon", "frost", "danger", "museum", "shield", "olive"];
// placeholder amount, as crowdfunding.ts keeps the badge ones
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

// A name code: "SN", the shortest name it covers (a convention: the service's table decides),
// then 19 payload characters and a check character over the length digits and the payload.
export function nameCode(minLength, seed) {
  const len = String(minLength);
  const params = [...len].map((c) => ALPHABET.indexOf(c));
  let x = seed;
  const payload = Array.from({ length: PAYLOAD }, () => {
    x = (x * 48271) % 2147483647;
    return x % BASE;
  });
  const body = [...payload, checkValue([...params, ...payload])].map((v) => ALPHABET[v]).join("");
  return `SN${len}-${body.match(/.{5}/g).join("-")}`;
}

// an investor code: names of 6 or more letters, for 7 years
export const APPLIED = nameCode(6, 101);
export const APPLIED_YEARS = 7;
