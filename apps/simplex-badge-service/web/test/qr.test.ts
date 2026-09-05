import { timedTest } from "./boot.js";
import assert from "node:assert/strict";

const qrTest = timedTest(20000);

import { StubElement, SVG_NS, installDocument } from "./stub-dom.js";

// The stub has to be installed before `qr.ts` is evaluated, because `qrSvg`
// reaches for `document` the moment it is called.
installDocument();

const qr = await import("../src/qr.js");
const {
  alignmentPositions, byteCapacity, dataCodewords, eccOf, encodeQr, formatBits,
  generatorPoly, interleave, numDataCodewords, paymentUri, qrSvg, rawDataModules, versionBits,
} = qr;
type QrSymbol = import("../src/qr.js").QrSymbol;

const screens = await import("../src/screens.js");
const order = await import("../src/order.js");

// ------------------------------------------------- the tables, from the standard

/** Table 7, byte mode, error correction level M: the payload bytes each version holds. */
const PUBLISHED_BYTE_CAPACITY_M: readonly number[] = [
  14, 26, 42, 62, 84, 106, 122, 152, 180, 213,
  251, 287, 331, 362, 412, 450, 504, 560, 624, 666,
  711, 779, 857, 911, 997, 1059, 1125, 1190, 1264, 1370,
  1452, 1538, 1628, 1722, 1809, 1911, 1989, 2099, 2213, 2331,
];

/** Table 9, level M: the number of error correction blocks a version splits into. Transcribed rather than
 * derived, since the block count and the EC codewords per block cannot be separated from their product; the
 * capacity test below is what proves `src/qr.ts` holds the same numbers. */
const PUBLISHED_BLOCKS_M: readonly number[] = [
  1, 1, 1, 2, 2, 4, 4, 4, 5, 5, 5, 8, 9, 9, 10, 10, 11, 13, 14, 16,
  17, 17, 18, 20, 21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49,
];

/** Table E.1: the row and column centres of the alignment patterns. */
const PUBLISHED_ALIGNMENT: ReadonlyArray<readonly number[]> = [
  [], [6, 18], [6, 22], [6, 26], [6, 30], [6, 34], [6, 22, 38], [6, 24, 42], [6, 26, 46], [6, 28, 50],
  [6, 30, 54], [6, 32, 58], [6, 34, 62], [6, 26, 46, 66], [6, 26, 48, 70], [6, 26, 50, 74],
  [6, 30, 54, 78], [6, 30, 56, 82], [6, 30, 58, 86], [6, 34, 62, 90], [6, 28, 50, 72, 94],
  [6, 26, 50, 74, 98], [6, 30, 54, 78, 102], [6, 28, 54, 80, 106], [6, 32, 58, 84, 110],
  [6, 30, 58, 86, 114], [6, 34, 62, 90, 118], [6, 26, 50, 74, 98, 122], [6, 30, 54, 78, 102, 126],
  [6, 26, 52, 78, 104, 130], [6, 30, 56, 82, 108, 134], [6, 34, 60, 86, 112, 138],
  [6, 30, 58, 86, 114, 142], [6, 34, 62, 90, 118, 146], [6, 30, 54, 78, 102, 126, 150],
  [6, 24, 50, 76, 102, 128, 154], [6, 28, 54, 80, 106, 132, 158], [6, 32, 58, 84, 110, 136, 162],
  [6, 26, 54, 82, 110, 138, 166], [6, 30, 58, 86, 114, 142, 170],
];

/** Table C.1, all thirty-two of them, indexed by the two-bit level indicator and then by mask. A decoder that
 * cannot find what it read in this table has read the wrong modules, which is how a misplaced format block
 * shows up. */
const PUBLISHED_FORMAT: Readonly<Record<number, readonly string[]>> = {
  0b01: [ // L
    "111011111000100", "111001011110011", "111110110101010", "111100010011101",
    "110011000101111", "110001100011000", "110110001000001", "110100101110110",
  ],
  0b00: [ // M
    "101010000010010", "101000100100101", "101111001111100", "101101101001011",
    "100010111111001", "100000011001110", "100111110010111", "100101010100000",
  ],
  0b11: [ // Q
    "011010101011111", "011000001101000", "011111100110001", "011101000000110",
    "010010010110100", "010000110000011", "010111011011010", "010101111101101",
  ],
  0b10: [ // H
    "001011010001001", "001001110111110", "001110011100111", "001100111010000",
    "000011101100010", "000001001010101", "000110100001100", "000100000111011",
  ],
};

const ECL_M_BITS = 0b00;

/** Table D.1, the first four rows. */
const PUBLISHED_VERSION_BITS: Readonly<Record<number, string>> = {
  7: "000111110010010100",
  8: "001000010110111100",
  9: "001001101010011001",
  10: "001010010011010011",
};

/** Annex A, as α exponents, highest power first. */
const PUBLISHED_GENERATORS: Readonly<Record<number, readonly number[]>> = {
  7: [0, 87, 229, 146, 149, 238, 102, 21],
  10: [0, 251, 67, 46, 61, 118, 70, 64, 94, 32, 45],
  13: [0, 74, 152, 176, 100, 86, 100, 106, 104, 130, 218, 206, 140, 78],
  16: [0, 120, 104, 107, 109, 102, 161, 76, 3, 91, 191, 147, 169, 182, 194, 225, 120],
  18: [0, 215, 234, 158, 94, 184, 97, 118, 170, 79, 187, 152, 148, 252, 179, 5, 98, 96, 153],
  22: [0, 210, 171, 247, 242, 93, 230, 14, 109, 221, 53, 200, 74, 8, 172, 98, 80, 219, 134, 160, 105, 165, 231],
  26: [0, 173, 125, 158, 2, 103, 182, 118, 17, 145, 201, 111, 28, 165, 53, 161, 21, 245, 142, 13, 102, 48, 227, 153, 145, 218, 70],
  28: [0, 168, 223, 200, 104, 224, 234, 108, 180, 110, 190, 195, 147, 205, 27, 232, 201, 21, 43, 245, 87, 42, 195, 212, 119, 242, 37, 9, 123],
  30: [0, 41, 173, 145, 152, 216, 31, 179, 182, 50, 48, 110, 86, 239, 96, 222, 125, 42, 173, 226, 193, 224, 130, 156, 37, 251, 216, 238, 40, 192, 180],
};

/** The bits left over once a version's codewords are placed. */
function publishedRemainderBits(version: number): number {
  if (version === 1) return 0;
  if (version <= 6) return 7;
  if (version <= 13) return 0;
  if (version <= 20) return 3;
  if (version <= 27) return 4;
  if (version <= 34) return 3;
  return 0;
}

/** Annex I's worked example, 1-M: its sixteen data codewords and their ten EC codewords. */
const ANNEX_I_DATA = [0x10, 0x20, 0x0c, 0x56, 0x61, 0x80, 0xec, 0x11, 0xec, 0x11, 0xec, 0x11, 0xec, 0x11, 0xec, 0x11];
const ANNEX_I_ECC = [0xa5, 0x24, 0xd4, 0xc1, 0xed, 0x36, 0xc7, 0x87, 0x2c, 0x55];

// ------------------------------------------------------ a reader of the standard

// GF(2^8) again, from the field polynomial the standard names. Written here so
// that nothing below borrows the encoder's arithmetic.
const EXP = new Uint8Array(512);
const LOG = new Uint8Array(256);
{
  let x = 1;
  for (let i = 0; i < 255; i++) {
    EXP[i] = x;
    LOG[x] = i;
    x <<= 1;
    if ((x & 0x100) !== 0) x ^= 0x11d;
  }
  for (let i = 255; i < 512; i++) EXP[i] = EXP[i - 255]!;
}
function mul(a: number, b: number): number {
  return a === 0 || b === 0 ? 0 : EXP[LOG[a]! + LOG[b]!]!;
}

const MASK_RULES: ReadonlyArray<(x: number, y: number) => boolean> = [
  (x, y) => (x + y) % 2 === 0,
  (_x, y) => y % 2 === 0,
  (x, _y) => x % 3 === 0,
  (x, y) => (x + y) % 3 === 0,
  (x, y) => (Math.floor(x / 3) + Math.floor(y / 2)) % 2 === 0,
  (x, y) => ((x * y) % 2) + ((x * y) % 3) === 0,
  (x, y) => (((x * y) % 2) + ((x * y) % 3)) % 2 === 0,
  (x, y) => (((x + y) % 2) + ((x * y) % 3)) % 2 === 0,
];

/** Which modules are function modules, as rectangles rather than as drawings: the three finder squares with
 * their separators, the two format blocks, the timing lines, the version blocks and the alignment squares,
 * whose centres come from the published table above, not from `src/qr.ts`. */
function functionMap(version: number): boolean[][] {
  const size = version * 4 + 17;
  const map = Array.from({ length: size }, () => new Array<boolean>(size).fill(false));
  const block = (x0: number, y0: number, w: number, h: number): void => {
    for (let y = y0; y < y0 + h; y++) for (let x = x0; x < x0 + w; x++) map[y]![x] = true;
  };
  // Finders and their separators, then the format module beyond each.
  block(0, 0, 9, 9);
  block(size - 8, 0, 8, 9);
  block(0, size - 8, 9, 8);
  // Timing.
  block(0, 6, size, 1);
  block(6, 0, 1, size);
  if (version >= 7) {
    block(size - 11, 0, 3, 6);
    block(0, size - 11, 6, 3);
  }
  const centres = PUBLISHED_ALIGNMENT[version - 1]!;
  for (const cy of centres) {
    for (const cx of centres) {
      if ((cx === 6 && cy === 6) || (cx === 6 && cy === size - 7) || (cx === size - 7 && cy === 6)) continue;
      block(cx - 2, cy - 2, 5, 5);
    }
  }
  return map;
}

/** Copy 1 of the format information, module by module, as bit index 14 down to 0. */
function formatCopyOne(): Array<readonly [number, number]> {
  const at: Array<readonly [number, number]> = [];
  for (let i = 14; i >= 9; i--) at.push([14 - i, 8]);
  at.push([7, 8], [8, 8], [8, 7]);
  for (let i = 5; i >= 0; i--) at.push([8, i]);
  return at;
}

/** Copy 2, in the same order: the row beside the bottom-left finder, then the column above the top-right one. */
function formatCopyTwo(size: number): Array<readonly [number, number]> {
  const at: Array<readonly [number, number]> = [];
  for (let i = 14; i >= 8; i--) at.push([8, size - 15 + i]);
  for (let i = 7; i >= 0; i--) at.push([size - 1 - i, 8]);
  return at;
}

function readBits(modules: boolean[][], at: ReadonlyArray<readonly [number, number]>): string {
  return at.map(([x, y]) => (modules[y]![x]! ? "1" : "0")).join("");
}

interface Decoded {
  version: number;
  mask: number;
  eclBits: number;
  payload: string;
  /** Where the traversal put each data bit, so a test can pin the first few. */
  order: Array<readonly [number, number]>;
}

/** Reads a symbol back, refusing rather than guessing: an unknown format string, two copies that disagree, a
 * mode that is not byte, or a block with non-zero syndromes all throw, so a symbol that decodes here is one a
 * conforming reader would accept. */
function decodeQr(modules: boolean[][]): Decoded {
  const size = modules.length;
  assert.equal(size % 4, 1, "a symbol is 4v+17 modules on a side");
  const version = (size - 17) / 4;
  assert.ok(version >= 1 && version <= 40 && Number.isInteger(version), `no such version: ${version}`);

  const one = readBits(modules, formatCopyOne());
  const two = readBits(modules, formatCopyTwo(size));
  assert.equal(one, two, "the two copies of the format information must agree");
  let eclBits = -1;
  let mask = -1;
  for (const [bits, strings] of Object.entries(PUBLISHED_FORMAT)) {
    const found = strings.indexOf(one);
    if (found >= 0) {
      eclBits = Number(bits);
      mask = found;
    }
  }
  assert.ok(mask >= 0, `${one} is not one of the thirty-two published format strings`);

  if (version >= 7) {
    const block = [];
    for (let i = 17; i >= 0; i--) {
      const x = size - 11 + (i % 3);
      const y = Math.floor(i / 3);
      block.push(modules[y]![x]! ? "1" : "0");
      assert.equal(modules[x]![y]!, modules[y]![x]!, "the two version blocks are each other's transpose");
    }
    assert.equal(block.join(""), versionBits(version).toString(2).padStart(18, "0"),
      "the version block must carry this version");
  }

  const map = functionMap(version);
  const rule = MASK_RULES[mask]!;
  const bits: number[] = [];
  const order: Array<readonly [number, number]> = [];
  for (let right = size - 1; right >= 1; right -= 2) {
    if (right === 6) right = 5;
    const columns = [right, right - 1];
    // Upward for every other pair of columns, counting from the right edge.
    const upward = ((right + 1) & 2) === 0;
    for (let step = 0; step < size; step++) {
      const y = upward ? size - 1 - step : step;
      for (const x of columns) {
        if (map[y]![x]!) continue;
        bits.push((modules[y]![x]! !== rule(x, y)) ? 1 : 0);
        order.push([x, y]);
      }
    }
  }
  // A cross-check rather than an independent one: this file's function map and
  // `src/qr.ts`'s pattern drawing are two derivations of the same paragraph, and
  // they must leave the same modules free. The count itself is anchored to the
  // published remainder-bit table by a test below.
  assert.equal(bits.length, rawDataModules(version),
    "every module that is not a function module carries a bit");

  const totalCodewords = Math.floor(bits.length / 8);
  const stream: number[] = [];
  for (let i = 0; i < totalCodewords; i++) {
    let byte = 0;
    for (let j = 0; j < 8; j++) byte = (byte << 1) | bits[i * 8 + j]!;
    stream.push(byte);
  }
  for (let i = totalCodewords * 8; i < bits.length; i++) {
    assert.equal(bits[i], 0, "remainder bits are light");
  }

  // De-interleaving needs the data codeword count and the block count, and both
  // come from the published tables rather than from `src/qr.ts`: the data count
  // is the published byte capacity plus its header, rounded up to a codeword.
  const dataLen = PUBLISHED_BYTE_CAPACITY_M[version - 1]! + (version < 10 ? 2 : 3);
  const numBlocks = PUBLISHED_BLOCKS_M[version - 1]!;
  const eccLen = (totalCodewords - dataLen) / numBlocks;
  assert.ok(Number.isInteger(eccLen), "the EC codewords divide evenly among the blocks");
  const shortLen = Math.floor(dataLen / numBlocks);
  const numShort = numBlocks - (dataLen % numBlocks);
  const blocks = Array.from({ length: numBlocks }, (_v, b) => ({
    data: new Array<number>(b < numShort ? shortLen : shortLen + 1).fill(0),
    ecc: new Array<number>(eccLen).fill(0),
  }));
  let at = 0;
  for (let i = 0; i <= shortLen; i++) {
    for (const b of blocks) if (i < b.data.length) b.data[i] = stream[at++]!;
  }
  for (let i = 0; i < eccLen; i++) for (const b of blocks) b.ecc[i] = stream[at++]!;
  assert.equal(at, totalCodewords, "de-interleaving consumes the whole stream");

  // The syndromes: every block, evaluated at α^0 … α^(eccLen−1), must be zero.
  // Those are the roots the generator polynomial is built from, so this checks
  // the error correction without reusing a single one of its coefficients.
  for (const [index, b] of blocks.entries()) {
    const codeword = [...b.data, ...b.ecc];
    for (let i = 0; i < eccLen; i++) {
      let syndrome = 0;
      for (const [j, c] of codeword.entries()) {
        syndrome ^= mul(c, EXP[((codeword.length - 1 - j) * i) % 255]!);
      }
      assert.equal(syndrome, 0, `block ${index} fails its Reed–Solomon syndrome at α^${i}`);
    }
  }

  const data = blocks.flatMap((b) => b.data);
  const dataBits: number[] = [];
  for (const byte of data) for (let i = 7; i >= 0; i--) dataBits.push((byte >>> i) & 1);
  const take = (n: number): number => {
    let v = 0;
    for (let i = 0; i < n; i++) v = (v << 1) | dataBits.shift()!;
    return v;
  };
  assert.equal(take(4), 0b0100, "byte mode");
  const length = take(version < 10 ? 8 : 16);
  const bytes = new Uint8Array(length);
  for (let i = 0; i < length; i++) bytes[i] = take(8);
  return { version, mask, eclBits, payload: new TextDecoder().decode(bytes), order };
}


// ------------------------------------------------------------- the tables

qrTest("qr: every version's byte capacity at level M is the published one (Table 7)", () => {
  for (let version = 1; version <= 40; version++) {
    assert.equal(byteCapacity(version), PUBLISHED_BYTE_CAPACITY_M[version - 1],
      `version ${version} holds ${PUBLISHED_BYTE_CAPACITY_M[version - 1]} bytes at level M`);
  }
});

qrTest("qr: the alignment centres are the published ones, version 32 included (Table E.1)", () => {
  for (let version = 1; version <= 40; version++) {
    assert.deepEqual(alignmentPositions(version), PUBLISHED_ALIGNMENT[version - 1],
      `version ${version}'s alignment centres`);
  }
  // The one version the general spacing rule gets wrong, called out because a
  // symbol with a misplaced alignment pattern still looks like a QR.
  assert.deepEqual(alignmentPositions(32), [6, 34, 60, 86, 112, 138]);
});

qrTest("qr: all thirty-two format information strings are the published ones (Table C.1)", () => {
  for (const [bits, strings] of Object.entries(PUBLISHED_FORMAT)) {
    for (let mask = 0; mask < 8; mask++) {
      assert.equal(formatBits(Number(bits), mask).toString(2).padStart(15, "0"), strings[mask],
        `format information for level ${bits}, mask ${mask}`);
    }
  }
});

qrTest("qr: the version information strings are published, and eight bits apart (Table D.1)", () => {
  for (const [version, expected] of Object.entries(PUBLISHED_VERSION_BITS)) {
    assert.equal(versionBits(Number(version)).toString(2).padStart(18, "0"), expected,
      `version information for version ${version}`);
  }
  // The published minimum distance of the code is 8. A wrong BCH generator
  // would still produce eighteen bits, and would not keep them this far apart.
  const all = [];
  for (let v = 7; v <= 40; v++) all.push(versionBits(v));
  for (let i = 0; i < all.length; i++) {
    for (let j = i + 1; j < all.length; j++) {
      let bits = all[i]! ^ all[j]!;
      let distance = 0;
      while (bits !== 0) {
        distance += bits & 1;
        bits >>>= 1;
      }
      assert.ok(distance >= 8, `versions ${i + 7} and ${j + 7} are only ${distance} bits apart`);
    }
  }
});

qrTest("qr: the generator polynomials are Annex A's, and their roots are α^0…α^(n−1)", () => {
  for (const [degree, exponents] of Object.entries(PUBLISHED_GENERATORS)) {
    const poly = generatorPoly(Number(degree));
    assert.deepEqual(poly.map((c) => LOG[c]!), [...exponents], `the generator of degree ${degree}`);
  }
  // And the property the coefficients exist to have, for every degree used at
  // level M: a wrong table cannot survive both.
  for (const degree of [10, 16, 18, 22, 24, 26, 28, 30]) {
    const poly = generatorPoly(degree);
    for (let i = 0; i < degree; i++) {
      let value = 0;
      for (const [j, c] of poly.entries()) value ^= mul(c, EXP[((poly.length - 1 - j) * i) % 255]!);
      assert.equal(value, 0, `α^${i} must be a root of the degree ${degree} generator`);
    }
  }
});

qrTest("qr: Annex I's worked 1-M example produces its published EC codewords", () => {
  const ecc = eccOf(ANNEX_I_DATA, 10);
  assert.deepEqual(ecc, ANNEX_I_ECC,
    `A5 24 D4 C1 ED 36 C7 87 2C 55 is the standard's answer, not ${ecc.map((b) => b.toString(16)).join(" ")}`);
  // And the vector is only a vector if a single wrong byte changes it.
  const off = eccOf([...ANNEX_I_DATA.slice(0, 15), 0x12], 10);
  assert.notDeepEqual(off, ANNEX_I_ECC);
});

qrTest("qr: the remainder bits per version are the published counts", () => {
  for (let version = 1; version <= 40; version++) {
    const codewords = Math.floor(rawDataModules(version) / 8);
    assert.equal(rawDataModules(version) - codewords * 8, publishedRemainderBits(version),
      `version ${version} leaves ${publishedRemainderBits(version)} remainder bits`);
  }
});

// ------------------------------------------------------------ the bit stream

qrTest("qr: the bit stream is mode, count, payload, terminator and the alternating pad", () => {
  const bytes = new TextEncoder().encode("SXB");
  const codewords = dataCodewords(bytes, 1);
  assert.equal(codewords.length, numDataCodewords(1), "a version is filled, not partly written");
  // 0100 then 00000011 then 'S' 'X' 'B', packed across the nibble boundary.
  assert.equal(codewords[0], 0b01000000);
  assert.equal(codewords[1], 0b00110101, "the length nibble carries into the first payload byte");
  assert.equal(codewords[2], 0b00110101, "'S' and the high nibble of 'X'");
  assert.equal(codewords[4], (("B".charCodeAt(0) & 0x0f) << 4) | 0b0000, "the terminator follows the last byte");
  for (let i = 5; i < codewords.length; i++) {
    assert.equal(codewords[i], i % 2 === 1 ? 0xec : 0x11, "the pad alternates 11101100 00010001");
  }
});

qrTest("qr: the version is chosen by payload length, at every boundary that moves", () => {
  const symbolFor = (n: number): QrSymbol | null => encodeQr("a".repeat(n));
  assert.equal(symbolFor(14)!.version, 1, "fourteen bytes is all version 1 holds");
  assert.equal(symbolFor(15)!.version, 2, "and the fifteenth crosses into version 2");
  // The boundary that also changes the header: the character count is 8 bits up
  // to version 9 and 16 from version 10, so this pair is the one a hardcoded
  // header width gets wrong.
  assert.equal(symbolFor(180)!.version, 9);
  assert.equal(symbolFor(181)!.version, 10);
  assert.equal(symbolFor(122)!.version, 7);
  assert.equal(symbolFor(123)!.version, 8);
  assert.equal(symbolFor(2331)!.version, 40, "the largest payload there is");
  assert.equal(symbolFor(2332), null, "and one byte more has no symbol at all");
});

// --------------------------------------------------------------- the structure

function symbolOf(payload: string): QrSymbol {
  const symbol = encodeQr(payload);
  assert.ok(symbol !== null, "this payload must encode");
  return symbol;
}

qrTest("qr: the function patterns are where a scanner looks for them", () => {
  for (const version of [1, 7, 14, 32]) {
    const symbol = symbolOf("a".repeat(PUBLISHED_BYTE_CAPACITY_M[version - 1]!));
    assert.equal(symbol.version, version, "the payload was sized to this version");
    const m = symbol.modules;
    const n = symbol.size;
    for (const [ox, oy] of [[0, 0], [n - 7, 0], [0, n - 7]] as const) {
      for (let dy = 0; dy < 7; dy++) {
        for (let dx = 0; dx < 7; dx++) {
          const ring = Math.max(Math.abs(dx - 3), Math.abs(dy - 3));
          assert.equal(m[oy + dy]![ox + dx], ring !== 2, `finder at ${ox},${oy} module ${dx},${dy}`);
        }
      }
    }
    for (let i = 8; i < n - 8; i++) {
      assert.equal(m[6]![i], i % 2 === 0, `horizontal timing at ${i}`);
      assert.equal(m[i]![6], i % 2 === 0, `vertical timing at ${i}`);
    }
    assert.equal(m[n - 8]![8], true, "the module at (8, 4v+9) is always dark");
  }
});

qrTest("qr: the first codeword starts at the bottom-right corner and runs upward", () => {
  // The one statement about the traversal that comes from outside this
  // repository: a reader that started anywhere else would read a different
  // symbol. Checked against the encoder's own output, not against the decoder.
  const payload = "SXB-YDC8A-YGQTM-PUYZ9-2TUXP";
  const symbol = symbolOf(payload);
  const codewords = interleave(dataCodewords(new TextEncoder().encode(payload), symbol.version), symbol.version);
  const rule = MASK_RULES[symbol.mask]!;
  const unmasked = (x: number, y: number): number =>
    (symbol.modules[y]![x]! !== rule(x, y)) ? 1 : 0;
  const n = symbol.size;
  const first = codewords[0]!;
  assert.equal(unmasked(n - 1, n - 1), (first >>> 7) & 1, "bit 1 at the bottom-right corner");
  assert.equal(unmasked(n - 2, n - 1), (first >>> 6) & 1, "bit 2 one module to its left");
  assert.equal(unmasked(n - 1, n - 2), (first >>> 5) & 1, "bit 3 one module up, back at the right");
  assert.equal(unmasked(n - 2, n - 2), (first >>> 4) & 1, "bit 4 beside it");
});

qrTest("qr: the mask is one of the eight, and is the one the format block declares", () => {
  for (const payload of ["a", "SXB-YDC8A-YGQTM-PUYZ9-2TUXP", "x".repeat(400)]) {
    const symbol = symbolOf(payload);
    assert.ok(symbol.mask >= 0 && symbol.mask <= 7);
    const decoded = decodeQr(symbol.modules);
    assert.equal(decoded.mask, symbol.mask, "the format block must declare the mask that was applied");
    assert.equal(decoded.eclBits, ECL_M_BITS, "and level M");
  }
});

// ------------------------------------------------------------- the round trips

const MONERO_ADDRESS = "48HqK2XmVexampleAddress9fRtWcExampleAddress2nQyVXaLbEEXampleAddr9SDFGHJK9fRtWcQ8Uv7VJj3mExample";
const MONERO_INTEGRATED = "4LEXampleIntegratedAddress9fRtWcExampleAddress2nQyVXaLbEEXampleAddr9SDFGHJK9fRtWcQ8Uv7VJj3mExampleAddr9fRt";
const BITCOIN_ADDRESS = "bc1qar0srrr7xfkvy5l643lydnw9re59gtzzwf5mdq";
const BADGE_CODE = "SXB-YDC8A-YGQTM-PUYZ9-2TUXP";

function roundTrip(payload: string, what: string): Decoded {
  const symbol = symbolOf(payload);
  const decoded = decodeQr(symbol.modules);
  assert.equal(decoded.payload, payload, `${what} did not survive the round trip`);
  assert.equal(decoded.version, symbol.version);
  return decoded;
}

qrTest("qr: a Monero payment URI with a 95-character address round-trips", () => {
  assert.equal(MONERO_ADDRESS.length, 95, "a standard Monero address is 95 characters");
  const uri = paymentUri("xmr", MONERO_ADDRESS, "1.482")!;
  const decoded = roundTrip(uri, "the Monero URI");
  assert.equal(decoded.payload, `monero:${MONERO_ADDRESS}?tx_amount=1.482`);
  assert.ok(decoded.version >= 7, `${uri.length} bytes needs more than version 6, not version ${decoded.version}`);
});

qrTest("qr: a Monero integrated address, 106 characters, round-trips — and needs version 8", () => {
  assert.equal(MONERO_INTEGRATED.length, 106, "an integrated address is 106 characters");
  const uri = paymentUri("xmr", MONERO_INTEGRATED, "1.482")!;
  assert.equal(uri.length, 129);
  const decoded = roundTrip(uri, "the integrated-address URI");
  // The number this task exists for: 124 bytes does not fit version 7's 122, so
  // an encoder that hardcoded a version would emit a truncated symbol here.
  assert.equal(decoded.version, 8);
});

qrTest("qr: a Bitcoin BIP-21 URI round-trips", () => {
  const uri = paymentUri("btc", BITCOIN_ADDRESS, "0.00412")!;
  assert.equal(uri, `bitcoin:${BITCOIN_ADDRESS}?amount=0.00412`);
  roundTrip(uri, "the Bitcoin URI");
});

qrTest("qr: a badge code round-trips exactly, hyphens and all", () => {
  const decoded = roundTrip(BADGE_CODE, "the badge code");
  assert.equal(BADGE_CODE.length, 27);
  assert.equal(decoded.version, 3, "twenty-seven characters is more than version 2's twenty-six");
});

qrTest("qr: payloads on both sides of a version boundary round-trip", () => {
  for (const length of [14, 15, 122, 123, 180, 181, 213, 214]) {
    const payload = "9".repeat(length);
    const decoded = roundTrip(payload, `${length} bytes`);
    assert.equal(decoded.payload.length, length);
  }
});

qrTest("qr: a representative version from every table breakpoint round-trips", () => {
  // One per change of block count, of alignment row, and of remainder bits,
  // version 32 among them, whose alignment spacing is the exception.
  for (const version of [1, 2, 6, 7, 9, 10, 13, 14, 26, 32, 40]) {
    const payload = "Z".repeat(PUBLISHED_BYTE_CAPACITY_M[version - 1]!);
    const symbol = symbolOf(payload);
    assert.equal(symbol.version, version, `a full version ${version} payload must take version ${version}`);
    const decoded = decodeQr(symbol.modules);
    assert.equal(decoded.payload, payload, `version ${version} did not survive`);
  }
});

qrTest("qr: a multi-byte payload is encoded as UTF-8 bytes, not as characters", () => {
  const payload = "Zahlung — 1,482 XMR ✓";
  const decoded = roundTrip(payload, "the UTF-8 payload");
  assert.equal(decoded.payload, payload);
  assert.ok(new TextEncoder().encode(payload).length > payload.length, "this payload is longer in bytes than in characters");
});

qrTest("qr: paymentUri refuses half a destination", () => {
  assert.equal(paymentUri("xmr", "", "1.482"), null, "no address is nothing to scan");
  assert.equal(paymentUri("xmr", MONERO_ADDRESS, ""), null, "and a bare address does not prefill an amount");
  assert.equal(paymentUri("btc", BITCOIN_ADDRESS, "0.5"), `bitcoin:${BITCOIN_ADDRESS}?amount=0.5`);
});

// ------------------------------------------------------------------- the SVG

/** A fresh regex each time: `matchAll` and `replace` on one shared object is a trap. */
const runPattern = (): RegExp => /M(\d+) (\d+)h(\d+)v1h-(\d+)z/g;

/** The symbol the SVG actually draws, read back out of the one path's geometry. */
function modulesOfSvg(svg: StubElement): boolean[][] {
  assert.equal(svg.tagName, "svg");
  const viewBox = svg.getAttribute("viewBox");
  assert.ok(viewBox !== null, "an SVG with no viewBox does not scale");
  const [, , w, h] = viewBox.split(" ").map(Number);
  assert.equal(w, h, "a symbol is square");
  const quiet = 4;
  const size = w! - quiet * 2;
  const modules = Array.from({ length: size }, () => new Array<boolean>(size).fill(false));
  const path = svg.querySelector("path");
  assert.ok(path !== null, "the dark modules are one path");
  const d = path.getAttribute("d") ?? "";
  let runs = 0;
  for (const run of d.matchAll(runPattern())) {
    const x = Number(run[1]) - quiet;
    const y = Number(run[2]) - quiet;
    const width = Number(run[3]);
    assert.equal(run[3], run[4], "a run closes exactly as wide as it opened");
    for (let i = 0; i < width; i++) modules[y]![x + i] = true;
    runs++;
  }
  assert.ok(runs > 0, "a symbol with no dark modules is not a symbol");
  // The whole `d` is consumed: a stray command would mean modules drawn that
  // this reading never saw.
  assert.equal(d.replace(runPattern(), "").length, 0, `unread path commands in ${d.slice(0, 120)}`);
  return modules;
}

qrTest("svg: the elements are in the SVG namespace, which is what makes them render", () => {
  const svg = qrSvg(BADGE_CODE, "Badge code as a scannable code") as unknown as StubElement;
  assert.equal(svg.namespaceURI, SVG_NS,
    "an <svg> created in the HTML namespace lays out as an empty inline box and draws nothing");
  for (const child of svg.children) {
    assert.equal((child as StubElement).namespaceURI, SVG_NS);
  }
  assert.equal(svg.getAttribute("role"), "img");
  assert.equal(svg.getAttribute("aria-label"), "Badge code as a scannable code");
});

qrTest("svg: the drawn modules ARE the symbol, quiet zone included", () => {
  const payload = paymentUri("xmr", MONERO_INTEGRATED, "1.482")!;
  const svg = qrSvg(payload, "Monero payment code") as unknown as StubElement;
  const symbol = symbolOf(payload);
  const drawn = modulesOfSvg(svg);
  assert.deepEqual(drawn, symbol.modules, "the path's geometry must be the encoder's modules, not a transposition");
  assert.equal(decodeQr(drawn).payload, payload, "and reading the drawing back gives the payload");

  // The four-module quiet zone the standard requires, painted light rather than
  // inherited: this page has a dark theme, and a QR without a light border does
  // not scan.
  const span = symbol.size + 8;
  assert.equal(svg.getAttribute("viewBox"), `0 0 ${span} ${span}`);
  const background = svg.querySelector("rect")!;
  assert.equal(background.getAttribute("width"), String(span));
  assert.equal(background.getAttribute("height"), String(span));
  assert.equal(background.getAttribute("fill"), "#ffffff");
});

qrTest("svg: nothing in the tree carries the payload as text or as an attribute", () => {
  const svg = qrSvg(BADGE_CODE, "Badge code as a scannable code") as unknown as StubElement;
  const dump = svg.serialize();
  for (const form of [BADGE_CODE, BADGE_CODE.replace(/-/g, ""), "SXB-"]) {
    assert.ok(!dump.includes(form), `the SVG smuggled ${form} into ${dump.slice(0, 200)}`);
  }
  assert.deepEqual(svg.texts, [], "a symbol has no text nodes at all");
});

qrTest("svg: a payload too long for any version draws nothing", () => {
  assert.equal(qrSvg("a".repeat(2332), "too long"), null,
    "no symbol at all, so the caller keeps its text rather than showing a broken one");
  assert.ok(qrSvg("a".repeat(2331), "the largest there is") !== null);
});

// -------------------------------------------------------------- on the screens

type View = import("../src/api.js").InvoiceView;
type Rec = import("../src/domain.js").OrderRecord;

const NOW = Date.parse("2026-08-28T12:00:00Z");
const noop = (): void => {};
const noopAsync = (): Promise<void> => Promise.resolve();

function record(over: Partial<Rec> = {}): Rec {
  return {
    orderId: "inv_9f3a", badgeType: "legend", months: 12,
    createdAt: "2026-08-28T11:46:00Z", status: "open", ...over,
  };
}

const xmrInvoice: View = {
  status: "open", badgeType: "legend", months: 12, amount: 42000,
  currency: "usd", expiresAt: "2026-08-28T12:58:12Z",
  address: MONERO_ADDRESS, cryptoAmount: "1.482", cryptoCurrency: "xmr",
};

/** The payment screen handed an order that still carries its code, as a caller that forgot to strip would
 * pass, `OrderRecord` being assignable to `UnpaidOrder`. A screen must not surface a code even when handed
 * one, and a QR is the form of surfacing that no text search would notice. */
function awaitingPayment(invoice: View = xmrInvoice, method: "btc" | "xmr" = "xmr"): StubElement {
  const built = screens.awaitingPayment({
    order: record({ code: BADGE_CODE }), invoice, method, nowMs: NOW,
    resumed: false, onNewInvoice: noop, onCancel: noopAsync,
  });
  // The rate countdown ticks. Nothing here reads it, and a live interval
  // would hold the test process open, so it is stopped as soon as it is built.
  built.stop();
  return built.node as unknown as StubElement;
}

qrTest("the payment screen: the QR is the payment URI, and it decodes to the address and the amount", () => {
  const panel = awaitingPayment();
  const svg = panel.querySelector("svg.qr");
  assert.ok(svg !== null, "the payment screen draws a QR");
  const decoded = decodeQr(modulesOfSvg(svg));
  assert.equal(decoded.payload, `monero:${MONERO_ADDRESS}?tx_amount=1.482`,
    "a wrong payload here sends the money somewhere else");
  // A bare address would not prefill the amount, which is the reason the payment-URI rule gives
  // for encoding a URI at all.
  assert.notEqual(decoded.payload, MONERO_ADDRESS);
});

qrTest("the payment screen: Bitcoin gets BIP-21 with `amount`, Monero `tx_amount`", () => {
  const btc = awaitingPayment({ ...xmrInvoice, address: BITCOIN_ADDRESS, cryptoAmount: "0.00412", cryptoCurrency: "btc" }, "btc");
  const decoded = decodeQr(modulesOfSvg(btc.querySelector("svg.qr")!));
  assert.equal(decoded.payload, `bitcoin:${BITCOIN_ADDRESS}?amount=0.00412`);
});

qrTest("the payment screen: with no address there is no QR, and the screen keeps its text", () => {
  const { address: _noAddress, ...withoutAddress } = xmrInvoice;
  const panel = awaitingPayment(withoutAddress);
  assert.equal(panel.querySelector("svg.qr"), null, "half a destination is not scannable");
  assert.ok(panel.texts.some((t) => t.includes("Waiting for the payment to confirm")),
    "and the rest of the screen is unaffected");
});

qrTest("the code screen: the QR is the code, and the code screen is the only screen that has one", () => {
  const panel = screens.codeIssued({ code: BADGE_CODE, savedLocally: true }) as unknown as StubElement;
  const svg = panel.querySelector("svg.qr");
  assert.ok(svg !== null, "support reconciliation: with a QR, scan to carry it to your phone");
  assert.equal(decodeQr(modulesOfSvg(svg)).payload, BADGE_CODE);
  assert.ok(panel.texts.includes("scan to carry it to your phone"));
});

qrTest("only the code screen draws a QR of the code, and every other screen draws none", () => {
  const held = record({ code: BADGE_CODE });
  const unpaid = held as import("../src/order.js").UnpaidOrder;
  const panels: Array<[string, StubElement]> = [
    ["awaitingPayment", awaitingPayment()],
    ["awaitingConfirmation", screens.awaitingConfirmation({ invoice: undefined, method: undefined, order: unpaid, gaveUp: false, onCheckAgain: noop }) as unknown as StubElement],
    ["the confirming screen gave up", screens.awaitingConfirmation({ invoice: undefined, method: undefined, order: unpaid, gaveUp: true, onCheckAgain: noop }) as unknown as StubElement],
    ["windowClosed", screens.windowClosed({ order: unpaid, invoice: xmrInvoice, onNewInvoice: noop }) as unknown as StubElement],
    ["paidNoCode", screens.paidNoCode({ order: unpaid, settledAt: undefined }) as unknown as StubElement],
    ["the card form", screens.cardForm({ order: unpaid, invoice: xmrInvoice, resumed: false, onNewInvoice: noop }) as unknown as StubElement],
  ];
  const rows = order.historyRows([record({ status: "open", code: BADGE_CODE }), record({ orderId: "inv_2", status: "expired", code: BADGE_CODE })]);
  panels.push(["the history list", screens.purchaseHistory({ keepsNewCodes: true, rows, onOpen: noop, onStart: noop }) as unknown as StubElement]);

  for (const [where, panel] of panels) {
    // the payment screen draws one, and it is the payment URI: the ban is on the CODE, and
    // the only way to know which a symbol carries is to decode it. Every other
    // screen here draws nothing at all.
    for (const svg of panel.all("svg.qr")) {
      assert.equal(where, "awaitingPayment", `${where} drew a QR while its order is unpaid`);
      const payload = decodeQr(modulesOfSvg(svg)).payload;
      assert.ok(payload.startsWith("monero:"), `the payment screen's QR must be the payment URI, not ${payload.slice(0, 40)}`);
      assert.ok(!payload.includes("SXB"), "and never the code");
    }
    // Every OTHER symbol on the screen is artwork, be it the logo, a badge or a
    // payment mark, and artwork is decorative by construction: it is built
    // from a fixed enum, so it can hold no order data, and it must not be
    // announced, or a screen reader would read the page's furniture aloud.
    for (const art of panel.all("svg").filter((s) => (s.getAttribute("class") ?? "") !== "qr")) {
      assert.equal(art.getAttribute("aria-hidden"), "true",
        `${where} draws a symbol that is neither the QR nor hidden: ${art.serialize().slice(0, 120)}`);
      assert.equal(art.getAttribute("aria-label"), null, "and artwork never carries a name");
    }
    const dump = panel.serialize();
    for (const form of [BADGE_CODE, BADGE_CODE.replace(/-/g, ""), "SXB-"]) {
      assert.ok(!dump.includes(form), `${where} leaked a code (${form})`);
    }
  }

  // And the one screen that may: its symbol decodes to the code itself.
  const codeIssued = screens.codeIssued({ code: BADGE_CODE, savedLocally: true }) as unknown as StubElement;
  assert.equal(codeIssued.all("svg.qr").length, 1, "the code screen draws exactly one");
  assert.equal(decodeQr(modulesOfSvg(codeIssued.all("svg.qr")[0]!)).payload, BADGE_CODE);
});

qrTest("qr: an address carrying URI syntax cannot add parameters of its own", () => {
  // `?` and `&` in an address would otherwise put a second `amount` in front of ours, and most
  // wallets read the first they are given; `#` swallows everything after it.
  const withQuery = paymentUri("btc", "bc1q?amount=99&x=1", "0.5")!;
  assert.ok(!withQuery.includes("amount=99"), `the address must not carry parameters: ${withQuery}`);
  assert.ok(withQuery.endsWith("?amount=0.5"), `ours is the only one: ${withQuery}`);
  const withFragment = paymentUri("xmr", "4A#frag", "0.5")!;
  assert.ok(!withFragment.includes("#"), `a fragment would swallow the amount: ${withFragment}`);
  // a real address is alphanumeric, so the encoding leaves it exactly as it was
  assert.equal(paymentUri("xmr", MONERO_ADDRESS, "1.482"), `monero:${MONERO_ADDRESS}?tx_amount=1.482`);
});
