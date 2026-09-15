// A QR encoder for byte mode at error correction level M, written here rather than taken from a
// dependency: the symbol is computed from the payload the page already holds, never fetched and
// never a raster. Its tables come from the published QR spec, and `test/qr.test.ts` holds its own
// copies of them and decodes what this builds.

const MODE_BYTE = 0b0100;
const ECC_LEVEL_M_BITS = 0b00;
const MIN_VERSION = 1;
const MAX_VERSION = 40;

const ECC_PER_BLOCK_M: readonly number[] = [
  -1, 10, 16, 26, 18, 24, 16, 18, 22, 22, 26, 30, 22, 22, 24, 24, 28, 28, 26, 26, 26,
  26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
];

const NUM_BLOCKS_M: readonly number[] = [
  -1, 1, 1, 1, 2, 2, 4, 4, 4, 5, 5, 5, 8, 9, 9, 10, 10, 11, 13, 14, 16,
  17, 17, 18, 20, 21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49,
];

export interface QrSymbol {
  version: number;
  size: number;
  mask: number;
  modules: boolean[][];
}

const GF_PRIMITIVE = 0x11d;

const GF_EXP = new Uint8Array(512);
const GF_LOG = new Uint8Array(256);
{
  let x = 1;
  for (let i = 0; i < 255; i++) {
    GF_EXP[i] = x;
    GF_LOG[x] = i;
    x <<= 1;
    if ((x & 0x100) !== 0) x ^= GF_PRIMITIVE;
  }
  for (let i = 255; i < 512; i++) GF_EXP[i] = GF_EXP[i - 255]!;
}

function gfMul(a: number, b: number): number {
  if (a === 0 || b === 0) return 0;
  return GF_EXP[GF_LOG[a]! + GF_LOG[b]!]!;
}

export function generatorPoly(degree: number): number[] {
  let poly = [1];
  for (let i = 0; i < degree; i++) {
    const next = new Array<number>(poly.length + 1).fill(0);
    for (let j = 0; j < poly.length; j++) {
      next[j] = next[j]! ^ poly[j]!;
      next[j + 1] = next[j + 1]! ^ gfMul(poly[j]!, GF_EXP[i]!);
    }
    poly = next;
  }
  return poly;
}

export function eccOf(data: readonly number[], eccLen: number): number[] {
  const gen = generatorPoly(eccLen);
  const rem = new Array<number>(eccLen).fill(0);
  for (const byte of data) {
    const factor = byte ^ rem[0]!;
    rem.shift();
    rem.push(0);
    for (let i = 0; i < eccLen; i++) rem[i] = rem[i]! ^ gfMul(gen[i + 1]!, factor);
  }
  return rem;
}

export function rawDataModules(version: number): number {
  let bits = (16 * version + 128) * version + 64;
  if (version >= 2) {
    const numAlign = Math.floor(version / 7) + 2;
    bits -= (25 * numAlign - 10) * numAlign - 55;
    if (version >= 7) bits -= 36;
  }
  return bits;
}

function totalCodewords(version: number): number {
  return Math.floor(rawDataModules(version) / 8);
}

export function numDataCodewords(version: number): number {
  return totalCodewords(version) - ECC_PER_BLOCK_M[version]! * NUM_BLOCKS_M[version]!;
}

function charCountBits(version: number): number {
  return version < 10 ? 8 : 16;
}

export function byteCapacity(version: number): number {
  return Math.floor((numDataCodewords(version) * 8 - 4 - charCountBits(version)) / 8);
}

function versionFor(byteLength: number): number | null {
  for (let version = MIN_VERSION; version <= MAX_VERSION; version++) {
    if (byteLength <= byteCapacity(version)) return version;
  }
  return null;
}

// Computed rather than tabulated. Version 32 is the exception below, because the general
// rule gets its spacing wrong.
export function alignmentPositions(version: number): number[] {
  if (version === 1) return [];
  const numAlign = Math.floor(version / 7) + 2;
  const step = version === 32 ? 26 : Math.ceil((version * 4 + 4) / (numAlign * 2 - 2)) * 2;
  const positions = [6];
  for (let pos = 4 * version + 10; positions.length < numAlign; pos -= step) positions.splice(1, 0, pos);
  return positions;
}

export function dataCodewords(bytes: Uint8Array, version: number): number[] {
  const capacity = numDataCodewords(version);
  const bits: number[] = [];
  const push = (value: number, width: number): void => {
    for (let i = width - 1; i >= 0; i--) bits.push((value >>> i) & 1);
  };
  push(MODE_BYTE, 4);
  push(bytes.length, charCountBits(version));
  for (const byte of bytes) push(byte, 8);
  for (let i = 0; i < 4 && bits.length < capacity * 8; i++) bits.push(0);
  while (bits.length % 8 !== 0) bits.push(0);
  const codewords: number[] = [];
  for (let i = 0; i < bits.length; i += 8) {
    let byte = 0;
    for (let j = 0; j < 8; j++) byte = (byte << 1) | bits[i + j]!;
    codewords.push(byte);
  }
  for (let pad = 0xec; codewords.length < capacity; pad ^= 0xec ^ 0x11) codewords.push(pad);
  return codewords;
}

export function interleave(data: readonly number[], version: number): number[] {
  const numBlocks = NUM_BLOCKS_M[version]!;
  const eccLen = ECC_PER_BLOCK_M[version]!;
  const shortLen = Math.floor(data.length / numBlocks);
  const numShort = numBlocks - (data.length % numBlocks);
  const blocks: Array<{ data: number[]; ecc: number[] }> = [];
  let at = 0;
  for (let b = 0; b < numBlocks; b++) {
    const len = b < numShort ? shortLen : shortLen + 1;
    const block = data.slice(at, at + len);
    at += len;
    blocks.push({ data: block, ecc: eccOf(block, eccLen) });
  }
  const out: number[] = [];
  for (let i = 0; i <= shortLen; i++) {
    for (const block of blocks) if (i < block.data.length) out.push(block.data[i]!);
  }
  for (let i = 0; i < eccLen; i++) for (const block of blocks) out.push(block.ecc[i]!);
  return out;
}

export function formatBits(eclBits: number, mask: number): number {
  const data = (eclBits << 3) | mask;
  let rem = data;
  for (let i = 0; i < 10; i++) rem = (rem << 1) ^ ((rem >>> 9) * 0x537);
  return ((data << 10) | rem) ^ 0x5412;
}

export function versionBits(version: number): number {
  let rem = version;
  for (let i = 0; i < 12; i++) rem = (rem << 1) ^ ((rem >>> 11) * 0x1f25);
  return (version << 12) | rem;
}

const MASKS: ReadonlyArray<(x: number, y: number) => boolean> = [
  (x, y) => (x + y) % 2 === 0,
  (_x, y) => y % 2 === 0,
  (x, _y) => x % 3 === 0,
  (x, y) => (x + y) % 3 === 0,
  (x, y) => (Math.floor(x / 3) + Math.floor(y / 2)) % 2 === 0,
  (x, y) => ((x * y) % 2) + ((x * y) % 3) === 0,
  (x, y) => (((x * y) % 2) + ((x * y) % 3)) % 2 === 0,
  (x, y) => (((x + y) % 2) + ((x * y) % 3)) % 2 === 0,
];

const N1 = 3;
const N2 = 3;
const N3 = 40;
const N4 = 10;

class Matrix {
  readonly size: number;
  readonly modules: boolean[][];
  private readonly reserved: boolean[][];

  constructor(readonly version: number) {
    this.size = version * 4 + 17;
    this.modules = Array.from({ length: this.size }, () => new Array<boolean>(this.size).fill(false));
    this.reserved = Array.from({ length: this.size }, () => new Array<boolean>(this.size).fill(false));
  }

  private set(x: number, y: number, dark: boolean): void {
    this.modules[y]![x] = dark;
    this.reserved[y]![x] = true;
  }

  isReserved(x: number, y: number): boolean {
    return this.reserved[y]![x]!;
  }

  drawFunctionPatterns(): void {
    for (const [cx, cy] of [[3, 3], [this.size - 4, 3], [3, this.size - 4]] as const) {
      for (let dy = -4; dy <= 4; dy++) {
        for (let dx = -4; dx <= 4; dx++) {
          const x = cx + dx;
          const y = cy + dy;
          if (x < 0 || x >= this.size || y < 0 || y >= this.size) continue;
          const ring = Math.max(Math.abs(dx), Math.abs(dy));
          this.set(x, y, ring !== 2 && ring <= 3);
        }
      }
    }
    for (let i = 0; i < this.size; i++) {
      if (!this.reserved[6]![i]!) this.set(i, 6, i % 2 === 0);
      if (!this.reserved[i]![6]!) this.set(6, i, i % 2 === 0);
    }
    const positions = alignmentPositions(this.version);
    for (const cy of positions) {
      for (const cx of positions) {
        if ((cx === 6 && cy === 6) || (cx === 6 && cy === this.size - 7) || (cx === this.size - 7 && cy === 6)) continue;
        for (let dy = -2; dy <= 2; dy++) {
          for (let dx = -2; dx <= 2; dx++) {
            this.set(cx + dx, cy + dy, Math.max(Math.abs(dx), Math.abs(dy)) !== 1);
          }
        }
      }
    }
    for (let i = 0; i < 9; i++) {
      if (!this.reserved[i]![8]!) this.set(8, i, false);
      if (!this.reserved[8]![i]!) this.set(i, 8, false);
    }
    for (let i = 0; i < 8; i++) {
      this.set(this.size - 1 - i, 8, false);
      this.set(8, this.size - 1 - i, false);
    }
    this.set(8, this.size - 8, true);
    if (this.version >= 7) this.drawVersionBlocks();
  }

  private drawVersionBlocks(): void {
    const bits = versionBits(this.version);
    for (let i = 0; i < 18; i++) {
      const dark = ((bits >>> i) & 1) !== 0;
      const a = this.size - 11 + (i % 3);
      const b = Math.floor(i / 3);
      this.set(a, b, dark);
      this.set(b, a, dark);
    }
  }

  drawFormat(mask: number): void {
    const bits = formatBits(ECC_LEVEL_M_BITS, mask);
    const bit = (i: number): boolean => ((bits >>> i) & 1) !== 0;
    for (let i = 0; i <= 5; i++) this.set(8, i, bit(i));
    this.set(8, 7, bit(6));
    this.set(8, 8, bit(7));
    this.set(7, 8, bit(8));
    for (let i = 9; i < 15; i++) this.set(14 - i, 8, bit(i));
    for (let i = 0; i < 8; i++) this.set(this.size - 1 - i, 8, bit(i));
    for (let i = 8; i < 15; i++) this.set(8, this.size - 15 + i, bit(i));
    this.set(8, this.size - 8, true);
  }

  drawCodewords(codewords: readonly number[]): void {
    let bit = 0;
    const total = codewords.length * 8;
    for (let right = this.size - 1; right >= 1; right -= 2) {
      if (right === 6) right = 5;
      for (let vert = 0; vert < this.size; vert++) {
        for (let j = 0; j < 2; j++) {
          const x = right - j;
          const upward = ((right + 1) & 2) === 0;
          const y = upward ? this.size - 1 - vert : vert;
          if (this.reserved[y]![x]! || bit >= total) continue;
          this.modules[y]![x] = ((codewords[bit >>> 3]! >>> (7 - (bit & 7))) & 1) !== 0;
          bit++;
        }
      }
    }
  }

  applyMask(mask: number): void {
    const rule = MASKS[mask]!;
    for (let y = 0; y < this.size; y++) {
      for (let x = 0; x < this.size; x++) {
        if (this.reserved[y]![x]!) continue;
        if (rule(x, y)) this.modules[y]![x] = !this.modules[y]![x]!;
      }
    }
  }

  penalty(): number {
    const m = this.modules;
    const n = this.size;
    let score = 0;
    for (let i = 0; i < n; i++) {
      let runRow = 1;
      let runCol = 1;
      for (let j = 1; j < n; j++) {
        if (m[i]![j] === m[i]![j - 1]) {
          runRow++;
          if (runRow === 5) score += N1;
          else if (runRow > 5) score += 1;
        } else runRow = 1;
        if (m[j]![i] === m[j - 1]![i]) {
          runCol++;
          if (runCol === 5) score += N1;
          else if (runCol > 5) score += 1;
        } else runCol = 1;
      }
    }
    for (let y = 0; y < n - 1; y++) {
      for (let x = 0; x < n - 1; x++) {
        const c = m[y]![x]!;
        if (c === m[y]![x + 1] && c === m[y + 1]![x] && c === m[y + 1]![x + 1]) score += N2;
      }
    }
    const at = (x: number, y: number): boolean => (x < 0 || y < 0 || x >= n || y >= n ? false : m[y]![x]!);
    const RUN = [true, false, true, true, true, false, true, false, false, false, false];
    for (let y = 0; y < n; y++) {
      for (let x = 0; x < n; x++) {
        let forward = true;
        let backward = true;
        for (let k = 0; k < RUN.length; k++) {
          if (at(x + k, y) !== RUN[k]!) forward = false;
          if (at(x + RUN.length - 1 - k, y) !== RUN[k]!) backward = false;
        }
        if (forward) score += N3;
        if (backward) score += N3;
        forward = true;
        backward = true;
        for (let k = 0; k < RUN.length; k++) {
          if (at(x, y + k) !== RUN[k]!) forward = false;
          if (at(x, y + RUN.length - 1 - k) !== RUN[k]!) backward = false;
        }
        if (forward) score += N3;
        if (backward) score += N3;
      }
    }
    let dark = 0;
    for (let y = 0; y < n; y++) for (let x = 0; x < n; x++) if (m[y]![x]!) dark++;
    const total = n * n;
    score += Math.floor(Math.abs(dark * 20 - total * 10) / total) * N4;
    return score;
  }
}

export function encodeQr(payload: string): QrSymbol | null {
  const bytes = new TextEncoder().encode(payload);
  const version = versionFor(bytes.length);
  if (version === null) return null;
  const matrix = new Matrix(version);
  matrix.drawFunctionPatterns();
  matrix.drawCodewords(interleave(dataCodewords(bytes, version), version));

  let best = -1;
  let bestPenalty = Infinity;
  for (let mask = 0; mask < MASKS.length; mask++) {
    matrix.applyMask(mask);
    matrix.drawFormat(mask);
    const score = matrix.penalty();
    if (score < bestPenalty) {
      bestPenalty = score;
      best = mask;
    }
    matrix.applyMask(mask);
  }
  matrix.applyMask(best);
  matrix.drawFormat(best);
  return { version, size: matrix.size, mask: best, modules: matrix.modules };
}

// A payment URI rather than a bare address, because a bare address does not prefill an
// amount.
export function paymentUri(method: "btc" | "xmr", address: string, amount: string): string | null {
  if (address === "" || amount === "") return null;
  const scheme = method === "btc" ? "bitcoin" : "monero";
  const parameter = method === "btc" ? "amount" : "tx_amount";
  // the address is encoded too: one carrying `?`, `&` or `#` would otherwise add its own
  // parameters to this URI, and most wallets read the first `amount` they are given
  return `${scheme}:${encodeURIComponent(address)}?${parameter}=${encodeURIComponent(amount)}`;
}

const SVG_NS = "http://www.w3.org/2000/svg";
const QUIET_ZONE = 4;

function pathOf(symbol: QrSymbol): string {
  let d = "";
  for (let y = 0; y < symbol.size; y++) {
    const row = symbol.modules[y]!;
    let x = 0;
    while (x < symbol.size) {
      if (!row[x]!) {
        x++;
        continue;
      }
      let run = 1;
      while (x + run < symbol.size && row[x + run]!) run++;
      d += `M${x + QUIET_ZONE} ${y + QUIET_ZONE}h${run}v1h-${run}z`;
      x += run;
    }
  }
  return d;
}

// SVG elements use createElementNS: createElement("svg") in an HTML document produces an
// HTMLUnknownElement that draws nothing. `label` is the accessible name and never the
// payload, since on the code screen the payload is a badge code.
export function qrSvg(payload: string, label: string): SVGElement | null {
  const symbol = encodeQr(payload);
  if (symbol === null) return null;
  const span = symbol.size + QUIET_ZONE * 2;
  const svg = document.createElementNS(SVG_NS, "svg");
  svg.setAttribute("class", "qr");
  svg.setAttribute("viewBox", `0 0 ${span} ${span}`);
  svg.setAttribute("role", "img");
  svg.setAttribute("aria-label", label);
  svg.setAttribute("shape-rendering", "crispEdges");
  const background = document.createElementNS(SVG_NS, "rect");
  background.setAttribute("width", String(span));
  background.setAttribute("height", String(span));
  background.setAttribute("fill", "#ffffff");
  const dark = document.createElementNS(SVG_NS, "path");
  dark.setAttribute("d", pathOf(symbol));
  dark.setAttribute("fill", "#000000");
  svg.append(background, dark);
  return svg;
}
