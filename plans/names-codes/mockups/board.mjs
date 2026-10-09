#!/usr/bin/env node
// Renders every frame of layout.mjs to ../screens/<tag>.jpg and composes ../names-flow.svg around them.
// Needs `npm run build` in apps/simplex-badge-service/web first, and Playwright with Chromium, which is
// deliberately not a dependency of the webapp:
//   npm install --prefix /tmp/pw playwright && npx --prefix /tmp/pw playwright install chromium
//   PLAYWRIGHT=/tmp/pw/node_modules/playwright/index.mjs node plans/names-codes/mockups/board.mjs
import { cpSync, existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { extname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { CORNER, FRAMES, NOTES, SECTIONS, SUBTITLE, TITLE } from "./layout.mjs";

const HERE = fileURLToPath(new URL(".", import.meta.url));
const OUT = join(HERE, "..");
const SCREENS_DIR = join(OUT, "screens");
const DIST = join(HERE, "../../../apps/simplex-badge-service/web/dist/assets");
// A made-up origin: module scripts do not load from file://, and intercepting requests needs no server.
const ORIGIN = "http://mockups.local";
// JPEG, not PNG: the page's background wash makes a PNG about four times larger, and the board embeds 49 of them.
const JPEG_QUALITY = 85;
const CONTENT_TYPES = { ".html": "text/html", ".js": "text/javascript", ".css": "text/css", ".png": "image/png", ".svg": "image/svg+xml", ".woff2": "font/woff2" };

const KINDS = {
  desktop: { viewport: { width: 1024, height: 900 }, scale: 400 / 1024, pitch: 500 },
  phone: { viewport: { width: 390, height: 844 }, scale: 200 / 390, pitch: 300 },
  // on the desktop grid, so the arrows running down between desktop columns pass between its frames too
  app: { viewport: { width: 800, height: 900 }, scale: 200 / 390, pitch: 500, element: true },
  operator: { viewport: { width: 900, height: 900 }, scale: 0.58, pitch: 560, element: true },
};

const COLORS = { blue: "#0053D0", orange: "#E8871E", grey: "#9AA0A6" };
const FONT = "Manrope, -apple-system, 'Segoe UI', Helvetica, Arial, sans-serif";
const MARGIN = 60;
const DESKTOP_BAR = 26;
const PHONE_PAD = 8;
const PHONE_BAR = 16;
const ROW_HEAD = 84;
const LANE_STEP = 7;
const LANES = 7;

const PAGE = (id, theme) => `<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<link rel="stylesheet" href="assets/styles.css"><link rel="stylesheet" href="mockups.css"></head>
<body><div id="chrome"></div><main id="app"></main><footer id="contact"><a href="#">simplex.chat/contact</a></footer>
<script type="module">
import { render } from "./screens.js";
try { render(${JSON.stringify(id)}, ${theme ? JSON.stringify(theme) : "undefined"}); document.body.dataset.ready = "1"; }
catch (e) { document.body.dataset.error = String(e && e.stack || e); }
</script></body></html>`;

function buildHash() {
  if (!existsSync(DIST)) throw new Error(`board: ${DIST} is missing - run npm run build in apps/simplex-badge-service/web`);
  const builds = readdirSync(DIST);
  if (builds.length !== 1) throw new Error(`board: expected one build in ${DIST}, found ${builds.length}`);
  return join(DIST, builds[0]);
}

function frameList() {
  const list = [];
  SECTIONS.forEach((section) => section.rows.forEach((row) => row.cells.forEach((tag) => {
    if (tag === null) return;
    if (FRAMES[tag] === undefined) throw new Error(`board: ${tag} is placed but has no entry in FRAMES`);
    list.push({ tag, kind: row.kind, ...FRAMES[tag] });
  })));
  return list;
}

async function renderAll(frames) {
  const { chromium } = await import(process.env.PLAYWRIGHT ?? "playwright");
  const site = mkdtempSync(join(tmpdir(), "names-board-"));
  try {
    cpSync(buildHash(), join(site, "assets"), { recursive: true });
    cpSync(join(HERE, "screens.js"), join(site, "screens.js"));
    cpSync(join(HERE, "mockups.css"), join(site, "mockups.css"));
    rmSync(SCREENS_DIR, { recursive: true, force: true });
    mkdirSync(SCREENS_DIR, { recursive: true });
    const browser = await chromium.launch();
    try {
      for (const f of frames) {
        const kind = KINDS[f.kind];
        const page = await browser.newPage({ viewport: kind.viewport, colorScheme: f.theme === "dark" ? "dark" : "light" });
        try {
          await page.route(`${ORIGIN}/**`, (route) => {
            const path = new URL(route.request().url()).pathname;
            const body = path === "/page.html" ? PAGE(f.screen ?? f.tag, f.theme) : readFileOr(join(site, path));
            if (body === undefined) return route.fulfill({ status: 404, body: "" });
            return route.fulfill({ status: 200, contentType: CONTENT_TYPES[extname(path)] ?? "application/octet-stream", body });
          });
          await page.goto(`${ORIGIN}/page.html`);
          await page.waitForFunction(() => document.body.dataset.ready || document.body.dataset.error);
          const error = await page.evaluate(() => document.body.dataset.error);
          if (error) throw new Error(`board: ${f.tag} failed to render: ${error}`);
          await page.evaluate(() => document.fonts.ready);
          const file = join(SCREENS_DIR, `${f.tag}.jpg`);
          // animations frozen, or the waiting dot's pulse changes the bytes on every run
          const shot = { path: file, type: "jpeg", quality: JPEG_QUALITY, animations: "disabled", caret: "hide" };
          if (kind.element) await page.locator("#shot").screenshot(shot);
          else await page.screenshot({ ...shot, fullPage: true });
          f.jpeg = readFileSync(file);
          f.natural = jpegSize(f.jpeg);
        } finally {
          await page.close();
        }
      }
    } finally {
      await browser.close();
    }
  } finally {
    rmSync(site, { recursive: true, force: true });
  }
}

// The width and height from the first start-of-frame segment (SOF0 to SOF15, except DHT, JPG and DAC).
function jpegSize(bytes) {
  let at = 2;
  while (at < bytes.length) {
    const marker = bytes[at + 1];
    const length = bytes.readUInt16BE(at + 2);
    if (marker >= 0xc0 && marker <= 0xcf && ![0xc4, 0xc8, 0xcc].includes(marker)) {
      return { height: bytes.readUInt16BE(at + 5), width: bytes.readUInt16BE(at + 7) };
    }
    at += 2 + length;
  }
  throw new Error("board: no start-of-frame segment in a screenshot");
}

function readFileOr(path) {
  return existsSync(path) ? readFileSync(path) : undefined;
}

function esc(s) {
  return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
}

function wrap(text, chars) {
  const lines = [];
  let line = "";
  for (const word of text.split(/\s+/)) {
    if (line !== "" && line.length + 1 + word.length > chars) { lines.push(line); line = word; }
    else line = line === "" ? word : `${line} ${word}`;
  }
  if (line !== "") lines.push(line);
  return lines;
}

// Cut to fit a frame's address bar; a monospace character is about 0.6 of the font size.
function fit(s, width, size) {
  const chars = Math.floor(width / (size * 0.6));
  return s.length <= chars ? s : `${s.slice(0, chars - 1)}…`;
}

function text(x, y, s, size, attrs = "") {
  return `<text x="${x}" y="${y}" font-size="${size}" ${attrs}>${esc(s)}</text>`;
}

function image(f, x, y, w, h) {
  // xlink:href alone: SVG 2 viewers read it too, and naming the data twice doubles the file
  return `<image x="${x}" y="${y}" width="${w}" height="${h}" preserveAspectRatio="none" xlink:href="data:image/jpeg;base64,${f.jpeg.toString("base64")}"/>`;
}

// The outer box of a frame with its screen inside, by kind.
function frameBox(f) {
  const kind = KINDS[f.kind];
  const w = Math.round(f.natural.width * kind.scale);
  const h = Math.round(f.natural.height * kind.scale);
  if (f.kind === "desktop") return { w, h: h + DESKTOP_BAR, img: { dx: 0, dy: DESKTOP_BAR, w, h } };
  if (f.kind === "phone" || f.kind === "app") return { w: w + 2 * PHONE_PAD, h: h + 2 * PHONE_PAD + PHONE_BAR, img: { dx: PHONE_PAD, dy: PHONE_PAD + PHONE_BAR, w, h } };
  return { w, h, img: { dx: 0, dy: 0, w, h } };
}

function drawFrame(f) {
  const { x, y, w, h, img } = f.box;
  const out = [];
  if (f.kind === "desktop") {
    out.push(`<rect x="${x}" y="${y}" width="${w}" height="${h}" rx="6" fill="#ffffff" stroke="#D0D5DD"/>`);
    out.push(`<rect x="${x + 0.5}" y="${y + 0.5}" width="${w - 1}" height="${DESKTOP_BAR}" rx="5" fill="#EEF0F3"/>`);
    ["#FF5F57", "#FEBC2E", "#28C840"].forEach((c, i) => out.push(`<circle cx="${x + 12 + i * 11}" cy="${y + 13}" r="3.6" fill="${c}"/>`));
    out.push(`<rect x="${x + 52}" y="${y + 5}" width="${w - 64}" height="16" rx="8" fill="#ffffff"/>`);
    if (f.url) out.push(text(x + 62, y + 16.5, fit(f.url, w - 84, 9.5), 9.5, `fill="#5F6368" font-family="ui-monospace, Menlo, Consolas, monospace"`));
  } else if (f.kind === "phone" || f.kind === "app") {
    const dashed = f.kind === "app" ? ` stroke-dasharray="6 5"` : "";
    const stroke = f.kind === "app" ? COLORS.grey : "#1F2328";
    out.push(`<rect x="${x}" y="${y}" width="${w}" height="${h}" rx="24" fill="#ffffff" stroke="${stroke}" stroke-width="2.5"${dashed}/>`);
    const label = f.kind === "app" ? "SimpleX app" : fit(f.url ?? "", w - 24, 8.5);
    out.push(text(x + w / 2, y + PHONE_PAD + 11, label, 8.5, `text-anchor="middle" fill="#5F6368" font-family="ui-monospace, Menlo, Consolas, monospace"`));
  } else {
    out.push(`<rect x="${x - 1}" y="${y - 1}" width="${w + 2}" height="${h + 2}" rx="10" fill="none" stroke="#D0D5DD"/>`);
  }
  out.push(image(f, x + img.dx, y + img.dy, img.w, img.h));
  return out.join("\n");
}

function captionLines(f) {
  return wrap(f.text, Math.max(24, Math.floor(f.box.w / 6.3)));
}

function drawCaption(f) {
  const { x, y, h } = f.box;
  const top = y + h + 26;
  const out = [text(x, top, `${f.tag}. ${f.title}`, 16, `font-weight="700" fill="#111827"`)];
  captionLines(f).forEach((line, i) => out.push(text(x, top + 20 + i * 16, line, 12.5, `fill="#4B5563"`)));
  return out.join("\n");
}

function layout(frames) {
  const byTag = new Map(frames.map((f) => [f.tag, f]));
  const parts = { sections: [], rows: [] };
  let y = 150;
  for (const section of SECTIONS) {
    parts.sections.push({ section, y });
    y += 64;
    for (const row of section.rows) {
      const rowInfo = { row, top: y, lanes: 0 };
      parts.rows.push(rowInfo);
      let tallest = 0;
      row.cells.forEach((tag, col) => {
        if (tag === null) return;
        const f = byTag.get(tag);
        const box = frameBox(f);
        f.box = { x: MARGIN + col * KINDS[row.kind].pitch, y: y + ROW_HEAD, ...box };
        f.row = rowInfo;
        f.col = col;
        tallest = Math.max(tallest, box.h + 34 + captionLines(f).length * 16);
      });
      y += ROW_HEAD + tallest + 56;
    }
  }
  return { byTag, parts, height: y };
}

function arrows(frames, byTag) {
  const out = [];
  const outgoing = new Map();
  for (const t of frames) {
    if (t.from === undefined) continue;
    const s = byTag.get(t.from);
    if (s === undefined) throw new Error(`board: ${t.tag} comes from ${t.from}, which is not on the board`);
    const i = outgoing.get(s.tag) ?? 0;
    outgoing.set(s.tag, i + 1);
    const sx = s.box.x + s.box.w;
    const sy = s.box.y + 46 + i * 12;
    const ty = t.box.y + 46;
    const cx = t.box.x - 26;
    const adjacent = s.row === t.row && t.col > s.col && s.row.row.cells.slice(s.col + 1, t.col).every((c) => c === null);
    let d;
    if (adjacent) d = `M${sx},${sy} H${cx} V${ty} H${t.box.x - 3}`;
    else {
      const lane = t.row.lanes++ % LANES;
      const gx = sx + 20 + i * 7;
      const by = t.row.top + 24 + lane * LANE_STEP;
      d = `M${sx},${sy} H${gx} V${by} H${cx} V${ty} H${t.box.x - 3}`;
    }
    const color = t.color ?? "grey";
    const dash = t.dashed ? ` stroke-dasharray="9 6"` : "";
    out.push(`<path d="${d}" fill="none" stroke="${COLORS[color]}" stroke-width="2"${dash} marker-end="url(#arrow-${color})"/>`);
  }
  return out.join("\n");
}

function arrowLabels(frames) {
  return frames.map((f) => {
    if (f.label === undefined) return "";
    const color = f.from === undefined ? "#8A8F98" : COLORS[f.color ?? "grey"];
    return text(f.box.x, f.box.y - 12, f.label, 13, `font-weight="700" fill="${color}"`);
  }).join("\n");
}

function compose(frames) {
  const { byTag, parts, height } = layout(frames);
  const width = Math.max(...frames.map((f) => f.box.x + f.box.w)) + MARGIN + 40;
  const notesTop = height + 30;
  const noteLines = NOTES.flatMap((n) => wrap(n, Math.floor((width - 2 * MARGIN) / 7.2)));
  const total = notesTop + 40 + noteLines.length * 20 + 50;
  const out = [];
  out.push(`<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="${width}" height="${total}" viewBox="0 0 ${width} ${total}" font-family="${FONT}">`);
  out.push(`<defs>${Object.entries(COLORS).map(([name, c]) =>
    `<marker id="arrow-${name}" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="${c}"/></marker>`).join("")}</defs>`);
  out.push(`<rect width="${width}" height="${total}" fill="#ffffff"/>`);
  out.push(`<rect width="${width}" height="120" fill="#1D2026"/>`);
  out.push(text(MARGIN, 58, TITLE, 34, `font-weight="700" fill="#ffffff"`));
  out.push(text(MARGIN, 90, SUBTITLE, 15, `fill="#B9BEC7"`));
  CORNER.forEach((line, i) => out.push(text(width - MARGIN, 52 + i * 20, line, 12, `text-anchor="end" fill="#8A8F98"`)));
  for (const { section, y } of parts.sections) {
    out.push(`<line x1="${MARGIN}" y1="${y}" x2="${width - MARGIN}" y2="${y}" stroke="#E5E7EB"/>`);
    out.push(text(MARGIN, y + 40, section.title, 26, `font-weight="700" fill="#111827"`));
    out.push(text(MARGIN + section.title.length * 14 + 28, y + 40, section.note, 14, `fill="#6B7280"`));
  }
  for (const { row, top } of parts.rows) out.push(text(MARGIN, top + 6, row.label, 14, `font-weight="700" fill="#8A8F98"`));
  out.push(arrows(frames, byTag));
  for (const f of frames) out.push(drawFrame(f), drawCaption(f));
  out.push(arrowLabels(frames));
  out.push(`<line x1="${MARGIN}" y1="${notesTop}" x2="${width - MARGIN}" y2="${notesTop}" stroke="#E5E7EB"/>`);
  out.push(text(MARGIN, notesTop + 32, "Design notes", 16, `font-weight="700" fill="#111827"`));
  noteLines.forEach((line, i) => out.push(text(MARGIN, notesTop + 58 + i * 20, line, 13, `fill="#4B5563"`)));
  out.push("</svg>");
  return out.join("\n");
}

const frames = frameList();
await renderAll(frames);
writeFileSync(join(OUT, "names-flow.svg"), compose(frames));
console.log(`board: ${frames.length} frames in screens/, and names-flow.svg`);
