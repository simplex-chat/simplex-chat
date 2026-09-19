#!/usr/bin/env node
// The build hash is written back into public/index.html and public/sw.js, so a served shell can never name a build other than the one on disk.

import { createHash } from "node:crypto";
import { copyFileSync, mkdirSync, readFileSync, readdirSync, rmSync, writeFileSync } from "node:fs";
import { fileURLToPath, pathToFileURL } from "node:url";

const root = fileURLToPath(new URL("./", import.meta.url));
export const paths = {
  compiled: `${root}build/src`,
  stylesheet: `${root}public/styles.css`,
  images: `${root}public/img`,
  indexHtml: `${root}public/index.html`,
  worker: `${root}public/sw.js`,
  site: `${root}dist`,
};

/** This is long enough to avoid a collision and short enough to read in a URL. */
export const HASH_CHARS = 16;
export const ASSET_PATTERN = new RegExp(`/assets/[0-9a-f]{${HASH_CHARS}}/`, "g");
export const BUILD_PATTERN = /const BUILD = "[0-9a-f]+";/;

/** The sourceMappingURL comment is removed because the maps are not copied and would 404 on every devtools open. */
export function served(source) {
  return source.replace(/\n?\/\/# sourceMappingURL=.*\n?$/, "\n");
}

export function hashOf(files) {
  const digest = createHash("sha256");
  for (const [name, content] of [...files].sort((a, b) => a[0].localeCompare(b[0]))) {
    digest.update(name);
    digest.update("\0");
    digest.update(content);
  }
  return digest.digest("hex").slice(0, HASH_CHARS);
}

const IMAGE_TYPES = [".png", ".svg"];

/** Images sit flat beside styles.css so that a url(hero-light.png) reference in the stylesheet resolves. */
/**
 * This runs as a classic non-module script so that it executes before the first paint.
 * The CSP allows script-src 'self' but not inline, so it must be served as a file rather than inlined.
 */
const INIT_JS = `(function () {
  var r = document.documentElement;
  try {
    var raw = localStorage.getItem("sb.theme.v1");
    var t = raw ? JSON.parse(raw) : "system";
    if (t !== "light" && t !== "dark" && t !== "system") t = "system";
    if (t === "system") r.removeAttribute("data-theme"); else r.setAttribute("data-theme", t);
    r.style.colorScheme = (t === "dark" || (t === "system" && matchMedia("(prefers-color-scheme: dark)").matches)) ? "dark" : "light";
  } catch (e) {}
  try {
    var h = location.hash;
    var landing = (h === "" || h === "#" || h === "#/") && location.search.indexOf("order=") < 0;
    if (!landing) r.classList.add("sb-booting");
  } catch (e) {}
})();
`;

export function assets(compiled = paths.compiled, stylesheet = paths.stylesheet, images = paths.images) {
  const modules = readdirSync(compiled).filter((f) => f.endsWith(".js")).sort();
  if (modules.length === 0) throw new Error("build: build/src holds no modules — run tsc first");
  const pictures = readdirSync(images).filter((f) => IMAGE_TYPES.some((t) => f.endsWith(t))).sort();
  return [
    ...modules.map((name) => [name, served(readFileSync(`${compiled}/${name}`, "utf8"))]),
    ["styles.css", readFileSync(stylesheet, "utf8")],
    ["init.js", INIT_JS],
    ...pictures.map((name) => [name, readFileSync(`${images}/${name}`)]),
  ];
}

/** This throws rather than returning the text unchanged, because a silent no-op would ship a shell that still names the previous build. */
export function retarget(text, pattern, replacement, what) {
  if (!new RegExp(pattern.source).test(text)) throw new Error(`build: no ${what} to rewrite`);
  return text.replace(pattern, replacement);
}

export function withBuild(html, build) {
  return retarget(html, ASSET_PATTERN, `/assets/${build}/`, "asset path");
}

export async function prerenderShell() {
  const dom = await import("./build/test/stub-dom.js");
  const prevDoc = Object.getOwnPropertyDescriptor(globalThis, "document");
  const prevNav = Object.getOwnPropertyDescriptor(globalThis, "navigator");
  dom.installDocument();
  try {
    const screens = await import("./build/src/screens.js");
    const noop = () => {};
    const chromeHtml = screens.chrome({
      theme: "system", onNewPurchase: noop, onHistory: noop, onTheme: noop, onToggle: noop, onHome: noop,
    }).node.serialize();
    const landingHtml = screens.landing({ onStart: noop }).serialize();
    return { chromeHtml, appHtml: `<div class="track"><div class="rail">${landingHtml}</div></div>` };
  } finally {
    if (prevDoc) Object.defineProperty(globalThis, "document", prevDoc); else delete globalThis.document;
    if (prevNav) Object.defineProperty(globalThis, "navigator", prevNav); else delete globalThis.navigator;
  }
}

export const SHELL_SLOTS = /** @type {const} */ ([
  ["chrome", /(<!--shell:chrome-->)[\s\S]*?(<!--\/shell:chrome-->)/],
  ["app", /(<!--shell:app-->)[\s\S]*?(<!--\/shell:app-->)/],
]);

/** The markers are kept so the next build can find the slots again. */
export function injectShell(html, shell) {
  let out = html;
  for (const [slot, pattern] of SHELL_SLOTS) {
    const body = slot === "chrome" ? shell.chromeHtml : shell.appHtml;
    out = retarget(out, pattern, (_m, open, close) => `${open}${body}${close}`, `${slot} shell slot`);
  }
  return out;
}

export function withBuildId(js, build) {
  return retarget(js, BUILD_PATTERN, `const BUILD = "${build}";`, "BUILD constant");
}

function put(file, content) {
  let before = null;
  try { before = readFileSync(file, "utf8"); } catch { /* the file may not exist yet */ }
  if (before === content) return false;
  writeFileSync(file, content);
  return true;
}

export async function assemble() {
  const files = assets();
  const build = hashOf(files);
  const shell = await prerenderShell();

  // The whole site directory is removed first so a stale build hash cannot remain beside the current one.
  rmSync(paths.site, { recursive: true, force: true });
  mkdirSync(`${paths.site}/assets/${build}`, { recursive: true });
  for (const [name, content] of files) writeFileSync(`${paths.site}/assets/${build}/${name}`, content);

  // The shell is injected before the build hash is rewritten, because the injected markup carries no asset URLs while the hash rewrite still finds the ones in the head.
  const indexSource = withBuild(injectShell(readFileSync(paths.indexHtml, "utf8"), shell), build);
  const moved = [
    put(paths.indexHtml, indexSource),
    put(paths.worker, withBuildId(readFileSync(paths.worker, "utf8"), build)),
  ].some(Boolean);
  copyFileSync(paths.indexHtml, `${paths.site}/index.html`);
  copyFileSync(paths.worker, `${paths.site}/sw.js`);

  return { build, files: files.length, moved };
}

if (process.argv[1] !== undefined && pathToFileURL(process.argv[1]).href === import.meta.url) {
  const { build, files, moved } = await assemble();
  console.log(`build ${build}: ${files} files in dist/assets/${build}/, with index.html and sw.js`);
  if (moved) console.log("build: public/index.html and public/sw.js now name this build — commit them");
}
