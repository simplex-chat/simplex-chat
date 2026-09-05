#!/usr/bin/env node
// Assembles `dist/`, the directory the service serves: the shell, the worker, and this
// build's modules under /assets/<buildHash>/.
//
// The hash covers the bytes that are served and is written back into `public/index.html`
// and `public/sw.js`, so a shell can never name a build other than the one on disk.
// `assemble` runs only when this file is executed directly, so importing it writes nothing.

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

/** Long enough that a collision is not a thing that happens, short enough to read in a URL. */
export const HASH_CHARS = 16;
export const ASSET_PATTERN = new RegExp(`/assets/[0-9a-f]{${HASH_CHARS}}/`, "g");
export const BUILD_PATTERN = /const BUILD = "[0-9a-f]+";/;

/**
 * What is served for one compiled module. The `sourceMappingURL` comment goes:
 * the maps are not copied (they point at `../../src/*.ts`, which the service
 * does not serve), so leaving the comment would be a 404 on every devtools open.
 */
export function served(source) {
  return source.replace(/\n?\/\/# sourceMappingURL=.*\n?$/, "\n");
}

/**
 * The build hash: a digest of exactly the bytes that will be served, keyed by
 * the name each is served under. Pure, so a test can recompute it.
 */
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

/**
 * Everything that goes under /assets/<hash>/, as [name, content]. Images are read as bytes so
 * they hash with the modules and are precached under the same hash, and they sit flat beside
 * `styles.css`, which is what makes `url(hero-light.png)` resolve. The wordmark and the symbol
 * are the official files copied out of `website/` and `media-logos/`, never transcribed.
 */
export function assets(compiled = paths.compiled, stylesheet = paths.stylesheet, images = paths.images) {
  const modules = readdirSync(compiled).filter((f) => f.endsWith(".js")).sort();
  if (modules.length === 0) throw new Error("build: build/src holds no modules — run tsc first");
  const pictures = readdirSync(images).filter((f) => IMAGE_TYPES.some((t) => f.endsWith(t))).sort();
  return [
    ...modules.map((name) => [name, served(readFileSync(`${compiled}/${name}`, "utf8"))]),
    ["styles.css", readFileSync(stylesheet, "utf8")],
    ...pictures.map((name) => [name, readFileSync(`${images}/${name}`)]),
  ];
}

/**
 * Rewrites every asset path in a text to name `build`. It throws rather than
 * returning the text unchanged, because a silent no-op here is a shell that
 * still names the previous build, the exact failure this file exists
 * to prevent, and it would ship looking like a successful build.
 */
export function retarget(text, pattern, replacement, what) {
  if (!new RegExp(pattern.source).test(text)) throw new Error(`build: no ${what} to rewrite`);
  return text.replace(pattern, replacement);
}

export function withBuild(html, build) {
  return retarget(html, ASSET_PATTERN, `/assets/${build}/`, "asset path");
}

export function withBuildId(js, build) {
  return retarget(js, BUILD_PATTERN, `const BUILD = "${build}";`, "BUILD constant");
}

/** Writes a file only when its content changed, so a rebuild of nothing touches nothing. */
function put(file, content) {
  let before = null;
  try { before = readFileSync(file, "utf8"); } catch { /* absent */ }
  if (before === content) return false;
  writeFileSync(file, content);
  return true;
}

export function assemble() {
  const files = assets();
  const build = hashOf(files);

  // Rebuilt from scratch: a hash that is no longer current must not be left
  // sitting in the served directory beside the one that is.
  rmSync(paths.site, { recursive: true, force: true });
  mkdirSync(`${paths.site}/assets/${build}`, { recursive: true });
  for (const [name, content] of files) writeFileSync(`${paths.site}/assets/${build}/${name}`, content);

  const moved = [
    put(paths.indexHtml, withBuild(readFileSync(paths.indexHtml, "utf8"), build)),
    put(paths.worker, withBuildId(readFileSync(paths.worker, "utf8"), build)),
  ].some(Boolean);
  copyFileSync(paths.indexHtml, `${paths.site}/index.html`);
  copyFileSync(paths.worker, `${paths.site}/sw.js`);

  return { build, files: files.length, moved };
}

if (process.argv[1] !== undefined && pathToFileURL(process.argv[1]).href === import.meta.url) {
  const { build, files, moved } = assemble();
  console.log(`build ${build}: ${files} files in dist/assets/${build}/, with index.html and sw.js`);
  if (moved) console.log("build: public/index.html and public/sw.js now name this build — commit them");
}
