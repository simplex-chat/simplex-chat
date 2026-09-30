import { timedTest } from "./boot.js";
import assert from "node:assert/strict";
import { readFileSync, readdirSync } from "node:fs";

const buildTest = timedTest(2000);

interface BuildModule {
  HASH_CHARS: number;
  ASSET_PATTERN: RegExp;
  BUILD_PATTERN: RegExp;
  served(source: string): string;
  hashOf(files: Array<readonly [string, string | Buffer]>): string;
  assets(compiled?: string, stylesheet?: string, images?: string): Array<[string, string | Buffer]>;
  retarget(text: string, pattern: RegExp, replacement: string, what: string): string;
  withBuild(html: string, build: string): string;
  withBuildId(js: string, build: string): string;
}

// A non-literal specifier keeps tsc from resolving build.js, which this project does not compile.
const build = (await import(new URL("../../build.js", import.meta.url).href)) as BuildModule;

const indexHtml = readFileSync(new URL("../../public/index.html", import.meta.url), "utf8");
const workerJs = readFileSync(new URL("../../public/sw.js", import.meta.url), "utf8");

function declaredIn(text: string): string[] {
  return [...text.matchAll(/[0-9a-f]{16}/g)].map((m) => m[0]);
}

// Fails when the committed shell or worker names a build other than the one on disk, forcing a rebuild before commit.
buildTest("build: the committed shell and worker name the build that is on disk", () => {
  const current = build.hashOf(build.assets());
  assert.equal(new Set(declaredIn(indexHtml)).size, 1, "the shell names exactly one build");
  assert.equal(declaredIn(indexHtml)[0], current,
    "public/index.html names a build that is not this one — run `npm run build` and commit the result");
  assert.ok(workerJs.includes(`const BUILD = "${current}";`),
    "public/sw.js names a build that is not this one — run `npm run build` and commit the result");
});

buildTest("build: the hash is derived from the bytes that are served, and from nothing else", () => {
  const files: Array<readonly [string, string]> = [["main.js", "a"], ["styles.css", "b"]];
  assert.equal(build.hashOf(files), build.hashOf(files), "same content, same hash — twice in a row");
  assert.equal(build.hashOf(files), build.hashOf([...files].reverse()), "and independent of the order read");
  assert.notEqual(build.hashOf(files), build.hashOf([["main.js", "a "], ["styles.css", "b"]]),
    "one byte of one module changes it: this is what stops a cache-first page serving last week's code");
  assert.notEqual(build.hashOf(files), build.hashOf([["main.js", "a"], ["styles.css", "b "]]),
    "the stylesheet is served under the same hash, so it counts too");
  assert.notEqual(build.hashOf(files), build.hashOf([["other.js", "a"], ["styles.css", "b"]]),
    "and so does the name each is served under");
  assert.match(build.hashOf(files), new RegExp(`^[0-9a-f]{${build.HASH_CHARS}}$`));
});

buildTest("build: a rebuild of unchanged sources produces the same hash and the same files", () => {
  const first = build.assets();
  const second = build.assets();
  assert.deepEqual(first, second);
  assert.equal(build.hashOf(first), build.hashOf(second), "idempotent: nothing changed, nothing moves");
});

buildTest("build: what is hashed is what is served — every module, the stylesheet, the images, the fonts, no maps", () => {
  const names = build.assets().map(([name]) => name).sort();
  const modules = readdirSync(new URL("../../src", import.meta.url))
    .filter((f) => f.endsWith(".ts")).map((f) => f.replace(/\.ts$/, ".js"));
  const images = readdirSync(new URL("../../public/img", import.meta.url))
    .filter((f) => f.endsWith(".png") || f.endsWith(".svg"));
  const fonts = readdirSync(new URL("../../public/fonts", import.meta.url)).filter((f) => f.endsWith(".woff2"));
  assert.ok(images.some((f) => f.endsWith(".png")), "the hero has to be somewhere for the stylesheet to point at");
  assert.ok(images.some((f) => f.endsWith(".svg")), "and so does the wordmark");
  assert.ok(fonts.length > 0, "and the faces the stylesheet declares");
  assert.deepEqual(names, [...modules, "styles.css", "init.js", ...images, ...fonts].sort());
  assert.ok(!names.some((n) => n.endsWith(".map")),
    "a source map under the hash would 404 on its sources, which are not served");
});

buildTest("build: an image's BYTES move the hash, so a redrawn hero is a new build", () => {
  // Read as bytes, since a PNG decoded as UTF-8 is a lossy string two different images can collide on.
  const [, content] = build.assets().find(([name]) => name.endsWith(".png"))!;
  assert.ok(Buffer.isBuffer(content), "images are hashed and written as bytes");
  const base: Array<readonly [string, string | Buffer]> = [["styles.css", "a"], ["hero-light.png", content]];
  const flipped = Buffer.from(content);
  const last = flipped.length - 1;
  flipped[last] = flipped[last]! ^ 0xff;
  assert.notEqual(build.hashOf(base), build.hashOf([["styles.css", "a"], ["hero-light.png", flipped]]),
    "one byte of the hero is a different build");
});

buildTest("build: the sourceMappingURL comment is stripped, because the map is not published", () => {
  assert.equal(build.served("const a = 1;\n//# sourceMappingURL=a.js.map\n"), "const a = 1;\n");
  assert.equal(build.served("const a = 1;\n"), "const a = 1;\n", "a module without one is untouched");
  for (const [, content] of build.assets()) {
    assert.ok(!content.includes("sourceMappingURL"), "no served file may point at a map");
  }
});

buildTest("build: both files are retargeted in one pass, and each really changes", () => {
  const to = "0123456789abcdef";
  const html = build.withBuild(indexHtml, to);
  assert.deepEqual([...new Set(declaredIn(html))], [to], "every asset path in the shell moves together");
  assert.ok(html.includes(`/assets/${to}/main.js`) && html.includes(`/assets/${to}/styles.css`));
  const js = build.withBuildId(workerJs, to);
  assert.ok(js.includes(`const BUILD = "${to}";`));
  assert.ok(!js.includes(declaredIn(workerJs)[0]!), "the old build id is gone from the worker");
});

buildTest("build: retargeting is idempotent", () => {
  const to = "0123456789abcdef";
  assert.equal(build.withBuild(build.withBuild(indexHtml, to), to), build.withBuild(indexHtml, to));
  assert.equal(build.withBuildId(build.withBuildId(workerJs, to), to), build.withBuildId(workerJs, to));
});

buildTest("build: retargeting nothing FAILS rather than quietly shipping the old hash", () => {
  assert.throws(() => build.withBuild('<script src="/js/main.js"></script>', "0123456789abcdef"),
    /no asset path to rewrite/);
  assert.throws(() => build.withBuildId('const VERSION = "1";', "0123456789abcdef"),
    /no BUILD constant to rewrite/);
  assert.doesNotThrow(() => build.withBuild(indexHtml, "0123456789abcdef"));
  assert.doesNotThrow(() => build.withBuildId(workerJs, "0123456789abcdef"));
});
