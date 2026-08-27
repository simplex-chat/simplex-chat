// Build for the supporter-badge checkout site. Run it with `npm run build`.
//
// Compiles src/ to ES modules in dist/, copies assets/ in beside them, checks
// that the emitted modules can resolve each other in a browser, and writes
// dist/dev.html — index.html with its @@name@@ tokens resolved, for design work
// over a local static server before the service can serve the real page.
//
// Node's own modules only: this project is capped at one devDependency, tsc.

import {execFileSync} from "node:child_process"
import {cpSync, mkdirSync, readdirSync, readFileSync, rmSync, writeFileSync} from "node:fs"
import {dirname, join, posix, relative, resolve, sep} from "node:path"
import {fileURLToPath} from "node:url"

const webDir = dirname(fileURLToPath(import.meta.url))
const assetsDir = join(webDir, "assets")
const distDir = join(webDir, "dist")
const tscBin = join(webDir, "node_modules", "typescript", "bin", "tsc")

// Served from web/ rather than from dist/, so that an edit to styles.css is
// picked up without a rebuild in web_dir mode. dev.html sits in dist/, so it
// reaches both of them one directory up.
const ROOT_FILES = ["index.html", "styles.css"]
// Generated into dist/, never served: the service filters it out of the assets.
const DEV_HTML = "dev.html"
// The only token that names something other than a file. The service reads the
// real value from [web] support_contact; dev.html has no configuration to read.
const NON_FILE_TOKENS = new Map([["support_contact", "https://example.invalid/dev-support-contact"]])
const TOKEN_RE = /@@([\w.-]+)@@/g
// Relative import specifiers in the emitted modules: `from "./x.js"`,
// `import "./x.js"` and `import("./x.js")`. Textual, so a specifier-shaped
// string inside a comment or a literal would be checked too; that only ever
// reports a path that does not exist, which is worth knowing either way.
const IMPORT_RES = [/\bfrom\s*["'](\.[^"']*)["']/g, /^\s*import\s*["'](\.[^"']*)["']/gm, /\bimport\s*\(\s*["'](\.[^"']*)["']\s*\)/g]

// Rebuild from empty, so that a deleted module or asset also leaves dist/ and
// the committed build stays a function of the sources alone.
rmSync(distDir, {recursive: true, force: true})

compile()
copyAssets()
const built = listFiles(distDir)
checkImportsResolve(built)
writeDevHtml(built)
console.log(`built ${built.length} asset(s) and ${DEV_HTML} into ${relative(webDir, distDir)}/`)

function compile() {
  // Spawned through node rather than through the PATH shim, so that the script
  // also works when run as `node build.mjs`.
  execFileSync(process.execPath, [tscBin, "--project", webDir], {stdio: "inherit"})
}

function copyAssets() {
  mkdirSync(distDir, {recursive: true})
  for (const entry of readdirSync(assetsDir)) {
    // .gitkeep and friends: they keep the directory in git, they are not assets.
    if (entry.startsWith(".")) continue
    cpSync(join(assetsDir, entry), join(distDir, entry), {recursive: true})
  }
}

// Every file in dir, as a path relative to it, in POSIX form and sorted.
function listFiles(dir) {
  const walk = (d) =>
    readdirSync(d, {withFileTypes: true}).flatMap((e) => {
      const p = join(d, e.name)
      return e.isDirectory() ? walk(p) : [relative(dir, p).split(sep).join(posix.sep)]
    })
  return walk(dir).sort()
}

// tsc does not rewrite import specifiers, so an extensionless or misspelled one
// compiles cleanly and then 404s in the browser, leaving a blank page.
function checkImportsResolve(built) {
  const emitted = new Set(built)
  for (const name of built) {
    if (!name.endsWith(".js")) continue
    const source = readFileSync(join(distDir, name), "utf8")
    for (const re of IMPORT_RES) {
      for (const [, specifier] of source.matchAll(re)) {
        const target = relative(distDir, resolve(dirname(join(distDir, name)), specifier)).split(sep).join(posix.sep)
        if (!emitted.has(target)) {
          throw new Error(`${name} imports "${specifier}", which is not in the build. A browser would fail to load it.`)
        }
      }
    }
  }
}

// index.html with every token resolved. The rule is the service's, minus the
// hashed prefix: a token naming a file in the served set resolves to that file,
// anything else is an error. Nothing here knows which files exist today, so an
// asset added later needs a token and no change to this script.
function writeDevHtml(built) {
  // dist/ was wiped above and dev.html is written last, so it is not in `built`
  // and cannot resolve a token of its own name.
  const served = new Map()
  for (const name of built) served.set(name, `./${name}`)
  for (const name of ROOT_FILES) served.set(name, `../${name}`)
  const html = readFileSync(join(webDir, "index.html"), "utf8").replace(TOKEN_RE, (_, token) => {
    const href = served.get(token)
    if (href !== undefined) return href
    const value = NON_FILE_TOKENS.get(token)
    if (value !== undefined) return value
    throw new Error(
      `index.html references @@${token}@@, which is neither a file in the served set ` +
        `(${[...served.keys()].join(", ")}) nor a known non-file token. ` +
        `Add the file to assets/ or src/, or add the token to NON_FILE_TOKENS here and to the service.`
    )
  })
  // One short line: everything above index.html's charset declaration has to
  // fit in the 1024 bytes HTML5 gives it.
  const banner = "<!-- Generated from index.html. Open over a static server rooted at web/, not file://. -->\n"
  writeFileSync(join(distDir, DEV_HTML), banner + html)
}
