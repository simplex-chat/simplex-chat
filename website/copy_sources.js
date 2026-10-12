const fs = require("fs")
const path = require("path")
const customizeDocs = require("./customize_docs_frontmatter")

const repoDir = path.resolve(__dirname, "..")

const docs = {
  from: "docs",
  to: "src/docs",
  exclude: [/^contributing\//, /^rfcs\//, /^dependencies\//, /^LINKS\.md$/, /^lang\/[^/]+\/README\.md$/],
}

const sources = [
  docs,
  { from: "docs/links/images", to: "src/link-images", exclude: [] },
  { from: "blog", to: "src/blog", exclude: [/^README\.md$/, /^new\//] },
  { from: "images", to: "src/images", exclude: [] },
  { from: "PRIVACY.md", to: "src/privacy.md", exclude: [] },
]

function sourcePath(source, file = "") {
  return path.join(repoDir, source.from, file)
}

function targetPath(source, file = "") {
  return path.join(__dirname, source.to, file)
}

function relativeFiles(root, dir = "") {
  if (!fs.statSync(path.join(root, dir)).isDirectory()) return [dir]
  return fs.readdirSync(path.join(root, dir)).flatMap((name) => relativeFiles(root, path.join(dir, name)))
}

function isExcluded(source, file) {
  return source.exclude.some((pattern) => pattern.test(file))
}

function isDocsMarkdown(source, file) {
  return source === docs && file.endsWith(".md")
}

function sourceFiles(source) {
  return relativeFiles(sourcePath(source)).filter((file) => !isExcluded(source, file))
}

function copyFile(source, file) {
  fs.mkdirSync(path.dirname(targetPath(source, file)), { recursive: true })
  fs.copyFileSync(sourcePath(source, file), targetPath(source, file))
}

function writeDocsMarkdown() {
  const markdownFiles = sourceFiles(docs).filter((file) => isDocsMarkdown(docs, file))
  customizeDocs(sourcePath(docs), targetPath(docs), markdownFiles)
}

function copySources() {
  sources.filter((source) => fs.existsSync(sourcePath(source))).forEach((source) => {
    fs.rmSync(targetPath(source), { recursive: true, force: true })
    sourceFiles(source).filter((file) => !isDocsMarkdown(source, file)).forEach((file) => copyFile(source, file))
  })
  writeDocsMarkdown()
}

function syncFile(source, event, file) {
  if (event === "unlink" || event === "unlinkDir") fs.rmSync(targetPath(source, file), { recursive: true, force: true })
  if (isDocsMarkdown(source, file) || (source === docs && event === "unlinkDir")) writeDocsMarkdown()
  else if (event === "add" || event === "change") copyFile(source, file)
}

function watchSources() {
  const chokidar = require("chokidar")
  sources.forEach((source) => {
    chokidar.watch(sourcePath(source), { ignoreInitial: true }).on("all", (event, changedPath) => {
      const file = path.relative(sourcePath(source), changedPath)
      if (!isExcluded(source, file)) syncFile(source, event, file)
    })
  })
}

if (process.argv.includes("--watch")) watchSources()
else copySources()
