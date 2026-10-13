const fs = require("fs")
const path = require("path")
const matter = require("gray-matter")
const parse5 = require("parse5")

const siteLocation = "https://simplex.chat"

const htmlPages = new Set([
  "index",
  "why",
  "messaging",
  "fdroid",
  "contact",
  "invitation",
  "file",
  "crowdfunding",
  "crowdfunding-news",
  "news",
  "livestream",
  "blog",
  "links",
  "directory",
])

const removedTags = new Set([
  "script",
  "style",
  "noscript",
  "svg",
  "button",
  "select",
  "input",
  "textarea",
])

const removedIds = new Set([
  "navbar",
  "mobile-header",
])

const removedClasses = new Set([
  "footer",
  "glossary-tooltip",
  "glossary-overlay",
])

const blockTags = new Set([
  "address",
  "article",
  "aside",
  "dd",
  "details",
  "div",
  "dl",
  "dt",
  "fieldset",
  "figcaption",
  "figure",
  "form",
  "header",
  "main",
  "nav",
  "section",
  "summary",
])

const codeFencePattern = /(^```[\s\S]*?^```[^\n]*$)/gm
const linkTargetPattern = /\]\((<[^>]*>|[^)\s]+)/g
const referenceTargetPattern = /^( {0,3}\[[^\]]+\]:[ \t]*)(<[^>]*>|\S+)/gm
const textEntityPattern = /&(?!lt;|gt;|amp;)(#\d+|#x[0-9a-f]+|[a-z]+\d*);/gi
const headingPattern = /^#\s+(.+)$/m
const invisibleCharacters = /[​⁠]/g

function markdownKind(inputPath, langs) {
  const relativePath = path.relative("src", inputPath)
  if (/^docs\/(?!lang\/).+\.md$/.test(relativePath)) return "source"
  if (/^blog\/[^/]+\.md$/.test(relativePath)) return "source"
  if (relativePath === "privacy.md" || relativePath === "token.md") return "source"
  const parts = relativePath.split("/")
  const fileName = parts.length === 1 ? parts[0] : parts.length === 2 && langs.includes(parts[0]) ? parts[1] : ""
  return fileName.endsWith(".html") && htmlPages.has(fileName.slice(0, -5)) ? "html" : null
}

function markdownUrl(url, langs) {
  if (url.endsWith(".html")) return url.slice(0, -5) + ".md"
  const isRoot = url === "/" || langs.some((lang) => url === `/${lang}/`)
  return isRoot ? url + "index.md" : url.slice(0, -1) + ".md"
}

function isMarkdownPagePath(pathname) {
  const isDoc = pathname.startsWith("/docs/") && !pathname.startsWith("/docs/lang/")
  return pathname.endsWith(".html") && (isDoc || pathname.startsWith("/blog/"))
}

function absoluteMarkdownLink(link, inputPath, resolveLink) {
  if (link.startsWith("<")) return `<${absoluteMarkdownLink(link.slice(1, -1), inputPath, resolveLink)}>`
  if (link.startsWith("#")) return link
  let resolved
  try {
    resolved = resolveLink(link, { page: { inputPath } })
  } catch {
    return link
  }
  if (!resolved.startsWith("/")) return resolved
  const [pathname, fragment] = resolved.split("#")
  const target = isMarkdownPagePath(pathname) ? pathname.slice(0, -5) + ".md" : pathname
  return siteLocation + target + (fragment === undefined ? "" : "#" + fragment)
}

function markdownFromSource(inputPath, resolveLink) {
  const { content } = matter(fs.readFileSync(inputPath, "utf8"))
  const rewrite = (link) => absoluteMarkdownLink(link, inputPath, resolveLink)
  const markdown = content
    .split(codeFencePattern)
    .map((part, index) => index % 2 === 1 ? part : part
      .replace(linkTargetPattern, (_, link) => "](" + rewrite(link))
      .replace(referenceTargetPattern, (_, prefix, link) => prefix + rewrite(link))
      .replace(textEntityPattern, plainText))
    .join("")
  return markdown.trim() + "\n"
}

function markdownTitle(inputPath) {
  const { data, content } = matter(fs.readFileSync(inputPath, "utf8"))
  return plainText(data.title || content.match(headingPattern)?.[1] || "")
}

function attribute(node, name) {
  return node.attrs.find((attr) => attr.name === name)?.value
}

function isRemoved(element) {
  const classes = (attribute(element, "class") || "").split(/\s+/)
  return removedTags.has(element.tagName)
    || (element.tagName === "template" && attribute(element, "data-markdown") === undefined)
    || attribute(element, "data-markdown-skip") !== undefined
    || removedIds.has(attribute(element, "id"))
    || classes.some((name) => removedClasses.has(name))
}

function childElements(node) {
  return node.childNodes.filter((child) => child.tagName && !isRemoved(child))
}

function findElements(node, predicate) {
  return childElements(node).flatMap((child) => predicate(child) ? [child] : findElements(child, predicate))
}

function findElement(node, predicate) {
  return findElements(node, predicate)[0]
}

function textContent(node) {
  return node.nodeName === "#text" ? node.value : (node.childNodes || []).map(textContent).join("")
}

function singleLine(text) {
  return text.trim().replace(/\s*\n\s*/g, " ")
}

function emphasis(marker, content) {
  return content.replace(/^(\s*)([\s\S]*?)(\s*)$/, (_, leading, text, trailing) =>
    text ? leading + marker + text + marker + trailing : leading + trailing)
}

function linkMarkdown(element, content, pageUrl) {
  const href = attribute(element, "href")
  if (href && href.startsWith("javascript:")) return ""
  const image = findElement(element, (child) => child.tagName === "img" && attribute(child, "alt") !== undefined)
  const label = attribute(element, "aria-label") || singleLine(content) || attribute(element, "title") || (image && attribute(image, "alt")) || ""
  if (!href) return label
  return label ? `[${label}](${new URL(href, siteLocation + pageUrl).href})` : ""
}

function listMarkdown(element, ordered, pageUrl) {
  const items = childElements(element).filter((child) => child.tagName === "li")
  const lines = items.map((item, index) => (ordered ? `${index + 1}. ` : "- ") + singleLine(childrenMarkdown(item, pageUrl)))
  return `\n\n${lines.join("\n")}\n\n`
}

function tableMarkdown(element, pageUrl) {
  const rows = findElements(element, (child) => child.tagName === "tr").map((row) =>
    childElements(row).map((cell) => singleLine(childrenMarkdown(cell, pageUrl)).replace(/\|/g, "\\|")))
  if (rows.length === 0) return ""
  const width = Math.max(...rows.map((row) => row.length))
  const line = (cells) => "| " + Array.from({ length: width }, (_, index) => cells[index] || "").join(" | ") + " |"
  const [header, ...body] = rows
  return `\n\n${[line(header), line(Array(width).fill("---")), ...body.map(line)].join("\n")}\n\n`
}

function nodeMarkdown(node, pageUrl) {
  if (node.nodeName === "#text") return node.value.replace(invisibleCharacters, "").replace(/\s+/g, " ")
  if (!node.tagName || isRemoved(node)) return ""
  const tag = node.tagName
  const content = () => childrenMarkdown(node, pageUrl)
  switch (tag) {
    case "h1":
    case "h2":
    case "h3":
    case "h4":
    case "h5":
    case "h6":
      return `\n\n${"#".repeat(Number(tag[1]))} ${singleLine(content())}\n\n`
    case "p":
      return `\n\n${content().trim()}\n\n`
    case "br":
      return "\n"
    case "hr":
      return "\n\n---\n\n"
    case "a":
      return linkMarkdown(node, content(), pageUrl)
    case "strong":
    case "b":
      return emphasis("**", content())
    case "em":
    case "i":
      return emphasis("*", content())
    case "code":
      return "`" + textContent(node) + "`"
    case "pre":
      return "\n\n```\n" + textContent(node).trim() + "\n```\n\n"
    case "sup":
      return `[${content().trim()}]`
    case "img":
      return ""
    case "ul":
    case "ol":
      return listMarkdown(node, tag === "ol", pageUrl)
    case "table":
      return tableMarkdown(node, pageUrl)
    case "blockquote":
      return "\n\n" + content().trim().split("\n").map((line) => "> " + line).join("\n") + "\n\n"
    case "template":
      return childrenMarkdown(node.content, pageUrl)
    default:
      return blockTags.has(tag) ? `\n\n${content()}\n\n` : content()
  }
}

function childrenMarkdown(node, pageUrl) {
  return node.childNodes.map((child) => nodeMarkdown(child, pageUrl)).join("")
}

function normalizedMarkdown(markdown) {
  let inFence = false
  const lines = markdown.split("\n").map((line) => {
    if (line.startsWith("```")) inFence = !inFence
    return inFence || line.startsWith("```") ? line : line.trim()
  })
  return lines.join("\n").replace(/\n{3,}/g, "\n\n").trim() + "\n"
}

function markdownFromHtml(html, pageUrl) {
  const document = parse5.parse(html)
  const body = findElement(document, (node) => node.tagName === "body")
  const title = findElement(document, (node) => node.tagName === "title")
  const heading = findElement(body, (node) => node.tagName === "h1") ? "" : `# ${singleLine(title ? textContent(title) : "")}\n\n`
  return normalizedMarkdown(heading + childrenMarkdown(body, pageUrl))
}

function plainText(html) {
  return textContent(parse5.parseFragment(html))
}

module.exports = {
  markdownKind,
  markdownUrl,
  markdownFromSource,
  markdownTitle,
  markdownFromHtml,
  plainText,
}
