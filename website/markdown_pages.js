const fs = require("fs")
const path = require("path")
const matter = require("gray-matter")
const { JSDOM } = require("jsdom")

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

const removedSelectors = [
  "script",
  "style",
  "noscript",
  "svg",
  "button",
  "select",
  "input",
  "textarea",
  "template:not([data-markdown])",
  "[data-markdown-skip]",
  "#navbar",
  "#mobile-header",
  ".footer",
  ".glossary-tooltip",
  ".glossary-overlay",
].join(", ")

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

function singleLine(text) {
  return text.trim().replace(/\s*\n\s*/g, " ")
}

function emphasis(marker, content) {
  return content.replace(/^(\s*)([\s\S]*?)(\s*)$/, (_, leading, text, trailing) =>
    text ? leading + marker + text + marker + trailing : leading + trailing)
}

function linkMarkdown(element, content, pageUrl) {
  const href = element.getAttribute("href")
  if (href && href.startsWith("javascript:")) return ""
  const label = element.getAttribute("aria-label") || singleLine(content) || element.getAttribute("title") || element.querySelector("img[alt]")?.getAttribute("alt") || ""
  if (!href) return label
  return label ? `[${label}](${new URL(href, siteLocation + pageUrl).href})` : ""
}

function listMarkdown(element, ordered, pageUrl) {
  const items = Array.from(element.children).filter((child) => child.tagName === "LI")
  const lines = items.map((item, index) => (ordered ? `${index + 1}. ` : "- ") + singleLine(childrenMarkdown(item, pageUrl)))
  return `\n\n${lines.join("\n")}\n\n`
}

function tableMarkdown(element, pageUrl) {
  const rows = Array.from(element.querySelectorAll("tr")).map((row) =>
    Array.from(row.children).map((cell) => singleLine(childrenMarkdown(cell, pageUrl)).replace(/\|/g, "\\|")))
  if (rows.length === 0) return ""
  const width = Math.max(...rows.map((row) => row.length))
  const line = (cells) => "| " + Array.from({ length: width }, (_, index) => cells[index] || "").join(" | ") + " |"
  const [header, ...body] = rows
  return `\n\n${[line(header), line(Array(width).fill("---")), ...body.map(line)].join("\n")}\n\n`
}

function nodeMarkdown(node, pageUrl) {
  if (node.nodeType === node.TEXT_NODE) return node.textContent.replace(/[​⁠]/g, "").replace(/\s+/g, " ")
  if (node.nodeType !== node.ELEMENT_NODE) return ""
  const tag = node.tagName.toLowerCase()
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
      return "`" + node.textContent + "`"
    case "pre":
      return "\n\n```\n" + node.textContent.trim() + "\n```\n\n"
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
  return Array.from(node.childNodes).map((child) => nodeMarkdown(child, pageUrl)).join("")
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
  const { document } = new JSDOM(html).window
  document.querySelectorAll(removedSelectors).forEach((element) => element.remove())
  const title = document.querySelector("h1") ? "" : `# ${document.title}\n\n`
  return normalizedMarkdown(title + childrenMarkdown(document.body, pageUrl))
}

function plainText(html) {
  return JSDOM.fragment(`<p>${html}</p>`).textContent
}

module.exports = {
  markdownKind,
  markdownUrl,
  markdownFromSource,
  markdownTitle,
  markdownFromHtml,
  plainText,
}
