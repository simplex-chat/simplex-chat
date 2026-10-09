const markdownIt = require("markdown-it")
const markdownItAnchor = require("markdown-it-anchor")
const markdownItReplaceLink = require('markdown-it-replace-link')
const markdownItFootnote = require('markdown-it-footnote')
const slugify = require("slugify")
const uri = require('fast-uri')
const fs = require("fs")
const path = require("path")
const matter = require('gray-matter')
const pluginRss = require('@11ty/eleventy-plugin-rss')
const { JSDOM } = require('jsdom')
const parse5 = require('parse5')
const markdownPages = require('./markdown_pages')


// Links page data
const parseLinks = require('./parse_links')
const linksFilePath = path.resolve(__dirname, '../docs/LINKS.md')
const linkImagesDir = path.resolve(__dirname, 'src/link-images')

function readLinksPage() {
  const entries = fs.existsSync(linksFilePath) ? parseLinks(linksFilePath) : []
  entries.forEach(entry => {
    entry.imageExists = entry.image && fs.existsSync(path.join(linkImagesDir, entry.image))
  })

  const catCounts = {}
  entries.forEach(e => { if (e.category) { const c = e.category.toLowerCase(); catCounts[c] = (catCounts[c] || 0) + 1 } })
  const mediaPills = ["Video", "Audio"].filter(p => entries.some(e => e.mediaType === p.toLowerCase()))
  const catPills = Object.keys(catCounts).sort()

  return {
    entries,
    languages: [...new Set(entries.map(e => e.language).filter(Boolean))].sort(),
    pills: mediaPills.concat(catPills),
  }
}

const permalinks = new Map()

function filePermalink(file) {
  if (!permalinks.has(file)) permalinks.set(file, matter(fs.readFileSync(file, 'utf8')).data?.permalink)
  return permalinks.get(file)
}

// Rewrites relative markdown links to website permalinks or GitHub URLs.
// Shared by the main markdown renderer (markdownLib) and the glossary renderer
// below, so glossary tooltips injected into pages get absolute, working links.
function replaceLink(link, _env) {
  let parsed = uri.parse(link)
  if (parsed.scheme || parsed.host) return link

  let hostFile = path.resolve(_env.page.inputPath)
  let linkFile = path.resolve(hostFile, '..', parsed.path)
  if (parsed.path.startsWith('/')) {
    let srcIndex = hostFile.indexOf("/src")
    if (srcIndex !== -1) {
      linkFile = path.join(hostFile.slice(0, srcIndex + 4), parsed.path)
    }
  }

  if (fs.existsSync(linkFile) && fs.statSync(linkFile).isFile()) {
    // this condition works if the link is a valid website file
    parsed.path = (filePermalink(linkFile) || parsed.path).replace(/\.md$/, ".html").toLowerCase()
  } else if (!fs.existsSync(linkFile)) {
    linkFile = linkFile.replace('/website/src', '')
    if (fs.existsSync(linkFile)) {
      // this condition works if the link is a valid project file
      const githubUrl = "https://github.com/simplex-chat/simplex-chat/blob/stable"
      const repoRoot = hostFile.slice(0, hostFile.indexOf('/website/src'))
      const repoPath = linkFile.slice(repoRoot.length)
      return `${githubUrl}${repoPath}`
    } else {
      // if the link is not a valid website file or project file
      throw new Error(`Broken link: ${parsed.path} in ${hostFile}`)
    }
  }

  return uri.serialize(parsed)
}

// The implementation of Glossary feature
const md = new markdownIt({ replaceLink }).use(markdownItReplaceLink)
// Resolve the glossary's relative links (e.g. ./SIMPLEX.md) as if rendered from
// its built location, so tooltips embedded on other pages get absolute URLs.
const glossaryInputPath = path.resolve(__dirname, 'src/docs/GLOSSARY.md')
const glossaryMarkdownContent = fs.readFileSync(path.resolve(__dirname, '../docs/GLOSSARY.md'), 'utf8')
const glossaryHtmlContent = md.render(glossaryMarkdownContent, { page: { inputPath: glossaryInputPath } })
const glossaryDOM = new JSDOM(glossaryHtmlContent)
const glossaryDocument = glossaryDOM.window.document
const glossary = require('./src/_data/glossary.json')

glossary.forEach(item => {
  const headers = Array.from(glossaryDocument.querySelectorAll("h2"))
  const matchingHeader = headers.find(header => header.textContent.trim() === item.definition)

  if (matchingHeader) {
    let sibling = matchingHeader.nextElementSibling
    let definition = ''
    let firstParagraph = ''
    let paragraphCount = 0

    while (sibling && sibling.tagName !== 'H2') {
      if (sibling.tagName === 'P') {
        paragraphCount += 1
        if (firstParagraph === '') {
          firstParagraph = sibling.innerHTML
        }
      }
      definition += sibling.outerHTML || sibling.textContent
      sibling = sibling.nextElementSibling
    }

    item.definition = definition
    item.tooltip = firstParagraph
    item.hasMultipleParagraphs = paragraphCount > 1
  }

  const definitionLinks = new JSDOM(item.definition).window.document.querySelectorAll('a[href*="#"]')
  item.linkedHashes = Array.from(definitionLinks, a => a.href.substring(a.href.indexOf("#") + 1))
  item.id = item.term.toLowerCase().replace(/\s/g, '-')
  item.pattern = new RegExp(`(?<![/#])\\b${item.term}\\b`, 'gi')
})

const glossaryById = new Map(glossary.map(item => [item.id, item]))
const glossaryTermPattern = new RegExp(`(?<![/#])\\b(${glossary.map(item => item.term).join('|')})\\b`, 'i')
const glossaryContentTags = new Set(['p', 'td', 'a', 'h1', 'h2', 'h3', 'h4'])
const htmlParseOptions = { scriptingEnabled: false }
const closeOverlayIcon = '<svg class="close-overlay-btn" id="cross" width="16" height="16" viewBox="0 0 13 13" xmlns="http://www.w3.org/2000/svg"><path d="M12.7973 11.5525L7.59762 6.49833L12.7947 1.44675C13.055 1.19371 13.0658 0.771991 12.8188 0.505331C12.5718 0.238674 12.1602 0.227644 11.8999 0.480681L6.65343 5.58028L1.09979 0.182228C0.805 0.002228 0.430001 0.002228 0.135211 0.182228C-0.159579 0.362228 -0.159579 0.697228 0.135211 0.877228L5.68885 6.27528L0.4918 11.3295C0.231501 11.5825 0.220703 12.0042 0.467664 12.2709C0.714625 12.5376 1.12625 12.5486 1.38655 12.2956L6.63302 7.196L12.1867 12.5941C12.4815 12.7741 12.8565 12.7741 13.1513 12.5941C13.4461 12.4141 13.4461 12.0791 13.1513 11.8991L12.7973 11.5525Z"></path></svg>'

function glossaryTooltipHtml(term) {
  const readMoreButton = term.hasMultipleParagraphs
    ? `<button class="read-more-btn open-overlay-btn" data-show-overlay="${term.id}">Read more</button>`
    : ''
  return `<div id="tooltip-${term.id}" class="glossary-tooltip"><div class="tooltip-content"><h4 class="tooltip-title">${term.term}</h4><p>${term.tooltip}</p>${readMoreButton}</div></div>`
}

function glossaryOverlayHtml(term) {
  return `<div id="${term.id}" class="overlay glossary-overlay hidden"><div class="overlay-card"><h1 class="overlay-title">${term.term}</h1><div class="overlay-content">${term.definition}</div>${closeOverlayIcon}</div></div>`
}

function descendantElements(node, tags) {
  return (node.childNodes || []).flatMap((child) =>
    tags.has(child.tagName) ? [child, ...descendantElements(child, tags)] : descendantElements(child, tags))
}

function parseChildren(node, html) {
  const children = parse5.parseFragment(node, html, htmlParseOptions).childNodes
  children.forEach((child) => child.parentNode = node)
  return children
}


const globalConfig = {
  onionLocation: "http://isdb4l77sjqoy2qq7ipum6x3at6hyn3jmxfx4zdhc72ufbmuq4ilwkqd.onion",
  siteLocation: "https://simplex.chat"
}

const translationsDirectoryPath = './langs'
const supportedRoutes = ["blog", "contact", "invitation", "messaging", "docs", "fdroid", "why", "file", "downloads", "faq", "reproduce", "transparency", "security", "jobs", ""]
let supportedLangs = []
fs.readdir(translationsDirectoryPath, (err, files) => {
  if (err) {
    console.error('Could not list the directory.', err)
    process.exit(1)
  }
  const jsonFileNames = files.filter(file => {
    return file.endsWith('.json') && fs.statSync(translationsDirectoryPath + '/' + file).isFile()
  })
  supportedLangs = jsonFileNames.map(file => file.replace('.json', ''))
})

const translations = require("./translations.json")

const outputDir = '_site'

module.exports = function (ty) {
  ty.on("eleventy.before", () => permalinks.clear())

  // Add this after your markdownLib definition
  ty.addShortcode("mdInclude", function (filepath) {
    const fullPath = path.join(__dirname, 'src/_includes', filepath);
    const content = fs.readFileSync(fullPath, 'utf8');
    return markdownLib.render(content);
  });

  ty.addGlobalData("linksPage", readLinksPage)
  ty.addWatchTarget(linksFilePath)

  ty.addShortcode("cfg", (name) => globalConfig[name])

  ty.addFilter("getlang", (path) => {
    const lang = path.split("/")[1]
    if (supportedRoutes.includes(lang)) return "en"
    else if (supportedLangs.includes(lang)) return lang
    return "en"
  })

  ty.addFilter("getlang", (path) => {
    const urlParts = path.split("/")
    if (urlParts[1] === "docs") {
      if (urlParts[2] === "lang") {
        return urlParts[3]
      }
      return "en"
    }
    else {
      if (supportedRoutes.includes(urlParts[1])) return "en"
      else if (supportedLangs.includes(urlParts[1])) return urlParts[1]
      return "en"
    }
  })

  ty.addFilter('applyGlossary', function (content) {
    if (!glossaryTermPattern.test(content)) return content
    const document = parse5.parse(content, htmlParseOptions)
    const body = document.childNodes.find((node) => node.tagName === 'html').childNodes.find((node) => node.tagName === 'body')
    const allContentNodes = descendantElements(document, glossaryContentTags)
    const contentHtml = allContentNodes.map((node) => parse5.serialize(node))
    const overlayIds = []
    const matchedIds = new Set()

    glossary.forEach((term) => {
      const id = term.id
      allContentNodes.forEach((node, nodeIndex) => {
        const beforeContent = contentHtml[nodeIndex]
        const afterContent = beforeContent.replace(term.pattern, (match) => {
          return `<span data-glossary="tooltip-${id}" class="glossary-term">${match}</span>`
        })
        if (afterContent !== beforeContent) {
          node.childNodes = parseChildren(node, afterContent)
          contentHtml[nodeIndex] = parse5.serialize(node)
          matchedIds.add(id)
        }
      })
    })

    const neededIds = new Set(matchedIds)
    neededIds.forEach((id) => glossaryById.get(id).linkedHashes.forEach((hash) => {
      if (glossaryById.has(hash)) neededIds.add(hash)
    }))

    glossary.forEach((term) => {
      const id = term.id

      if (matchedIds.has(id)) {
        body.childNodes.push(...parseChildren(body, glossaryTooltipHtml(term)))
      }

      const hashList = [id, ...term.linkedHashes]

      hashList.forEach(hash => {
        if (neededIds.has(hash) && !overlayIds.includes(hash)) {
          body.childNodes.push(...parseChildren(body, glossaryOverlayHtml(glossaryById.get(hash))))
          overlayIds.push(hash)
        }
      })
    })

    return parse5.serialize(document)
  })

  ty.addFilter('wrapH3s', function (content, page) {
    if (!page.url.includes("/jobs/")) {
      return content
    }

    const dom = new JSDOM(content)
    const document = dom.window.document

    const makeBlock = (block) => {
      const jobTab = document.createElement('div')
      jobTab.className = "job-tab"

      const flexDiv = document.createElement('div')
      flexDiv.className = "flex items-center justify-between job-tab-btn cursor-pointer"
      flexDiv.innerHTML = `
        <${block.tagName}>${block.innerHTML}</${block.tagName}>
        <svg class="fill-grey-black dark:fill-white" width="10" height="5" viewBox="0 0 10 5" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path fill-rule="evenodd" clip-rule="evenodd" d="M8.40813 4.79332C8.69689 5.06889 9.16507 5.06889 9.45384 4.79332C9.7426 4.51775 9.7426 4.07097 9.45384 3.7954L5.69327 0.206676C5.65717 0.17223 5.61827 0.142089 5.57727 0.116255C5.29026 -0.064587 4.90023 -0.0344467 4.64756 0.206676L0.886983 3.7954C0.598219 4.07097 0.598219 4.51775 0.886983 4.79332C1.17575 5.06889 1.64393 5.06889 1.93269 4.79332L5.17041 1.70356L8.40813 4.79332Z"></path>
        </svg>
      `
      jobTab.appendChild(flexDiv)

      const jobContent = document.createElement('div')
      jobContent.className = "job-tab-content"
      jobTab.appendChild(jobContent)

      block.parentNode.insertBefore(jobTab, block)
      block.remove()

      let sibling = jobTab.nextElementSibling
      const siblingsToMove = []
      while (sibling && !['H3', 'H2'].includes(sibling.tagName)) {
        siblingsToMove.push(sibling)
        sibling = sibling.nextElementSibling
      }

      siblingsToMove.forEach(el => jobContent.appendChild(el))
    }

    Array.from(document.querySelectorAll("h3")).forEach(makeBlock)

    return dom.serialize()
  })

  ty.addShortcode("completeRoute", (obj) => {
    const urlParts = obj.url.split("/")

    if (supportedRoutes.includes(urlParts[1])) {
      if (urlParts[1] == "blog")
        return `/blog`

      else if (urlParts[1] === "docs") {
        if (urlParts[2] === "lang") {
          if (obj.lang === "en")
            return `/docs/${urlParts.slice(4).join('/')}`
          return `/docs/lang/${obj.lang}/${urlParts.slice(4).join('/')}`
        }
        else {
          if (obj.lang === "en")
            return `${obj.url}`
          return `/docs/lang/${obj.lang}/${urlParts.slice(2).join('/')}`
        }
      }

      else if (obj.lang === "en")
        return `${obj.url}`
      return `/${obj.lang}${obj.url}`
    } else if (urlParts[1] === "old") {
      return `/${obj.lang}${obj.url}`
    } else if (supportedLangs.includes(urlParts[1])) {
      if (urlParts[2] == "blog")
        return `/blog`
      else if (obj.lang === "en")
        return `/${urlParts.slice(2).join('/')}`
      return `/${obj.lang}/${urlParts.slice(2).join('/')}`
    }
  })

  ty.addFilter("markdownUrl", (url) => markdownPages.markdownUrl(url, supportedLangs))

  ty.addFilter("markdownAlternate", (page) =>
    markdownPages.markdownKind(page.inputPath, supportedLangs) ? markdownPages.markdownUrl(page.url, supportedLangs) : "")

  ty.addFilter("markdownSource", (inputPath) => markdownPages.markdownFromSource(inputPath, replaceLink))

  ty.addFilter("markdownTitle", markdownPages.markdownTitle)

  ty.addFilter("plainText", markdownPages.plainText)

  const outputSections = ["pages", "blog pages", "doc pages", "language pages", "other files", "Markdown files"]
  const outputCounts = new Map()

  function countOutput(section) {
    outputCounts.set(section, (outputCounts.get(section) || 0) + 1)
  }

  function outputSection(outputPath) {
    const [folder] = path.relative(outputDir, outputPath).split(path.sep)
    if (supportedLangs.includes(folder)) return "language pages"
    if (folder === "blog") return "blog pages"
    if (folder === "docs") return "doc pages"
    return outputPath.endsWith(".html") ? "pages" : "other files"
  }

  ty.on("eleventy.before", () => outputCounts.clear())

  ty.on("eleventy.after", () => {
    const counts = outputSections.filter((section) => outputCounts.has(section)).map((section) => `${outputCounts.get(section)} ${section}`)
    console.log(`[11ty] Wrote ${counts.join(", ")}`)
  })

  ty.addTransform("countOutputs", function (content) {
    if (this.outputPath) countOutput(outputSection(this.outputPath))
    return content
  })

  ty.addTransform("markdownPages", function (content) {
    const kind = markdownPages.markdownKind(this.inputPath, supportedLangs)
    if (kind && this.outputPath) {
      const pageUrl = "/" + path.relative(outputDir, this.outputPath).replace(/index\.html$/, "")
      const markdown = kind === "source"
        ? markdownPages.markdownFromSource(this.inputPath, replaceLink)
        : markdownPages.markdownFromHtml(content, pageUrl)
      const markdownPath = path.join(outputDir, markdownPages.markdownUrl(pageUrl, supportedLangs))
      fs.mkdirSync(path.dirname(markdownPath), { recursive: true })
      fs.writeFileSync(markdownPath, markdown)
      countOutput("Markdown files")
    }
    return content
  })

  ty.addPlugin(pluginRss)

  const unknownStrings = new Set()

  ty.addFilter("i18n", function (key, _data, lang) {
    const locale = lang || (this.page || this.ctx.page)?.url?.split("/")[1]
    const strings = translations[key] || {}
    if (strings.en === undefined) unknownStrings.add(key)
    return strings[locale] ?? strings.en ?? key
  })

  ty.on("eleventy.before", () => unknownStrings.clear())

  ty.on("eleventy.after", () => {
    const allStrings = Object.values(translations)
    const untranslated = supportedLangs.filter((lang) => lang !== "en").sort()
      .map((lang) => [lang, allStrings.filter((strings) => strings[lang] === undefined).length])
      .filter(([, count]) => count > 0)
    if (untranslated.length > 0) {
      console.warn(`[i18n] Untranslated of ${allStrings.length} strings, English used: ${untranslated.map(([lang, count]) => `${lang} ${count}`).join(", ")}`)
    }
    if (unknownStrings.size > 0) console.warn(`[i18n] Missing in en.json: ${Array.from(unknownStrings).join(", ")}`)
  })

  // Keeps the same directory structure.
  ty.addPassthroughCopy("src/assets/")
  ty.addPassthroughCopy("src/fonts")
  ty.addPassthroughCopy("src/img")
  ty.addPassthroughCopy("src/video")
  ty.addPassthroughCopy("src/css")
  ty.addPassthroughCopy("src/js/**/*.js")
  ty.addPassthroughCopy("src/lottie_file")
  ty.addPassthroughCopy("src/contact/*.js")
  ty.addPassthroughCopy("src/call")
  ty.addPassthroughCopy("src/hero-phone")
  ty.addPassthroughCopy("src/hero-phone-dark")
  ty.addPassthroughCopy({ "src/link-images": "links/images" })
  ty.addPassthroughCopy("src/blog/images")
  ty.addPassthroughCopy("src/docs/*.png")
  ty.addPassthroughCopy("src/docs/images")
  ty.addPassthroughCopy("src/docs/guide/images")
  ty.addPassthroughCopy("src/docs/guide/diagrams")
  ty.addPassthroughCopy("src/docs/themes")
  ty.addPassthroughCopy("src/docs/protocol/diagrams")
  ty.addPassthroughCopy("src/docs/protocol/*.json")
  ty.addPassthroughCopy("src/images")
  ty.addPassthroughCopy("src/CNAME")
  ty.addPassthroughCopy("src/.well-known")
  ty.addPassthroughCopy("src/file-assets")
  ty.addPassthroughCopy("src/credits")

  ty.addCollection('blogs', function (collection) {
    return collection.getFilteredByGlob('src/blog/*.md').reverse()
  })

  ty.addCollection('docs', function (collection) {
    const docs = collection.getFilteredByGlob('src/docs/**/*.md')
      .map(doc => {
        return { url: doc.url, title: doc.data.title, inputPath: doc.inputPath }
      })

    let referenceContent = fs.readFileSync(path.resolve(__dirname, 'src/_data/docs_sidebar.json'), 'utf-8')
    referenceContent = JSON.parse(referenceContent).items

    const newDocs = []

    referenceContent.forEach(referenceMenu => {
      referenceMenu.data.forEach(referenceSubmenu => {
        docs.forEach(doc => {
          const url = doc.url.replace("/docs/", "")
          let urlParts = url.split("/")
          urlParts = urlParts.filter((ele) => ele !== "")

          if (doc.inputPath.split('/').includes(referenceSubmenu)) {
            if (urlParts.length === 1 && urlParts[0] !== "") {
              const index = newDocs.findIndex((ele) => ele.lang === 'en' && ele.menu === referenceMenu.menu)
              if (index !== -1) {
                newDocs[index].data.push(doc)
              }
              else {
                newDocs.push({
                  lang: 'en',
                  menu: referenceMenu.menu,
                  data: [doc],
                })
              }
            }
            else if (urlParts.length > 1 && urlParts[0] !== "" && urlParts[0] !== "lang") {
              const index = newDocs.findIndex((ele) => ele.lang === 'en' && ele.menu === referenceMenu.menu)
              if (index !== -1) {
                newDocs[index].data.push(doc)
              } else {
                newDocs.push({
                  lang: 'en',
                  menu: referenceMenu.menu,
                  data: [doc],
                })
              }
            }
            else if (urlParts.length === 3 && urlParts[0] === "lang" && urlParts[2] !== '') {
              const index = newDocs.findIndex((ele) => ele.lang === urlParts[1] && ele.menu === referenceMenu.menu)
              if (index !== -1) {
                newDocs[index].data.push(doc)
              }
              else {
                newDocs.push({
                  lang: urlParts[1],
                  menu: referenceMenu.menu,
                  data: [doc],
                })
              }
            }
            else if (urlParts.length > 3 && urlParts[0] === "lang" && urlParts[2] !== '') {
              const index = newDocs.findIndex((ele) => ele.lang === urlParts[1] && ele.menu === referenceMenu.menu)
              if (index !== -1) {
                newDocs[index].data.push(doc)
              }
              else {
                newDocs.push({
                  lang: urlParts[1],
                  menu: referenceMenu.menu,
                  data: [doc],
                })
              }
            }
          }
        })
      })
    })

    return newDocs
  })

  ty.addCollection("markdownDocs", (collection) =>
    collection.getFilteredByGlob("src/docs/**/*.md")
      .filter((doc) => markdownPages.markdownKind(doc.inputPath, supportedLangs) === "source")
      .sort((a, b) => a.url.localeCompare(b.url)))

  ty.setQuietMode(true)
  ty.setWatchThrottleWaitTime(100)
  ty.addWatchTarget("src/css")
  ty.addWatchTarget("markdown/")
  ty.addWatchTarget("components/Card.js")

  const markdownLib = markdownIt({
    html: true,
    breaks: true,
    linkify: true,
    replaceLink: replaceLink
  }).use(markdownItAnchor, {
    slugify: (str) =>
      slugify(str, {
        lower: true,
        strict: true,
      })
  }).use(markdownItReplaceLink)
  .use(markdownItFootnote)

  markdownLib.renderer.rules.footnote_anchor_name = function (tokens, idx, options, env) {
    var token = tokens[idx]
    var label = token.meta.label
    if (label) return label
    var n = Number(token.meta.id + 1).toString()
    var prefix = typeof env.docId === 'string' ? '-' + env.docId + '-' : ''
    return prefix + n
  }
  markdownLib.renderer.rules.footnote_caption = function (tokens, idx) {
    var n = Number(tokens[idx].meta.id + 1).toString()
    if (tokens[idx].meta.subId > 0) n += ':' + tokens[idx].meta.subId
    return n
  }
  markdownLib.renderer.rules.footnote_ref = function (tokens, idx, options, env, slf) {
    var id = slf.rules.footnote_anchor_name(tokens, idx, options, env, slf)
    var caption = slf.rules.footnote_caption(tokens, idx, options, env, slf)
    var refid = id
    if (tokens[idx].meta.subId > 0) refid += ':' + tokens[idx].meta.subId
    return '<sup class="footnote-ref"><a href="#note-' + id + '" id="ref-' + refid + '">' + caption + '</a></sup>'
  }
  markdownLib.renderer.rules.footnote_open = function (tokens, idx, options, env, slf) {
    var id = slf.rules.footnote_anchor_name(tokens, idx, options, env, slf)
    if (tokens[idx].meta.subId > 0) id += ':' + tokens[idx].meta.subId
    return '<li id="note-' + id + '" class="footnote-item">'
  }
  markdownLib.renderer.rules.footnote_anchor = function (tokens, idx, options, env, slf) {
    var id = slf.rules.footnote_anchor_name(tokens, idx, options, env, slf)
    if (tokens[idx].meta.subId > 0) id += ':' + tokens[idx].meta.subId
    return ' <a href="#ref-' + id + '" class="footnote-backref">↩︎</a>'
  }

  // replace the default markdown-it instance
  ty.setLibrary("md", markdownLib)

  return {
    dir: {
      input: 'src',
      includes: '_includes',
      output: outputDir,
    },
    templateFormats: ['md', 'njk', 'html'],
    markdownTemplateEngine: 'njk',
    htmlTemplateEngine: 'njk',
    dataTemplateEngine: 'njk',
  }
}
