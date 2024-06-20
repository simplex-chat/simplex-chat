const markdownIt = require("markdown-it")
const markdownItAnchor = require("markdown-it-anchor")
const markdownItReplaceLink = require('markdown-it-replace-link')
const slugify = require("slugify")
const uri = require('fast-uri')
const i18n = require('eleventy-plugin-i18n')
const fs = require("fs")
const path = require("path")
const matter = require('gray-matter')
const pluginRss = require('@11ty/eleventy-plugin-rss')
const { JSDOM } = require('jsdom')


// The implementation of Glossary feature
const md = new markdownIt()
const glossaryMarkdownContent = fs.readFileSync(path.resolve(__dirname, '../docs/GLOSSARY.md'), 'utf8')
const glossaryHtmlContent = md.render(glossaryMarkdownContent)
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
})


const globalConfig = {
  onionLocation: "http://isdb4l77sjqoy2qq7ipum6x3at6hyn3jmxfx4zdhc72ufbmuq4ilwkqd.onion",
  siteLocation: "https://simplex.chat"
}

const translationsDirectoryPath = './langs'
const supportedRoutes = ["blog", "contact", "invitation", "docs", "fdroid", ""]
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

module.exports = function (ty) {
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
    const dom = new JSDOM(content)
    const { document } = dom.window
    const body = document.querySelector('body')
    const allContentNodes = document.querySelectorAll('p, td, a, h1, h2, h3, h4')
    const overlayIds = []

    glossary.forEach((term, index) => {
      let changeNoted = false
      const id = term.term.toLowerCase().replace(/\s/g, '-')

      allContentNodes.forEach((node) => {
        const regex = new RegExp(`(?<![/#])\\b${term.term}\\b`, 'gi')
        const replacement = `<span data-glossary="tooltip-${id}" class="glossary-term">${term.term}</span>`
        const beforeContent = node.innerHTML
        node.innerHTML = node.innerHTML.replace(regex, replacement)
        if (beforeContent !== node.innerHTML && !changeNoted) {
          changeNoted = true
        }
      })

      if (changeNoted) {
        const definitionTooltipDiv = document.createElement('div')
        definitionTooltipDiv.id = `tooltip-${id}`
        definitionTooltipDiv.className = "glossary-tooltip"
        const titleH4 = document.createElement('h4')
        titleH4.innerHTML = term.term
        titleH4.className = "tooltip-title"
        const p = document.createElement('p')
        p.innerHTML = term.tooltip
        const innerDiv = document.createElement('div')
        innerDiv.appendChild(titleH4)
        innerDiv.appendChild(p)
        if (term.hasMultipleParagraphs) {
          const readMoreBtn = document.createElement('button')
          readMoreBtn.innerHTML = "Read more"
          readMoreBtn.className = "read-more-btn open-overlay-btn"
          readMoreBtn.setAttribute('data-show-overlay', id)
          innerDiv.appendChild(readMoreBtn)
        }
        innerDiv.className = "tooltip-content"
        definitionTooltipDiv.appendChild(innerDiv)
        body.appendChild(definitionTooltipDiv)
      }

      let tooltipDom = new JSDOM(term.definition)
      let tooltipDocument = tooltipDom.window.document
      const hashList = [term.term.toLowerCase().replace(/\s/g, '-')]
      tooltipDocument.querySelectorAll('a[href*="#"]').forEach(a => {
        let hashIndex = a.href.indexOf("#")
        if (hashIndex !== -1) {
          let hash = a.href.substring(hashIndex + 1)
          hashList.push(hash)
        }
      })

      hashList.forEach(hash => {
        if (!overlayIds.includes(hash)) {
          let termFromHash = glossary.find(term => term.term.toLowerCase().replace(/\s/g, '-') === hash)
          if (!termFromHash) return

          const overlayDiv = document.createElement('div')
          overlayDiv.id = hash
          overlayDiv.className = "overlay glossary-overlay hidden"
          const overlayCardDiv = document.createElement('div')
          overlayCardDiv.className = "overlay-card"
          const overlayTitleH1 = document.createElement('h1')
          overlayTitleH1.className = "overlay-title"
          overlayTitleH1.innerHTML = termFromHash.term
          const overlayContent = document.createElement('div')
          overlayContent.className = "overlay-content"
          overlayContent.innerHTML = termFromHash.definition
          const crossSVG = document.createElementNS("http://www.w3.org/2000/svg", "svg")
          crossSVG.setAttribute('class', 'close-overlay-btn')
          crossSVG.setAttribute('id', 'cross')
          crossSVG.setAttribute('width', '16')
          crossSVG.setAttribute('height', '16')
          crossSVG.setAttribute('viewBox', '0 0 13 13')
          crossSVG.setAttribute('xmlns', 'http://www.w3.org/2000/svg')
          const crossPath = document.createElementNS("http://www.w3.org/2000/svg", "path")
          crossPath.setAttribute('d', 'M12.7973 11.5525L7.59762 6.49833L12.7947 1.44675C13.055 1.19371 13.0658 0.771991 12.8188 0.505331C12.5718 0.238674 12.1602 0.227644 11.8999 0.480681L6.65343 5.58028L1.09979 0.182228C0.805 0.002228 0.430001 0.002228 0.135211 0.182228C-0.159579 0.362228 -0.159579 0.697228 0.135211 0.877228L5.68885 6.27528L0.4918 11.3295C0.231501 11.5825 0.220703 12.0042 0.467664 12.2709C0.714625 12.5376 1.12625 12.5486 1.38655 12.2956L6.63302 7.196L12.1867 12.5941C12.4815 12.7741 12.8565 12.7741 13.1513 12.5941C13.4461 12.4141 13.4461 12.0791 13.1513 11.8991L12.7973 11.5525Z')
          crossSVG.appendChild(crossPath)

          overlayCardDiv.appendChild(overlayTitleH1)
          overlayCardDiv.appendChild(overlayContent)
          overlayCardDiv.appendChild(crossSVG)
          overlayDiv.appendChild(overlayCardDiv)
          body.appendChild(overlayDiv)
          overlayIds.push(hash)
        }
      })
    })

    return dom.serialize()
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
    }
    else if (supportedLangs.includes(urlParts[1])) {
      if (urlParts[2] == "blog")
        return `/blog`
      else if (obj.lang === "en")
        return `/${urlParts.slice(2).join('/')}`
      return `/${obj.lang}/${urlParts.slice(2).join('/')}`
    }
  })

  ty.addPlugin(pluginRss)

  ty.addPlugin(i18n, {
    translations,
    fallbackLocales: {
      '*': 'en'
    },
    defaultLocale: 'en',
  })

  // Keeps the same directory structure.
  ty.addPassthroughCopy("src/assets/")
  ty.addPassthroughCopy("src/fonts")
  ty.addPassthroughCopy("src/img")
  ty.addPassthroughCopy("src/video")
  ty.addPassthroughCopy("src/css")
  ty.addPassthroughCopy("src/js")
  ty.addPassthroughCopy("src/lottie_file")
  ty.addPassthroughCopy("src/contact/*.js")
  ty.addPassthroughCopy("src/call")
  ty.addPassthroughCopy("src/hero-phone")
  ty.addPassthroughCopy("src/hero-phone-dark")
  ty.addPassthroughCopy("src/blog/images")
  ty.addPassthroughCopy("src/docs/*.png")
  ty.addPassthroughCopy("src/docs/images")
  ty.addPassthroughCopy("src/docs/protocol/diagrams")
  ty.addPassthroughCopy("src/docs/protocol/*.json")
  ty.addPassthroughCopy("src/images")
  ty.addPassthroughCopy("src/CNAME")
  ty.addPassthroughCopy("src/.well-known")

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

  ty.addWatchTarget("src/css")
  ty.addWatchTarget("markdown/")
  ty.addWatchTarget("components/Card.js")

  const markdownLib = markdownIt({
    html: true,
    breaks: true,
    linkify: true,
    replaceLink: function (link, _env) {
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
        const fileContent = fs.readFileSync(linkFile, 'utf8')
        parsed.path = (matter(fileContent).data?.permalink || parsed.path).replace(/\.md$/, ".html").toLowerCase()
        return parsed.path
      } else if (!fs.existsSync(linkFile)) {
        linkFile = linkFile.replace('/website/src', '')
        if (fs.existsSync(linkFile)) {
          // this condition works if the link is a valid project file
          const githubUrl = "https://github.com/simplex-chat/simplex-chat/blob/stable"
          const keyword = "/simplex-chat"
          index = linkFile.indexOf(keyword)
          linkFile = linkFile.substring(index + keyword.length)
          parsed.path = `${githubUrl}${linkFile}`
          return parsed.path
        } else {
          // if the link is not a valid website file or project file
          throw new Error(`Broken link: ${parsed.path} in ${hostFile}`)
        }
      }

      return uri.serialize(parsed)
    }
  }).use(markdownItAnchor, {
    slugify: (str) =>
      slugify(str, {
        lower: true,
        strict: true,
      })
  }).use(markdownItReplaceLink)

  // replace the default markdown-it instance
  ty.setLibrary("md", markdownLib)

  return {
    dir: {
      input: 'src',
      includes: '_includes',
      output: '_site',
    },
    templateFormats: ['md', 'njk', 'html'],
    markdownTemplateEngine: 'njk',
    htmlTemplateEngine: 'njk',
    dataTemplateEngine: 'njk',
  }
}