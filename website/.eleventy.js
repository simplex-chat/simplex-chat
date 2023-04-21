const markdownIt = require("markdown-it")
const markdownItAnchor = require("markdown-it-anchor")
const markdownItReplaceLink = require('markdown-it-replace-link')
const slugify = require("slugify")
const uri = require('fast-uri')
const i18n = require('eleventy-plugin-i18n')
const fs = require("fs")
const path = require("path")

const globalConfig = {
  onionLocation: "http://isdb4l77sjqoy2qq7ipum6x3at6hyn3jmxfx4zdhc72ufbmuq4ilwkqd.onion",
  siteLocation: "https://simplex.chat"
}

const translationsDirectoryPath = './langs'
const supportedRoutes = ["blog", "contact", "invitation", "docs", ""]
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
    .filter(doc => !doc.data.ignoreForWeb)
    .map(doc => {
      return { url: doc.url, title: doc.data.title, displayAt: doc.data.displayAt || 0 }
    })

    const newDocs = []
    docs.forEach(doc => {
      const url = doc.url.replace("/docs/", "")
      const urlParts = url.split("/")

      if (urlParts.includes("rfcs")) return false

      if (urlParts.length === 1 && urlParts[0] !== "") {
        const index = newDocs.findIndex((ele) => ele.lang === 'en' && ele.menu === 'root')
        if (index !== -1) {
          newDocs[index].data.push(doc)
        }
        else {
          newDocs.push({
            lang: 'en',
            menu: 'root',
            priority: 1,
            data: [doc],
          })
        }
      }
      else if (urlParts.length > 1 && urlParts[0] !== "" && urlParts[0] !== "lang") {
        const index = newDocs.findIndex((ele) => ele.lang === 'en' && ele.menu === urlParts[0])
        if (index !== -1) {
          newDocs[index].data.push(doc)
        } else {
          newDocs.push({
            lang: 'en',
            menu: urlParts[0],
            priority: 2,
            data: [doc],
          })
        }
      }
      else if (urlParts.length === 3 && urlParts[0] === "lang" && urlParts[2] !== '') {
        const index = newDocs.findIndex((ele) => ele.lang === urlParts[1] && ele.menu === 'root')
        if (index !== -1) {
          newDocs[index].data.push(doc)
        }
        else {
          newDocs.push({
            lang: urlParts[1],
            menu: 'root',
            priority: 1,
            data: [doc],
          })
        }
      }
      else if (urlParts.length > 3 && urlParts[0] === "lang" && urlParts[2] !== '') {
        const index = newDocs.findIndex((ele) => ele.lang === urlParts[1] && ele.menu === urlParts[2])
        if (index !== -1) {
          newDocs[index].data.push(doc)
        }
        else {
          newDocs.push({
            lang: urlParts[1],
            menu: urlParts[2],
            priority: 2,
            data: [doc],
          })
        }
      }
    })

    newDocs.forEach(obj => {
      obj.data.sort((a, b) => a.displayAt - b.displayAt);
    });

    return newDocs.sort((a, b) => a.priority - b.priority)
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
      if (parsed.scheme || parsed.host || !parsed.path.endsWith(".md")) {
        return link
      }
      parsed.path = parsed.path.replace(/\.md$/, ".html")
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
