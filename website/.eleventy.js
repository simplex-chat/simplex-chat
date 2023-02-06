const markdownIt = require("markdown-it")
const markdownItAnchor = require("markdown-it-anchor")
const markdownItReplaceLink = require('markdown-it-replace-link')
const slugify = require("slugify")
const uri = require('fast-uri')
const fs = require("fs");
const path = require("path");

const globalConfig = {
  onionLocation: "http://isdb4l77sjqoy2qq7ipum6x3at6hyn3jmxfx4zdhc72ufbmuq4ilwkqd.onion",
  siteLocation: "https://simplex.chat"
}

// Load the JSON file
const translations = JSON.parse(fs.readFileSync(path.resolve(__dirname, "translations.json"), "utf-8"))
const supportedLangs = Object.keys(translations)

module.exports = function (ty) {
  ty.addShortcode("cfg", (name) => globalConfig[name])

  ty.addShortcode("getValue", (obj) => {
    const lang = obj.url.split("/")[1]
    if (supportedLangs.includes(lang)) {
      const value = translations[lang][obj.key]
      if (value) {
        return value
      }
    }
    return translations["en"][obj.key]
  })

  ty.addShortcode("getlang", (path) => {
    const lang = path.split("/")[1]
    if (supportedLangs.includes(lang)) {
      return lang
    }
    return "en"
  })

  ty.addShortcode("getlangRoute", (path) => {
    const supportedRoutes = ["blog", "contact", "invitation", ""]
    const lang = path.split("/")[1]
    if (supportedRoutes.includes(lang)) return ""
    if (supportedLangs.includes(lang)) return `/${lang}`
    return "/en"
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
  supportedLangs.forEach(lang => ty.addPassthroughCopy(`src/${lang}/blog/images`))
  ty.addPassthroughCopy("src/images")
  ty.addPassthroughCopy("src/CNAME")
  ty.addPassthroughCopy("src/.well-known")

  ty.addCollection('blogs', function (collection) {
    return collection.getFilteredByGlob('src/blog/*.md').reverse()
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

  // Pass the translations to Nunjucks as a global variable
  ty.addNunjucksGlobal("translations", translations)

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
