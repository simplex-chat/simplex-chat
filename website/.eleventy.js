const markdownIt = require("markdown-it")
const markdownItAnchor = require("markdown-it-anchor")
const markdownItReplaceLink = require('markdown-it-replace-link')
const slugify = require("slugify")
const uri = require('fast-uri')

module.exports = function (ty) {
  // Keeps the same directory structure.
  ty.addPassthroughCopy("src/assets/")
  ty.addPassthroughCopy("src/img")
  ty.addPassthroughCopy("src/css")
  ty.addPassthroughCopy("src/js")
  ty.addPassthroughCopy("src/contact")
  ty.addPassthroughCopy("src/app-demo")
  ty.addPassthroughCopy("src/blog/images")
  ty.addPassthroughCopy("src/images")
  ty.addPassthroughCopy("src/CNAME")

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
