// const Card = require('./src/_includes/components/Card');

module.exports = (ty) => {
  // Keeps the same directory structure.
  ty.addPassthroughCopy("src/assets/")
  ty.addPassthroughCopy("src/img")
  ty.addPassthroughCopy("src/css")
  ty.addPassthroughCopy("src/js")
  ty.addPassthroughCopy("src/contact")
  ty.addPassthroughCopy("src/app-demo")

  ty.addCollection("blogs", (collection) =>
    collection.getFilteredByGlob("src/blog/*.md").reverse()
  )

  ty.addWatchTarget("src/css")
  ty.addWatchTarget("markdown/")
  ty.addWatchTarget("components/Card.js")

  // ty.addShortcode("Card",Card);

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
