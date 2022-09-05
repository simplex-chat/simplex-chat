module.exports = function (eleventyConfig) {
  // Keeps the same directory structure.
  eleventyConfig.addPassthroughCopy("src/assets/");
  eleventyConfig.addPassthroughCopy("src/img");
  eleventyConfig.addPassthroughCopy("src/css");
  eleventyConfig.addPassthroughCopy("src/js");
  eleventyConfig.addPassthroughCopy("src/contact");
  eleventyConfig.addPassthroughCopy("src/app-demo");
  eleventyConfig.addPassthroughCopy("src/blog/images");

  eleventyConfig.addCollection('blogs', function (collection) {
    return collection.getFilteredByGlob('src/blog/*.md').reverse();
  });

  eleventyConfig.addWatchTarget("src/css");
  eleventyConfig.addWatchTarget("markdown/");
  eleventyConfig.addWatchTarget("components/Card.js");

  const markdownIt = require("markdown-it");
  const markdownItAnchor = require("markdown-it-anchor");
  const slugify = require("slugify");

  const markdownLib = markdownIt({
    html: true,
    breaks: true,
    linkify: true
  }).use(markdownItAnchor, {
    slugify: (str) =>
      slugify(str, {
        lower: true,
        strict: true,
      })
  });

  // replace the default markdown-it instance
  eleventyConfig.setLibrary("md", markdownLib);

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
