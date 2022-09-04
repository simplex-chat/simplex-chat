module.exports = function (eleventyConfig) {
    // Keeps the same directory structure.
    eleventyConfig.addPassthroughCopy("src/assets/");
    eleventyConfig.addPassthroughCopy("src/img");
    eleventyConfig.addPassthroughCopy("src/css");
    eleventyConfig.addPassthroughCopy("src/js");
    eleventyConfig.addPassthroughCopy("src/contact");
    eleventyConfig.addPassthroughCopy("src/app-demo");

    eleventyConfig.addCollection('blogs', function (collection) {
        return collection.getFilteredByGlob('src/blog/*.md').reverse();
    });

    eleventyConfig.addWatchTarget("src/css");
    eleventyConfig.addWatchTarget("markdown/");
    eleventyConfig.addWatchTarget("components/Card.js");

    // this works because Eleventy also installs markdown-it 
    const markdownIt = require("markdown-it");

    // create a new markdown-it instance with the plugin
    const markdownItAnchor = require("markdown-it-anchor");

    const markdownLib = markdownIt({
        html: true,
        breaks: true,
        linkify: true
    }).use(markdownItAnchor);

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
