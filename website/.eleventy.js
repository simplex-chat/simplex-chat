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
  const markdownItReplaceLink = require('markdown-it-replace-link');
  const slugify = require("slugify");

  const markdownLib = markdownIt({
    html: true,
    breaks: true,
    linkify: true,
    replaceLink: function (link, env) {
      const dotSepList = link.split(".");
      if (dotSepList[0] == "") {
        const hashSepList = dotSepList[dotSepList.length - 1].split("#")
        if (hashSepList[0] == "md") {
          let str = `.${dotSepList[1]}`;
          for (let i = 2; i < dotSepList.length; i++) {
            if (dotSepList[i].substring(0, 2) != "md") {
              str += "." + dotSepList[i];
            } else {
              str += ".html";
              break;
            };
          }
          if (hashSepList[1]) {
            str += "#" + hashSepList[1];
          }
          return str;
        }
        else {
          return link;
        }
      }
      else {
        return link;
      }
    }
  }).use(markdownItAnchor, {
    slugify: (str) =>
      slugify(str, {
        lower: true,
        strict: true,
      })
  }).use(markdownItReplaceLink);

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
