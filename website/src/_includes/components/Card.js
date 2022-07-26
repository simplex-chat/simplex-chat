const { html } = require("common-tags");

const Card = ({ title, link, linkText, raised }) => {
  return html`
    <div>
      <h2>${title}</h2>
      <a href="${link}" class="text-blue-700 mb-4 inline-block underline">${linkText}</a>
    </div>
  `;
};

module.exports = Card;
