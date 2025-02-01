const { expect } = require('chai');
const { JSDOM } = require('jsdom');

describe('Web App', () => {
  let dom;
  let document;

  beforeEach(async () => {
    dom = await JSDOM.fromFile('website/src/index.html', {
      runScripts: 'dangerously',
      resources: 'usable'
    });
    document = dom.window.document;

    return new Promise((resolve) => {
      dom.window.addEventListener('load', resolve);
    });
  });

  it('should display the header with the title "Simplex Chat Web App"', () => {
    const header = document.querySelector('header h1');
    expect(header.textContent).to.equal('Simplex Chat Web App');
  });

  it('should add a user message to the container when the button is clicked', () => {
    const input = document.querySelector('.web-app-input');
    const button = document.querySelector('.web-app-button');
    const container = document.querySelector('.web-app-container');

    input.value = 'Test message';
    button.click();

    const userMessage = container.querySelector('p');
    expect(userMessage.textContent).to.equal('Test message');
  });

  it('should clear the input field after adding a user message', () => {
    const input = document.querySelector('.web-app-input');
    const button = document.querySelector('.web-app-button');

    input.value = 'Test message';
    button.click();

    expect(input.value).to.equal('');
  });
});
