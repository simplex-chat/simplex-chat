const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');
const test = require('node:test');
const vm = require('node:vm');

const lib = {};
vm.runInNewContext(fs.readFileSync(path.join(__dirname, '../src/js/simplex-lib.jsc'), 'utf8'), lib);
const { escapeHtml, renderMarkdown } = lib;

const plain = (text) => ({ text });
const markup = (type, text, fields) => ({ format: { type: 'markup', token: { type, ...fields } }, text });
const lineBreak = markup('lineBreak', '\n');
const hardBreak = markup('hardBreak', '\n');
const paragraphBreak = (lineBreaks, text = '\n'.repeat(lineBreaks)) => markup('paragraphBreak', text, { lineBreaks });
const headingMark = markup('headingMark', '# ', { level: 1 });
const listMark = (text) => markup('listMark', text);
const quoteMark = (text) => markup('quoteMark', text);
const fence = markup('fenceMark', '```');

const range = (from, to) => ({ from, to });
const paragraph = (from, to) => ({ type: 'paragraph', content: range(from, to) });
const heading = (from, to) => ({ type: 'heading', level: 1, content: range(from, to) });
const entry = (from, to, items = []) => ({ content: range(from, to), items });
const list = (...items) => ({ type: 'list', items });

const br = (lines) => '<br>'.repeat(lines);
const div = (html) => `<div>${html}</div>`;
const h1 = (html) => `<div class="md-h1">${html}</div>`;
const li = (html, level = 0) => `<div class="md-entry" style="padding-left: ${1.25 * level}em;"><span class="md-mark">•</span><span class="md-text">${html}</span></div>`;

const check = (fts, blocks, html, text = '') => assert.strictEqual(renderMarkdown(fts, blocks, text), html);

test('each empty line between blocks is one line', () => {
  const blocks = [heading(1, 2), paragraph(3, 4)];
  check([headingMark, plain('a'), lineBreak, plain('b')], blocks, h1('a') + 'b');
  check([headingMark, plain('a'), paragraphBreak(2), plain('b')], blocks, h1('a') + br(1) + 'b');
  check([headingMark, plain('a'), paragraphBreak(3), plain('b')], blocks, h1('a') + br(2) + 'b');
  check([headingMark, plain('a'), paragraphBreak(3, '\n  \n\t\n'), plain('b')], blocks, h1('a') + br(2) + 'b');
});

test('empty lines before the first block', () => {
  check([paragraphBreak(2), headingMark, plain('a')], [heading(2, 3)], br(2) + h1('a'));
  check([paragraphBreak(2, '\n \n'), listMark('- '), plain('a')], [list(entry(2, 3))], br(2) + li('a'));
});

test('empty lines after the last block', () => {
  const fts = [headingMark, plain('a')];
  const blocks = [heading(1, 2)];
  check(fts, blocks, h1('a'), '# a');
  check(fts, blocks, h1('a'), '# a\n');
  check([...fts, lineBreak], blocks, h1('a') + br(1), '# a\n\n');
  check([...fts, paragraphBreak(2)], blocks, h1('a') + br(2), '# a\n\n\n');
  check([...fts, paragraphBreak(1, '\n  ')], blocks, h1('a'), '# a\n  ');
  check([...fts, paragraphBreak(1, '\n  ')], blocks, h1('a') + br(1), '# a\n  \n');
  check([...fts, paragraphBreak(2, '\n\n  ')], blocks, h1('a') + br(1), '# a\n\n  ');
  check([...fts, paragraphBreak(2, '\n\n  ')], blocks, h1('a') + br(2), '# a\n\n  \n');
});

test('a message ending in a paragraph ends in inline text', () => {
  const twoParagraphs = [paragraph(0, 1), paragraph(2, 3)];
  const oneParagraph = [paragraph(0, 1)];
  check([plain('a'), paragraphBreak(2), plain('b')], twoParagraphs, div('a') + br(1) + 'b', 'a\n\nb');
  check([plain('a'), paragraphBreak(3), plain('b')], twoParagraphs, div('a') + br(2) + 'b', 'a\n\n\nb');
  check([paragraphBreak(2), plain('a')], [paragraph(1, 2)], br(2) + 'a', '\n\na');
  check([plain('a'), paragraphBreak(1, '\n  ')], oneParagraph, 'a', 'a\n  ');
});

test('a message ending in a paragraph with empty lines at the end', () => {
  const oneParagraph = [paragraph(0, 1)];
  check([plain('a'), lineBreak], oneParagraph, div('a') + br(1), 'a\n\n');
  check([plain('a'), paragraphBreak(2)], oneParagraph, div('a') + br(2), 'a\n\n\n');
  check([plain('a'), paragraphBreak(1, '\n  ')], oneParagraph, div('a') + br(1), 'a\n  \n');
});

test('empty lines between list entries', () => {
  check(
    [listMark('- '), plain('a'), paragraphBreak(2), listMark('- '), plain('b'), paragraphBreak(3), listMark('- '), plain('c')],
    [list(entry(1, 2), entry(4, 5), entry(7, 8))],
    li('a') + br(1) + li('b') + br(2) + li('c')
  );
  check(
    [listMark('- '), plain('a'), lineBreak, listMark('  - '), plain('b'), paragraphBreak(2), listMark('- '), plain('c')],
    [list(entry(1, 2, [entry(4, 5)]), entry(7, 8))],
    li('a') + li('b', 1) + br(1) + li('c')
  );
  check(
    [listMark('- '), plain('a'), paragraphBreak(2), listMark('  - '), plain('b')],
    [list(entry(1, 2, [entry(4, 5)]))],
    li('a') + br(1) + li('b', 1)
  );
});

test('empty lines next to lists', () => {
  check(
    [plain('a'), paragraphBreak(3), listMark('- '), plain('b')],
    [paragraph(0, 1), list(entry(3, 4))],
    div('a') + br(2) + li('b')
  );
  check(
    [listMark('- '), plain('a'), paragraphBreak(2), plain('b')],
    [list(entry(1, 2)), paragraph(3, 4)],
    li('a') + br(1) + 'b'
  );
});

test('empty lines next to code blocks, tables and rules', () => {
  const html = renderMarkdown(
    [fence, hardBreak, plain('x'), hardBreak, fence, paragraphBreak(2), markup('ruleMark', '---')],
    [{ type: 'codeBlock', content: range(2, 3) }, { type: 'rule', mark: range(6, 7) }],
    ''
  );
  assert.ok(html.startsWith('<div class="md-code"><pre>x</pre>'));
  assert.ok(html.endsWith('</button></div><br><hr class="md-rule">'));
  check(
    [plain('a'), paragraphBreak(2), markup('rowMark', '| '), plain('x'), markup('cellMark', ' |'), hardBreak, markup('delimiterRow', '|---|', { columns: ['none'] }), paragraphBreak(3), plain('b')],
    [paragraph(0, 1), { type: 'table', columns: ['none'], header: [range(3, 4)], rows: [] }, paragraph(8, 9)],
    div('a') + br(1) + '<div class="md-table"><div class="md-frame"><table><tr><th style="text-align: left;">x</th></tr></table></div></div>' + br(2) + 'b'
  );
});

test('empty lines in a quote are lines of its text', () => {
  check(
    [quoteMark('> '), plain('a'), hardBreak, quoteMark('>'), hardBreak, quoteMark('> '), plain('b')],
    [{ type: 'quote', content: range(1, 7) }],
    '<div class="md-quote"><span class="md-bar"></span><div class="md-text">a<br><br>b</div></div>'
  );
});

test('at most 100 empty lines in a row', () => {
  check([plain('a'), paragraphBreak(5000), plain('b')], [paragraph(0, 1), paragraph(2, 3)], div('a') + br(100) + 'b');
  check([paragraphBreak(5000), plain('a')], [paragraph(1, 2)], br(100) + 'a');
  check([plain('a'), paragraphBreak(5000)], [paragraph(0, 1)], div('a') + br(100));
  check(
    [listMark('- '), plain('a'), paragraphBreak(5000), listMark('- '), plain('b')],
    [list(entry(1, 2), entry(4, 5))],
    li('a') + br(100) + li('b')
  );
});

test('plain text and inline formatting without blocks', () => {
  assert.strictEqual(escapeHtml('a\n\n\nb'), 'a<br><br><br>b');
  check([{ format: { type: 'bold', singleStar: true }, text: 'a' }, plain('\nb')], undefined, '<strong>a</strong><br>b');
});

test('invalid blocks are drawn as text', () => {
  check([plain('a'), paragraphBreak(2), plain('b')], [{ type: 'unknown' }], 'a<br><br>b');
});

test('text outside messages keeps one gap between blocks', () => {
  const page = (fts, blocks, html) => assert.strictEqual(renderMarkdown(fts, blocks), html);
  const gap = '<div class="md-gap"></div>';
  const blocks = [heading(1, 2), paragraph(3, 4)];
  page([headingMark, plain('a'), lineBreak, plain('b')], blocks, h1('a') + div('b'));
  page([headingMark, plain('a'), paragraphBreak(3), plain('b')], blocks, h1('a') + gap + div('b'));
  page([plain('a'), paragraphBreak(2), listMark('- '), plain('b')], [paragraph(0, 1), list(entry(3, 4))], div('a') + gap + li('b'));
  page([listMark('- '), plain('a'), paragraphBreak(2), listMark('- '), plain('b')], [list(entry(1, 2), entry(4, 5))], li('a') + li('b'));
  page([paragraphBreak(2), plain('a'), paragraphBreak(2)], [paragraph(1, 2)], div('a'));
});
