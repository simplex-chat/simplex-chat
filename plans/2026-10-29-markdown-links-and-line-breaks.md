# Markdown: links in parentheses, line breaks

Date: 2026-10-29. Revises `2026-10-13-block-markdown.md`, sections 1 and 3.

References are to master at 1217fbea4. Haskell paths without a directory are in `src/Simplex/Chat/`.

## Decisions

```
M1  links in parentheses           GFM extended autolinks: a link after "(", without unmatched ")" at its end
M2  hyperlink texts                the anti-spoofing rule of sowLinkP (Markdown.hs:444-445) unchanged
M3  line breaks                    each line break inside a paragraph, a list item or a quote: HardBreak
M4  lines without a marker         after a list item: a continuation only when indented
                                   after a quote line: never a continuation
M5  *x* in messages with blocks    italic, as decided 2026-10-18
                                   the Markdown help shows **bold**
```

## Links

`wordMD`:

```
word        a link by linkMD, or else
            the text to the end of the first run of "(" without format, then a link by linkMD of the rest
            otherwise text
linkMD      the link text: the word without linkEndLength characters at its end
            a URI, a domain or an e-mail address, as now
            a SimpleX link: decoded from the link text
linkEnd     from the end, in one pass:
              punctuation by isPunctuation', as now
              ")" while the word has more ")" than "("
```

- GFM also starts links after "*", "_" and "~"; these start formats, as now.
- The address of a refused hyperlink is a link: `[What's new](https://simplex.chat)` is the text `[What's new](`, the link, the text `)`.

## Line breaks

```
breakAfter          HardBreak in each case; the syntax of a hard break stays in its token, as now
listItemP           continuation lines: indented (takeWhile1 isIndentChar)
quoteP              next lines: quote lines and blank quote lines
SoftBreak           not produced; kept in BlockToken for decoding
```

- An unindented line after a list item, and a line without ">" after a quote line, start a paragraph after a `LineBreak`.

## Help

```
Help.hs markdownInfo                   **bold**, Bold singleStar = False
MarkdownHelpView.kt                    "**$bold**"
MarkdownHelp.swift                     "**", the localized "bold", "**", as one Text
```

## Tests

`tests/MarkdownTests.hs`:

```
links in parentheses         "(x)", "see (x).", "(see x)", "(x_(y))", "x))", "(example.com)", "(chat@simplex.chat)"
refused hyperlinks           the address is a link
list items                   a URI, a hyperlink, formats, a link in parentheses, a refused hyperlink
line breaks                  HardBreak in paragraphs, list items, quotes, with CR, in setext headings
lines without a marker       after a list item and a quote line: a paragraph
```
