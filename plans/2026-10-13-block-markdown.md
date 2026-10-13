# Block markdown: core

Date: 2026-10-13.

Block syntax of standard Markdown is parsed in the core for every text and returned in `formattedText` as marker segments of the same flat list.

## Syntax

### Lines

```
line break       LF; a CR before it is part of the line break; a final line break is dropped, as now (T.lines, Markdown.hs:162)
tab              4 columns
line             indentation, quote markers, indentation, content
quote marker     ">" before a space, a tab, ">" or the line end; one space or tab after it is part of the marker
                 the first marker is after at most 3 columns of indentation
                 at most maxQuoteDepth markers; a further ">" is content
blank line       empty content
content column   the column after the marker of the innermost open list item, plus 1; 0 outside list items
```

### Blocks

The content of a line indented less than 4 columns from the content column is tested in this order. Every other line is a paragraph line.

```
code        3 or more "`", then an info string of characters other than "`"
            until a line of "`" only, at least as many, then white space; or until the end of the text
heading     1 to 6 "#", a space or tab, text
rule        3 or more of one character of "-", "*", "_"; then white space only
list item   "-", "*" or "+"; or 1 to 9 digits and "." or ")"; then a space or tab, text
row         a line with "|", while a table is open
table       a line with "|", when the next line is a delimiter row with the same number of cells
escaped     "\" before a quote marker or before a line of the kinds above
            "\" before "." or ")" of a list number
paragraph   any other line
```

- Text of a heading, list item, table cell and paragraph is parsed by `markdownP`. Code is plain text.
- Code: `language` is the trimmed info string. In each line of the block, the quote markers of the fence line and the indentation up to the column of the fence are syntax; the rest of the line is code. A closing fence is indented less than 4 columns from the column of the fence. The block is ended by a line with fewer quote markers.
- List item: the text after the marker is inline text. `number` is the digits and the delimiter, as written. Nesting is set by indentation (Parser).
- Table: a delimiter cell is `:?-+:?`; a delimiter row has at least one `|` and the quote depth of the header row. Cells are divided at each `|`, except a `|` after `\`; the outer `|` are optional; cells are trimmed. In a row longer than the header, the rest of the line is the text of the last cell. The table is ended by a blank line, another block or another quote depth.
- Paragraph: line breaks and blank lines between its lines are text, as now. Indentation beyond the content column is text, as now.

### Existing syntax

```
#secret#   text is directly after "#" (secret, Markdown.hs:244-249); a heading has a space or tab after "#"
#name      text is directly after "#" (nameRefP, :252-265)
*bold*     text is directly after "*" (md, :233); a list item has a space or tab after its marker
+phone     a digit is directly after "+" (phoneP, :311)
/command   a one-line text with "/" first is one command (:151-157); tested before blocks
           in other lines a command is inline, as now
```

### Inline

`markdownP` (`Markdown.hs:205-378`):

```
**x**         Strong (boldP, :236-241)
*x*           Bold, as now
~~x~~         StrikeThrough; edge rule of ~x~ (md, :231-235)
[text](uri)   text: ended by "]" or "["; a text ended by "[" is text; punctuation test below
              uri: until ")" or white space; "(" ")" pairs inside, one level
```

Link text, `sowLinkP` (`:281-293`):

- A link with one address in the text and another in the URI is rejected by the punctuation test (`:284-285`): a text with punctuation is a link only when the URI equals the text, or `https://` and the text.
- Ignored by the test, as now: the characters of `-_`. Added: the characters of `,;!?'"()&`.
- Every other punctuation character is tested as now.

### Excluded: text, as now

```
raw HTML
images ![alt](uri)
setext headings; "---" under a text line is a rule
indented code
"~~~" fences
__x__
inline backslash escapes; "\|" in a table cell is text
hard break syntax; every line break is a line break
lazy continuation; a quote or a list item is ended by a line that has less than its quote markers or indentation
a quote inside a list item; with at most 3 columns of indentation it is a quote outside the list
nested inline formats
emphasis with its start inside a word (fragmentP, Markdown.hs:209-226)
```

### Incomplete syntax in a live message

```
open code fence                   code to the end of the text
table header row                  last line of a paragraph; a table once the delimiter row is complete
rule characters at the text end   rule; text once another character is added to the line
block marker before its text      text; a block once a space and text are added
```

## Types

`Markdown.hs`:

```haskell
data Format
  = Bold                   -- *x*
  | ...                    -- other existing constructors (:53-67)
  | Strong                 -- **x**
  | Block
      { block :: BlockFormat,
        quoteDepth :: Int, -- enclosing quotes, 0 .. maxQuoteDepth
        listDepth :: Int   -- enclosing list items, 0 .. maxListDepth
      }
  | TableRow
  | TableCell
  | Syntax
  | Unknown {json :: J.Value}

data BlockFormat
  = BFParagraph
  | BFHeading {level :: Int}           -- 1 .. 6
  | BFCode {language :: Maybe Text}
  | BFListItem {number :: Maybe Text}  -- "2)", "007."; absent for a bullet
  | BFTable {columns :: [TableAlign]}  -- one per cell of the header row
  | BFRule

data TableAlign
  = TANone
  | TALeft
  | TACenter
  | TARight
```

- Markers: `Block`, `TableRow`, `TableCell`, `Syntax` (`isMarker :: Format -> Bool`).
- `FormattedText`, `MarkdownList`, `ParsedMarkdown` and every record with `formattedText` are unchanged.
- JSON: `Format` as now (`sumTypeJSON fstToLower`, `:536`); `BlockFormat` with `sumTypeJSON (dropPrefix "BF")`; `TableAlign` with `enumJSON (dropPrefix "TA")`.

Values of `format`, default and `swiftJSON`; `B` is a value of the field `block`; `language` and `number` are omitted when absent:

```
strong      {"type":"strong"}                              {"_owsf":true,"strong":{}}
tableRow    {"type":"tableRow"}                            {"_owsf":true,"tableRow":{}}
tableCell   {"type":"tableCell"}                           {"_owsf":true,"tableCell":{}}
syntax      {"type":"syntax"}                              {"_owsf":true,"syntax":{}}
block       {"type":"block","block":B,"quoteDepth":0,"listDepth":0}
                                                           {"_owsf":true,"block":{"block":B,"quoteDepth":0,"listDepth":0}}
B:
paragraph   {"type":"paragraph"}                           {"_owsf":true,"paragraph":{}}
heading     {"type":"heading","level":2}                   {"_owsf":true,"heading":{"level":2}}
code        {"type":"code","language":"haskell"}           {"_owsf":true,"code":{"language":"haskell"}}
listItem    {"type":"listItem","number":"2)"}              {"_owsf":true,"listItem":{"number":"2)"}}
table       {"type":"table","columns":["none","center"]}   {"_owsf":true,"table":{"columns":["none","center"]}}
rule        {"type":"rule"}                                {"_owsf":true,"rule":{}}
```

Segments of `| a | b |`, `|---|:-:|`, `| 1 | 2 |`:

```
block table [none, center]   "| "
plain                        "a"
tableCell                    " | "
plain                        "b"
tableRow                     " |\n|---|:-:|\n| "
plain                        "1"
tableCell                    " | "
plain                        "2"
syntax                       " |"
```

Text of segments:

```
marker    the source characters from the end of the previous segment to the start of the block text; it can be empty
          only syntax characters, white space, the list number and the info string; the last two are also block fields
order     segments are in source order; every source character is in one segment, as text or as a delimiter restored by markdownText
source    T.concat (map markdownText ft) is the text
          except, as now: the final line break; colour names written as digits (:494-505)
          except, new: ~~x~~ written as ~x~
          markdownText (:472-505): Strong is **t**; a marker is t
```

## Dialect by layout

- The core has one dialect. Every call site of `parseMaybeMarkdownList` and `chatParseMarkdown` (`Mobile.hs:396`) is unchanged.
- `Bold` and `Strong` are the single and the double asterisk forms. `Strong` is bold in both layouts. `Bold` is drawn by the rule of `2026-10-13-flat-chat-layout-ui.md` (2.5, 3, 4).
- After a layout change the loaded items are drawn again from the same `formattedText`.

## Core

### Parser

`parseMaybeMarkdownList` (`Markdown.hs:148-167`):

- The one-line command rule is unchanged.
- `ml` is `mergeSegments (parseBlocks ls)`, in place of the per-line `intercalate` (`:163`).
- `Nothing` when every segment is plain, as now; also when the segments other than markers have only white space and every block is other than a rule.

`parseBlocks :: [Text] -> MarkdownList`:

```
state
  quote    quote depth of the previous line
  items    content columns of the open list items, innermost first
  cont     block continued by a paragraph line: (quoteDepth, listDepth); or none
  fence    open code block: fence length, quote depth, column; or none
  table    open table; or none
  count    segments so far
initial: cont = (0, 0)
for each line:
  count would be over maxSegments with the segments of the line
                             -> this line and the lines after it are one plain segment,
                                after Block paragraph 0 0 unless cont == (0, 0); end
  fence open:
    fewer than fence.quote quote markers   -> fence = none; the line is processed below
    rest = the line after fence.quote quote markers and indentation up to fence.column
    rest closes the fence                  -> Syntax (line break, line); fence = none
    otherwise                              -> line break as code text; Syntax (prefix); code text (rest)
  q = quote markers; n = columns of indentation after them; c = content
  c empty                    -> table = none; blank line, kept for the next segment
  q /= quote                 -> items = []; table = none; quote = q
  items = dropWhile (> n) items
  col = head items, or 0
  d = min maxListDepth (length items)
  kind = paragraph when n - col >= 4; otherwise the first match of Blocks
  kind /= row                -> table = none
  kind is
    code                     -> Block code q d; fence = open; cont = none
    heading                  -> Block heading q d; inline text; cont = none
    rule                     -> Block rule q d; cont = none
    list item                -> Block listItem q d; inline text; cont = (q, min maxListDepth (d + 1))
                                items = (n + marker length + 1) : items
    row                      -> TableRow; cells
    table                    -> Block table q d; cells; table = open; the delimiter line is consumed; cont = none
    escaped                  -> as paragraph; the "\" is Syntax
    paragraph, cont == (q, d) -> line break as text; Syntax (prefix); inline text
    paragraph                -> Block paragraph q d; inline text; cont = (q, d)
```

- `prefix`: the quote markers and the indentation up to `col`; in code, up to the column of the fence. A tab over that column is text. The line break after an opening fence is part of the block marker. The delimiter line of a table is part of the next marker.
- Kept blank lines: text when the block is continued by the next line, with their quote markers as `Syntax`; otherwise part of the next marker.
- At the end of the text, the source characters after the last drawn segment are `Syntax`; blank lines after a paragraph or list item are text, as now.
- `mergeSegments`, in place of `acc` (`:164-167`): adjacent segments other than markers, with the same format, are joined with one `T.concat` per run.
- `markdownP`: `strongP` (renamed `boldP`); `strikeP`; `sowLinkP` as in Syntax; the fragments of a run are joined once, in place of `mconcat` (`:206`); in `phoneP` (`:310-316`) the repetition of segments is ended, and the parser fails, as soon as the length of `s` is over 22.
- `parseMarkdownList` (`:169-170`) is removed.

### Limits and cost

```
maxQuoteDepth   3
maxListDepth    6      deeper items have listDepth 6
maxSegments     8192   counted before merging; applied to every text
```

- The time of `parseMaybeMarkdownList` is linear in the text length. Text sizes:
  - 65,536 bytes in a compressed message (`maxDecompressedMsgLength`, `Protocol.hs:945`)
  - `maxMsgParts × maxEncodedMsgLength` = 124,816 bytes with `2026-10-13-message-parts.md`
  - any size in a note folder item (`Library/Commands.hs:5109-5122`)

## Terminal

- `Styled.hs`, `sgr` (`:71-79`): `Strong` as `Bold`.
- `Styled.hs`, `styleMarkdownLines :: MarkdownList -> [StyledString]`, exported (`:6-14`), in place of `styleMarkdownList` (`:38-41`):
  - the text of the segments is divided into output lines as by `T.lines`; a CR before a line break is dropped
  - marker text is printed as plain text
  - plain segments of a heading are styled `Bold`
- `View.hs:2461-2462`: `msgPlain t = styleMarkdownLines $ fromMaybe [FormattedText Nothing t] (parseMaybeMarkdownList t)`.
- The suffixes of `appendLast` are appended to the last line returned by `ttyMsgContent` (`View.hs:2362, 2400`), in this order: `msgVerifiedStr`; ` (silent)` of `2026-10-13-silent-messages.md`; ` (incomplete)` of `2026-10-13-message-parts.md`.
- `Help.hs:307-318` `markdownInfo`: a line for `**bold**` and for each block of "Syntax".

## Web, libraries and docs

- `Web.hs` and the directory service (`Directory/Listing.hs:52-60, 197-198`; `Directory/Service.hs:599, 1074`) are unchanged. Markers are part of `formattedText` of `WebMessage`, of `toFormattedText` (`Web.hs:426-431`), and of `shortDescr` and `welcomeMessage` of the listing (`Directory/Listing.hs:143-144`).
- `website/src/js/simplex-lib.jsc:57` `renderMarkdown`, used by the channel preview (`channel-preview.jsc:990, 1522`) and the directory page (`directory.jsc:251-261`): `strong` is `<strong>`; marker text is written by the default case (`:119-120`) as source text; `strong` text is plain text in a cached older script. Directory page search (`directory.jsc:133-136`): marker text is searched as text.
- `bots/src/API/Docs/Types.hs`, `chatTypesDocsData` (`:288`): `(sti @BlockFormat, STUnion, "BF", [], "", "")`; `(sti @TableAlign, STEnum, "TA", [], "", "")`; description of `Format`: `bold` is `*x*`, `strong` is `**x**`, the text of a marker is source text. `Generic` instances (`:521`).
- The generated type files of the bots API and of both libraries are written by `tests/APIDocs.hs` (`testGenerate`, `:153-157`), with the types of the other plans.
- `bots/README.md`: `*x*` is italic in a flat chat; `**x**` is bold in both layouts; the block syntax of "Syntax".

## Compatibility

Protocol and storage: unchanged. A peer with an old core shows the source text. Items stored earlier are parsed when read (`Store/Messages.hs:1122, 2334, 2426`). Texts drawn differently after the change; a literal marker at a line start is written with `\`:

```
text                          now       after
# of users: 5                 text      heading
8) nice                       text      list item "8)"
- a                           text      list item; the same for "+ a" and "* a"
> cmd                         text      quote
---                           text      rule; the same for "***" and "___"
a line with "|" above "-|-"   text      table
~~x~~                         text      strikethrough
[a, b](uri)                   text      link
```

- `has_link` is written when an item is created (`Internal.hs:3012, 3043`); the flags of old items are unchanged. A URI in code is plain text. A SimpleX link in code is found by `hasObfuscatedSimplexLink` (`Internal.hs:429`), as now.
- Event items: their text is parsed by the same function, as now; it is drawn as one text (`ChatPreviewView.kt:261-265`).
- Text rebuilt by `updatedMentionNames` (`Internal.hs:225, 296, 1536`; `Commands.hs:1136`): `~~x~~` is written `~x~`. A member with an old core writes `**x**` as `*x*` (`Markdown.hs:476`); it is italic in `CLFlat`. A table cell is divided by `|` in a renamed mention.
- Bots: `**x**` has the tag `strong`. Markers are new tags. A name of `mentions` that is only in code is rejected with `SEInvalidMention` (`Internal.hs:324-333`).
- Apps: decoding of a response fails on an unknown `Format` tag. In `apiChats` and `apiChat` the item is an invalid item (`SimpleXAPI.kt:6710-6718`, `API.swift:280-288`); other responses are dropped (`SimpleXAPI.kt:6635-6641, 6659-6683`). Release 1: `Strong` (constructor, `strongP`, `sgr`, `markdownText`, JSON tag `strong`, type docs) with part 1 of the rendering plan; release 2: every other change of this plan.
- Remote desktop: the host JSON is decoded by the controller core (`Remote/Protocol.hs:185`). In an older controller a new tag is `Unknown` (`Markdown.hs:538-539`) and its text is drawn as plain text (`ChatModel.kt:5187`, `TextItemView.kt:341`).

## Tests

`tests/MarkdownTests.hs`; each block case with `<<==>>` (parse, and the source from `markdownText`):

```
heading     each level
list        each bullet marker
            numbers as written, with each delimiter
            nesting by each indentation of 2 to 4 spaces
            depth above maxListDepth
            continuation line
            white space of several columns after the marker
quote       each depth up to maxQuoteDepth
            depth above maxQuoteDepth
            code in a quote
            after a list item: at column 0
            after a list item: indented by 2 columns
            after a list item: indented by 4 columns
code        open at the end of the text
            inline syntax in code
            in a list item
            ">>> x" in a block at quote depth 0
            "> x" in a block at quote depth 1
            white space after the closing fence
            a tab over the fence column
rule        each rule character
table       each alignment
            "\|" in a cell
            a row shorter than the header
            a row longer than the header
            a header row with another line below it: paragraph
            "a |" above "---": paragraph and rule
            a line with "|" after a blank line: paragraph
indented    4 columns before each block marker: paragraph line
escaped     "\" before each block marker and in a list number: paragraph line
markers     quote markers only: Nothing
existing    every present case with a marker character at a line start: unchanged
            CRLF line breaks
inline      "**x**", with the double asterisk cases (:77-89): Strong
            "~~x~~"
            a URI with parentheses
            link text "Hello, world!": link
            link text "example . com": text
            link text with "[": text
consumers   mentionedNames: a mention in a table cell
            hasLinks: a URI in code
limits      a text over maxSegments: the rest is one plain segment
            texts of 65,536 and 124,816 bytes, each parsed within a time bound:
              "[ " repeated
              "1 " repeated
              one line of words
live        property: for a text and a longer text with it as a prefix, the blocks before the last two blocks of the first result are equal
            "---", then "---x"
            an opening fence, then a snippet of three backticks in the same line
            a delimiter row, then more text in the same line
```

- `tests/ChatTests/Groups.hs:8366`: `updatedMentionNames` on a text with a list, a table and a code block: the rebuilt text is the source with the new name.
- `tests/JSONFixtures.hs`, `tests/JSONTests.hs:29`, `tests/MobileTests.hs:168-169`: one block segment in both encodings, for `owsf2tagged` and `chatParseMarkdown`; each new constructor is encoded and decoded; an unknown block tag is `Unknown`.
- `removeSimplexLink` (`Internal.hs:1369-1380`): a link in code and in a table cell.
- Directory `linkOnlyChange` (`Directory/Service.hs:588-600`): a description with block syntax.
- `tests/ChatTests/Direct.hs`: a message with a code block and a list is printed with its source lines.

## Rendering contract

Guarantees of the parser output. Block building, drawing, joined segment text and call sites: `2026-10-13-block-markdown-rendering.md` (2.2, 2.4, 5, 6).

- Block view: blocks are drawn separately; the display text of a marker is empty.
- One text: the text of every segment is drawn in order, as now; the display text of a marker is its text.
- Every row and cell of a table except the first is preceded by a marker. A row can have fewer cells than `columns`; the missing cells are empty.
- Inline formats in blocks are drawn and tapped as now. `Strong` is bold. `Bold` is drawn by the rule of "Dialect by layout".
- A segment is identified by its index in `formattedText` in both views (`TextItemView.kt:242, 316, 337`).
- Segments appended by an app to a list with a `Block` marker: a `Block` paragraph marker (`quoteDepth` 0, `listDepth` 0) with a line break as text is appended first.
- Live message: blocks are built again from each `formattedText`. After text is appended, the blocks before the last two blocks of the previous result are unchanged. The last block can be an open code block.

## Interfaces

- The names and JSON tags of "Types", "Limits and cost" and "Terminal". The signature of `parseMaybeMarkdownList` is unchanged.
- `2026-10-13-block-markdown-rendering.md`: owner of the Kotlin and iOS types, block views and one-text views.
- `2026-10-13-flat-chat-layout-ui.md`: owner of the rule for `Bold`.
- `2026-10-13-message-read-more.md`: the teaser is a prefix of the segment list; markers are kept whole; the appended segments rule of "Rendering contract".
- `2026-10-13-message-parts.md`: `hasObfuscatedSimplexLink` is changed by that plan (windows). `parseMaybeMarkdownList` has `maxSegments` as its only limit, for a text of any length; the whole text is parsed after each part and head, in linear time.

## Depends on

- `2026-10-13-block-markdown-rendering.md`: release 1: `Strong` (constructor, `strongP`, `sgr`, `markdownText`, JSON tag `strong`, type docs) with part 1 of the rendering plan; release 2: every other change of this plan. Part 1:
  - Kotlin `Format` classes and `Format.style`
  - the `Strong` and marker branches of `MarkdownText`
  - iOS `Format` cases and the `messageText` cases
- `2026-10-13-silent-messages.md`, `2026-10-13-message-parts.md`: the order of the terminal suffixes ("Terminal").

## Open questions

1. Maximum text of one item: 8 parts (124,816 bytes, 16 items per recipient queue) or 32 parts (499,264 bytes, 4 items). Recommended: 8.
2. Stored and new texts with a marker at a line start (`# x`, `- a`, `> x`, `8) x`, `---`) are drawn as blocks after the upgrade. Recommended: accept, with the `\` escape in the same release.
3. `*x*` is kept as `Bold` (bold in bubbles, italic in flat rows) and `**x**` is `Strong` (bold in both). Surfaces of a flat chat with italic `*x*`: rows only, or rows and the viewer. Recommended: rows and the viewer; bold in previews, compose context and item info.
4. Name of the second layout. Recommended: `flat`.
5. Block views in chat rows and in the viewer only; in the chat list preview, quotes, item info, descriptions and welcome messages block syntax is shown as written. Recommended: keep in this release.
6. Dialect: a single line break is a line break; nested inline formats and emphasis inside a word are text; punctuation `, ; ! ? ' " ( ) &` is accepted in link text; quote depth 3, list depth 6, 8192 segments. Recommended: as listed.
7. Release grouping: three releases, message parts last. Recommended: yes.
