# Block markdown: core

Date: 2026-10-13. Revised: 2026-10-14.

The core parses the blocks of standard Markdown in every message text. `formattedText` is a list of blocks; the inline formats of a block are parsed per line, as now.

## Syntax

### Lines

```
line break    LF; the text is divided by T.lines (Markdown.hs:162), as now; a final line break is dropped
CR            a CR at a line end is syntax
tab           4 columns of indentation
indentation   spaces and tabs before the content of a line
```

### Line kinds

The content of a line is tested in this order. Every kind is recognized at any indentation.

```
escaped     "\" as the first character, or between the digits and the delimiter of a list number,
            in a line that is a fence, heading, quote, rule or list item when this "\" is removed
            the line is a paragraph line; the "\" is syntax
fence       3 or more "`", then an info string without "`"
heading     1 to 6 "#", a space or tab, then text with a character other than white space
quote       ">"; one space or tab after it is syntax; the rest of the line is the quote text
rule        3 or more of one of "-", "*", "_"; spaces and tabs between and after them
list item   "-", "*" or "+", or 1 to 9 digits and "." or ")"
            then a space or tab, then text with a character other than white space
table       a line with "|", when the next line is a delimiter row with the same number of cells
row         a line with "|", while a table is open
blank       white space only
paragraph   any other line
```

- Delimiter row: at least one `|`; every trimmed cell is `:?-+:?`.
- Inside a code block every line is a code line, until its closing fence.

### Blocks

```
Paragraph   consecutive paragraph lines and the blank lines between them, until a line of a kind other than paragraph and blank
            line breaks and blank lines are text, as now
            blank lines before its first line and after its last line are dropped
            a paragraph line is text as written, indentation included, as now
            paragraph lines directly after a list item are lines of the item
Heading     one heading line; level: the number of "#"
CodeBlock   a fence line, the code lines after it, and the closing fence line
            closing fence: at least as many "`" as the opening fence, then white space only; at any indentation
            without a closing fence the block is closed at the end of the text
            code: the code lines with line breaks between them
            in a code line the indentation up to the column of the opening fence is syntax
            language: the first word of the info string; absent for an empty info string
Quote       consecutive quote lines
            content: their quote texts with line breaks between them
            quote texts of white space only at the start and at the end of a quote are dropped
            a quote with such texts only is a paragraph of its lines
ListItem    one list item line and the paragraph lines directly after it
            content: the text after the marker and the white space after the marker, then the following lines with line breaks between them
            the indentation of a following line is syntax
            number: the digits and the delimiter, as written; absent for "-", "*" and "+"
            indent: by the list stack below
Table       a table line, its delimiter row and the row lines directly after it
            columns: one per delimiter cell; "-" is one or more "-"
              ":-:"   center
              "-:"    right
              ":-"    left
              "-"     none
            rows: the header row, then the body rows
            each row has one cell per column; a missing cell is empty
            in a longer row the last cell is the text from the start of its column to the end of the row
            cells are divided at each "|" other than "\|"; the "|" at the line start and at the line end are optional
            a cell is trimmed; the "\" of "\|" is dropped
Rule        one rule line
```

List stack:

```
items        marker columns of the open list items
item at c    items with a column above c are closed
             c is added when items is empty or its last column is below c
indent       number of items, less 1; at most maxListIndent
items = []   at a fence, heading, quote, rule, table or paragraph line at column 0 of a block other than a list item
```

- Each line of the content of a paragraph, heading, quote, list item and table cell is parsed by `markdownP` (`Markdown.hs:205-378`), as now (`:162-163`). A format is within one line.
- Code is text.

### Existing syntax

```
#secret#   a heading has a space or tab after "#" (secretP, Markdown.hs:242-249)
#name      the same (nameRefP, :252-265)
*bold*     a list item has a space or tab after "*" (md, :231-235)
+phone     a list item has a space or tab after "+" (phoneP, :310-316)
/command   a one-line text with "/" first is one command (:149-157); tested before blocks
           in other lines a command is inline, as now
```

### Inline

`markdownP` (`Markdown.hs:205-378`):

```
**x**         Strong (boldP, :236-241)
*x*           Bold, as now
~~x~~         StrikeThrough; the edge rule of ~x~ (md, :231-235)
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
closing "#" characters of a heading: text of the heading
a list, a fence or a quote marker inside a quote
lazy continuation of a quote; a line without ">" ends the quote
inline backslash escapes
hard break syntax; every line break is a line break
nested inline formats
emphasis with its start inside a word (fragmentP, Markdown.hs:209-226)
```

### Incomplete syntax in a live message

```
open code fence                   code to the end of the text
table header row                  last line of a paragraph; a table once the delimiter row is complete
rule characters at the text end   rule; text once another character is added to the line
block marker before its text      text; a block once a space and text are added
">" alone                         paragraph; a quote once text is added
```

## Types

`Markdown.hs`:

```haskell
data Format
  = Bold
  | ...
  | Strong
  | Unknown {json :: J.Value}

data FormattedText = FormattedText {format :: Maybe Format, text :: Text}

type InlineMarkdown = [FormattedText]

data MarkdownBlock
  = Paragraph {content :: InlineMarkdown}
  | Heading {level :: Int, content :: InlineMarkdown}
  | CodeBlock {language :: Maybe Text, code :: Text}
  | Quote {content :: InlineMarkdown}
  | ListItem {number :: Maybe Text, indent :: Int, content :: InlineMarkdown}
  | Table {columns :: [TableAlign], rows :: [[InlineMarkdown]]}
  | Rule
  | UnknownBlock {json :: J.Value}

type MarkdownList = [MarkdownBlock]

data TableAlign = TANone | TALeft | TACenter | TARight

data ParsedMarkdown = ParsedMarkdown {formattedText :: Maybe InlineMarkdown}
```

- `InlineMarkdown` is the type that `MarkdownList` named (`:141`).
- `Format` is inline only; `Strong` is added (`:51-69`).
- The name and the type name of `formattedText :: Maybe MarkdownList` are unchanged in `ChatItem`, `CIQuote` and `ChatItemVersion` (`Messages.hs:265, 614, 1362`). The code that creates and passes it is unchanged; its JSON is a list of blocks.
- Files of the web channel preview and of the directory listing have the source form: `WebMessage.formattedText`, `shortDescription` and `welcomeMessage` (`Web.hs:103, 114-115`), `DirectoryEntry.shortDescr` and `welcomeMessage` (`Directory/Listing.hs:94-95`) are `Maybe InlineMarkdown`; their JSON is unchanged (open question 2).
- JSON: `MarkdownBlock` with `sumTypeJSON fstToLower`, as `Format` (`:536`); an unknown tag is decoded as `UnknownBlock`, as in `Format` (`:538-539`); `TableAlign` with `enumJSON (dropPrefix "TA")`.
- `indent` is defined in `Help.hs` (`:34-35`); there `Simplex.Chat.Markdown` is imported with `hiding (indent)` (`:23`). In the other modules that import `Simplex.Chat.Markdown` without a list (`Styled.hs`, `Messages.hs`, `View.hs`, `Store/Messages.hs`, `Terminal/Output.hs`, `Library/Internal.hs`, `Library/Commands.hs`, `Directory/Listing.hs`, `tests/MarkdownTests.hs`, `bots/src/API/Docs/Types.hs`), `content`, `code`, `level`, `number`, `columns`, `rows` and `language` are local names only.

Values; `C` is a list of `FormattedText`, as now; `language` and `number` are omitted when absent:

```
paragraph    {"type":"paragraph","content":C}
             {"_owsf":true,"paragraph":{"content":C}}
heading      {"type":"heading","level":2,"content":C}
             {"_owsf":true,"heading":{"level":2,"content":C}}
codeBlock    {"type":"codeBlock","language":"bash","code":"ls -l"}
             {"_owsf":true,"codeBlock":{"language":"bash","code":"ls -l"}}
quote        {"type":"quote","content":C}
             {"_owsf":true,"quote":{"content":C}}
listItem     {"type":"listItem","number":"2.","indent":0,"content":C}
             {"_owsf":true,"listItem":{"number":"2.","indent":0,"content":C}}
table        {"type":"table","columns":["none","center"],"rows":[[C,C],[C,C]]}
             {"_owsf":true,"table":{"columns":["none","center"],"rows":[[C,C],[C,C]]}}
rule         {"type":"rule"}
             {"_owsf":true,"rule":{}}
strong       {"type":"strong"}
             {"_owsf":true,"strong":{}}
```

## Core

### Parser

```
parseMaybeMarkdownList :: Text -> Maybe MarkdownList       blocks; signature as now (Markdown.hs:148)
parseMaybeInlineMarkdown :: Text -> Maybe InlineMarkdown    source form
markdownSegments :: MarkdownList -> InlineMarkdown         one-text form ("Rendering contract")
```

- The one-line command rule (`:149-157`) is applied first: blocks `Just [Paragraph [command]]`; source form `Just [command]`.
- Both parse functions are built from one pass over the lines, `markdownLines`. Each line is divided into parts, in source order:

```
syntax   markers, indentation of block lines, fence lines, delimiter rows, "|", CR, the "\" of an escape
inline   text parsed by markdownP
code     code text
```

- Blocks: built from the inline and code parts by "Blocks"; syntax parts are dropped.
- Source form: syntax and code parts are segments without format; inline parts are the segments of `markdownP`; a line break between lines; adjacent segments with the same format are joined, as now (`acc`, `:164-167`).
- `T.concat (map markdownText ft)` of the source form is the text, except, as now, the final line break and colour names written as digits (`:494-505`), and, new, `~~x~~` written `~x~`.
- `Nothing`: for blocks, when the result is empty or one Paragraph of segments without format; for the source form, when every segment is without format, as now (`:159`).
- `markdownP`: `strongP` (renamed `boldP`), with `Strong`; `strikeP`; `sowLinkP` as in "Inline"; the fragments of a run are joined once, in place of `mconcat` (`:206`); in `phoneP` (`:310-316`) the repetition of segments is ended with a failure as soon as the length of `s` is over 22.
- `markdownText` (`:472-505`): `Strong` is `**t**`.
- `parseMarkdownList` (`:169-170`) and `styleMarkdownList` (`Styled.hs:9, 38-41`) are removed. `View.hs:2474`: `msgPlain = map (styleMarkdown . parseMarkdown) . T.lines`, output as now.

### Limits and cost

```
maxListIndent   6      deeper items have indent 6
maxSegments     8192   counted before segments are joined; for blocks and for the source form
```

- When the number of segments would exceed `maxSegments`, the line where it is exceeded and the lines after it are one segment without format: in blocks, a last Paragraph; in the source form, the last segment.
- The time of both parse functions is linear in the text length. Text sizes:
  - 65,536 bytes in a compressed message (`maxDecompressedMsgLength`, `Protocol.hs:945`)
  - `maxMsgParts × maxEncodedMsgLength` = 124,816 bytes with `2026-10-13-message-parts.md` (`Protocol.hs:930`)
  - any size in a note folder item (`Library/Commands.hs:5125-5155`)

### Consumers

```
site                                                        form            change
mentionedNames (Markdown.hs:71-76)                          blocks          over markdownSegments
hasLinks (Markdown.hs:192-193)                              blocks          over markdownSegments
prohibitedSimplexLinks (Library/Internal.hs:426-433)        blocks          over markdownSegments
getMessageMentions (Library/Internal.hs:356-363)            blocks          unchanged
updatedMentionNames (Library/Internal.hs:291-322)           source form     the names are changed in parseMaybeInlineMarkdown of the text of mc
                                                                            the returned list: parseMaybeMarkdownList of the new text
removeSimplexLink (Library/Internal.hs:1370-1381)           source form     parseMaybeInlineMarkdown
chatParseMarkdown (Mobile.hs:395-396)                       source form     parseMaybeInlineMarkdown; JSON as now
msgText (Terminal/Output.hs:231-237)                        blocks          texts of markdownSegments; Secret as "..."
WebMessage.formattedText (Web.hs:103, 325)                  source form     parseMaybeInlineMarkdown (msgContentText mc)
toFormattedText (Web.hs:426-431; :114-115, 252-253)         source form     parseMaybeInlineMarkdown
descriptionContainsLink (Directory/Listing.hs:59-60)        blocks          over markdownSegments
toFormattedText (Directory/Listing.hs:197-198; :94-95)      source form     parseMaybeInlineMarkdown
withoutLink (Directory/Service.hs:598-600)                  source form     parseMaybeInlineMarkdown
groupLinkUri (Directory/Service.hs:1074)                    blocks          over markdownSegments
```

- Unchanged: `msgContentTexts`, `ciContentTexts` (`Library/Internal.hs:365-369`), the parse sites of `Store/Messages.hs` (`:693, 1122, 2305, 2334, 2426, 2583, 3373`), `has_link` (`Library/Internal.hs:3013, 3044, 3088-3096`; `Library/Commands.hs:2674`), `DCSearchGroup` (`Directory/Events.hs:106, 169, 201-205`).
- The list returned by `updatedMentionNames` is used at `Library/Internal.hs:225-226` only; at `Library/Internal.hs:1537` and `Library/Commands.hs:1136` the new text is parsed by `msgContentTexts`.

### Terminal

- `Styled.hs`, `sgr` (`:71-79`): `Strong` as `Bold`.
- The terminal prints the source lines with their inline formats, as now (`View.hs:2473-2474`).
- The suffixes of `appendLast` are appended to the last line of `ttyMsgContent` (`View.hs:2374, 2412`), in this order: `msgVerifiedStr`; ` (incomplete)` of `2026-10-13-message-parts.md`.

## Web, libraries and docs

- Web channel preview (`Web.hs`) and directory listing (`Directory/Listing.hs`) files: lists of the source form, JSON as now.
- `website/src/js/simplex-lib.jsc:57-128` `renderMarkdown`, used by the channel preview (`channel-preview.jsc:990, 1522`) and the directory page (`directory.jsc:251-261`): `strong` is `<strong>`; in a cached older script `strong` text is written as plain text (`:119-120`).
- `bots/src/API/Docs/Types.hs`, `chatTypesDocsData` (`:289-290`): `(sti @MarkdownBlock, STUnion, "", ["UnknownBlock"], "", "")`; `(sti @TableAlign, STEnum, "TA", [], "", "")`; `Generic` instances (`:523-524`). Description of `Format`: `bold` is `*x*`, `strong` is `**x**`.
- `bots/api/TYPES.md` and the type files of both libraries are written by `tests/APIDocs.hs` (`testGenerate`, `:153-157`), with the types of the other plans.
- `bots/README.md`, new section "Message formatting": the syntax of "Syntax"; `formattedText` is a list of blocks; `*x*` is italic in the terminal layout; `**x**` is bold in both layouts.

## Compatibility

Protocol and storage: unchanged. A peer with an older core shows the source text with inline formats. Items stored earlier are parsed when read (`Store/Messages.hs:1122, 2334, 2426`). Texts drawn differently after the change; a marker at a line start is written with `\`:

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

- `has_link` is written when an item is created (`Library/Internal.hs:3013, 3044`); the flags of old items are unchanged. A URI in code is text. A SimpleX link in code is found by `hasObfuscatedSimplexLink` (`Library/Internal.hs:429`), as now.
- `updatedMentionNames` (`Library/Internal.hs:225, 1537`; `Library/Commands.hs:1136`): `~~x~~` is written `~x~`; a mention in code is kept, and is renamed by an older core. In a text with mentions renamed by an older core, `**x**` is written `*x*` (`Markdown.hs:476`); it is italic in the terminal layout.
- `removeSimplexLink`: a SimpleX link in code of a profile description is text; the description is dropped by `hasObfuscatedSimplexLink` (`Library/Internal.hs:1375, 1381`).
- Bots: `formattedText` of an item, a quote and an item version is a list of blocks; the segments are in `content` and in the cells. In bot code that reads `formattedText` as a list of segments, blocks are read in their place. `**x**` has the tag `strong`. A name of `mentions` that is only in code is rejected with `SEInvalidMention` (`Library/Internal.hs:330-331`).
- Remote desktop: the host JSON is decoded by the controller core (`Remote/Protocol.hs:185`). A controller with an older core fails to decode a block list as `[FormattedText]`: a response with such an item fails with `RPEInvalidJSON`, and a chat with a formatted item is not loaded; such an event is delivered to the controller app as an error (`Remote.hs:236-245`). A controller with this core and a host with an older core fail in the same way. Versions: `Remote.hs:77-91`; open question 3.
- The JSON change of `formattedText` and its effect on bots and on desktop controllers of another release are accepted (2026-10-14).
- Release 1, remote desktop: an older controller decodes the tag `strong` as `Unknown` (`Markdown.hs:538-539`); its text is drawn as plain text.
- Release 1: `Strong` (constructor, `strongP`, `sgr`, `markdownText`, JSON tag `strong`, type docs, website) with part 1 of the rendering plan; release 2: every other change of this plan.

## Tests

`tests/MarkdownTests.hs`; the operators `==>>` and `<<==>>` (`:48-55`) are applied to `parseMaybeInlineMarkdown`; a new operator is applied to `parseMaybeMarkdownList` and a block list:

```
heading     each level; "#", "# " and "#x": paragraph
list        each bullet marker; numbers as written, with each delimiter
            nesting by 2, 3 and 4 columns; indent above maxListIndent
            a continuation line; a blank line between items
            a fence under a numbered item: CodeBlock after the item
quote       ">" with and without a space after it; "> > x"; "> - a" and "> ```": text
            ">" alone: paragraph
code        open at the end of the text; an info string of two words
            a closing fence with white space after it; a fence in a list item
            inline syntax in code: text
rule        each character; "- - -"; "---" under a text line
table       each alignment; "\|" in a cell; a row shorter and a row longer than the header
            a header row without a delimiter row: paragraph; a table ended by a line without "|"
paragraph   blank lines between lines: text; blank lines before a heading: dropped
escaped     "\" before each block marker and in a list number: paragraph
source      each case above: <<==>> round trip of the source form
existing    every present case with a block character at a line start: unchanged in the source form
            CRLF line breaks
inline      "**x**", with the double asterisk cases (:77-89): Strong
            "~~x~~"
            a URI with parentheses
            link text "Hello, world!": link
            link text "example . com": text
            link text with "[": text
consumers   mentionedNames: a mention in a table cell is found; a mention in code is ignored
            hasLinks: a URI in code
            markdownSegments: each block kind
limits      a text over maxSegments: the rest is one segment without format
            texts of 65,536 and 124,816 bytes, each parsed within a time bound:
              "[ " repeated
              "1 " repeated
              one line of words
live        property: for a text and a longer text with it as a prefix, the blocks before the last two blocks of the first result are equal
            "---", then "---x"
            an opening fence, then three backticks in the same line
            a delimiter row, then more text in the same line
```

- `tests/ChatTests/Groups.hs:8348-8372`, `testUpdatedMentionNames`: a text with a list, a table and a code block, with a renamed mention outside code and in code: the new text is the source with the new name outside code.
- `tests/JSONFixtures.hs`, `tests/JSONTests.hs:29`: a block list of each constructor in both encodings, for `owsf2tagged`; each constructor is encoded and decoded; an unknown block tag is `UnknownBlock`. `tests/MobileTests.hs:168-169`: unchanged.
- `removeSimplexLink`: a link in code and in a table cell.
- Directory `linkOnlyChange` (`Directory/Service.hs:588-600`): a description with block syntax.
- `tests/ChatTests/Direct.hs`: a message with a code block and a list is printed with its source lines.

## Rendering contract

Guarantees of the parser output. Drawing and call sites: `2026-10-13-block-markdown-rendering.md`.

- Blocks are in source order. Paragraph, Heading, Quote and ListItem have content with a character other than white space. A Table has the header row; each row has one cell per column. `level` is 1 to 6; `indent` is 0 to `maxListIndent`.
- Line breaks in content are text of segments without format. A segment with a format is within one line.
- `Nothing`: a text without formats and blocks. One Paragraph: a text with inline formats only; it is drawn as now. A list of Paragraphs only is drawn as one text, its one-text form.
- One-text form, `markdownSegments`; the apps use the same rules:

```
block          segments
paragraph      content
heading        content
quote          content
list item      without format: 2 spaces per indent, "•" for a bullet or the number, a space; then content
code block     code, without format
table          each row: its cells with " | " between them; a line break between rows
rule           "———", without format
unknownBlock   none, and no line break before it
between blocks a line break
```

- Live message: after text is appended, the blocks before the last two blocks of the previous result are unchanged: "x\n---", then "x\n---x", changes a Paragraph and a Rule into one Paragraph. The last block can be a CodeBlock without a closing fence.

## Later

- Code highlighting, a later patch: one tokenizer for all languages, with comments, strings, numbers, keywords and punctuation. `code` is one text; token ranges over it can be added to `CodeBlock` as an optional field. The field is ignored by earlier decoders: Kotlin `ignoreUnknownKeys` (`SimpleXAPI.kt:6567`), Swift `Decodable`, aeson records.

## Interfaces

- The names and JSON tags of "Types"; the three functions of "Parser"; `maxListIndent`, `maxSegments`; the one-text form of "Rendering contract".
- `2026-10-13-block-markdown-rendering.md`: owner of the Kotlin and iOS types, of the block views and of the one-text form in the apps.
- `2026-10-13-flat-chat-layout-ui.md`: owner of the rule for `Bold`. Layout names in the code: `CLClassic`, `CLTerminal` (`Types.hs:782`).
- `2026-10-13-message-read-more.md`: the teaser is a prefix of the block list; only its last block is cut.
- `2026-10-13-message-parts.md`: `hasObfuscatedSimplexLink` is changed by that plan (windows). Both parse functions have `maxSegments` as their only limit, for a text of any length; the whole text is parsed after each part and head, in linear time.

## Depends on

- `2026-10-13-block-markdown-rendering.md`: release 1: `Strong` with part 1 of the rendering plan (Kotlin and iOS `Format.strong`, `Format.style`, `MarkdownText`, `messageText`, the `MarkdownBlock` app types, `markdownSegments`); release 2: blocks with part 2.
- `2026-10-13-message-priority.md`, `2026-10-13-message-parts.md`: the order of the terminal suffixes ("Terminal").

## Open questions

1. Stored and new texts with a block marker at a line start (`# x`, `- a`, `> x`, `8) x`, `---`) are drawn as blocks after the upgrade. Recommended: accept, with the `\` escape in the same release.
2. Web channel preview and directory page: the source form, block syntax drawn as written, in this release; block rendering in the website later, with a script for both forms. Recommended: as stated.
3. Remote desktop with cores of two releases fails to decode responses ("Compatibility"). `minRemoteCtrlVersion` and `minRemoteHostVersion` (`Remote.hs:77-82`) can be raised to the release with blocks; the pair is then refused with a version error (`Remote.hs:174, 494-497`). Recommended: raise both.
4. Dialect: a single line break is a line break; nested blocks, nested inline formats and emphasis inside a word are text; punctuation `, ; ! ? ' " ( ) &` is accepted in link text; list indent 6, 8192 segments; a block marker is recognized at any indentation. Recommended: as listed.
5. Maximum text of one item: 8 parts (124,816 bytes, 16 items per recipient queue) or 32 parts (499,264 bytes, 4 items). Recommended: 8.
6. Release grouping: three releases: (1) message priority, chat layout, terminal layout, read more, `Strong` with rendering part 1; (2) blocks of the core with rendering part 2; (3) message parts. Recommended: yes.
