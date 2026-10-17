# Block Markdown: core

Date: 2026-10-13. Revised: 2026-10-16, for master. Revised: 2026-10-17, block markers in the flat list.

The core parses the blocks of standard Markdown in every message text. `formattedText` stays a flat list of segments. In a text with a block other than a paragraph, marker segments delimit the blocks, the lines and the table cells; the text of a marker is the syntax of the source. The inline formats are parsed per line by the present parser.

References are to ep/markdown 382dc7480. Haskell paths without a directory are in `src/Simplex/Chat/`.

## 1. Syntax

### Lines

```
line          T.lines (Markdown.hs:162)
              a final line break is dropped, as now
CR            one CR at the end of a line is removed before the kind of the line is tested
              a line of a block: in the end syntax of the line
              a blank line outside blocks: in the segment outside blocks
indentation   spaces and tabs before the content of a line
              tab stops every 4 columns
```

### Line kinds

Tested in this order, at any indentation:

```
code line    a line inside a code block, until its closing fence
table start  a line with "|", when the next line is a delimiter row with the same number of cells
fence        3 or more "`", then an info string without "`"
             when a line with a character other than white space follows it, before its closing fence
heading      1 to 6 "#", a space, then text with a character other than white space
rule         3 or more of one of "-", "*", "_", with spaces and tabs between and after them
list item    a marker, a space, then text with a character other than white space
             marker: "-", "*", "+", or 1 to 9 digits and "." or ")"
quote        ">", then text, in a run of such lines with a text other than white space
             the run ends before a line that is a table start
table row    a line with "|", while a table is open
blank        white space only
paragraph    any other line
```

- Delimiter row: a line with "|" where every cell, trimmed, is `:?-+:?`.
- Units of paragraph lines:
  - a fence line that is not a fence by the rule above, with the blank lines and the closing fence after it, when only blank lines are between them
  - a run of quote lines with texts of white space only
  - any other paragraph line
- A unit belongs to one block.
- A table row is a unit of one line.
- The line after a closing fence is tested anew.

### Blocks

```
CodeBlock   a fence line, the code lines after it, and the closing fence line
            closing fence: at least as many "`" as the opening fence, then white space only, at any indentation
            without a closing fence the block is closed at the end of the text
            in a code line the white space up to the column of the opening fence is syntax
            a tab that crosses that column is syntax
            language: the first word of the info string
            language is absent for an empty info string
Heading     one heading line
            level: the number of "#"
Rule        one rule line
ListItem    one list item line and the units of paragraph lines directly after it
            content: the text after the marker and the space
            then the following lines, without their indentation
            number: the digits and the delimiter, as written
            number is absent for "-", "*" and "+"
            indent: by the list stack below
Quote       consecutive quote lines
            text of a quote line: after ">" and one optional space or tab
            a text of white space only is syntax
            quote lines with texts of white space only at its start and end are syntax
Table       a table start line, its delimiter row, and the table rows directly after them
            columns: one per delimiter cell
            "-" below: one or more "-"
              ":-:"   TACenter
              "-:"    TARight
              ":-"    TALeft
              "-"     TANone
            rows: the header row, then the body rows
            cells are divided at each "|"
            the "|" at the line start and at the line end are optional
            in a longer row the last cell is the text from the start of its column to the end of the row, without a "|" at the line end
            the white space around the text of a cell is syntax
Paragraph   consecutive units of paragraph lines
            a blank line ends it
            a paragraph line is text as written, indentation included, as now
```

- A line of white space only in a Paragraph, a ListItem or a Quote is an empty line: its white space is syntax.

List stack:

```
items        marker columns of the open list items
item at c    items with a column above c are dropped
             c is added when items is empty or its last column is below c
indent       the number of items, less 1
             at most maxListIndent (6)
items = []   at a block other than a list item
blank line   items are kept
```

Content:

```
lines      each line of a paragraph
           the line of a heading
           each quote text
           each line of a list item
           each cell
parse      parseMarkdown (Markdown.hs:176-177)
code       each code line, after its syntax: one segment without format
```

- A format is within one line.
- Code is text.

### Present syntax

```
text        now                    after
#secret#    Secret                 Secret, in a paragraph
#name       SimplexName            SimplexName, in a paragraph
*bold*      Bold                   Bold, in a paragraph
**bold**    Bold                   Bold, in a paragraph
+44 20 1    Phone                  Phone, in a paragraph
/link 1     Command, one line      Command, one line, as now
```

### Text, as now

```
raw HTML
images ![alt](uri)
setext headings
indented code
"~~~" fences
escapes of block markers
closing "#" characters of a heading
a list marker inside a quote
a fence inside a quote
a quote marker inside a quote
a line without ">" after a quote
nested inline formats
```

### Incomplete syntax in a live message

```
fence line at the text end        paragraph line
                                  a code block once a line with text follows
open code fence                   code to the end of the text
                                  BlockEnd with empty text
table header row                  paragraph line
                                  a table once the delimiter row is complete
rule characters at the text end   rule
                                  text once another character is added to the line
block marker before its text      text
                                  a block once text is added
```

- Each version of a live message is parsed from its whole text (`Store/Messages.hs:2583`).

## 2. Types

`Markdown.hs`:

```haskell
data Format
  = Bold
  | ...                                  -- present constructors, unchanged
  | Phone
  | BlockStart {block :: MarkdownBlock}
  | BlockEnd
  | LineBreak
  | CellBreak
  | Unknown {json :: J.Value}

data MarkdownBlock
  = Paragraph
  | Heading {level :: Int}
  | CodeBlock {language :: Maybe Text}
  | Quote
  | ListItem {number :: Maybe Text, indent :: Int}
  | Table {columns :: [TableAlign]}
  | Rule
  | UnknownBlock {json :: J.Value}
  deriving (Eq, Show)

data TableAlign = TANone | TALeft | TACenter | TARight
  deriving (Eq, Show)
```

- `FormattedText` is unchanged.
- `MarkdownList` is unchanged: `[FormattedText]`.
- `formattedText :: Maybe MarkdownList` is unchanged in ChatItem (`Messages.hs:265`), CIQuote (`Messages.hs:614`), ChatItemVersion (`Messages.hs:1362`), WebMessage and WebChannelPreview (`Web.hs:103, 114-115`), DirectoryEntry (`Directory/Listing.hs:94-95`).
- Marker: a segment with BlockStart, BlockEnd, LineBreak or CellBreak.
- JSON of `MarkdownBlock`: `sumTypeJSON fstToLower`, as `Format` (`Markdown.hs:536`).
- An unknown block tag is decoded as `UnknownBlock`, as in `Format` (`Markdown.hs:538-539`).
- JSON of `TableAlign`: `enumJSON (dropPrefix "TA")`.
- `language` and `number` are omitted when absent.
- Block content is the output of `parseMarkdownList`; markers are made only by `parseMaybeMarkdownList`.
- `Format` is not indexed by kind: `Format` derives `Generic` for the API docs (`bots/src/API/Docs/Types.hs:521`), which a GADT does not allow.
- Field names hidden in the import of `Simplex.Chat.Markdown`, as local names clash:

```
Help.hs                indent
View.hs                indent
Store/Messages.hs      columns
Library/Commands.hs    block
```

Values:

```
blockStart   {"type":"blockStart","block":{"type":"heading","level":2}}
blockEnd     {"type":"blockEnd"}
lineBreak    {"type":"lineBreak"}
cellBreak    {"type":"cellBreak"}
paragraph    {"type":"paragraph"}
heading      {"type":"heading","level":2}
codeBlock    {"type":"codeBlock","language":"bash"}
quote        {"type":"quote"}
listItem     {"type":"listItem","number":"2.","indent":0}
table        {"type":"table","columns":["none","center"]}
rule         {"type":"rule"}
```

## 3. Marker form

`parseMaybeMarkdownList` (`Markdown.hs:148-167`):

```
command      the one-line command rule (Markdown.hs:149-157) first, as now
no blocks    the blocks are Paragraphs only: as now (Markdown.hs:158-167)
             Nothing when no segment has a format
blocks       otherwise: the marker form
```

Marker form, the groups of the lines in source order:

```
block        BlockStart, the lines of the block with LineBreak between them, BlockEnd
line         the content segments of the line
             in a Table: the cells with CellBreak between them
between      the line breaks and blank lines between blocks, before the first block and after the last block: one segment without format
join         adjacent content segments with the same format, within a line only
             markers are never joined
             content segments with empty text are dropped
```

Texts of the markers:

```
BlockStart   the block prefix, then the start syntax of the first line
LineBreak    the end syntax of a line, the line break, the start syntax of the next line
CellBreak    the white space after a cell, the "|", the white space before the next cell
             empty for a missing cell
BlockEnd     the end syntax of the last line, then the block suffix
```

Block prefix and suffix:

```
             prefix                                  suffix
Quote        each leading syntax line with its CR    the line break before each trailing syntax line, and the line
             and line break                          with its CR
CodeBlock    the fence line, its CR, its line        the line break and the closing fence line with its CR, when the fence is closed
             break
others       empty                                   empty
```

Start and end syntax of a line:

```
             start                                               end
Paragraph    empty; a line of white space only: the line         the CR
Heading      the indentation, the "#" run, the space             the CR
Rule         the rule line                                       the CR
ListItem     first line: the indentation, the marker, the space  the CR
             other lines: the indentation
Quote        the indentation, ">", the optional space or tab,    the CR
             and a text of white space only
CodeBlock    the white space up to the column of the opening     the CR
             fence
Table row    the indentation, the "|" at the line start, the     the white space after the last cell, the "|" at the line end and the white space after it, the CR
             white space before the first cell                   header row: then the line break, the delimiter row and its CR
```

- A cell of white space only: its white space is before it.

Example, `"# *Title*\n- a\n  b\n\n| x |\n|---|\n| 1 |"`:

```
BlockStart Heading 1     "# "
Bold                     "Title"
BlockEnd                 ""
                         "\n"
BlockStart ListItem 0    "- "
                         "a"
LineBreak                "\n  "
                         "b"
BlockEnd                 ""
                         "\n\n"
BlockStart Table [none]  "| "
                         "x"
LineBreak                " |\n|---|\n| "
                         "1"
BlockEnd                 " |"
```

Properties:

```
source     T.concat (map markdownText ft) is the text, except, as now:
             **x**              written *x* (Markdown.hs:236-241, 476)
             colour names       written as digits (Markdown.hs:295-309, 494-503)
             final line break   dropped (Markdown.hs:162)
pairs      each BlockStart has one BlockEnd after it, before the next BlockStart
inside     LineBreak and CellBreak are between a BlockStart and its BlockEnd
           CellBreak in a Table only
outside    segments without format, of white space only
cells      each row of a Table: one cell per column, the cells divided by CellBreak
rows       each row of a Table is one line; the first row is the header
lines      a line without content segments is an empty line
content    a segment with a format is within one line
           code lines: segments without format
level      1 to 6
indent     0 to 6
```

## 4. Functions and consumers

```
markdownText (Markdown.hs:472-491)                   a marker: its text
hasObfuscatedSimplexLink (Markdown.hs:195-203)       also a SimpleX link in the segments of each line parsed by parseMarkdownList
```

Unchanged, with the marker form:

```
mentionedNames (Markdown.hs:71-76)                    a mention in code is text
hasLinks (Markdown.hs:192-193)                        a link in code is text
styleMarkdownList, styleFormat (Styled.hs:38-47)      markers: their text without style (sgr, Styled.hs:79)
updatedMentionNames (Library/Internal.hs:291-322)     segments mapped one to one, markers kept
prohibitedSimplexLinks (Library/Internal.hs:426)
removeSimplexLink (Library/Internal.hs:1369-1380)     the kept text includes the marker texts
msgText (Terminal/Output.hs:231-237)
chatParseMarkdown (Mobile.hs:396)                     the marker form
Web.hs:252-253, 325, 426-431
Directory/Listing.hs:52-60, 143-144, 197-198
Directory/Service.hs:599-600, 1074
Store/Messages.hs:693, 1122, 2305, 2334, 2426, 2583, 3373    parse sites
Library/Internal.hs:356-369, 3094-3102
msgPlain (View.hs:2474)                               parseMarkdownList per line, as now
```

## 5. Docs and libraries

`bots/src/API/Docs/Types.hs`:

```
chatTypesDocsData, next to :288-289    (sti @MarkdownBlock, STUnion, "", ["UnknownBlock"], "", "")
                                       (sti @TableAlign, STEnum, "TA", [], "", "")
the Format entry (:288)                description: the markers, their text as source syntax
next to :521-522                       deriving instance Generic MarkdownBlock
                                       deriving instance Generic TableAlign
```

Written by the "Bot API docs" test (`tests/APIDocs.hs`):

```
bots/api/TYPES.md
the type files of the TypeScript library
the type files of the Python library
```

## 6. Compatibility

- Protocol: unchanged.
- Storage: unchanged.
- A peer with an older core shows the source text with inline formats.
- Items stored earlier are parsed when read (`Store/Messages.hs:1122, 2334, 2426`). Texts drawn as blocks after the change:

```
text                          now     after
# of users: 5                 text    heading
8) nice                       text    list item "8)"
- a                           text    list item
+ a                           text    list item
* a                           text    list item
> cmd                         text    quote
---                           text    rule
***                           text    rule
___                           text    rule
a line with "|" above "-|-"   text    table
```

- `has_link` of earlier items is unchanged.
- A link in code is text.
- A SimpleX link in code is found by `hasObfuscatedSimplexLink` (`Library/Internal.hs:429, 1375, 1380`).
- A SimpleX link in code of a compose field: the app checks of the segments do not find it (`ComposeView.kt:435-445`, `ShareListView.kt:81-84`, `ChatUtils.swift:104-113`); the core rejects the message where SimpleX links are prohibited (`Library/Internal.hs:429`), accepted.
- A group link in code of a directory group description: not found by `descriptionContainsLink` (`Directory/Listing.hs:60`); the listing adds the link line (`Directory/Listing.hs:129-134`), accepted.
- A directory group description that ends in an open code fence: the link line added after it is code text in the web listing; the group image still links to the group (`website/src/js/directory.jsc:333-338`), accepted.
- Bot and service texts with a list and a paragraph of several lines draw that paragraph with soft breaks, for example the spam filter reply of the directory service (`Directory/Service.hs:1149-1163`).

Costs:

```
JSON          each line of a block after the first: one LineBreak segment
              each cell after the first: one CellBreak segment
              each block: BlockStart and BlockEnd segments
parsing       the line kinds of each text with more than one line, on each read of an item
              inline parsing of paragraph lines only when the text has a block
apps          the marker form is drawn by three implementations: Kotlin, Swift, the web script
```

Consumers of earlier releases:

```
web pages     a marker: its text, by the default case (website/src/js/simplex-lib.jsc:119-120)
              the page shows the source text
bots          the text of each segment, as now
              the Format union has 4 more types
apps          remote desktop only: minRemoteCtrlVersion and minRemoteHostVersion (Remote.hs:77-82) are the version of the release, set at its version change
```

## 7. Tests

`tests/MarkdownTests.hs`:

```
operators       ==>> (:48-49), <<== (:51-52), <<==>> (:54-55): unchanged
new operator    ===> for parseMaybeMarkdownList and the marker form
new spec        blockMarkdown
```

`blockMarkdown`, the marker form of each text:

```
heading     each level
            "#": no blocks
            "# ": no blocks
            "#x": no blocks
            "#\tx#": no blocks
list        each bullet marker
            numbers as written, with each delimiter
            nesting by 2 columns
            nesting by 3 columns
            nesting by 4 columns
            tab indentation
            indent above 6
            a continuation line
            a blank line between items
            a list item after a heading: indent 0
quote       ">" with a space after it
            ">" without a space after it
            ">" alone: no blocks
            "> a\n>\n> b": one quote with an empty line
            ">\n> a\n>": syntax lines in BlockStart and BlockEnd
            "*a*\n>": no blocks
code        closed
            open at the end of the text
            an empty code line
            an info string of two words
            indentation of the fence
            a tab across the column of the fence
            a closing fence with white space after it
            inline syntax in code: text
            "```" alone: no blocks
            "```\n```": no blocks
            "```\n```\nx": no blocks
            "```\n\n```": no blocks
            "```\n   \n```": no blocks
            "*a*\n```": no blocks
rule        each character
            "- - -"
            "---" under a text line
table       each alignment
            a row shorter than the header: empty CellBreak
            a row longer than the header
            a longer row ending with "|": the last cell without that "|"
            a last cell with white space and no "|" after it
            an empty cell
            a cell of white space only
            "||" between cells
            indented rows
            "a | b\n- | -": a table
            a header row without a delimiter row: no blocks
            a table ended by a line without "|"
paragraph   two paragraphs with a heading: two Paragraph blocks
            indentation of a paragraph line: content
            a line of white space only in a unit: an empty line, in a Paragraph and in a ListItem
            blank lines before the first block and after the last block
command     "/link 1": as now
CRLF        each block kind with CR at line ends
live        "```", then "```\nls": no blocks, then a code block
            "---", then "---x"
            an opening fence, then three backticks in the same line
            a delimiter row, then more text in it
```

Properties, for each text above:

```
source     T.concat (map markdownText ft) is the text
markers    pairs, inside, outside and cells of section 3
```

Consumers:

```
mentionedNames   a mention in a table cell is found
                 a mention in code is not found
hasLinks         a URI in code is not found
markdownText     each marker
```

Other modules:

```
tests/ChatTests/Groups.hs:8427        testUpdatedMentionNames
                                        text: a list, a table, a code block
                                        a renamed mention outside code
                                        the same mention in code
                                        the new text: the source with the new name outside code
tests/ChatTests/Groups.hs             a mention name only in code: SEInvalidMention
tests/ChatTests/Profiles.hs:3787      testGroupPrefsSimplexLinksForRole: a SimpleX link in a list item, in a table cell, in code: rejected
tests/ChatTests/Profiles.hs:382       testMemberDescriptionRedacted: a SimpleX link in a list item of a description: removed
                                      a SimpleX link in code of a description: the description removed
tests/MarkdownTests.hs:310            obfuscatedSimplexLinks: an invitation link in a code block
tests/JSONFixtures.hs                 the marker form with each block constructor, in both encodings
tests/JSONTests.hs:29                 the fixtures, for owsf2tagged
tests/JSONTests.hs                    an unknown block tag is UnknownBlock
tests/ChatTests/Direct.hs             a message with a code block, a list, a table: printed with its source lines
```

## Interfaces

```
types       Format: BlockStart, BlockEnd, LineBreak, CellBreak
            MarkdownBlock
            TableAlign
functions   parseMaybeMarkdownList: the marker form
constants   maxListIndent 6
```

`2026-10-13-block-markdown-rendering.md`:

```
the Kotlin types
the iOS types
the web page script
the drawing of the blocks
```
