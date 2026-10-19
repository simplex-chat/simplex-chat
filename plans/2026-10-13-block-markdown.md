# Block Markdown: core

Date: 2026-10-13. Revised: 2026-10-16, for master. Revised: 2026-10-17, two passes over the whole text. Revised: 2026-10-18, CommonMark syntax (S4, S6), soft breaks (S5), block lines then inline content.

Each message text is parsed in two passes. In pass 1, the lines of the text are grouped into blocks. The content of the lines of a block is joined with "\n" and parsed by the inline parser of master; each "\n" is then replaced by the break token and the syntax tokens of the next line. Pass 2 divides the tokens at the breaks between blocks and builds the block structure. `formattedText` is the joined token list; the text of each token is its source. `markdownBlocks` is the block structure: index ranges into `formattedText`.

References are to master at 382dc7480. Haskell paths without a directory are in `src/Simplex/Chat/`.

## 1. Syntax

### Lines

```
line break    "\n", with an optional "\r" before it
final         one line break at the text end is removed before pass 1, as by T.lines (Markdown.hs:162)
line          the text between two line breaks, or between a line break and the text start or end
hard break    two or more spaces, or an odd number of backslashes, before a line break, when the next line is a continuation of the block (section 3)
              CommonMark 0.31.2 section 6.7, examples 633, 634
soft break    a line break before a line that is a continuation of a paragraph, a list entry or a quote, without a hard break
              CommonMark 0.31.2, section "Soft line breaks"
indentation   spaces and tabs at a line start
              tab stops every 4 columns
blank line    white space only
```

- Drawing of line breaks in messages with blocks: rendering plan, section 3.

### Line kinds

Tested in this order at a line start, at any indentation:

```
code line    a line inside a code block, until its closing fence
table start  a line with "|", when the next line is a delimiter row with the same number of cells
fence        3 or more "`", then an info string without "`"
             or 3 or more "~", then an info string
             when a line follows it; a fence line at the text end is text
heading      1 to 6 "#", a space, then text with a character other than white space
             closing sequence: a space or a tab, one or more "#", then white space only, at the line end
underline    "=" only, or "-" only, then white space only, when the open block is a paragraph
             CommonMark 0.31.2 section 4.3 (setext heading underline)
rule         3 or more of one of "-", "*", "_", with spaces and tabs between and after them
list item    a marker, a space, then text with a character other than white space
             marker: "-", "*", "+", or 1 to 9 digits and "." or ")"
quote        ">", then text with a character other than white space
             or blank quote lines before such a line
             after a paragraph line: such a line, or one blank quote line before it
table row    a line with "|", while a table is open
blank        white space only
paragraph    any other line
```

- Delimiter row: a line with "|" where every cell, trimmed, is `:?-+:?`.
- The line after a closing fence is tested anew.

### Blocks

```
CodeBlock   a fence line, the code lines after it, and the closing fence line
            closing fence: at least as many of the character of the opening fence, then white space only, at any indentation
            without a closing fence the block is closed at the end of the text
            in a code line the white space up to the column of the opening fence is syntax
            a tab that crosses that column is syntax
            language: the first word of the info string
            language is absent for an empty info string
Heading     one heading line
            level: the number of "#"
            the closing sequence is syntax
            or: the lines of a paragraph and the underline after them
            level: 1 for "=", 2 for "-"
Rule        one rule line
List        consecutive list items, blank lines between them included
ListEntry   a list item line and its continuation lines (section 3)
            content: the text after the marker and the space, then the continuation lines without their indentation
            number: the digits and the delimiter, as written
            number is absent for "-", "*" and "+"
Quote       consecutive quote lines, and the paragraph lines that are continuations of them (section 3)
            CommonMark 0.31.2 section 5.1, example 247
            text of a quote line: after ">" and one optional space or tab
            a text of white space only is syntax
            the lines with texts of white space only at its start and at its end are outside its content
Table       a table start line, its delimiter row, and the table rows after them
            columns: one per delimiter cell
            "-" below: one or more "-"
              ":-:"   TACenter
              "-:"    TARight
              ":-"    TALeft
              "-"     TANone
            header: the cells of the table start line
            rows: the cells of each table row
            cells are divided at each "|"
            the "|" at the line start and at the line end are optional
            in a row longer than the header, the last cell is the text from the start of its column to the end of the row, without a "|" at the line end
            the white space around the text of a cell is syntax
Paragraph   consecutive paragraph lines (section 3)
            a paragraph line is text as written, indentation included, as now
```

List nesting:

```
parent        the nearest entry before it with a smaller column, in the same List
depth         at most 7 (maxListDepth)
              an entry below depth 7: added at depth 7
blank line    the List continues
other block   the List ends
```

Content:

```
inline       the lines of a paragraph, of a list entry, of a quote: one text (section 3, inlineLines)
             the text of a heading line
             each cell
code         each code line after its syntax: one segment without format
```

### Inline formats across line breaks

Decided 2026-10-17.

```
formats     Bold, Italic, StrikeThrough, Snippet, Secret, Small, Colored
across      a format is continued into the next line when that line is a continuation of the block (section 3)
            one segment of the format per line
            the line break and the syntax of the next line between them
            never across a blank line
            "*", "_", "~": across lines when the content does not start or end with white space; otherwise as now, within the line
within      headings and cells: within one line
            links, SimpleX links, names, mentions, commands, e-mails, phones: within one line
```

Bold, decided 2026-10-18:

```
*x*      Bold, singleStar = True
**x**    Bold, singleStar = False
```

- `*x*` is drawn as italic in messages with blocks (rendering plan, section 3).

### Inline syntax of GFM

Decided 2026-10-18 (S4, S6). In all messages, with and without blocks:

```
~~x~~          StrikeThrough                                   GFM example 491
__x__          Bold, singleStar = False                        GFM example 391
`x`            Snippet                                         CommonMark section 6.1, GFM example 339
`` x ` y ``    a run of backticks, the content, a run of the same length
               one space removed at each end when the content starts and ends with a space and is not spaces only
               a space at one end only: kept (CommonMark example 332); spaces only: kept (example 334)
               master's check of spaces at the ends (Markdown.hs:231-235): not applied to Snippet
<x>            as [x](x) (sowLinkP, Markdown.hs:281-293), its text "<x>"    GFM example 603
               x an e-mail address: HyperLink {showText = Just x, linkUri = "mailto:" <> x}
\c             c an ASCII punctuation character: Escape "\", then c without format     CommonMark examples 12, 14
               not in code blocks and snippets
               "\" before a line break: a hard break (section 1, Lines)
```

### Text, as now

```
raw HTML
images ![alt](uri)
indented code
a list marker inside a quote
a fence inside a quote
a quote marker inside a quote
nested inline formats
```

### Incomplete syntax in a live message

```
fence line at the text end        paragraph line
                                  a code block once a line follows
open code fence                   code to the end of the text
table header row                  paragraph line
                                  a table once the delimiter row is complete
rule characters at the text end   rule
                                  text once another character is added to the line
block marker before its text      text
                                  a block once text is added
open format at the text end       text
                                  a format once it is closed
```

- Each version of a live message is parsed from its whole text (`Store/Messages.hs:2583`).

## 2. Types

`Markdown.hs`:

```haskell
data Format
  = Bold {singleStar :: BoolDef}
  | ...                                  -- present constructors, unchanged
  | Phone
  | Escape
  | Markup {token :: BlockToken}
  | Unknown {json :: J.Value}

data BlockToken
  = LineBreak
  | SoftBreak
  | HardBreak
  | ParagraphBreak {lineBreaks :: Int}
  | Indent
  | HeadingMark {level :: Int}
  | HeadingClose
  | HeadingUnderline {level :: Int}
  | ListMark {number :: Maybe Text}
  | QuoteMark
  | FenceMark {language :: Maybe Text}
  | RowMark
  | CellMark
  | DelimiterRow {columns :: [TableAlign]}
  | RuleMark
  deriving (Eq, Show)

data MarkdownBlock
  = Paragraph {content :: SegmentRange}
  | Heading {level :: Int, content :: SegmentRange}
  | CodeBlock {language :: Maybe Text, content :: SegmentRange}
  | Quote {content :: SegmentRange}
  | List {items :: NonEmpty ListEntry}
  | Table {columns :: NonEmpty TableAlign, header :: [SegmentRange], rows :: [[SegmentRange]]}
  | Rule {mark :: SegmentRange}
  | UnknownBlock {json :: J.Value}
  deriving (Eq, Show)

data ListEntry = ListEntry {number :: Maybe Text, content :: SegmentRange, items :: [ListEntry]}
  deriving (Eq, Show)

data SegmentRange = SegmentRange {from :: Int, to :: Int}
  deriving (Eq, Show)

data TableAlign = TANone | TALeft | TACenter | TARight
  deriving (Eq, Show)

data ParsedMarkdown = ParsedMarkdown
  { formattedText :: Maybe MarkdownList,
    markdownBlocks :: Maybe (NonEmpty MarkdownBlock)
  }
```

- `FormattedText` is unchanged.
- `MarkdownList` is unchanged: `[FormattedText]`.
- Markup token: a segment with format `Markup`.
- `Escape`: the backslash of a backslash escape; text `"\"`.
- `markdownText` (`Markdown.hs:472-491`):
  - a markup token, `Escape`: its text
  - `Bold`: `*x*` when `singleStar`, `**x**` otherwise (`Markdown.hs:476`)
  - `Snippet`: delimited by a backtick run longer than any backtick run in its text, with a space inside each delimiter when the text starts or ends with a backtick, or starts and ends with a space and is not spaces only (`Markdown.hs:479`)
- `SegmentRange`: `[from, to)` into `formattedText`.
- JSON of `BlockToken` and `MarkdownBlock`: `sumTypeJSON fstToLower`, as `Format` (`Markdown.hs:536`).
- An unknown block tag is decoded as `UnknownBlock`, as in `Format` (`Markdown.hs:538-539`).
- An unknown token tag: the format is decoded as `Unknown`.
- `Bold` without `singleStar` in JSON is decoded with `singleStar = False`: the field type is `BoolDef` (`Types.hs:2313-2318`).
- JSON of `ListEntry`, `SegmentRange` and `ParsedMarkdown`: `defaultJSON`.
- JSON of `TableAlign`: `enumJSON (dropPrefix "TA")`.
- Field names equal to local names in importing modules (`-Werror=name-shadowing`, simplex-chat.cabal:362) are hidden in those imports.

Values:

```
bold             {"type":"bold","singleStar":true}
escape           {"type":"escape"}
lineBreak        {"type":"markup","token":{"type":"lineBreak"}}
softBreak        {"type":"markup","token":{"type":"softBreak"}}
hardBreak        {"type":"markup","token":{"type":"hardBreak"}}
headingClose     {"type":"markup","token":{"type":"headingClose"}}
headingUnderline {"type":"markup","token":{"type":"headingUnderline","level":1}}
paragraphBreak   {"type":"markup","token":{"type":"paragraphBreak","lineBreaks":2}}
headingMark      {"type":"markup","token":{"type":"headingMark","level":2}}
listMark         {"type":"markup","token":{"type":"listMark","number":"2."}}
fenceMark        {"type":"markup","token":{"type":"fenceMark","language":"bash"}}
delimiterRow     {"type":"markup","token":{"type":"delimiterRow","columns":["none","center"]}}
paragraph        {"type":"paragraph","content":{"from":1,"to":4}}
codeBlock        {"type":"codeBlock","language":"bash","content":{"from":2,"to":5}}
list             {"type":"list","items":[{"number":"1.","content":{"from":4,"to":9},"items":[]}]}
table            {"type":"table","columns":["none"],"header":[{"from":1,"to":2}],"rows":[[{"from":6,"to":7}]]}
rule             {"type":"rule","mark":{"from":2,"to":3}}
```

- `language` and `number` are omitted when absent.

## 3. Pass 1: tokens

```
input      the text without its final line break
output     segments in source order
           the concatenation of their texts is the input
```

Block parsers, tested in this order at a line start:

```
tableP  fenceP  headingP  ruleP  listItemP  quoteP  paragraphP
```

- Block start (`blockStartP`): a table start, a fence, a heading line, a rule, a list item line, a quote start.
- Between blocks: `LineBreak`, or `ParagraphBreak` with the blank lines.

Continuation lines:

```
block        next line                                            its syntax tokens
paragraph    text, not a block start, not an underline            none: the indentation is content
list item    text, not a block start                              Indent, when indented
quote        a quote line with text                               QuoteMark
             a blank quote line                                   QuoteMark: the whole line
             after a line with text: a line with text,            none
               without ">", not a block start
table        a line with "|", not a block start                   the row tokens
code         each line before the closing fence                   Indent, then the code
```

- An unindented line after a list item is a continuation of it (B11; CommonMark 0.31.2 section 5.2, rule 5).
- A paragraph line after a quote line is a continuation of the quote (CommonMark 0.31.2 section 5.1, example 247).

Inline content of a paragraph, a list item, a quote (`inlineLines`):

```
lines       the text of each line after its syntax tokens
hard break  two or more spaces, or an odd number of "\", at the end of a line with a next line: removed from the line
runs        consecutive lines with text; a blank line is a run alone
parse       each run joined with "\n", parsed by markdownP; the runs joined with "\n"
breaks      each "\n" replaced by the break token and the syntax tokens of the next line
            a segment with "\n" is divided there; empty parts without a format are dropped
break       HardBreak: after a hard break, or with a blank line before or after it
            SoftBreak: otherwise
```

- `markdownP` keeps each "\n" of its input in the texts of its segments.

Texts of the markup tokens:

```
LineBreak          a line break between two blocks
SoftBreak          a soft break (section 1)
HardBreak          a hard break (section 1): the spaces or the backslash, and the line break
                   the line break before and after a blank quote line inside a quote
                   the line break between the lines of a code block, between the lines of a table, before a setext underline
ParagraphBreak n   a line break and the blank lines after it, each with its line break
                   the blank lines at the text start, with their line breaks
                   n: the number of "\n"
Indent             the indentation of a continuation line of a list item
                   in a code line: the white space up to the column of the opening fence
HeadingMark        the indentation, the "#" run, the space
HeadingClose       the closing sequence of a heading line (section 1)
HeadingUnderline   the underline line (section 1); level 1 for "=", 2 for "-"
ListMark           the indentation, the marker, the space
QuoteMark          the indentation, ">", the optional space or tab
                   in a quote line with a text of white space only: also that text
FenceMark          the opening or the closing fence line
RowMark            the indentation, the "|" at the line start, the white space before the first cell
                   empty in a row without them
CellMark           after a cell before the last: its white space, the "|", the white space before the next cell
                   after the last cell: its white space, the "|" at the line end, the white space after it
                   empty after a last cell without them
DelimiterRow       the delimiter row
RuleMark           the rule line
```

- Inside a code block the lines are divided by `HardBreak` only, blank code lines and spaces at line ends included.
- Spaces at a line end before a line break other than a hard break: content, as now.

Tokens of a line, in order:

```
paragraph line   inline content, the indentation included
heading          HeadingMark, inline content, HeadingClose when present
underline        HeadingUnderline
list item        ListMark, inline content
continuation     Indent when indented, inline content
quote line       QuoteMark, inline content
fence line       FenceMark
code line        Indent when indented, one segment without format when not empty
table row        RowMark, then for each cell: inline content, CellMark
delimiter row    DelimiterRow
rule             RuleMark
```

Changes to `Markdown.hs`:

```
parseMaybeMarkdownList (148-167)   formattedText of parsedMarkdown
parsedMarkdown                     new: the command rule (149-157), as now
                                   T.lines (162), intercalate (163): replaced by pass 1
                                   the join (164-167): joinSegments, markup tokens not joined
documentP, block parsers           new (above)
inlineLines                        new (above)
fragmentP (209)                    '\n': "\n" without format
'*' case (214)                     doubleP '*' (Bold False); then formattedP '*' (Bold True)
'_' case (215)                     doubleP '_' (Bold False); then formattedP '_' Italic
'~' case (216)                     doubleP '~' StrikeThrough; then formattedP '~' StrikeThrough
'`' case (217)                     code span (section 1), in place of formattedP '`' Snippet
'<' case                           new: autolink (section 1); then wordP
'\' case                           new: escape (section 1); then wordP
formattedP (228-230)               content to the delimiter within the line, as now
                                   without the delimiter in the line: across lines (section 1)
boldP (236-241)                    doubleP: "**", "__", "~~"
                                   white space at the content ends by isSpace
secret (245-249)                   white space at the content ends by isSpace
styledP (267-272)                  the space after the style: not "\n"
                                   white space at the content ends by isSpace
sowLinkP (281-293)                 the format of the link: hyperLinkFormat, shared with autolinks
wordP (320)                        to a space, "\n", or a backslash before an ASCII punctuation character
secretFallback (251)               to a space or "\n"
nameRefP (253)                     to a space or "\n"
inParens (294)                     within one line
displayNameTextP_ (512-517)        a quoted name: within one line
parseMarkdown (176-177)            unchanged: the inline parser, used by msgPlain (View.hs:2474)
```

## 4. Pass 2: blocks

```
blocks         the tokens divided at LineBreak and ParagraphBreak; empty parts dropped
kind           by the first token of a block
                 HeadingMark   Heading
                 ListMark      list entry: the column of its indentation, its number
                 QuoteMark     Quote
                 FenceMark     CodeBlock
                 RowMark       Table
                 RuleMark      Rule: mark, the range of the RuleMark token
                 other         Paragraph; Heading when the last token is HeadingUnderline
lists          consecutive list entries: one List, nested by column (section 1)
```

Ranges:

```
content    from the first segment without Markup to the last
           the breaks and the markup between them included
code       from the token after the line break of the opening fence to the line break before the closing fence, excluded
           without a closing fence: to the end
cell       from the token after its RowMark or CellMark to its CellMark
           empty for an empty cell
row        from a RowMark to the next RowMark
columns    the DelimiterRow
```

### Texts without blocks

Decided 2026-10-17.

```
condition   pass 1 yields no markup other than LineBreak, SoftBreak, HardBreak and ParagraphBreak
            or more than 2000 markup tokens (maxMarkupTokens; decided 2026-10-18)
result      each markup token a segment without format, joined by joinSegments
            Nothing when no segment has a format, as now
            markdownBlocks: Nothing
```

- `Escape` is an inline format: kept in texts without blocks.

### Example

Text:

```
# Plan
- buy *milk*
  and eggs
  - nested

> a
> b
```

`formattedText`:

```
0   headingMark 1      "# "
1                      "Plan"
2   lineBreak          "\n"
3   listMark           "- "
4                      "buy "
5   bold               "milk"
6   softBreak          "\n"
7   indent             "  "
8                      "and eggs"
9   lineBreak          "\n"
10  listMark           "  - "
11                     "nested"
12  paragraphBreak 2   "\n\n"
13  quoteMark          "> "
14                     "a"
15  softBreak          "\n"
16  quoteMark          "> "
17                     "b"
```

`markdownBlocks`:

```
Heading 1      [1, 2)
List
  ListEntry    [4, 9)
    ListEntry  [11, 12)
Quote          [14, 18)
```

Format across a line break, `"> *a\n> b*"`:

```
0   quoteMark   "> "
1   bold        "a"
2   softBreak   "\n"
3   quoteMark   "> "
4   bold        "b"
Quote [1, 5)
```

## 5. Records and parse sites

`Markdown.hs`:

```haskell
parsedMarkdown :: Text -> ParsedMarkdown
parseMaybeMarkdownList :: Text -> Maybe MarkdownList     -- formattedText of parsedMarkdown
```

Field `markdownBlocks :: Maybe (NonEmpty MarkdownBlock)` after `formattedText`:

```
ChatItem            Messages.hs:265
CIQuote             Messages.hs:614
ChatItemVersion     Messages.hs:1362
ParsedMarkdown      Markdown.hs:143
WebMessage          Web.hs:103
```

Other new fields, of the same type:

```
WebChannelPreview   Web.hs:114-115                 shortDescriptionBlocks, welcomeMessageBlocks
DirectoryEntry      Directory/Listing.hs:94-95     shortDescrBlocks, welcomeMessageBlocks
```

`(Text, Maybe MarkdownList)` becomes `(Text, ParsedMarkdown)`:

```
Library/Internal.hs:365, 368          msgContentTexts, ciContentTexts
Library/Internal.hs:2993              itemTexts
Library/Internal.hs:3030, 3034, 3037  saveRcvChatItem, ciContentNoParse, saveRcvChatItem'
Library/Internal.hs:3088              mkChatItem_
Library/Internal.hs:3406, 3416        createLocalChatItems
Library/Commands.hs:5155, 5263        ComposedMessageReq and its list
```

- `ciContentNoParse` (`Library/Internal.hs:3034-3035`): `ParsedMarkdown Nothing Nothing`.

Parse sites:

```
Store/Messages.hs:693, 2305                       CIQuote: parsedMarkdown of the quoted text
Store/Messages.hs:1122, 2334, 2426, 2583          ChatItem: parsedMarkdown of the item text
Store/Messages.hs:3373                            ChatItemVersion
Store/Messages.hs:1284, 1630, 1870, 2864, 2936    markdownBlocks = Nothing, with formattedText = Nothing
Library/Internal.hs:226                           CIQuote: markdownBlocks of the quoted item
Library/Commands.hs:4931                          CIQuote: markdownBlocks of the quoted item
Messages.hs:1369-1379                             mkItemVersion: markdownBlocks of the item
Web.hs:252-253, 325                               parsedMarkdown of the description texts; its two fields set the two sibling fields of the record
                                                  toFormattedText (426-431) removed: equal to parseMaybeMarkdownList
Directory/Listing.hs:143-144, 197-198             toFormattedText returns the list and the blocks; they set the two sibling fields of the record
Store/Messages.hs:693, 2305                       mkCIQuote (Messages.hs): CIQuote with the parsed text of its content
Mobile.hs:396                                     chatParseMarkdown: J.encode . parsedMarkdown . safeDecodeUtf8
```

- In `updatedMentionNames` (`Library/Internal.hs:291-322`), segments are mapped one to one, so the ranges of `markdownBlocks` are valid for its result.

## 6. Consumers

Unchanged:

```
mentionedNames (Markdown.hs:71-76)                    a mention in code is text
hasLinks (Markdown.hs:192-193)                        a link in code is text
styleMarkdownList, styleFormat (Styled.hs:38-47)      markup: its text without style (sgr, Styled.hs:71-79)
updatedMentionNames (Library/Internal.hs:291-322)     segments mapped one to one
prohibitedSimplexLinks (Library/Internal.hs:426-429)
removeSimplexLink (Library/Internal.hs:1369-1380)     the kept text includes the markup texts
hasObfuscatedSimplexLink (Markdown.hs:195-203)        a SimpleX link in code is found in the whole text
msgText (Terminal/Output.hs:231-237)
Directory/Listing.hs:52-60
Directory/Service.hs:599-600
Directory/Events.hs:106
msgPlain (View.hs:2474)                               parseMarkdownList per line, as now
```

`Bold {singleStar}` sites:

```
sgr (Styled.hs:73)          Bold {}: bold, as now
bold' (View.hs:2172)        styled (Bold False)
Help.hs:312                 markdown (Bold True) "bold text"
```

## 6a. Directory service

```
spam filter reply (Directory/Service.hs:1160-1163)   each command line ends with two spaces: hard breaks between the command lines
tests/Bots/DirectoryTests.hs                        unchanged: the terminal output of the tests has no trailing spaces
```

## 7. Docs and libraries

`bots/src/API/Docs/Types.hs`:

```
chatTypesDocsData, next to :288-289    (sti @BlockToken, STUnion, "", [], "", "")
                                       (sti @MarkdownBlock, STUnion, "", ["UnknownBlock"], "", "")
                                       (sti @ListEntry, STRecord, "", [], "", "")
                                       (sti @SegmentRange, STRecord, "", [], "", "")
                                       (sti @TableAlign, STEnum, "TA", [], "", "")
the Format entry (:288)                description: markup tokens, their text as source syntax; Escape; Bold singleStar
next to :521-522                       deriving instance Generic for BlockToken, MarkdownBlock, ListEntry, SegmentRange, TableAlign
```

Written by the "Bot API docs" test (`tests/APIDocs.hs`):

```
bots/api/TYPES.md
the type files of the TypeScript library
the type files of the Python library
```

## 8. Compatibility

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
*a                            text    bold, across lines
b*
Title                         text    heading 2
---
Title                         text    heading 1
===
~~~ fences                    text    code block
\# a                          text    "# a", the "\" not drawn
~~x~~                         text    strike-through
__x__                         text    bold
``x``                         text    code
` a `                         text    code "a"
<https://x.y>                 text    link
```

- A quote continued by a line without ">": the line is drawn in the quote.

- In texts with blocks, after the change: `*x*` drawn as italic; single line breaks in paragraphs, list items and quotes drawn as spaces (rendering plan, section 3).
- `has_link` of earlier items is unchanged.
- A link in code is text.
- A SimpleX link in code is found by `hasObfuscatedSimplexLink` (`Library/Internal.hs:429, 1375, 1380`).
- A SimpleX link in code of a compose field: not found by the app checks of the segments (`ComposeView.kt:435-445`, `ShareListView.kt:81-84`, `ChatUtils.swift:104-113`); the core rejects the message where SimpleX links are prohibited (`Library/Internal.hs:429`), accepted.
- A group link in code of a directory group description: not found by `descriptionContainsLink` (`Directory/Listing.hs:60`); the link line is added to the listing (`Directory/Listing.hs:129-134`), accepted.
- A format across lines, rewritten by `updatedMentionNames`: delimiters on each line, `"> *a\n> b*"` written as `"> *a*\n> *b*"`.

Costs:

```
JSON          each line break and each block syntax: one markup segment, in texts with blocks
              markdownBlocks: the ranges of the blocks
parsing       one parse of the whole text, on each read of an item
```

Consumers of earlier releases:

```
web pages     a markup token: its text, by the default case (website/src/js/simplex-lib.jsc:119-120)
              new fields ignored
bots          the text of each segment, as now
              the Format union has one more type; records have new fields
apps          remote desktop only: minRemoteCtrlVersion and minRemoteHostVersion (Remote.hs:77-82) are the version of the release, set at its version change
```

## 9. Tests

`tests/MarkdownTests.hs`:

```
operators       ==>> (:48-49), <<== (:51-52), <<==>> (:54-55): unchanged
                multilineMarkdownList (:463-481): unchanged
new operator    ===> for parsedMarkdown: formattedText and markdownBlocks
new spec        blockMarkdown
```

`blockMarkdown`, the tokens and the blocks of each text:

```
heading     each level
            "#", "# ", "#x", "#\tx#": no blocks
            closing sequence: "# a #", "# a ##  "; "# a#": "a#" as content
setext      "a\n===": heading 1; "a\n---": heading 2; "a\nb\n=": heading 1 of two lines
            "- - -" under a text line: rule
            "===" under a list item, a quote, a heading, at the text start: text
list        each bullet marker
            numbers as written, with each delimiter
            nesting by 2, 3 and 4 columns
            tab indentation
            depth above 7
            an indented continuation line
            an unindented continuation line
            a blank line between items: one list
            a paragraph after a list
            a list item after a heading
quote       ">" with and without a space after it
            ">" alone: no blocks
            "> a\n>\n> b": one quote with an empty line
            ">\n> a\n>": the empty lines outside the content
            "> > b": "> b" is text
            "> a\nb": one quote, SoftBreak
            "> a\nb\n> c": one quote
            "> a\n\nb": a quote and a paragraph
code        closed
            "~~~" fences; "```" not closed by "~~~"; an info string with "`" after "~~~"
            open at the end of the text
            an empty code line, blank code lines at its end
            an info string of two words
            indentation of the fence
            a tab across the column of the fence
            a closing fence with white space after it
            inline syntax in code: text
            "```", "```\n", "a\n```": no blocks
            "```\n```", "```\n\n```", "```\n   \n```", "```\n```\nx": code blocks
rule        each character
            "- - -"
            "---" under a text line
table       each alignment
            a row shorter than the header
            a row longer than the header, with and without "|" at its end
            an empty cell, a cell of white space only, "||" between cells
            rows without "|" at the line start: empty RowMark
            indented rows
            "a | b\n- | -": a table
            a header row without a delimiter row: no blocks
            a table ended by a line without "|"
paragraph   a paragraph line with indentation: Indent as text
            blank lines before the first block and after the last block
across      bold, italic, strike-through, snippet, secret, colored across a line break: in a paragraph, a quote, a list item
            not across a blank line, a heading line, a cell
            an unclosed format: the delimiter and the rest of the line as text; formats on the next lines kept
            a delimiter with white space at the content end across lines: as now, within the line
breaks      ParagraphBreak counts
            CR before each line break, in each block kind
            final line break removed
            hard break: two and three spaces, and a backslash, before a line break in a paragraph, a list item, a quote
            one space before a line break: SoftBreak, the space as content
            two spaces or a backslash before a blank line, at the text end, in code: content
            SoftBreak in a paragraph, a list entry, a quote; HardBreak between table rows and code lines; LineBreak between blocks
escapes     "C:\dir\file.txt": text, one word
bold        "*x*": singleStar True
            "**x**", "__x__": singleStar False
gfm         "~~x~~": StrikeThrough
            "`` x ` y ``": Snippet "x ` y"; "```a```" in a line: Snippet "a"; "`` a`": text
            "` a `": Snippet "a"; "` a`": Snippet " a"; "` `": Snippet " "; "`  a  `": Snippet " a "
            "<https://x.y>": HyperLink, text "<https://x.y>"; "<a@b.c>": HyperLink to "mailto:a@b.c"; "<x>": text
escapes     "\*a*": Escape, "*a*" as text; "\# a" at a line start: no heading
            "a\_b": Escape inside a word; "\a": text
            in code and snippets: text
            in a text without blocks: kept
command     "/link 1": as now
live        "```", then "```\nls": no blocks, then a code block
            "---", then "---x"
            an opening fence, then three backticks in the same line
            a delimiter row, then more text in it
```

Properties, for each text above:

```
source      T.concat (map markdownText formattedText) is the text without its final line break, except:
              colour names       written as digits, as now (Markdown.hs:295-309, 494-503)
              formats across lines, delimited on each line; an empty part at a line break written as empty delimiters
              ~~x~~              written ~x~
              __x__              written **x**
              code spans         written with the delimiters of markdownText (section 2)
ranges      within formattedText, in source order, not overlapping
content     each segment without markup is in one content range or one cell
```

Consumers:

```
mentionedNames            a mention in a list item, a quote, a heading, a table cell is found
                          a mention in code and in a snippet is not found
hasLinks                  a URI in a list item and a table cell is found; in code not found
SimpleX links             in a list item and a table cell: a SimplexLink segment
                          in code: no segment; found by hasObfuscatedSimplexLink
                          the text without SimpleX link segments, as removeSimplexLink (Library/Internal.hs:1369-1380)
markdownText              each markup token
```

- The consumers above are tested on parsed text in `tests/MarkdownTests.hs`; chat tests are not added.

Other modules:

```
tests/ChatTests/Groups.hs:8427        testUpdatedMentionNames
                                        text: a list, a table, a code block
                                        a renamed mention outside code
                                        the same mention in code
                                        the new text: the source with the new name outside code
tests/JSONFixtures.hs                 a message with a heading, a list and a bold segment, in both encodings
tests/JSONTests.hs                    the fixtures, for owsf2tagged
                                      a round trip of each block and each markup token
                                      an unknown block tag is UnknownBlock
```

Linear time:

```
phone numbers          the scan ends after 22 characters (master: to the end of the digit groups)
link text "[x]"        the scan ends at "[" or a line break
link target "(x)"      the scan ends at white space
autolink "<x>"         the scan ends at "<" or white space
segment joining        each run of equal formats joined once (T.concat)
block starts           a table start: two lines; a fence: one line and a line break
table cells            one pass over the row and cell marks
```

- Tests: the bounded scans keep master's results; texts of 4000 repetitions of each adversarial pattern are parsed; 1000 list items have blocks, 1001 have none.

## Interfaces

```
types       Format: Markup, Escape, Bold {singleStar}
            BlockToken
            MarkdownBlock
            ListEntry
            SegmentRange
            TableAlign
            ParsedMarkdown: markdownBlocks
records     markdownBlocks: ChatItem, CIQuote, ChatItemVersion, WebMessage
            shortDescriptionBlocks, welcomeMessageBlocks: WebChannelPreview
            shortDescrBlocks, welcomeMessageBlocks: DirectoryEntry
functions   parsedMarkdown
            parseMaybeMarkdownList: formattedText of parsedMarkdown
constants   maxListDepth 7
            maxMarkupTokens 2000
```

`2026-10-13-block-markdown-rendering.md`:

```
the Kotlin types
the iOS types
the web page script
the drawing of the blocks
```
