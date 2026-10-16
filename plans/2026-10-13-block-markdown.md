# Block Markdown: core

Date: 2026-10-13. Revised: 2026-10-16, for master.

The core parses the blocks of standard Markdown in every message text. `formattedText` of an item is a list of blocks. The inline formats of a block are parsed per line by the present parser.

References are to master 41f0c429b. Haskell paths without a directory are in `src/Simplex/Chat/`.

## 1. Syntax

### Lines

```
line          T.lines (Markdown.hs:162)
              a final line break is dropped, as now
CR            one CR at the end of a line is removed before the kind of the line is tested
              the source form keeps it, as now
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
            code: the code lines with line breaks between them
            in a code line the white space up to the column of the opening fence is dropped
            a tab that crosses that column is dropped
            language: the first word of the info string
            language is absent for an empty info string
Heading     one heading line
            level: the number of "#"
Rule        one rule line
ListItem    one list item line and the units of paragraph lines directly after it
            content: the text after the marker and the space
            then the following lines without indentation, with line breaks between them
            number: the digits and the delimiter, as written
            number is absent for "-", "*" and "+"
            indent: by the list stack below
Quote       consecutive quote lines
            text of a quote line: after ">" and one optional space or tab
            content: the texts with line breaks between them
            texts of white space only at its start and end are dropped
Table       a table start line, its delimiter row, and the table rows directly after them
            columns: one per delimiter cell
            "-" below: one or more "-"
              ":-:"   TACenter
              "-:"    TARight
              ":-"    TALeft
              "-"     TANone
            rows: the header row, then the body rows
            each row has one cell per column
            a missing cell is empty
            cells are divided at each "|"
            the "|" at the line start and at the line end are optional
            in a longer row the last cell is the text from the start of its column to the end of the row, without a "|" at the line end
            a cell is trimmed
Paragraph   consecutive paragraph lines and the blank lines between them
            blank lines before its first line and after its last line belong to no block
            a paragraph line is text as written, indentation included, as now
```

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
join       the lines of one block with line breaks between them
           adjacent segments with the same format joined, as now (Markdown.hs:163-167)
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
/link 1     Command, one line      Command, one Paragraph
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
table header row                  paragraph line
                                  a table once the delimiter row is complete
rule characters at the text end   rule
                                  text once another character is added to the line
block marker before its text      text
                                  a block once text is added
```

## 2. Types

`Markdown.hs`:

```haskell
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
  deriving (Eq, Show)

type MarkdownList = [MarkdownBlock]

data TableAlign = TANone | TALeft | TACenter | TARight
  deriving (Eq, Show)

data ParsedMarkdown = ParsedMarkdown {formattedText :: Maybe InlineMarkdown}
```

- `InlineMarkdown`: the type that `MarkdownList` names now (`Markdown.hs:141`).
- `parseMarkdownList` (`Markdown.hs:169-170`) returns `InlineMarkdown`.
- `markdownToList` (`Markdown.hs:172-174`) returns `InlineMarkdown`.
- `styleMarkdownList` (`Styled.hs:38-41`) takes `InlineMarkdown`.
- `Format` is unchanged.
- `FormattedText` is unchanged.
- `formattedText :: Maybe MarkdownList` is unchanged in the source of:

```
ChatItem          Messages.hs:265
CIQuote           Messages.hs:614
ChatItemVersion   Messages.hs:1362
```

- The JSON of these `formattedText` fields is a list of blocks.
- JSON of `MarkdownBlock`: `sumTypeJSON fstToLower`, as `Format` (`Markdown.hs:536`).
- An unknown tag is decoded as `UnknownBlock`, as in `Format` (`Markdown.hs:538-539`).
- JSON of `TableAlign`: `enumJSON (dropPrefix "TA")`.
- `language` is omitted when absent.
- `number` is omitted when absent.
- A field name of `MarkdownBlock` that clashes in a module importing `Simplex.Chat.Markdown` without an import list is hidden in that import. The modules:

```
Styled.hs
Help.hs
Messages.hs
View.hs
Store/Messages.hs
Terminal/Output.hs
Library/Commands.hs
Library/Internal.hs
apps/simplex-directory-service/src/Directory/Listing.hs
tests/MarkdownTests.hs
bots/src/API/Docs/Types.hs
```

Values; `C` is a list of `FormattedText`, as now:

```
paragraph    {"type":"paragraph","content":C}
             {"_owsf":true,"paragraph":{"content":C}}
heading      {"type":"heading","level":2,"content":C}
codeBlock    {"type":"codeBlock","language":"bash","code":"ls -l"}
quote        {"type":"quote","content":C}
listItem     {"type":"listItem","number":"2.","indent":0,"content":C}
table        {"type":"table","columns":["none","center"],"rows":[[C,C],[C,C]]}
rule         {"type":"rule"}
             {"_owsf":true,"rule":{}}
```

## 3. Parser

```
parseMaybeMarkdownList :: Text -> Maybe MarkdownList       blocks
                                                           name as now (Markdown.hs:148)
parseMaybeInlineMarkdown :: Text -> Maybe InlineMarkdown    source form
inlineSegments :: MarkdownList -> InlineMarkdown           the content segments of the blocks
                                                           the segments of the cells
                                                           in block order
                                                           code excluded
```

`markdownGroups :: [Text] -> [(Maybe MarkdownBlock, InlineMarkdown)]`, internal:

```
pass       one pass over the lines by section 1
group      a block and its source segments, in source order
Nothing    the blank lines that belong to no block
coverage   every line is in one group
```

Parts of a line:

```
syntax    segments without format
            indentation dropped from content
            white space trimmed from cells
            a dropped tab
            the list marker and the space after it
            the "#" run and the space after it
            ">" and the optional space or tab after it, in a quote
            quote texts of white space only, dropped at the start and end of a quote
            the "|" that divides cells
            the white space around a "|" that divides cells
            a "|" at the line start of a table line
            a "|" at the line end of a table line
            fence lines of a code block
            delimiter rows
            rule lines
            code lines
            a final CR
content   the rest
            parsed once by parseMarkdown
            the same segments are in the block and in the source segments
```

`parseMaybeMarkdownList`:

```
command   the one-line command rule (Markdown.hs:149-157) first: Just [Paragraph [command]]
blocks    otherwise the blocks of markdownGroups
Nothing   when they are Paragraphs only and no segment has a format
```

`parseMaybeInlineMarkdown`:

```
command   the one-line command rule first: Just [command], as now
segments  otherwise the source segments of markdownGroups, with a line break between lines
join      adjacent segments with the same format, as now (Markdown.hs:164-167)
Nothing   when no segment has a format, as now (Markdown.hs:159)
```

`T.concat (map markdownText ft)` of the source form is the text, except, as now:

```
**x**              written *x* (Markdown.hs:236-241, 476)
colour names       written as digits (Markdown.hs:295-309, 494-503)
final line break   dropped (Markdown.hs:162)
```

- Mentions and links are the same segments in the block form and in the source form.
- `mentionedNames` (`Markdown.hs:71-76`) takes `MarkdownList` and reads `inlineSegments` of it.
- `hasLinks` (`Markdown.hs:192-193`) takes `MarkdownList` and reads `inlineSegments` of it.
- `hasObfuscatedSimplexLink` (`Markdown.hs:195-203`): a SimpleX link is also searched in the segments of each line parsed by `parseMarkdown`, as the lines are parsed now (`Markdown.hs:163`).

## 4. Consumers

```
site                                                     change
updatedMentionNames (Library/Internal.hs:291-322)        no mentions, or all names the same: the arguments returned
                                                         otherwise: names changed in parseMaybeInlineMarkdown (msgContentText mc)
                                                         the returned list: parseMaybeMarkdownList of the new text
prohibitedSimplexLinks (Library/Internal.hs:426-433)     any ftIsSimplexLink of inlineSegments
removeSimplexLink (Library/Internal.hs:1369-1380)        parseMaybeInlineMarkdown
chatParseMarkdown (Mobile.hs:396)                        parseMaybeInlineMarkdown
                                                         JSON as now
msgText (Terminal/Output.hs:231-237)                     the segments of parseMaybeInlineMarkdown (msgContentText mc)
                                                         the list argument removed
chatEventNotification (Terminal/Output.hs:178, 187)      msgText mc
WebMessage.formattedText (Web.hs:103, 325)               Maybe InlineMarkdown: parseMaybeInlineMarkdown (msgContentText mc)
toFormattedText (Web.hs:426-431)                         Maybe InlineMarkdown: parseMaybeInlineMarkdown
WebChannelPreview (Web.hs:114-115)                       Maybe InlineMarkdown
descriptionContainsLink (Directory/Listing.hs:59-60)     parseMaybeInlineMarkdown
toFormattedText (Directory/Listing.hs:197-198)           InlineMarkdown: parseMaybeInlineMarkdown
DirectoryEntry (Directory/Listing.hs:94-95)              InlineMarkdown
withoutLink (Directory/Service.hs:598-599)               parseMaybeInlineMarkdown
DCSearchGroup (Directory/Service.hs:1059)                ft >>= groupLinkUri . inlineSegments
```

Unchanged, with the block form:

```
Store/Messages.hs:693                parse site
Store/Messages.hs:1122               parse site
Store/Messages.hs:2305               parse site
Store/Messages.hs:2334               parse site
Store/Messages.hs:2426               parse site
Store/Messages.hs:2583               parse site
Store/Messages.hs:3373               parse site
Library/Internal.hs:365-366          msgContentTexts
Library/Internal.hs:368-369          ciContentTexts
Library/Internal.hs:324-333          getCIMentions
Library/Internal.hs:335-341          getRcvCIMentions
Library/Internal.hs:356-363          getMessageMentions
Library/Internal.hs:3094-3097        ciContentHasLink
Library/Internal.hs:3099-3102        msgContentHasLink
Library/Commands.hs:2661             hasLink of a broadcast
Directory/Events.hs:106-108          DCSearchGroup construction
Directory/Events.hs:169              DCSearchGroup type
Directory/Events.hs:201-205          directoryCmdP
```

- Unchanged: `msgPlain` (`View.hs:2463`), source lines parsed by `parseMarkdownList` per line.
- The list returned by `updatedMentionNames` is used at `Library/Internal.hs:225-226` only.

## 5. Docs and libraries

`bots/src/API/Docs/Types.hs`:

```
chatTypesDocsData, next to :288-289    (sti @MarkdownBlock, STUnion, "", ["UnknownBlock"], "", "")
                                       (sti @TableAlign, STEnum, "TA", [], "", "")
next to :521-522                       deriving instance Generic MarkdownBlock
                                       deriving instance Generic TableAlign
```

Written by the "Bot API docs" test (`tests/APIDocs.hs`):

```
bots/api/TYPES.md
the type files of the TypeScript library
the type files of the Python library
```

Source form:

```
channel preview files
directory page files
```

Unchanged:

```
website/src/js
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
- A member profile field with a SimpleX link in code is removed where SimpleX links are prohibited (`Library/Internal.hs:1375, 1380`).
- A SimpleX link in code of a compose field: sending is enabled by the apps, and the text is rejected by the core where SimpleX links are prohibited (`Library/Internal.hs:426-433`). The app checks read the source form:

```
apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/ComposeView.kt:432-445
apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/ComposeView.kt:1085
apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/ComposeView.kt:1736-1741
apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ShareListView.kt:81-84
apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift:463-467
apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift:586
apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift:1898-1906
apps/ios/SimpleXChat/ChatUtils.swift:104-113
apps/ios/Shared/Views/Chat/ChatItemForwardingView.swift:80
apps/ios/SimpleX SE/ShareView.swift:83
```

Bots:

```
formattedText   a list of blocks
                  of an item
                  of a quote
                  of an item version
                the segments are in content
                the segments are in the cells
mentions        a name that is only in code is rejected with SEInvalidMention (Library/Internal.hs:327-331)
```

Remote desktop:

```
versions   minRemoteCtrlVersion (Remote.hs:77-78): the version of this release
           minRemoteHostVersion (Remote.hs:81-82): the version of this release
           both set at the version change of this release, outside this change
refusal    a mobile of an earlier release with a desktop of this release: on the mobile, RCEBadVersion (Remote.hs:465-466, 494-497)
           a desktop of an earlier release with a mobile of this release: on the mobile, RCEBadVersion (Remote.hs:465-466, 494-497)
```

## 7. Rendering contract

- Blocks are in source order.
- Content with a character other than white space:

```
Paragraph
Heading
Quote
ListItem
```
- A CodeBlock has a code line with a character other than white space.
- A Table has the header row.
- Each row of a Table has one cell per column.
- `level` is 1 to 6.
- `indent` is 0 to 6.
- Line breaks in content are text of segments without format.
- A segment with a format is within one line.
- `Nothing`: a text without block syntax and inline formats.
- One Paragraph: a text with inline formats only.
- Drawing: `2026-10-13-block-markdown-rendering.md`.

## 8. Tests

`tests/MarkdownTests.hs`:

```
operators       ==>> (:48-49): InlineMarkdown
                ==>> (:48-49): parseMaybeInlineMarkdown
                <<== (:51-52): InlineMarkdown
                <<==>> (:54-55): InlineMarkdown
:470            unchanged
new operator    ===> for parseMaybeMarkdownList and a block list
new spec        blockMarkdown
```

`blockMarkdown`:

```
heading     each level
            "#": paragraph
            "# ": paragraph
            "#x": paragraph
            "#\tx#": paragraph
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
            ">" alone: paragraph
            "> a\n>\n> b": one quote with an empty line
            "*a*\n>": one paragraph
code        closed
            open at the end of the text
            an info string of two words
            indentation of the fence
            a tab across the column of the fence
            a closing fence with white space after it
            inline syntax in code: text
            "```" alone: paragraph
            "```\n```": paragraph
            "```\n```\nx": one paragraph
            "```\n\n```": paragraph
            "```\n   \n```": paragraph
            "*a*\n```": one paragraph
rule        each character
            "- - -"
            "---" under a text line
table       each alignment
            a row shorter than the header
            a row longer than the header
            a longer row ending with "|": the last cell without that "|"
            a last cell with white space and no "|" after it
            "a | b\n- | -": a table
            a header row without a delimiter row: paragraph
            a table ended by a line without "|"
paragraph   blank lines between lines: text
            blank lines before a heading: no block
command     "/link 1": one Paragraph with Command
CRLF        each block kind with CR at line ends
live        "```", then "```\nls": a paragraph, then a code block
            "---", then "---x"
            an opening fence, then three backticks in the same line
            a delimiter row, then more text in it
```

Source form:

```
round trip      each block kind: T.concat (map markdownText ft) is the text
                each test text above: the same
segments        each block kind: syntax parts as segments without format
                each block kind: content as in the block
same mentions   each block kind: equal Mention names in both forms
                each block kind: equal link segments in both forms
                "* see @bob"
                "* foo *@bob* bar @cath"
                "-\t*x @bob* @cath"
                "x|@bob" as a table row
present cases   texts without block syntax at a line start: unchanged
```

Consumers:

```
mentionedNames   a mention in a table cell is found
                 a mention in code is not found
hasLinks         a URI in code is not found
inlineSegments   each block kind
```

Other modules:

```
tests/ChatTests/Groups.hs:8425-8444   testUpdatedMentionNames
                                        text: a list
                                        text: a table
                                        text: a code block
                                        a renamed mention outside code
                                        the same mention in code
                                        the new text: the source with the new name outside code
                                        the returned list: the block form of the new text
tests/ChatTests/Groups.hs             a mention name only in code: SEInvalidMention
tests/ChatTests/Profiles.hs:3787      SimpleX links prohibited: a SimpleX link in a list item: rejected
tests/ChatTests/Profiles.hs:3787      SimpleX links prohibited: a SimpleX link in a table cell: rejected
tests/ChatTests/Profiles.hs:3787      SimpleX links prohibited: a SimpleX link in code: rejected
tests/ChatTests/Profiles.hs:382       a SimpleX link in a list item of a description: removed
tests/ChatTests/Profiles.hs:382       a SimpleX link in code of a description: the description removed
tests/MarkdownTests.hs:309            hasObfuscatedSimplexLink: an invitation link in a code block
tests/Bots/DirectoryTests.hs          search by a group link in a list item
tests/Bots/DirectoryTests.hs          a group link added to a list item of a welcome message: linkOnlyChange
tests/Bots/DirectoryTests.hs          a group link in a list item of a welcome message: descriptionContainsLink
tests/ChatTests/ChatRelays.hs:44      web preview: a channel message with block syntax: the source form in the file
tests/JSONFixtures.hs                 a block list with each constructor in both encodings
tests/JSONTests.hs:29                 the block list fixtures, for owsf2tagged
tests/JSONTests.hs                    an unknown block tag is UnknownBlock
tests/ChatTests/Direct.hs             a message with a code block: printed with its source lines
tests/ChatTests/Direct.hs             a message with a list: printed with its source lines
tests/ChatTests/Direct.hs             a message with a table: printed with its source lines
```

## Interfaces

```
types       InlineMarkdown
            MarkdownBlock
            MarkdownList: a list of blocks
            TableAlign
functions   parseMaybeMarkdownList: blocks
            parseMaybeInlineMarkdown
            inlineSegments
constants   maxListIndent 6
```

`2026-10-13-block-markdown-rendering.md`:

```
the Kotlin types
the iOS types
the drawing of the blocks
```
