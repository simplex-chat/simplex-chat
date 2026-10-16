# Block Markdown: rendering in the apps

Date: 2026-10-13. Revised: 2026-10-16, for master.

The apps decode the blocks of `formattedText` (`2026-10-13-block-markdown.md`). The blocks are drawn as one text: the one-text form of section 3, with the present inline formats.

References are to master 41f0c429b.

```
K/   apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/
I/   apps/ios/Shared/Views/
```

## 1. Scope

```
one-text form   every place where the text of an item is drawn: section 4
                every place where the text of a quote is drawn
                every place where the text of an item version is drawn
source form     chatParseMarkdown, as now
                  compose field
                  edit of a sent item
                  profile descriptions
                  welcome messages
drawing         MarkdownText (K/views/chat/item/TextItemView.kt:125-424), unchanged
                messageText (I/Chat/ChatItem/MsgContentView.swift:396-585), unchanged
```

- One text per item, as now:
  - taps
  - long presses
  - secrets
  - mentions
  - commands
  - the typing indicator
  - the meta reserve
  - text direction
- Lines are wrapped.
- Code blocks are scrolled with the chat.
- Tables are scrolled with the chat.

## 2. Types

Kotlin, `K/model/ChatModel.kt`, next to `FormattedText` (`:5146-5158`):

```
MarkdownBlock      @Serializable sealed class
                   @SerialName: the JSON tags of the block Markdown plan
                     Paragraph(content: List<FormattedText>)
                     Heading(level: Int, content: List<FormattedText>)
                     CodeBlock(language: String?, code: String)
                     Quote(content: List<FormattedText>)
                     ListItem(number: String?, indent: Int, content: List<FormattedText>)
                     Table(columns: List<TableAlign>, rows: List<List<List<FormattedText>>>)
                     Rule
TableAlign         @Serializable enum class: none, left, center, right
formattedText      List<MarkdownBlock>?
                     ChatItem (:3393)
                     CIQuote (:4399)
                     ChatItemVersion (:5700)
markdownSegments   (blocks: List<MarkdownBlock>?) -> List<FormattedText>?
                   section 3
                   null for null
```

iOS, `apps/ios/SimpleXChat/ChatTypes.swift`, next to `FormattedText` (`:5624-5650`):

```
MarkdownBlock      enum: Decodable, Hashable
                   cases as the Kotlin classes
                   associated values as the fields of the Kotlin classes
TableAlign         enum: String, Decodable, Hashable
formattedText      [MarkdownBlock]?
                     ChatItem (:3781, init :3767)
                     CIQuote (:4832)
                     ChatItemVersion (:6371)
markdownSegments   (_ blocks: [MarkdownBlock]?) -> [FormattedText]?
                   section 3
                   public: called from the Shared call sites of section 4
```

- An unknown block tag makes the item invalid, as an unknown `Format` tag.
- `FormattedText` is unchanged.
- `Format` is unchanged.

## 3. One-text form

```
block            segments
paragraph        content
heading          content
                 segments without format: Bold
list item        a segment without format
                   4 spaces per indent
                   "•", or the number
                   a space
                 then content
quote            a segment without format: "│ "
                 then content, with "│ " after each line break of its segments without format
code block       code: one Snippet segment
table            each row: the cells, padded with spaces to the column width
                 " │ " between cells
                 padding by the column alignment
                   none         after the text
                   left         after the text
                   right        before the text
                   center       half before, rounded down
                                the rest after
                 column width: the largest length of a cell of the column
                 under the header row: a run of "─" of the column width per column
                 "─┼─" between runs
                 a line break between rows
                 Snippet
                   the separators
                   the padding
                   the line under the header
                   the line breaks between rows
                   the segments without format
rule             "———", without format
between blocks   one segment without format
                   "\n\n"
                   "\n" between two list items
length           the number of Unicode code points
                   Kotlin: String.codePointCount
                   Swift: String.unicodeScalars.count
                 of the texts of the segments of a cell
```

- Adjacent segments are not joined.
- Cells drawn with display texts, with columns aligned by the texts of their segments:

```
a cell with a mention
a cell with a link
a cell with a command
```
- Kotlin, `CIMarkdownText` (`K/views/chat/item/FramedItemView.kt:445-477`): `remember(ci.formattedText) { markdownSegments(ci.formattedText) }`.

## 4. Call sites

Each site passes `markdownSegments` of the field in place of the field:

```
place                       Kotlin                                    iOS
chat item                   K/views/chat/item/FramedItemView.kt:464   I/Chat/ChatItem/FramedItemView.swift:362
quote in an item            K/views/chat/item/FramedItemView.kt:63    I/Chat/ChatItem/FramedItemView.swift:337
chat list preview           K/views/chatlist/ChatPreviewView.kt:264   I/ChatList/ChatPreviewView.swift:315
compose context item        K/views/chat/ContextItemView.kt:60        I/Chat/ComposeMessage/ContextItemView.swift:74
item info: version          K/views/chat/ChatItemInfoView.kt:88       I/Chat/ChatItemInfoView.swift:270
item info: quote            K/views/chat/ChatItemInfoView.kt:140      I/Chat/ChatItemInfoView.swift:345
notification text           K/platform/NtfManager.kt:135              apps/ios/SimpleXChat/Notifications.swift:201
desktop selection           K/views/chat/TextSelection.kt:270         -
desktop selection           K/views/chat/TextSelection.kt:296         -
preview of MsgContentView   -                                         I/Chat/ChatItem/MsgContentView.swift:624
```

Edit of a sent item:

```
Kotlin   K/views/chat/ComposeView.kt:130                    parseToMarkdown(editingItem.content.text) ?: FormattedText.plain(editingItem.content.text)
iOS      I/Chat/ComposeMessage/ComposeView.swift:83         parseSimpleXMarkdown(text) ?? FormattedText.plain(text)
```

Chat link items, the segment before the link:

```
Kotlin   stripFormattedTextLink (K/views/chat/item/TextItemView.kt:622-632)    every line break at its end removed
iOS      stripFormattedTextLink (I/Chat/ChatItem/MsgContentView.swift:608-616)  every line break at its end removed
```

- A link after an open code fence is code text.

Unchanged:

```
K/views/chat/item/FramedItemView.kt:349        short emoji test: formattedText == null
I/Chat/ChatItem/FramedItemView.swift:145       short emoji test: formattedText == nil
K/views/usersettings/Appearance.kt:302         sample quote of the theme preview
K/views/usersettings/MarkdownHelpView.kt       inline formats
apps/ios/Shared/Views/UserSettings/MarkdownHelp.swift   inline formats
```

Removed: `itemDisplayText` (`K/views/chat/item/TextItemView.kt:110-114`).

## 5. Desktop selection

- `selectedItemCopiedText` (`K/views/chat/TextSelection.kt:264-292`) reads `markdownSegments(ci.formattedText)`.
- `snapOffset` (`K/views/chat/TextSelection.kt:295-307`) reads `markdownSegments(ci.formattedText)`.
- The copied text of a selection has these parts of the one-text form:

```
markers
padding
separators
```

## 6. Docs

```
apps/multiplatform/spec/client/chat-view.md:319   TextItemView.kt: blocks drawn in the one-text form
apps/ios/spec/client/chat-view.md:192             MsgContentView: the same
```

## 7. Tests

`apps/multiplatform/common/src/commonTest/kotlin/chat/simplex/app/MarkdownBlocksTest.kt`:

```
decoding           each block value in the JSON of the core
                   each TableAlign value
                   with language
                   without language
                   with number
                   without number
markdownSegments   each block kind
                   a table with each alignment
                   a table with emoji in a cell
                   the separators between blocks
comparison         the text of each segment
                   the format class of each segment
```

Checks on each platform of the apps:

1. A sent message with each block kind: drawn by section 3.
2. A received message with each block kind: drawn by section 3.
3. A text without blocks: drawn as before.
4. Inline parts in each block with content: actions as in a paragraph.

```
inline parts   a link
               a mention
               a secret
               a command
blocks         a heading
               a list item
               a quote
               a table cell
```

5. A message with blocks in each place of section 4, except the chat item and the preview of MsgContentView.
6. Edit of a sent message with blocks: the compose field has the source text.
7. Desktop: selection inside a block, and its copy.
8. Desktop: selection across blocks, and its copy.
9. Desktop: selection across items, and its copy.
10. Live message with an open code fence.
11. Live message with a growing table.
12. Chat link item with text ending with a heading: no empty line at the end of the text.
13. iOS: the cases of `MarkdownBlocksTest.kt` drawn equal to Android.

## Interfaces

```
app types   MarkdownBlock
            TableAlign
functions   markdownSegments
files       MarkdownBlocksTest.kt
```

## Depends on

```
block Markdown plan, "Types"                constructors
                                            fields
                                            JSON tags
block Markdown plan, "Rendering contract"   the parser output
release                                     both plans together
```
