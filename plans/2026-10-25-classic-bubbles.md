# Classic bubbles: UI

Date: 2026-10-25.

Chats with the layout `classic` are drawn by the Android and desktop apps as bubbles. This plan measures the bubbles of the base commit and sets one rule set for them, from the principles of the terminal layout (plans/2026-10-13-flat-chat-layout-ui.md, section 2). iOS is not changed.

```
base             7810fc475 (master with #7811; apps/multiplatform as 8ce2e9e77 of ep/llm-ux)
terminal rules   plans/2026-10-13-flat-chat-layout-ui.md, D1–D33
specification    apps/multiplatform/spec/client/chat-view.md, sections 4 and 10
```

Paths are relative to `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/`. Line numbers are those of the base commit. Pixel values are measured on desktop renders at density 1 and font scale 1, light theme.

## 1. Inventory

### Bubble

```
shape            chat/item/ChatItemView.kt:45-49     radius 18dp × 1.2 for the tail shape, tail 9dp wide; roundness preference
fill             chat/item/FramedItemView.kt:256-261 sent or received colour; transparent for media without caption, quote or forward
tail space       chat/item/FramedItemView.kt:276     9dp on the tail side when the tail is drawn
item clip        chat/item/ChatItemView.kt:331       clipChatItem of the bubble shape
cards            chat/item/ChatItemView.kt:1381-1382 group invitations 18dp; other content 8dp
```

### Message text and captions

```
text             chat/item/FramedItemView.kt:503-508 12dp at the sides, 7dp above and below (CHAT_ITEM_TEXT_VERTICAL_PADDING, :559)
style            chat/item/FramedItemView.kt:514     body1 16sp Inter, line height 22sp, Compose default line height style
                                                      first and last lines trimmed: one line 19px high, baseline 15.1px
measured         one line: bubble 33px; capitals 11px below the top edge; baseline 10px above the bottom edge
                 lines 22px apart
markdown blocks  chat/item/MarkdownBlocks.kt:89-90   half the line height (11sp) before a paragraph break
                 chat/item/MarkdownBlocks.kt:94      code cards and tables without margins
                 chat/item/MarkdownBlocks.kt:143     rule 6dp above and below
                 chat/item/MarkdownBlocks.kt:98-103  headings 1.5, 1.3, 1.15 × 16sp
                 chat/item/MarkdownBlocks.kt:111     Markdown quote line height 22sp at 70% colour
                 chat/item/MarkdownBlocks.kt:122     code 14sp, line height 19.8sp
                 each block is its own text, trimmed: list items 19px apart, paragraph lines 22px apart
```

### Time and status

```
place            chat/item/FramedItemView.kt:464-474 over the bottom end of the bubble: 6dp above the bottom edge, 12dp from the end
meta row         chat/item/CIMetaView.kt:55          3dp start padding, icons and time centred vertically
icons            chat/item/CIMetaView.kt:277         12dp; sent and received status 17dp (:119), via proxy 17dp (:111); 4dp spacers (:110-140)
time             chat/item/CIMetaView.kt:141         12sp, default line height (15px)
measured         sent: time baseline on the text baseline (the 17dp tick sets the row height)
                 received: time baseline 1px under the text baseline
reserve          chat/item/TextItemView.kt:253       reserveSpaceForMeta string in the text style: spaces and time at 16sp
                 first character a breaking space: the time moves alone to a new line
measured         "✓✓ 10:15 AM": reserve 92px, drawn meta 80px; at font scale 1.5: 137px and 106px
emoji reserve    chat/item/FramedItemView.kt:383     the same string on its own line under an emoji
```

### Quote

```
band             chat/item/FramedItemView.kt:169     received or sent quote colour, the whole bubble width
text box         chat/item/FramedItemView.kt:92      12dp at the sides, 6dp above and below
sender           chat/item/FramedItemView.kt:102     13.5sp, default font family, default line height (16px)
text             chat/item/FramedItemView.kt:76      15sp, default font family (not Inter), default line height (17px), 2 or 3 lines
thumbnail        chat/item/FramedItemView.kt:182     68dp, flush with the band's top and end
icons            chat/item/FramedItemView.kt:205,220 22dp, 6dp from the band top, 4dp from the end
```

### Headers (report, deleted, moderated, blocked, live, forwarded)

```
band             chat/item/FramedItemView.kt:131     quote colour, the whole bubble width
insets           chat/item/FramedItemView.kt:136     start 8dp, end 12dp, top 6dp; bottom 6dp, or 0 before a quote or forwarded header
row              chat/item/FramedItemView.kt:144     4dp between icon and text; icon 18dp (:151) sets the row height
text             chat/item/FramedItemView.kt:122     12sp italic secondary in a 16sp paragraph, line height 22sp, trimmed to 15px
two rows         chat/item/FramedItemView.kt:364-367 6dp apart, chat name 15sp moved 2dp up
measured         band 30px; text 4px further in than the message text
```

### Media and captions

```
image, video     chat/item/FramedItemView.kt:394,405 flush with the bubble edges, clipped by the bubble shape
caption          chat/item/FramedItemView.kt:400,411 message text box: 7dp under the media
white time       chat/item/FramedItemView.kt:398     over the media without caption, 6dp above its bottom
link preview     helpers/LinkPreviews.kt:213-218     text 6dp under the image, 12dp at the sides; title 16sp/22sp, 4dp under it;
                                                      description 14sp/20sp; link 12sp default line height
chat link card   chat/item/CIChatLinkHeader.kt:23-77 8dp start, 12dp end, 8dp top, 4dp + 4dp bottom; 54dp image; 13sp/18sp lines
```

### Emoji

```
in a bubble      chat/item/FramedItemView.kt:375-388 12dp at the sides, 6dp above, 6dp + 2dp below; the reserve line under it
without bubble   chat/item/EmojiItemView.kt:31       12dp at the sides, 8dp above and below; the time under the emoji
```

### Files, voice, calls

```
file row         chat/item/CIFileView.kt:213         4sp above, 6sp below, 6sp start, 12sp end; icon 40sp, 2sp lower (:137)
file text        chat/item/CIFileView.kt:230-242     name 16sp, size 14sp with the reserve string at 14sp; default line heights
voice with text  chat/item/CIVoiceView.kt:50         14sp above and below, 6sp at the sides; 56sp button
voice alone      chat/item/CIVoiceView.kt:51         4sp above, 6sp below; time 6sp under the row (:207, :225)
call             chat/item/CICallItemView.kt:31-32   4dp at the sides, 8dp below; no bubble fill
```

### Deleted, moderated and error items

```
deleted          chat/item/DeletedItemView.kt:30-38  rounded 18dp card; 12dp at the sides, 6dp above and below; time 8dp after the text,
                                                      rows bottom-aligned
marked deleted   chat/item/MarkedDeletedItemView.kt:33-91 the same card; 12sp text in a 16sp paragraph, one line; time centred vertically
errors           chat/item/IntegrityErrorItemView.kt:63-71 as deleted
decryption       chat/item/CIRcvDecryptionError.kt:150,202 12dp at the sides, 6dp above and below; time over the bottom end;
                                                      reserve string at 16sp
group invitation chat/item/CIGroupInvitationView.kt:101-142 card: 8dp start, 12dp end, 8dp top, 4dp bottom; 13sp/18sp lines
```

### Events

```
text             chat/item/CIEventView.kt:19         12sp secondary light in a 16sp paragraph with line height 22sp: 15px for one line,
                                                      22px between wrapped lines; 4 lines at most
                 chat/item/ChatItemView.kt:639       a member name is joined by a 16sp space: that line 19px, its baseline 4px lower
padding          chat/item/CIEventView.kt:33         6dp at the sides, 6dp above and below
feature icons    chat/item/CIChatFeatureView.kt:133  20dp, sets the row height (32px against 27px for text events)
merged icons     chat/item/CIChatFeatureView.kt:42   8dp above and below, icons top-aligned
preference       chat/item/CIFeaturePreferenceView.kt:30 18dp icon
time             chat/item/CIEventView.kt:39         joined to the text by two spaces (D28)
```

### Reactions

```
row              chat/item/ChatItemView.kt:148       under the bubble, shifted with the tail
pill             chat/item/ChatItemView.kt:162,215   5dp apart, 2dp above and below, 2dp inside; 8dp corners
text             chat/item/ChatItemView.kt:216,238   emoji 12sp; count 11.5sp, 4dp after the emoji
```

### Gaps between rows

```
row box          chat/ChatView.kt:2000-2013          1dp above and below a message close to its neighbour; 4dp at a large gap;
                                                      8dp below the newest row
close messages   2dp between bubbles (same sender, under 60 s)
large gap        8dp between bubbles (another sender, or 60 s or more)
group name       chat/ChatView.kt:2063               8dp above the name column
                 chat/ChatView.kt:2072               2dp under the name row
                 chat/ChatView.kt:2088               13.5sp secondary, default line height
                 chat/ChatView.kt:2121               bubble 2dp under the avatar top, 4dp after the avatar
avatar           chat/ChatView.kt:2869               37dp × fontSizeSqrtMultiplier; with a one-line bubble it sets the row height
measured         previous bubble → name capitals 14px; name baseline → bubble edge 11px
                 a close message after a one-line message with avatar: 4px (2px elsewhere)
date separator   chat/ChatView.kt:3098-3106          14sp medium secondary, 12dp above and below, centred; shared with terminal rows
side insets      chat/ChatView.kt:2064,2143,2241,2257-2258 12dp at the screen edge; 66dp, 76dp or 104dp on the opposite side
```

## 2. Rules

The bubble rules follow the principles of the terminal layout. Bubble values are named for bubbles; terminal constants are reused where the relation is the same.

```
line height      every text of a chat item: font size × 22/16 × f(w), Proportional, Trim.None
                 except cards, files, voice and calls (D8) and the one-line text of marked deleted items (its insets are set by baselines)
                 principle: one line-height ratio with the contrast factor; full line height for every line (D1, D2, D7)
                 the same function as terminal rows (chatTextStyle): the relation is the same
inset            BUBBLE_TEXT_INSET 11sp (user): from a bubble or band edge to the capitals of the text under it,
                 and from the last baseline to the edge under it
                 principle: one inset for all parts; gaps measured from a baseline or an edge to the capitals or an edge
side inset       BUBBLE_SIDE_INSET 14dp (user) for message text, captions, the header, the quote, deleted and error items,
                 an emoji under a quote or header, and the end of the time
text paddings    BUBBLE_TEXT_TOP_GAP 5sp and BUBBLE_TEXT_BOTTOM_GAP 6.625sp: the inset for 16sp text with the full line height
bands            header and quote text: bubbleTextPadding, the inset from the band edges by the first and last baselines
time             the baseline of the time BUBBLE_TEXT_INSET above the bubble bottom, on the last baseline of the text
                 ticks without a time: where they stand next to a time (as terminal rows)
                 principle: inline elements on the text baseline (D3)
reserve          metaReserveCopy in bubbles: a breaking space after text, direction mark, icon spaces in dp, time at 12sp
                 the time alone moves to the next line when it does not fit (user)
                 principle: a precise reserve for the time (as terminal rows)
row gaps         BUBBLE_CLOSE_GAP 2sp between close messages, BUBBLE_FAR_GAP 8sp at a large gap
                 principle: constants in sp; the measured gaps are kept (D7)
markdown         the spacing of terminal rows: paragraph break and above a heading TERMINAL_LEVEL_STEP, under a heading
                 TERMINAL_HEADING_BOTTOM_GAP, cards and rules TERMINAL_BLOCK_TOP_GAP above, TERMINAL_BLOCK_BOTTOM_GAP below
                 principle: the same relations inside message text (levels 2 and 3)
icons            header, feature and preference icons centred on their text, without height
                 principle: icons centred on their text; the same gap for equivalent elements whatever the content
```

## 3. Decisions

```
B1   message text and captions: full line height; 5sp above, 6.625sp below; 14dp at the sides (user)
B2   time: baseline on the last text baseline, by its distance from the bubble bottom
B3   reserve: the copy of the time in message text, under an emoji, in decryption errors and group invitations,
     after text with a breaking space before it and without the word joiner (user);
     files keep the string reserve of their size line (shared with terminal rows)
B4   header: 14dp start, as message text (user); text inset by bubbleTextPadding; icon without height;
     header and quote bands kept (user)
B5   quote: text inset by bubbleTextPadding; Inter for the sender and the text (earlier owner decision: quotes in Inter)
B6   emoji in a bubble: the text paddings; the reserve line with the full line height
B7   deleted, moderated and error items: text inset by bubbleTextPadding; time on the text baseline;
     the fill takes the bubble shape and the text keeps the tail space of a message (user)
B8   file in a bubble: the last baseline of the size line BUBBLE_TEXT_INSET above the edge; the time and the ink bottom
     of the file icon on that baseline (user)
B9   events: line height of 12sp secondary text; feature icons and merged icons without height, one event line high (user)
B10  row gaps in sp, values kept; a sender change in direct chats 8sp (user)
     events: 2sp between the events of a run, 8sp around the run; a message next to an event at a large gap,
     with its tail and time; the avatar column of an event without its 8dp top padding (user)
B11  markdown blocks in bubbles: spacing and line heights of terminal rows (user); headings × 1.5625, 1.25, 1, bold,
     capitals placed by headingBox, in both layouts (user)
B12  unchanged: fills, tails, roundness, colours, themes, wallpapers, interaction; link preview, chat link card,
     group invitation, voice and call layout (D8: cards in the redesign); avatars; date separators
     (shared with terminal rows); reactions; emoji without bubble; chat item info
B13  calls: as on master, without the bubble typography (user)
B14  group sender name: starts at the bubble edge, 4dp after the avatar, as the bubble (user, as iOS)
```

## 4. Kotlin

```
chat/item/TerminalItemView.kt     LocalBubbleLayout; usesChatTypography(); chatTextStyle (was terminalTextStyle) in both layouts;
                                  inlineIcon in both layouts; metaBaseline shared by TerminalMetaLayout and the bubbles;
                                  hasTerminalSender shared with the event runs of bubbles
chat/item/FramedItemView.kt       BUBBLE_* constants, BUBBLE_SIDE_INSET among them; Modifier.bubbleTextPadding, Modifier.bubbleMeta(),
                                  RowScope.bubbleMetaOnBaseline(); header, quote, emoji, meta and text insets
chat/item/CIMetaView.kt           metaReserveCopy (was terminalReserveSpaceForMeta); breakBefore: a breaking space instead of the word joiner
chat/item/TextItemView.kt         the copy for every message text with a time, breakBefore in bubbles after text;
                                  ruleMinWidth in terminal rows only
chat/item/MarkdownBlocks.kt       spacing, heading sizes and headingBox of terminal rows when usesChatTypography();
                                  TERMINAL_HEADING_RATIO 1.25
chat/item/CIEventView.kt          chatEventTextStyle in both layouts; chatEventPadding without the vertical parameter
chat/item/CIChatFeatureView.kt    merged icons one event line high in both layouts
chat/item/CIFileView.kt           bubble: size line baseline inset from the bottom edge; fileIconOnBaseline
chat/item/DeletedItemView.kt      bubble: bubbleTextPadding, time on the text baseline; RectangleShape, bubbleTailSpace
chat/item/MarkedDeletedItemView.kt
chat/item/IntegrityErrorItemView.kt
chat/item/CIRcvDecryptionError.kt bubble: bubbleTextPadding, bubbleMeta, the copy of the time; RectangleShape, bubbleTailSpace
chat/item/CIGroupInvitationView.kt bubble: the copy of the time
chat/item/ChatItemView.kt         calls without LocalBubbleLayout; tailRendered for deleted and error items; bubbleTailSpace
chat/ChatView.kt                  LocalBubbleLayout provided for classic chats; row gaps in sp; event runs (eventRunLargeGap);
                                  sender name at the bubble edge
usersettings/Appearance.kt        LocalBubbleLayout for the classic theme preview
```

## 5. Tests

`apps/multiplatform/common/src/desktopTest/kotlin/chat/simplex/app/BubbleLayoutTest.kt`:

- time baseline BUBBLE_TEXT_INSET above the bubble bottom
- ticks without a time where they stand next to a time
- text capitals and last baseline inset from the edges, BUBBLE_SIDE_INSET at the sides
- no bottom inset before another part of the band
- time on the text baseline in a card row
- file icon ink bottom on the size-line baseline

`apps/multiplatform/common/src/desktopTest/kotlin/chat/simplex/app/TerminalMetaCopyTest.kt`:

- the bubble copy starts with a breaking space; the time alone moves to the next line

## 6. Results

Desktop renders of 54 items and 4 chat lists, light and dark, font scale 1 and 1.5, right-to-left; the same renders of terminal rows. Values at font scale 1, light theme; master in parentheses.

```
terminal rows    330 of 348 renders pixel-identical to the base; the other 18 are the heading fixtures (B11)
one-line bubble  34px (33px); capitals 11px below the top edge (11px), last baseline 11px above the bottom edge (10px)
                 the first ink 15px from the edge (13px), the end of the time 15px (13px), each with 1px of glyph bearing
time             baseline on the last text baseline for sent and received messages (received: 1px lower before)
reserve          "✓✓ 10:15 AM": 79px (92px) for a meta of 80px; at font scale 1.5: 105px (137px) for 106px
                 after text one space before it (4–5px); the time alone moves to the next line when it does not fit
header           text and icon 14dp from the edge (8dp); band 31px (30px)
quote            Inter; full line heights; band 33px with one line (29px), taller with a sender and two lines
deleted, errors  one-line card 34px (31px), as a one-line message; marked deleted 31px (27px); time on the text baseline
                 decryption errors and legacy deleted items: the tail drawn, the text 14px from the visible edge (3px)
file             size line baseline 11px above the edge (9px); the time and the ink bottom of the file icon on it
events           runs: between event rows 8px of background in groups (avatars), 12–14px in direct chats;
                 a message next to a run shows its tail and time
markdown         list items 22px apart (19px); paragraph break, headings, cards and rules at the gaps of terminal rows
headings         25, 20 and 16sp, bold, in bubbles and terminal rows (bubbles: 24, 20.8, 18.4sp; terminal rows: 27.6, 21, 16sp)
calls            pixel-identical to master (12 of 12 renders)
sender names     the first ink 0–1px from the bubble edge (6–7px)
row gaps         unchanged at font scale 1; × 1.5 at font scale 1.5
```

## 7. Open questions

```
C8   group invitation with long translations: the time and the text on one baseline (user); options rendered:
     A the time on the last baseline, joined to the last word; B on the baseline of the status line;
     C on the baseline of the group name; the code unchanged until an option is chosen
```
