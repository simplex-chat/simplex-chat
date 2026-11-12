# Flat layout for session groups, with the avatar on the name line

Status: questions answered by the user on 2026-11-12; the exact design of the rows waits for the user's approval before the app code. Code references: master `9d3241b06`.

Outcome:
- The group of each new Claude Code session opens in the Flat layout in the SimpleX app. The Main group and `serve`'s agent control chat keep the members' own layout.
- In every Flat chat with avatars, the avatar is centred on the line of the sender's name, and messages are laid out in the full width under it.

## 1. Goal

Roadmap line: "Sessions open in a flat layout by default, with the avatar raised and the text using the full width."

The user's words, 2026-11-11:
> - default sessions to flat layout, with option
> - improve flat layout to shift avatar a bit higher and avoid left gap by shifting text under avatar in flat layout

The user's answers, 2026-11-12:
> just default all new plugin session chats to flat layout, but not control chats, no settings are needed. Users will be discovering flat layout this way - that's what it was created for.

> yes, all flat layouts, that's what users want. Need to see exact design before it's implemented.

The target is the README mockup, `packages/simplex-chat-mcp/images/session.png`: the avatar on the name line, the message text under the avatar at the left padding.

One pull request: branch `ep/mcp-flat-sessions`, against master.

## 2. What users see

### 2.1 In the SimpleX app: the plugin's groups

```
group                                    before                                   after
group of a new session                   the member's own default layout,          Flat
                                         Classic unless they chose Flat
Main group                               the member's own default layout          the same
serve's agent control chat               the member's own default layout          the same
groups created by 0.3.0                  the member's own default layout          the same
```

Group info › `Chat theme` › `Chat layout` of a new session's group:

```
entry          before                after
default        Classic (default)     Flat (chat)
the other one  Flat                  Classic
```

`Classic` there changes the layout for this member's app only. Claude Code, the plugin's options and Claude's posts stay as they are.

### 2.2 In the SimpleX app: every Flat chat with avatars

This applies to groups and channels in the Flat layout, also outside the plugin. Direct chats and note folders, drawn without avatars, stay as they are. Values at the default text size: iOS 17 pt body, 23.4 pt name line, 34 pt avatar; Android and desktop 16 sp body, 22 dp name line, 37 dp avatar.

```
element                          before                                       after
avatar, vertical                 its top on the top of the name line          centred on the name line: 5.3 pt above it on iOS,
                                                                              7.5 dp on Android and desktop
avatar, horizontal               12 from the screen edge                      the same
name and role                    12 after the avatar                          the same
message text, horizontal         after the avatar column: 58 pt iOS,          12 from the screen edge
                                 61 dp Android and desktop
message top                      the bottom of the name line                  the bottom of the avatar: 5.3 pt lower on iOS,
                                                                              7.5 dp lower on Android and desktop
rows without a name              58 pt iOS, 61 dp Android and desktop         12 from the screen edge
events, such as carol connected  58 pt iOS, 61 dp Android and desktop         12 from the screen edge
gap above a name row             7.8 pt iOS, 7.3 dp Android and desktop       at least the avatar's part above the name line:
                                                                              7.8 pt iOS, 7.5 dp Android and desktop
the same, below a date           0                                            the avatar's part above the name line
separator or the chat banner,
and for the oldest loaded item
images on iOS                    the classic width, after the avatar column   the same width, 12 from the screen edge
wide images on Android and       the row less 12 dp on each side              the same rule in the wider row: 49 dp wider
desktop
time of an image without         over the image when the image and the        the same rule; on iOS the time moves beside the
caption                          time are wider than the row                  image, on Android and desktop it stays over it
```

Each line of text is 46 pt wider on iOS and 49 dp wider on Android and desktop.

The exact design is drawn for both platforms, before and after, and approved by the user before the app code. The drawings follow the code's geometry; their agreement with the apps is checked against the snapshot references of master.

```mermaid
flowchart TB
  subgraph Before
    B1[avatar column: avatar at the top of the name line] --- B2[name, then messages, after the column]
  end
  subgraph After
    A1[sender row: avatar centred on the name line, name 12 after it] --> A2[messages and events from the screen edge padding, below the avatar]
  end
```

### 2.3 Order of events

```mermaid
sequenceDiagram
  participant CC as Claude Code
  participant S as service, profile Claude
  participant O as owner's app
  CC->>S: POST /session
  S->>S: new group "N · NAME", groupProfile.chatLayout terminal
  S->>O: invitation through the Main group, with the group's profile
  O->>O: card "Tap to join"
  O->>S: joins
  O->>O: the group opens in Flat: avatar on the name line, messages under it
  opt the member prefers Classic
    O->>O: Group info › Chat theme › Chat layout: Classic, this app only
  end
```

## 3. Questions for the user

Answered on 2026-11-12:
- Q1 (b): the plugin has no option for the layout; new session groups are created Flat.
- Q2 (a): the new geometry applies to every Flat layout. The exact design is approved before the app code.
- Q3 (b): the Main group keeps the members' own layout.

## 4. Decisions

The layout in the core and the apps:
1. A group's layout is `chatLayout` of its group profile; `terminal` is the layout the apps name Flat (`src/Simplex/Chat/Types.hs:924`; `packages/simplex-chat-client/types/typescript/src/types.ts:1906-1909`, `:3235`; `apps/ios/SimpleXChat/ChatTypes.swift:278`; `ThemeModeEditor.kt:541` with `chat_layout_flat` "Flat" at `moko-resources/base/strings.xml:2316`).
2. The core stores the field when a group is created and when an invitation arrives, and sends the group's profile in an invitation through a group (`Store/Groups.hs:385`, `:409-413`, `:483`, `:491-492`; `Library/Commands.hs:4402-4413`). A member invited with a Flat profile gets Flat (`tests/ChatTests/Profiles.hs:3862-3871`).
3. The plugin's core supports it: the field was added in core 7.1.0.14 (`7810fc475`, `simplex-chat.cabal` there), and the plugin runs `core-7.1.3.0` (`packages/simplex-chat-nodejs/src/release.ts:3`; `packages/simplex-chat-mcp/package-lock.json:4321-4322`).
4. Apps from v7.1.0 read the field: `git tag --contains` lists v7.1.0 for `7810fc475` (Android, desktop) and `c3c733e0d` (iOS). A member joins a session group through the Main group only with chat version 22 (`Protocol.hs:149-150`, `Commands.hs:2885`), which is v7.1.0 or later (0.3.0 plan, line 535).
5. A member's own choice for a chat comes first, then the group's layout, then the member's profile and app settings (`ChatModel.kt:1871-1887`; `ChatTypes.swift:2455-2477`).
6. A member makes that choice in Group info › `Chat theme` › `Chat layout` (`GroupChatInfoView.kt:744`; `ChatInfoView.kt:1313-1333`; `GroupChatInfoView.swift:198-201`; `ThemeModeEditor.swift:459-478`). The default entry reads `Flat (chat)` when the group sets Flat (`ChatInfoView.kt:1316-1319`; `ThemeModeEditor.swift:466-473`).
7. The apps send a group's stored profile back whole when owners edit it, so its layout stays (`GroupPreferencesView.swift:238-240`; `GroupPreferences.kt:48`, a copy of `GroupProfile`, whose fields include `chatLayout`, `ChatModel.kt:2819-2829`).

The plugin:
8. Claude writes `terminal` once, in the profile of a new session group (`createSessionGroup`, `hub.ts:477-489`). Claude's later profile updates start from the group's current profile, so the layout stays, also after an owner's `/set layout` (`control.ts:632-640`). This follows decision 103 of the 0.3.0 plan for the Main group's texts.
9. The Main group (`createMainGroup`, `main-group.ts:195-205`) and `serve`'s agent control chat (`createControlChat`, `control.ts:614-623`) keep their profiles (Q3 (b); premise 4 of the 0.3.0 plan).
10. Groups created by 0.3.0 keep their profiles; a resumed session reuses its group (`hub.ts:414-416`).

The Flat layout in the apps:
11. The new geometry applies to every Flat row with an avatar, in groups and channels, also outside the plugin (Q2 (a)).
12. The avatar is centred on the name line (mockup source: `.avatar { top: calc(var(--line) / 2 - var(--avatar) / 2) }`).
13. Messages, events and rows without a name start at the row's start; their text is at the text edge, `CHAT_ITEM_TEXT_PADDING`, 12 (mockup: `.content { padding: 0 var(--pad) }`).
14. A message starts below the lower of the name line and the avatar. When it starts with a card, the paragraph gap is added below that, as below the name now (`TerminalItemView.kt:431`; `TerminalItemView.swift:404`).
15. The name stays 12 after the avatar, by the user's rule of one 12 padding (I26: `plans/2026-10-13-flat-chat-layout-ui.md:39`; `apps/ios/spec/client/chat-view.md:360`). The mockup has 8 px there; the README image is drawn again with 12 (section 8).
16. The gap above a row is at least the avatar's part above the name line. `terminalRowGap` is 0 below a date separator, below the chat banner and for the oldest loaded item (`TerminalItemView.kt:138-139`; `TerminalItemView.swift:130-135`); there the avatar's part sets the gap, so the avatar stays inside its own row.
17. Media sizes keep their rules. On iOS an image is as wide as the classic `maxWidth` (`ChatView.swift:936-944`), and the gutter is removed from the media limit `listWidth − leading − gutter − 24` (`ChatView.swift:2172`), so images keep their width. On Android and desktop a wide image asks for 500 dp (`CIImageView.kt:111`) inside 12 dp paddings on both sides (`FramedItemView.kt:57`), so in the wider row it is 49 dp wider.
18. The time's placement keeps its rules: over an image without caption only when the image and the time are wider than the row (`TerminalItemView.kt:294`, `TerminalItemView.swift:330`). So on iOS the time moves beside such an image; on Android and desktop it stays over it.
19. `gutter` keeps its value on both platforms, the avatar column, and now places only the name (`TerminalItemView.kt:392-394`; `ChatView.swift:2152`). Callers stay as they are.

## 5. Design

Structurally different options, one line each:

```
part     option                                            cost
profile  P1 chatLayout in the profile of a new group       one field; apps from v7.1.0 read it; older apps ignore the key; one key in the profile JSON
         P2 /set layout after creating the group           one more command per group, one more x.grp.info and a "group profile updated" item
         P3 each member's own choice                       written only by that member's app
rows     R1 one geometry for every Flat row with avatar     TerminalChatItemRow on each platform; snapshot references recorded again
         R2 a second Flat geometry for some chats          a flag from the profile or the peer type through ChatView, two geometries,
                                                           twice the tests
         R3 the avatar raised, the message column kept     keeps the left gap the user asked to remove
```

Chosen: P1 and R1. P2 costs a second command and an item for the same result. P3 requires an action of each member. R2 adds a second geometry beyond the request. With R3 the left gap stays.

### 5.1 Plugin

```
hub.ts
  createSessionGroup     profile.chatLayout = T.ChatLayout.Terminal
```

```mermaid
flowchart LR
  H[Hub.session: new session] --> G[createSessionGroup: groupProfile.chatLayout terminal]
  G --> K[core: group_profiles.chat_layout]
  K --> I[x.grp.inv through the Main group]
  I --> N[member's core]
  N --> A[app: activeChatLayout]
  A --> T[TerminalChatItemRow]
```

### 5.2 Android and desktop

`apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt`, `TerminalChatItemRow`:

```
measure        avatar
               name, in the width less gutter
               message, in the full width
avatarOffset   (name height − avatar height) / 2, negative for an avatar taller than the name line
nameTop        max(topGap, −avatarOffset)
avatarTop      nameTop + avatarOffset
messageTop     max(nameTop + name height, avatarTop + avatar height) + card gap when a card starts the message
avatar at      (CHAT_ITEM_TEXT_PADDING, avatarTop)
name at        (gutter, nameTop)
message at     (0, messageTop)
height         messageTop + message height
modifier       padding(top = topGap) replaced by nameTop in the layout
```

Without a sender, the avatar and the name measure 0 × 0, so the message is at `topGap`, as now. Callers in `ChatView.kt:2058-2063` and `Appearance.kt:310` stay.

### 5.3 iOS

`apps/ios/Shared/Views/Chat/ChatItem/TerminalItemView.swift`, `TerminalChatItemRow`:

```
body          VStack, leading
                sender row: HStack, centred
                  gutter column: ZStack, the avatar chatItemTextPadding from the start
                  name and role, chatItemTextPadding from the column
                  below it: terminalParagraphGap when contentStartsWithCard
                content
top padding   max(0, topGap − overhang)
overhang      with an avatar: max(0, (gutter − chatItemTextPadding − terminalLineHeight(body font)) / 2); otherwise 0
```

The sender row is as tall as the taller of the avatar and the name line, so the content starts below both. The top padding less the overhang keeps the name line `topGap` below the row above. `terminalItemRow` (`ChatView.swift:2172`) passes `min(maxWidth, listWidth - leading - 2 * chatItemTextPadding)`. Callers in `AppearanceSettings.swift:396`, `:399` stay.

```mermaid
flowchart LR
  T[ChatView: terminal] --> W[TerminalChatItemRow]
  S[chatItemSender: name, role, avatar] --> W
  G[gutter: avatar width + 12 in groups] --> W
  W --> X[sender row: avatar centred on the name line]
  W --> Y[content from the row start, below the avatar]
```

## 6. Compatibility

```
party                                       what it sees
apps from v7.1.0 to the release before      Flat session groups, with the avatar at the top of the name line and the messages after the avatar column
this change
apps with this change                       Flat session groups with the new geometry
apps before v7.1.0                          ignore chatLayout and show bubbles; they join session groups only when a member adds them directly
Claude Code                                 unchanged
plugin data folders of 0.3.0                Main group and session groups unchanged; session.json unchanged; new session groups get Flat
core and protocol                           unchanged; chatLayout is part of the group profile since core 7.1.0.14
serve                                       unchanged
```

## 7. Acceptance and tests

Done when:
```
#   criterion                                                                                   checked by
1   the user approves the exact design of the rows, both platforms, before and after            design images
2   a new session's group has chatLayout terminal; the Main group has none                      unit tests; e2e test
3   an owner's copy of the session group has chatLayout terminal                                e2e test
4   Flat rows: avatar centred on the name line, messages from the row start below it            desktop test; snapshot references
5   the name and the gap above it are unchanged where the gap covers the avatar                 desktop test; snapshot references
6   the apps build and pass their workflows                                                     android-emu, desktop-linux, ios-sim
7   the session group opens in Flat; Chat theme shows Flat (chat)                               real session (TESTING)
```

Tests that fail on master and pass after the change:
- `test/unit/sessions.test.ts`, "are created at the session start, ...": `chat.newGroups` holds `chatLayout: "terminal"`.
- `TerminalChatItemRowTest`, `messagesStartAtRowStartBelowAvatar`: with gutter 49 and a 36 px avatar, the message is at x 0 and at the avatar's bottom. On master it is at x 49, at the name's bottom.
- The snapshot references recorded on the branch: master renders differ, and `snapshots-compare.py` fails.

Unit tests, `packages/simplex-chat-mcp/test/unit/sessions.test.ts`: the test above; the Main group of `createMainGroup` and of `reset-owners` has no `chatLayout`; a rename keeps an owner's `/set layout`.

End-to-end tests, `test/e2e/service.test.ts`, "an owner joins the Main group and each session's group, ...": Alice's copy of "1 · one" has `chatLayout` `terminal`, and her copy of the Main group has none.

Android and desktop, `apps/multiplatform/common/src/desktopTest/kotlin/chat/simplex/app/TerminalChatItemRowTest.kt`:
- `messagesStartAtRowStartBelowAvatar`, above.
- `avatarCentredOnNameLine`: the avatar's centre is on the centre of the name line, measured against a row without an avatar.
- `gapAboveNameCoversAvatar`: with `topGap` 20, the avatar's top is at 20 less its part above the name line; with `topGap` 0, it is at 0.
- `cardBelowAvatarTakesParagraphGap`: a card starts the paragraph gap below the avatar.
- The three existing tests pass unchanged.

Snapshot references, recorded on the branch with `scripts/ci/snapshots-remote.py desktop record` and `ios record` against the fork:
- desktop: `ChatView.PreviewGroupChatLayoutTerminal`, light and dark, fonts 100, 140 and 150: 6 files.
- iOS: `TerminalAvatars`, `TerminalNames` and `TerminalSpacing` at three sizes, `TerminalOverviewGroup`, `TerminalOverviewChannel` and `TerminalMedia` at large, light and dark: 24 files.
- Unchanged and still equal: the direct chat scenes, such as `TerminalOverviewDirect`, `TerminalQuoteThumbnail`, `TerminalRuleOnly` and `TerminalRightToLeft`, and every classic scene.

CI on the fork branch: the reduced build with the `kotlin`, `ios` and `mcp` scopes; then `android-emu`, `desktop-linux` and `ios-sim` once each, with the app uploaded (`scripts/ci/README.md`, "Starting a run"). `desktop-linux` and `ios-sim` compare the snapshots.

## 8. Docs to change

```
file                                                  change
packages/simplex-chat-mcp/REFERENCE.md                Sessions and their groups: the group is created Flat; each member can choose another
                                                      layout in the group's Chat theme
packages/simplex-chat-mcp/ROADMAP.md                  the line "Sessions open in a flat layout by default, ..." removed
packages/simplex-chat-mcp/spec/api.md                 4.3: a chat layout row in the session group's profile table
packages/simplex-chat-mcp/spec/architecture.md        the row of createSessionGroup
packages/simplex-chat-mcp/TESTING.md                  automated tests: the layout of the session group; real session: Flat and Flat (chat)
packages/simplex-chat-mcp/images/session.png          drawn again from the mockup source with the name 12 after the avatar
packages/simplex-chat-mcp/README.md                   unchanged
apps/multiplatform/spec/client/chat-view.md           section 10, Row Layout, and the rows of terminalGutter and TerminalChatItemRow
apps/ios/spec/client/chat-view.md                     section 9, Rows: avatar, sender row, media width
apps/multiplatform/product/views/chat.md              Terminal Layout: the avatar on the name line, messages under it
apps/ios/product/views/chat.md                        Terminal Layout: the same
```

## 9. Conflict areas with the other items

```
file                                         this item                                          other items
packages/simplex-chat-mcp/src/hub.ts         createSessionGroup                                 harness-notes: Session.turn, Session.post
                                                                                                reactions, replies: Hub.onEvent, Session.onMessage, Session.deliver
                                                                                                agent-profile: possibly Hub.open; core.ts, user.ts
test/unit/sessions.test.ts                   session groups and Main group tests                every item: new tests in the same describe blocks
test/e2e/service.test.ts                     one existing test                                  every item: assertions in the same tests
REFERENCE.md                                 Sessions and their groups                          harness-notes, reactions, replies: Prompts and answers
ROADMAP.md                                   line 7                                             agent-profile: line 8, next to it
TESTING.md                                   the automated tests list                           every item
spec/api.md                                  4.3                                                every item
apps: TerminalItemView.kt, .swift,           TerminalChatItemRow, terminalItemRow               agent-profile, if it changes the avatar of an agent member:
ChatView.swift                                                                                  MemberImage, MemberProfileImage, chatItemSender
snapshot references (PNG)                    30 files                                           agent-profile, if its avatars appear in these scenes: record again after the merge
package version files                        —                                                  the release
```

## 10. Estimate

```
part                                    lines                    work
plugin source                           1                        createSessionGroup
plugin tests                            15                       unit and e2e
Kotlin                                  15 changed               TerminalChatItemRow
Kotlin tests                            50                       TerminalChatItemRowTest
Swift                                   20 changed               TerminalChatItemRow, terminalItemRow
snapshot references                     30 files                 one record run per platform, one check run
docs                                    30                       section 8
CI                                      reduced build; android-emu 15 min; desktop-linux 12 min; ios-sim 25 min, twice
total                                   about half a working day of an agent after the design approval, with the CI runs
```
