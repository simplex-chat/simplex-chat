# Fix: images can be sent to groups with "Files and media" disabled

## Problem

In a group where the "Files and media" preference is off, this command is
accepted and delivers a visible image to every member:

```
/_send #1 json [{"msgContent":{"type":"image","text":" ","image":"data:image/jpg;base64,..."}}]
```

No file is attached — the picture travels inside the message content itself.
Receiving clients render it normally, so the group preference is bypassed in
both directions: the sender's client raises no error, and recipients display the
image.

## Root cause

`prohibitedGroupContent` (`src/Simplex/Chat/Library/Internal.hs`) decided the
Files/media rule from the presence of a *file*, not from the *content type*:

```haskell
| isNothing scopeInfo && not (isVoice mc) && isJust file_ && not (groupFeatureMemberAllowed SGFFiles m gInfo) = Just GFFiles
```

But `MCImage {text, image :: ImageData}` and `MCVideo {text, image, duration}`
(`src/Simplex/Chat/Protocol.hs`) carry the media preview inline, independent of
any file transfer. With `fileSource` omitted, `file_` is `Nothing`, the guard
never fires, and the message is treated as ordinary text. `MCFile {text}` is
also file content the preference names, whatever is attached to it.

The guard immediately above it shows the correct shape — voice is judged by
content type (`isVoice mc`), never by file presence.

The update path had the same hole on both sides:

- `APIUpdateChatItem` for groups (`Library/Commands.hs`) only checked
  `prohibitedSimplexLinks`, so replacing a text item with image content was
  unchecked when sending.
- `groupMessageUpdate` (`Library/Subscriber.hs`) checked only
  `prohibitedSimplexLinks` on the ordinary path. Its `prohibitedGroupContent`
  call, added in #6870, sits inside the `catchCINotFound` handler — the branch
  that recreates an item deleted locally — so for an item that exists, which is
  the normal case, the Files guard was never reached at all.

The links-only pair predates chat scopes: #4330 added it to both sides when a
text edit could introduce no other prohibited content, which held only while
media required a file.

## Fix

Judge media by content type, the same way voice already is:

```haskell
| isNothing scopeInfo && not (isVoice mc) && (isJust file_ || isMedia mc) && not (groupFeatureMemberAllowed SGFFiles m gInfo) = Just GFFiles
```

with `isMedia` next to `isVoice`/`isReport` in `Protocol.hs`, matching
`MCImage`, `MCVideo` and `MCFile` — the preference is "Files and media", and all
three are its content, whether or not a file is attached.

Both sides of the update path now run the same shared check:

- `APIUpdateChatItem` for groups runs `prohibitedGroupContent` in place of its
  links-only check, restoring the symmetry #4330 established (it also covers
  voice, which was equally unchecked there). `getChatScopeInfo` is hoisted above
  the check to supply the scope; it is a read-only store lookup, so this changes
  only error ordering.
- `updateCI` in `groupMessageUpdate` runs it before applying the update, so it
  is reached for items that exist, which the `catchCINotFound` handler's call
  never was. A prohibited update is ignored with a warning and the previously
  received content remains — the shape the links check there already had, now
  applied to every feature and correctly gated on the item's scope.

Support-scope behaviour is unchanged: the Files guard remains skipped when
`scopeInfo` is set, as before.

## Also: SimpleX links in support chats

The update path carried its own `prohibitedSimplexLinks` check on both sides —
`APIUpdateChatItem` when sending and `groupMessageUpdate` when receiving — with
no scope condition, while the same guard inside `prohibitedGroupContent` is
gated on `isNothing scopeInfo`. So a **new** message containing a link could be
posted in a member support chat, but **editing** a message there to contain one
was rejected.

Support chats are not meant to prohibit content — that is why the files, reports
and links guards are all scope-gated — so the special case is removed from both
sides, and links in updates are judged by the same scope-gated guard as links in
new messages. Nothing stops being validated: the scope-gated guard is what the
send and receive update paths now run, so main-chat updates containing links are
still rejected on sending and ignored on receiving, as before.

## Alternatives considered

- **Check only on send (`assertGroupContentAllowed`).** Does not fix anything: a
  modified client or a direct API caller never runs the sender's check. The
  receiving side is the boundary that matters.
- **Include `MCImage` only.** `MCVideo` carries the identical inline preview and
  would leave the hole open; `MCFile` is named by the same preference.
- **Also gate `MCLink` previews and `MCChat` profile images.** Both embed images
  too, but they belong to the SimpleX-links/link-preview features, and gating
  them would change ordinary link messages in media-off groups. Out of scope.
- **Validate quoted content (`QuotedMsg.content`).** A crafted quote can also
  carry an inline image, and quote content is stored verbatim from the wire
  (`Store/Messages.hs`) — there is no local item to validate it against for a
  member who joined later or deleted the item. A check there could only reject
  the whole message, which would break the legitimate case of replying to an
  image sent while media was still enabled, destroying a text reply over
  incidental context. If that surface is closed later, the right shape is to
  degrade the quote preview to text in the UI, not to reject the message.

## Known effects

- Forwarding an image whose file is unavailable into a media-off group is now
  rejected. `forwardContent` keeps `MCImage` without a file in that case
  (`Library/Commands.hs`), and that payload is exactly what the preference
  forbids.
- Editing the caption of an image or of a file message that was sent while media
  was still enabled is now rejected in a group that has since disabled media — by
  the sender, and by recipients, which ignore the update and keep the previously
  received content. Previously such an edit was delivered.

## Verification

- `cabal build lib:simplex-chat` and `cabal build simplex-chat-test` — clean.
- `testProhibitFiles` (`tests/ChatTests/Files.hs`) extended with four sending
  vectors: `/_send` of image, of video and of file content with no `filePath`,
  and `/_update item` replacing a text message with image content; each expects
  `bad chat command: feature not allowed Files and media` and no delivery to the
  other members.
- Negative control: with the source fix reverted and the test kept, the test
  fails — the send succeeds and the other member receives the message
  (`expected: Nothing but got: Just "09:31 #team alice>"`), reproducing the
  report.
- Both tests also cover the receiving side, which is the boundary that matters,
  by resetting group preferences in the sender's database — the client that does
  not check them. A new message with prohibited content becomes
  `Files and media: received, prohibited` / `SimpleX links: received, prohibited`
  in the recipients' chats, and an update to prohibited content leaves the
  previously received message unchanged. The recipients' state is asserted with
  `/_get chat`, because items that are not `CIRcvMsgContent` are not printed by
  the terminal view for new item events.
- Negative control for the receiving side: with the check in `updateCI` removed,
  both tests fail with the prohibited edit applied at the recipients —
  `but got: Just "#team bob> [edited] hi"` and
  `but got: Just "#team bob> [edited] https://simplex.chat/invitation#/..."`.
- No regression in `group message update`, `group message edit history`,
  `group live message`, the 29 channel message tests, `update group profile`,
  and `group preferences` (direct messages, files & media, SimpleX links).
- `testGroupPrefsSimplexLinksForRole` extended with both sides of the scope
  boundary: with links restricted to owners, a member cannot post a link in the
  main chat and cannot edit a main chat message to contain one (and nothing
  reaches the other members), while in their support chat both the new message
  and the edit are allowed. The support chat edit fails on the previous code
  with `bad chat command: feature not allowed SimpleX links`.
