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
never fires, and the message is treated as ordinary text.

The guard immediately above it shows the correct shape — voice is judged by
content type (`isVoice mc`), never by file presence.

Two further consequences of the same assumption:

- `groupMessageUpdate` (`Library/Subscriber.hs`) calls `prohibitedGroupContent`
  with the file argument hardcoded to `Nothing`, so on the receiving side the
  Files guard could never fire for `x.msg.update` — an update could always turn
  a text item into an image.
- `APIUpdateChatItem` for groups (`Library/Commands.hs`) only checked
  `prohibitedSimplexLinks`, so the same substitution was unchecked when sending.
  That check was added in #4330 on both the send and receive side of the update
  path; links were then the only feature reachable by editing a message, which
  held only while media required a file. #6870 later upgraded the receive side to
  the full `prohibitedGroupContent`, leaving the two sides out of sync.

## Fix

Judge media by content type, the same way voice already is:

```haskell
| isNothing scopeInfo && not (isVoice mc) && (isJust file_ || isMedia mc) && not (groupFeatureMemberAllowed SGFFiles m gInfo) = Just GFFiles
```

with `isMedia` next to `isVoice`/`isReport` in `Protocol.hs`, matching
`MCImage`/`MCVideo`. Because all content paths share `prohibitedGroupContent`,
this one guard covers sending (`/_send`), receiving `x.msg.new`, and receiving
`x.msg.update`.

`APIUpdateChatItem` for groups now runs `prohibitedGroupContent` in place of its
links-only check, restoring the symmetry #4330 established and closing the same
substitution at the source (it also covers voice, which was equally unchecked
there). `getChatScopeInfo` is hoisted above the check to supply the scope; it is
a read-only store lookup, so this changes only error ordering.

Support-scope behaviour is unchanged: the Files guard remains skipped when
`scopeInfo` is set, as before.

## Also: SimpleX links in support chats

The update path carried its own `prohibitedSimplexLinks` check on both sides —
`APIUpdateChatItem` when sending and `groupMessageUpdate` when receiving — with
no scope condition, while the same guard inside `prohibitedGroupContent` is
gated on `isNothing scopeInfo`. So a **new** message containing a link could be
posted in a member support chat, but **editing** a message there to contain one
was rejected.

That check predates chat scopes: #4330 added it to both sides of the update path
when a text edit could introduce no other prohibited content, and the scope gate
added later never reached it. Support chats are not meant to prohibit content —
that is why the files, reports and links guards are all scope-gated — so the
special case is removed from both sides, and links in updates are judged by the
same scope-gated guard as links in new messages.

Main-chat updates containing links are still prohibited, via
`prohibitedGroupContent`. The only visible difference is on receiving: such an
update now creates a `CIRcvGroupFeatureRejected` item instead of being silently
ignored, which is what a new message containing a link already did.

## Alternatives considered

- **Check only on send (`assertGroupContentAllowed`).** Does not fix anything: a
  modified client or a direct API caller never runs the sender's check. The
  receiving side is the boundary that matters.
- **Include `MCImage` only.** `MCVideo` carries the identical inline preview and
  would leave the hole open.
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
- Editing the caption of an image that was sent while media was still enabled is
  now rejected in a group that has since disabled media — by the sender, and by
  recipients, which run the same guard. Previously such an edit was delivered:
  #6870 made the receiving side of the update path run the full check, but the
  Files guard there could not fire, as it tested the file that an update never
  carries.

## Verification

- `cabal build lib:simplex-chat` and `cabal build simplex-chat-test` — clean.
- `testProhibitFiles` (`tests/ChatTests/Files.hs`) extended with three vectors:
  `/_send` of image content and of video content with no `filePath`, and
  `/_update item` replacing a text message with image content; each expects
  `bad chat command: feature not allowed Files and media` and no delivery to the
  other members.
- Negative control: with the source fix reverted and the test kept, the test
  fails — the send succeeds and the other member receives the message
  (`expected: Nothing but got: Just "09:31 #team alice>"`), reproducing the
  report.
- No regression in `group message update`, `group message edit history`,
  `group live message`, the four channel message update tests, and
  `group preferences for specific member role` (direct messages, files & media,
  SimpleX links).
- `testGroupPrefsSimplexLinksForRole` extended with both sides of the scope
  boundary: with links restricted to owners, a member cannot post a link in the
  main chat and cannot edit a main chat message to contain one (and nothing
  reaches the other members), while in their support chat both the new message
  and the edit are allowed. The support chat edit fails on the previous code
  with `bad chat command: feature not allowed SimpleX links`.
