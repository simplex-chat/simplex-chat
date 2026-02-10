# Voice messages in member support scope

## Table of contents

1. Executive summary
2. Problem
3. High-level design
4. Detailed implementation plan

## 1. Executive summary

Allow voice messages from host/admin in member support scope (approval phase) regardless of group voice settings, gated behind chat protocol version 17. This enables the directory bot to send voice captchas in groups that prohibit voice messages. Old clients that don't support this exemption will receive text/image captchas instead.

## 2. Problem

The directory bot sends voice captchas to joining members via the member support scope (`GCSMemberSupport`). However, `prohibitedGroupContent` (Internal.hs:338) blocks voice messages when the group disables voice — with no scope exemption:

```haskell
| isVoice mc && not (groupFeatureMemberAllowed SGFVoice m gInfo) = Just GFVoice
```

Other content types (files, reports, simplex links) already have `isNothing scopeInfo` guards that exempt them in member support scope. Voice does not.

This means voice captchas fail in the majority of real groups that prohibit voice messages. The check runs on both sender side (Commands.hs:3856) and recipient side (Subscriber.hs:1738), so both the bot and the joining member reject voice in these groups.

## 3. High-level design

1. **Protocol version 17** (`memberSupportVoiceVersion`): gates the `prohibitedGroupContent` exemption for voice in member support scope.

2. **Core library change** (Internal.hs): exempt voice in `prohibitedGroupContent` when in member support scope AND sender is admin+ (host/moderator). Only the host can send voice during approval — pending members cannot.

3. **Directory bot change** (Service.hs): check member's protocol version and group voice settings before offering or sending voice captcha. Fall back to text/image captcha for old clients in voice-disabled groups.

## 4. Detailed implementation plan

### 4.1. Protocol.hs — add version 17

**File:** `src/Simplex/Chat/Protocol.hs`

Add to version history comment (after line 79):

```
-- 17 - allow voice messages in member support scope (2026-02-10)
```

Update `currentChatVersion` (line 85):

```haskell
currentChatVersion = VersionChat 17
```

Add version constant (after `shortLinkDataVersion`, line 146):

```haskell
-- support voice messages in member support scope regardless of group voice setting
memberSupportVoiceVersion :: VersionChat
memberSupportVoiceVersion = VersionChat 17
```

### 4.2. Internal.hs — exempt voice in member support scope

**File:** `src/Simplex/Chat/Library/Internal.hs`

Change function header (line 337) to destructure sender's role:

```haskell
prohibitedGroupContent gInfo@GroupInfo {membership = GroupMember {memberRole = userRole}} m@GroupMember {memberRole = senderRole} scopeInfo mc ft file_ sent
```

Change line 338 from:

```haskell
  | isVoice mc && not (groupFeatureMemberAllowed SGFVoice m gInfo) = Just GFVoice
```

to:

```haskell
  | isVoice mc && not (groupFeatureMemberAllowed SGFVoice m gInfo) && not (isJust scopeInfo && senderRole >= GRAdmin) = Just GFVoice
```

Voice is allowed in member support scope ONLY when the sender is admin+ (host/moderator). Pending members cannot send voice in support scope even with this exemption.

**Version gating:** Old clients (< v17) don't have this exemption. On the sender side this is handled by the bot (4.3). On the recipient side:

- Old recipient + voice-disabled group: recipient rejects the voice message (shows "Voice messages: received, prohibited")
- This is why the bot must check the member's version before sending voice

### 4.3. Service.hs — version-aware voice captcha logic

**File:** `apps/simplex-directory-service/src/Directory/Service.hs`

#### 4.3.1. Add import

Add `memberSupportVoiceVersion` to the `Protocol` import:

```haskell
import Simplex.Chat.Protocol (MsgContent (..), memberSupportVoiceVersion)
```

#### 4.3.2. Add helper predicate

Add a helper that checks if voice captcha can be delivered to a member:

```haskell
canSendVoiceCaptcha :: GroupInfo -> GroupMember -> Bool
canSendVoiceCaptcha gInfo m =
  isJust (voiceCaptchaGenerator opts)
    && (groupFeatureUserAllowed SGFVoice gInfo || supportsVersion m memberSupportVoiceVersion)
```

Logic:
- Voice captcha generator must be configured
- AND either the group allows voice for the bot/host (any client version works — old clients accept voice from permitted senders) OR the member's client supports v17 (exemption applies on receive side)

Note: `groupFeatureUserAllowed` checks if the bot (group owner) is permitted to send voice. This is what the recipient's `prohibitedGroupContent` checks — it validates the *sender's* permission (`m` parameter = sender's GroupMember), not the recipient's. Using `groupFeatureMemberAllowed SGFVoice m gInfo` (joining member) would be wrong: it would incorrectly block voice captcha in groups with role-based voice settings (e.g., "admins only").

#### 4.3.3. Update `dePendingMember` hint text (line 572)

Change from:

```haskell
<> if isJust (voiceCaptchaGenerator opts) then "\nSend /audio to receive a voice captcha." else ""
```

to:

```haskell
<> if canSendVoiceCaptcha g m then "\nSend /audio to receive a voice captcha." else ""
```

This hides the `/audio` hint when voice captcha cannot be delivered.

#### 4.3.4. Update `dePendingMemberMsg` `/audio` handling (lines 644-649)

When a member sends `/audio`, check `canSendVoiceCaptcha` before switching mode. If voice captcha is not possible, reply with an upgrade message:

```haskell
| isAudioCmd ->
    if canSendVoiceCaptcha g m
      then case captchaMode of
        CMText -> do
          atomically $ TM.insert gmId pc {captchaMode = CMAudio} $ pendingCaptchas env
          sendVoiceCaptcha sendRef (T.unpack captchaText)
        CMAudio ->
          sendComposedMessages_ cc sendRef [(Just ciId, MCText audioAlreadyEnabled)]
      else sendComposedMessages_ cc sendRef [(Just ciId, MCText voiceCaptchaUnavailable)]
```

#### 4.3.5. Add message constant

```haskell
voiceCaptchaUnavailable :: Text
voiceCaptchaUnavailable = "Voice captcha is not available - please update SimpleX Chat to v6.5+ or use text captcha."
```

#### 4.3.6. Update `dePendingMemberMsg` no-captcha `/audio` path (lines 640-642)

Same check for the case when no pending captcha exists yet:

```haskell
Nothing ->
  if isAudioCmd && canSendVoiceCaptcha g m
    then sendMemberCaptcha g m (Just ciId) noCaptcha 0 CMAudio
    else if isAudioCmd
      then sendComposedMessages_ cc (SRGroup groupId $ Just $ GCSMemberSupport (Just gmId)) [(Just ciId, MCText voiceCaptchaUnavailable)]
      else let mode = CMText
           in sendMemberCaptcha g m (Just ciId) noCaptcha 0 mode
```

### 4.4. Tests

**File:** `tests/Bots/DirectoryTests.hs`

Update existing audio captcha tests to cover:
1. Group with voice enabled + any client version: `/audio` works (existing behavior)
2. Group with voice disabled + member version >= 17: `/audio` works
3. Group with voice disabled + member version < 17: `/audio` shows unavailable message, hint is hidden

### 4.5. Changes summary

| File | Change | Lines affected |
|------|--------|----------------|
| `Protocol.hs` | Add v17 constant, bump `currentChatVersion` | ~4 lines added |
| `Internal.hs` | Exempt admin+ voice in member support scope | 2 lines modified |
| `Service.hs` | Version-aware voice captcha logic | ~15 lines modified/added |
| `DirectoryTests.hs` | Test coverage for version gating | TBD |
