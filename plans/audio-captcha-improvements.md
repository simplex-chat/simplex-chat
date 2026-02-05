# Audio Captcha Improvements Plan

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [High-Level Design](#high-level-design)
3. [Detailed Implementation Plan](#detailed-implementation-plan)
4. [Test Updates](#test-updates)
5. [Files Changed](#files-changed)

---

## Executive Summary

Improve the audio captcha feature by:

1. **Proper command parsing** — add `PendingMemberCmd` type and Attoparsec parser for pending member messages
2. **Audio captcha retry** — when user switches to audio mode, subsequent retries should send voice captcha (not image)
3. **Make `/audio` clickable** — use `/'audio'` format for clickable command in chat UI

---

## High-Level Design

```
┌──────────────────────────────────────────────────────────────────┐
│                      PendingCaptcha State                        │
├──────────────────────────────────────────────────────────────────┤
│  captchaText :: Text       -- the captcha answer                 │
│  sentAt      :: UTCTime    -- when captcha was sent              │
│  attempts    :: Int        -- number of attempts                 │
│  audioMode   :: Bool       -- NEW: user switched to audio mode   │
└──────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────┐
│                      PendingMemberCmd                            │
├──────────────────────────────────────────────────────────────────┤
│  PMCAudioCaptcha           -- user requests audio captcha        │
│  PMCCaptchaAnswer Text     -- user submits captcha answer        │
│  PMCUnknownCommand         -- unknown /command                   │
└──────────────────────────────────────────────────────────────────┘

Flow:
1. User joins group → sendMemberCaptcha (image) + captchaNotice with /'audio'
2. User sends /audio → parsed as PMCAudioCaptcha → set audioMode=True, sendVoiceCaptcha
3. User sends wrong answer (PMCCaptchaAnswer):
   - audioMode=False → send new IMAGE captcha
   - audioMode=True  → send new VOICE captcha  ← NEW BEHAVIOR
4. User sends correct answer → approve member

Message parsing flow (in Service.hs):
┌─────────────────────────────────────────────────────────────────┐
│ DEPendingMemberMsg ... msgText (Text)                           │
│   ↓                                                             │
│ dePendingMemberMsg handler                                      │
│   ↓                                                             │
│ parsePendingMemberCmd msgText                                   │
│   ↓                                                             │
│ T.strip (handles " /audio ")                                    │
│   ↓                                                             │
│ pendingMemberCmdP (case-insensitive for commands)               │
│   ├─ "/audio", "/AUDIO", "/Audio"  → PMCAudioCaptcha            │
│   ├─ "/other"                      → PMCUnknownCommand          │
│   └─ "text"                        → PMCCaptchaAnswer "text"    │
└─────────────────────────────────────────────────────────────────┘
```

---

## Detailed Implementation Plan

### 3.1 Add `PendingMemberCmd` type and parser in Options.hs

**File:** `apps/simplex-directory-service/src/Directory/Options.hs`

**Add type (after `MigrateLog`):**
```haskell
data PendingMemberCmd
  = PMCAudioCaptcha
  | PMCCaptchaAnswer Text
  | PMCUnknownCommand
  deriving (Show)
```

**Add parser (simple pattern matching, no Attoparsec needed):**
```haskell
parsePendingMemberCmd :: Text -> PendingMemberCmd
parsePendingMemberCmd t = case T.uncons (T.strip t) of
  Just ('/', cmd)
    | T.toLower (T.strip cmd) == "audio" -> PMCAudioCaptcha
    | otherwise -> PMCUnknownCommand
  _ -> PMCCaptchaAnswer (T.strip t)
```

**Note:** The parser:
- Strips leading/trailing whitespace (handles ` /audio `, ` abc123 `)
- Uses case-insensitive matching (handles `/AUDIO`, `/Audio`)
- Any input starting with `/` is treated as a command, NOT a captcha answer

**Edge case behavior:**

| Input | Result | Rationale |
|-------|--------|-----------|
| `/audio` | `PMCAudioCaptcha` | Exact command |
| `/AUDIO` | `PMCAudioCaptcha` | Case insensitive |
| ` /audio ` | `PMCAudioCaptcha` | Whitespace stripped |
| `/audio extra` | `PMCUnknownCommand` | Command with trailing text → unknown command (not captcha attempt) |
| `/other` | `PMCUnknownCommand` | Unknown command |
| `abc123` | `PMCCaptchaAnswer "abc123"` | No `/` prefix → captcha answer |
| `/abc123` | `PMCUnknownCommand` | Starts with `/` → command, not captcha |

**Key difference from current behavior:** Currently `/audio extra` is treated as a captcha answer (wrong attempt). With this change, it becomes `PMCUnknownCommand` — user sees "Unknown command" message but captcha attempt count is NOT incremented.

**Update module exports (no new imports needed — `T.uncons` and `T.strip` already available via `Data.Text`):**
```haskell
module Directory.Options
  ( DirectoryOpts (..),
    MigrateLog (..),
    PendingMemberCmd (..),
    getDirectoryOpts,
    mkChatOpts,
    parsePendingMemberCmd,
  )
where
```

---

### 3.2 Extend `PendingCaptcha` with `audioMode` field

**File:** `apps/simplex-directory-service/src/Directory/Service.hs`

**Location:** Lines 103-107

**Before:**
```haskell
data PendingCaptcha = PendingCaptcha
  { captchaText :: Text,
    sentAt :: UTCTime,
    attempts :: Int
  }
```

**After:**
```haskell
data PendingCaptcha = PendingCaptcha
  { captchaText :: Text,
    sentAt :: UTCTime,
    attempts :: Int,
    audioMode :: Bool
  }
```

**GHC compatibility note:** Service.hs already has `{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}` (line 12) and `{-# LANGUAGE DuplicateRecordFields #-}` (line 3). The record update syntax `pc {audioMode = True}` is unambiguous because `audioMode` is unique to `PendingCaptcha` within this module. No additional pragmas needed.

---

### 3.3 Update `sendMemberCaptcha` to accept `isAudioMode` parameter

**File:** `apps/simplex-directory-service/src/Directory/Service.hs`

**Location:** Function `sendMemberCaptcha` (lines 569-589)

**Before:**
```haskell
sendMemberCaptcha :: GroupInfo -> GroupMember -> Maybe ChatItemId -> Text -> Int -> IO ()
sendMemberCaptcha GroupInfo {groupId} m quotedId noticeText prevAttempts = do
  s <- getCaptchaStr captchaLength ""
  mc <- getCaptcha s
  sentAt <- getCurrentTime
  let captcha = PendingCaptcha {captchaText = T.pack s, sentAt, attempts = prevAttempts + 1}
  atomically $ TM.insert gmId captcha $ pendingCaptchas env
  sendCaptcha mc
  where
    getCaptcha s = case captchaGenerator opts of
      Nothing -> pure textMsg
      Just script -> content <$> readProcess script [s] ""
      where
        textMsg = MCText $ T.pack s
        content r = case T.lines $ T.pack r of
          [] -> textMsg
          "" : _ -> textMsg
          img : _ -> MCImage "" $ ImageData img
    sendRef = SRGroup groupId $ Just $ GCSMemberSupport (Just gmId)
    sendCaptcha mc = sendComposedMessages_ cc sendRef [(quotedId, MCText noticeText), (Nothing, mc)]
    gmId = groupMemberId' m
```

**After:**
```haskell
sendMemberCaptcha :: GroupInfo -> GroupMember -> Maybe ChatItemId -> Text -> Int -> Bool -> IO ()
sendMemberCaptcha GroupInfo {groupId} m quotedId noticeText prevAttempts isAudioMode = do
  s <- getCaptchaStr captchaLength ""
  sentAt <- getCurrentTime
  let captcha = PendingCaptcha {captchaText = T.pack s, sentAt, attempts = prevAttempts + 1, audioMode = isAudioMode}
  atomically $ TM.insert gmId captcha $ pendingCaptchas env
  if isAudioMode
    then do
      sendComposedMessages_ cc sendRef [(quotedId, MCText noticeText)]
      sendVoiceCaptcha sendRef s
    else do
      mc <- getCaptcha s
      sendCaptcha mc
  where
    getCaptcha s = case captchaGenerator opts of
      Nothing -> pure textMsg
      Just script -> content <$> readProcess script [s] ""
      where
        textMsg = MCText $ T.pack s
        content r = case T.lines $ T.pack r of
          [] -> textMsg
          "" : _ -> textMsg
          img : _ -> MCImage "" $ ImageData img
    sendRef = SRGroup groupId $ Just $ GCSMemberSupport (Just gmId)
    sendCaptcha mc = sendComposedMessages_ cc sendRef [(quotedId, MCText noticeText), (Nothing, mc)]
    gmId = groupMemberId' m
```

---

### 3.4 Update `sendMemberCaptcha` call site in `dePendingMember`

**File:** `apps/simplex-directory-service/src/Directory/Service.hs`

**Location:** Line 561

**Before:**
```haskell
| memberRequiresCaptcha a m = sendMemberCaptcha g m Nothing captchaNotice 0
```

**After:**
```haskell
| memberRequiresCaptcha a m = sendMemberCaptcha g m Nothing captchaNotice 0 False
```

---

### 3.5 Make `/audio` clickable in `captchaNotice`

**File:** `apps/simplex-directory-service/src/Directory/Service.hs`

**Location:** `dePendingMember` function, `captchaNotice` definition (lines 565-567)

**Before:**
```haskell
captchaNotice =
  "Captcha is generated by SimpleX Directory service.\n\n*Send captcha text* to join the group " <> displayName <> "."
    <> if isJust (voiceCaptchaGenerator opts) then "\nSend /audio to receive a voice captcha." else ""
```

**After:**
```haskell
captchaNotice =
  "Captcha is generated by SimpleX Directory service.\n\n*Send captcha text* to join the group " <> displayName <> "."
    <> if isJust (voiceCaptchaGenerator opts) then "\nSend /'audio' to receive a voice captcha." else ""
```

---

### 3.6 Refactor `dePendingMemberMsg` with pattern matching

**File:** `apps/simplex-directory-service/src/Directory/Service.hs`

**Location:** `dePendingMemberMsg` function (lines 618-656)

**Note on imports:** Service.hs uses implicit import `import Directory.Options` which will automatically include the new exports. No change needed.

**Before:**
```haskell
dePendingMemberMsg :: GroupInfo -> GroupMember -> ChatItemId -> Text -> IO ()
dePendingMemberMsg g@GroupInfo {groupId, groupProfile = GroupProfile {displayName = n}} m@GroupMember {memberProfile = LocalProfile {displayName}} ciId msgText
  | memberRequiresCaptcha a m = do
      let gmId = groupMemberId' m
          sendRef = SRGroup groupId $ Just $ GCSMemberSupport (Just gmId)
      if T.toLower (T.strip msgText) == "/audio"
        then
          atomically (TM.lookup gmId $ pendingCaptchas env) >>= \case
            Just PendingCaptcha {captchaText} ->
              sendVoiceCaptcha sendRef (T.unpack captchaText)
            Nothing -> sendMemberCaptcha g m (Just ciId) noCaptcha 0
        else do
          ts <- getCurrentTime
          atomically (TM.lookup gmId $ pendingCaptchas env) >>= \case
            Just PendingCaptcha {captchaText, sentAt, attempts}
              | ts `diffUTCTime` sentAt > captchaTTL -> sendMemberCaptcha g m (Just ciId) captchaExpired $ attempts - 1
              | matchCaptchaStr captchaText msgText -> do
                  sendComposedMessages_ cc sendRef [(Just ciId, MCText $ "Correct, you joined the group " <> n)]
                  approvePendingMember a g m
              | attempts >= maxCaptchaAttempts -> rejectPendingMember tooManyAttempts
              | otherwise -> sendMemberCaptcha g m (Just ciId) (wrongCaptcha attempts) attempts
            Nothing -> sendMemberCaptcha g m (Just ciId) noCaptcha 0
  | otherwise = approvePendingMember a g m
  where
    a = groupMemberAcceptance g
    rejectPendingMember rjctNotice = ...
    captchaExpired = ...
    wrongCaptcha attempts = ...
    noCaptcha = ...
    tooManyAttempts = ...
```

**After:**
```haskell
dePendingMemberMsg :: GroupInfo -> GroupMember -> ChatItemId -> Text -> IO ()
dePendingMemberMsg g@GroupInfo {groupId, groupProfile = GroupProfile {displayName = n}} m@GroupMember {memberProfile = LocalProfile {displayName}} ciId msgText
  | memberRequiresCaptcha a m = do
      let gmId = groupMemberId' m
          sendRef = SRGroup groupId $ Just $ GCSMemberSupport (Just gmId)
      case parsePendingMemberCmd msgText of
        PMCAudioCaptcha ->
          atomically (TM.lookup gmId $ pendingCaptchas env) >>= \case
            Just pc@PendingCaptcha {captchaText} -> do
              atomically $ TM.insert gmId pc {audioMode = True} $ pendingCaptchas env
              sendVoiceCaptcha sendRef (T.unpack captchaText)
            Nothing -> sendMemberCaptcha g m (Just ciId) noCaptcha 0 True
        PMCCaptchaAnswer text -> do
          ts <- getCurrentTime
          atomically (TM.lookup gmId $ pendingCaptchas env) >>= \case
            Just PendingCaptcha {captchaText, sentAt, attempts, audioMode}
              | ts `diffUTCTime` sentAt > captchaTTL -> sendMemberCaptcha g m (Just ciId) captchaExpired (attempts - 1) audioMode
              | matchCaptchaStr captchaText text -> do
                  sendComposedMessages_ cc sendRef [(Just ciId, MCText $ "Correct, you joined the group " <> n)]
                  approvePendingMember a g m
              | attempts >= maxCaptchaAttempts -> rejectPendingMember tooManyAttempts
              | otherwise -> sendMemberCaptcha g m (Just ciId) (wrongCaptcha attempts) attempts audioMode
            Nothing -> sendMemberCaptcha g m (Just ciId) noCaptcha 0 False
        PMCUnknownCommand ->
          atomically (TM.lookup gmId $ pendingCaptchas env) >>= \case
            Just _ -> sendComposedMessages_ cc sendRef [(Just ciId, MCText unknownCommand)]
            Nothing -> sendMemberCaptcha g m (Just ciId) noCaptcha 0 False
  | otherwise = approvePendingMember a g m
  where
    a = groupMemberAcceptance g
    rejectPendingMember rjctNotice = do
      let gmId = groupMemberId' m
      sendComposedMessages cc (SRGroup groupId $ Just $ GCSMemberSupport (Just gmId)) [MCText rjctNotice]
      sendChatCmd cc (APIRemoveMembers groupId [gmId] False) >>= \case
        Right (CRUserDeletedMembers _ _ (_ : _) _) -> do
          atomically $ TM.delete gmId $ pendingCaptchas env
          logInfo $ "Member " <> viewName displayName <> " rejected, group " <> tshow groupId <> ":" <> viewGroupName g
        r -> logError $ "unexpected remove member response: " <> tshow r
    captchaExpired = "Captcha expired, please try again."
    wrongCaptcha attempts
      | attempts == maxCaptchaAttempts - 1 = "Incorrect text, please try again - this is your last attempt."
      | otherwise = "Incorrect text, please try again."
    noCaptcha = "Unexpected message, please try again."
    unknownCommand = "Unknown command, please enter captcha text."
    tooManyAttempts = "Too many failed attempts, you can't join group."
```

---

## Test Updates

**File:** `tests/Bots/DirectoryTests.hs`

### 4.1 Update expected output for clickable command

**Location:** Line 1278

**Before:**
```haskell
cath <## "Send /audio to receive a voice captcha."
```

**After:**
```haskell
cath <## "Send /'audio' to receive a voice captcha."
```

### 4.2 Add test for audio captcha retry behavior

**Location:** New test function `testVoiceCaptchaRetry` after `testVoiceCaptchaScreening`

**Strategy:** Keep existing happy-path test UNCHANGED. Add SEPARATE test for retry behavior.

**Rationale:** Wrong answer generates a NEW captcha (line 638: `sendMemberCaptcha`). The original `captcha` text becomes invalid. Since the new captcha is only in the audio file, the test cannot complete the join. We verify:
1. Happy path in existing test (unchanged)
2. Retry-sends-voice behavior in new test (doesn't complete join)

**Existing test (`testVoiceCaptchaScreening`) — KEEP UNCHANGED except line 1278:**
```haskell
        -- cath requests audio captcha
        cath #> "#privacy (support) /audio"
        cath <# "#privacy (support) 'SimpleX Directory'> voice message (00:05)"
        cath <#. "#privacy (support) 'SimpleX Directory'> sends file "
        cath <##. "use /fr 1"
        -- send correct captcha (happy path - verifies join works with audio)
        sendCaptcha cath captcha
        cath <#. "#privacy 'SimpleX Directory'> Link to join the group privacy: https://"
        ...
```

**New test (`testVoiceCaptchaRetry`) — ADD after `testVoiceCaptchaScreening`:**

Add to test list in module and implement:
```haskell
testVoiceCaptchaRetry :: HasCallStack => TestParams -> IO ()
testVoiceCaptchaRetry ps = do
  -- same setup as testVoiceCaptchaScreening...
  -- after receiving initial image captcha and switching to audio:
        -- cath requests audio captcha
        cath #> "#privacy (support) /audio"
        cath <# "#privacy (support) 'SimpleX Directory'> voice message (00:05)"
        cath <#. "#privacy (support) 'SimpleX Directory'> sends file "
        cath <##. "use /fr 1"
        -- cath sends WRONG answer after switching to audio mode
        cath #> "#privacy (support) wrong_answer"
        cath <# "#privacy (support) 'SimpleX Directory'!> > cath wrong_answer"
        cath <## "      Incorrect text, please try again."
        -- KEY ASSERTION: retry sends VOICE captcha (not image) because audioMode=True
        cath <# "#privacy (support) 'SimpleX Directory'> voice message (00:05)"
        cath <#. "#privacy (support) 'SimpleX Directory'> sends file "
        cath <##. "use /fr 2"
        -- Test ends here: new captcha text unknown (in audio file only)
        -- Verifies: audioMode persists → retry uses voice, not image
```

**Alternative (simpler):** Extend existing test to verify retry, then have cath disconnect without completing join. This keeps test count lower but tests the key behavior.

---

## Files Changed

| File | Changes |
|------|---------|
| `apps/simplex-directory-service/src/Directory/Options.hs` | Add `PendingMemberCmd` type, `parsePendingMemberCmd` function; update exports |
| `apps/simplex-directory-service/src/Directory/Service.hs` | Import `PendingMemberCmd`; extend `PendingCaptcha` with `audioMode`; update `sendMemberCaptcha` signature; refactor `dePendingMemberMsg` with pattern matching; make `/audio` clickable |
| `tests/Bots/DirectoryTests.hs` | Update expected output (`/'audio'`); add `testVoiceCaptchaRetry` for audioMode persistence (existing happy-path test unchanged) |

---

## Summary of Changes

1. **New type and parser (Options.hs):**
   - `PendingMemberCmd = PMCAudioCaptcha | PMCCaptchaAnswer Text | PMCUnknownCommand`
   - `parsePendingMemberCmd` using simple pattern matching (no Attoparsec):
     - Whitespace stripping via `T.strip` (handles ` /audio `)
     - Case-insensitive command matching via `T.toLower` (handles `/AUDIO`)
     - Any `/`-prefixed input → command (not captcha answer); `/audio extra` → `PMCUnknownCommand`

2. **State tracking (Service.hs):**
   - `PendingCaptcha { ..., audioMode :: Bool }`

3. **Refactored `dePendingMemberMsg` (Service.hs):**
   - Parses message with `parsePendingMemberCmd`
   - Pattern matches on `PendingMemberCmd`
   - `PMCAudioCaptcha` sets `audioMode = True`
   - `PMCCaptchaAnswer` preserves `audioMode` on retry
   - `PMCUnknownCommand` sends error message only (preserves existing captcha and attempt count)

4. **Updated `sendMemberCaptcha` (Service.hs):**
   - Takes `isAudioMode :: Bool` parameter
   - When `isAudioMode = True`, sends voice captcha instead of image

5. **Clickable command:**
   - `"Send /'audio'"` instead of `"Send /audio"`

6. **Test coverage:**
   - `testVoiceCaptchaScreening` (unchanged): verifies happy path — `/audio` → voice captcha → correct answer → join
   - `testVoiceCaptchaRetry` (new): verifies retry behavior — wrong answer after `/audio` → voice retry (not image)
   - Note: Retry test cannot complete join (new captcha text only in audio file), but verifies `audioMode` persistence
