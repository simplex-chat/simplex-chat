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

1. **Proper command parsing** — add `DCCaptchaMode CaptchaMode` constructor to `DirectoryCmd` GADT, using existing Attoparsec parsing infrastructure
2. **Audio captcha retry** — when user switches to audio mode, subsequent retries send voice captcha (not image)
3. **Make `/audio` clickable** — use `/'audio'` format for clickable command in chat UI

---

## High-Level Design

```
┌──────────────────────────────────────────────────────────────────┐
│                      CaptchaMode (Events.hs)                     │
├──────────────────────────────────────────────────────────────────┤
│  CMText   -- default image/text captcha                          │
│  CMAudio  -- voice captcha mode                                  │
└──────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────┐
│                      PendingCaptcha State                        │
├──────────────────────────────────────────────────────────────────┤
│  captchaText :: Text           -- the captcha answer             │
│  sentAt      :: UTCTime        -- when captcha was sent          │
│  attempts    :: Int            -- number of attempts             │
│  captchaMode :: CaptchaMode    -- current mode (CMText/CMAudio)  │
└──────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────┐
│                      DirectoryCmd (Events.hs)                    │
├──────────────────────────────────────────────────────────────────┤
│  DCCaptchaMode :: CaptchaMode -> DirectoryCmd 'DRUser            │
│  (integrated into existing GADT, parsed via directoryCmdP)       │
└──────────────────────────────────────────────────────────────────┘

Flow:
1. User joins group → sendMemberCaptcha (image) + captchaNotice with /'audio'
2. User sends /audio → parsed as DCCaptchaMode CMAudio → set captchaMode=CMAudio, sendVoiceCaptcha
3. User sends wrong answer:
   - captchaMode=CMText → send new IMAGE captcha
   - captchaMode=CMAudio → send new VOICE captcha  ← NEW BEHAVIOR
4. User sends correct answer → approve member

Message parsing flow (in Service.hs dePendingMemberMsg):
┌─────────────────────────────────────────────────────────────────┐
│ 1. Parse msgText with directoryCmdP (existing infrastructure)   │
│    ↓                                                            │
│ 2. TM.lookup pendingCaptcha (ONCE, not per-branch)              │
│    ↓                                                            │
│    ├─ Nothing → sendMemberCaptcha with mode from parsed cmd     │
│    └─ Just pc → case on parsed cmd:                             │
│        ├─ DCCaptchaMode CMAudio → set mode, send voice captcha  │
│        ├─ DCSearchGroup _ → captcha answer (verify/retry)       │
│        └─ _ → unknown command (error message)                   │
└─────────────────────────────────────────────────────────────────┘
```

---

## Detailed Implementation Plan

### 3.1 Add `CaptchaMode` type in Events.hs

**File:** `apps/simplex-directory-service/src/Directory/Events.hs`

**Location:** After `DirectoryHelpSection` (line 146)

**Add:**
```haskell
data CaptchaMode = CMText | CMAudio
  deriving (Show)
```

**Update exports (line 10-19):**
```haskell
module Directory.Events
  ( DirectoryEvent (..),
    DirectoryCmd (..),
    ADirectoryCmd (..),
    DirectoryHelpSection (..),
    CaptchaMode (..),
    DirectoryRole (..),
    SDirectoryRole (..),
    crDirectoryEvent,
    directoryCmdP,
    directoryCmdTag,
  )
where
```

---

### 3.2 Add `DCCaptchaMode_` tag in Events.hs

**File:** `apps/simplex-directory-service/src/Directory/Events.hs`

**Location:** In `DirectoryCmdTag` GADT (after line 127, before admin commands)

**Add:**
```haskell
  DCCaptchaMode_ :: DirectoryCmdTag 'DRUser
```

---

### 3.3 Add `DCCaptchaMode` constructor in Events.hs

**File:** `apps/simplex-directory-service/src/Directory/Events.hs`

**Location:** In `DirectoryCmd` GADT (after line 160, with other user commands)

**Add:**
```haskell
  DCCaptchaMode :: CaptchaMode -> DirectoryCmd 'DRUser
```

---

### 3.4 Add "audio" tag parsing in Events.hs

**File:** `apps/simplex-directory-service/src/Directory/Events.hs`

**Location:** In `tagP` function (after line 205, in user commands section)

**Add:**
```haskell
        "audio" -> u DCCaptchaMode_
```

---

### 3.5 Add `DCCaptchaMode_` case in `cmdP`

**File:** `apps/simplex-directory-service/src/Directory/Events.hs`

**Location:** In `cmdP` function (after line 237, with other simple commands)

**Add:**
```haskell
      DCCaptchaMode_ -> pure $ DCCaptchaMode CMAudio
```

---

### 3.6 Add `DCCaptchaMode` case in `directoryCmdTag`

**File:** `apps/simplex-directory-service/src/Directory/Events.hs`

**Location:** In `directoryCmdTag` function (after line 316)

**Add:**
```haskell
  DCCaptchaMode _ -> "audio"
```

---

### 3.7 Update `PendingCaptcha` with `captchaMode` field

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
    captchaMode :: CaptchaMode
  }
```

---

### 3.8 Update import in Service.hs

**File:** `apps/simplex-directory-service/src/Directory/Service.hs`

**Location:** Line 41

**Before:**
```haskell
import Directory.Events
```

**After (no change needed):** The implicit import already imports all exports including the new `CaptchaMode`.

---

### 3.9 Update `sendMemberCaptcha` signature and implementation

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
    sendMemberCaptcha :: GroupInfo -> GroupMember -> Maybe ChatItemId -> Text -> Int -> CaptchaMode -> IO ()
    sendMemberCaptcha GroupInfo {groupId} m quotedId noticeText prevAttempts mode = do
      s <- getCaptchaStr captchaLength ""
      sentAt <- getCurrentTime
      let captcha = PendingCaptcha {captchaText = T.pack s, sentAt, attempts = prevAttempts + 1, captchaMode = mode}
      atomically $ TM.insert gmId captcha $ pendingCaptchas env
      case mode of
        CMAudio -> do
          sendComposedMessages_ cc sendRef [(quotedId, MCText noticeText)]
          sendVoiceCaptcha sendRef s
        CMText -> do
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

### 3.10 Update `dePendingMember` call site

**File:** `apps/simplex-directory-service/src/Directory/Service.hs`

**Location:** Line 561

**Before:**
```haskell
      | memberRequiresCaptcha a m = sendMemberCaptcha g m Nothing captchaNotice 0
```

**After:**
```haskell
      | memberRequiresCaptcha a m = sendMemberCaptcha g m Nothing captchaNotice 0 CMText
```

---

### 3.11 Make `/audio` clickable in `captchaNotice`

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

### 3.12 Refactor `dePendingMemberMsg` with inverted structure

**File:** `apps/simplex-directory-service/src/Directory/Service.hs`

**Location:** `dePendingMemberMsg` function (lines 618-656)

**Key changes:**
1. Parse command FIRST using existing `directoryCmdP`
2. Do TM.lookup ONCE (not per-branch)
3. Case on lookup result, then on command inside

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
        tooManyAttempts = "Too many failed attempts, you can't join group."
```

**After:**
```haskell
    dePendingMemberMsg :: GroupInfo -> GroupMember -> ChatItemId -> Text -> IO ()
    dePendingMemberMsg g@GroupInfo {groupId, groupProfile = GroupProfile {displayName = n}} m@GroupMember {memberProfile = LocalProfile {displayName}} ciId msgText
      | memberRequiresCaptcha a m = do
          let gmId = groupMemberId' m
              sendRef = SRGroup groupId $ Just $ GCSMemberSupport (Just gmId)
              cmd = fromRight (ADC SDRUser DCUnknownCommand) $ A.parseOnly (directoryCmdP <* A.endOfInput) $ T.strip msgText
          atomically (TM.lookup gmId $ pendingCaptchas env) >>= \case
            Nothing ->
              let mode = case cmd of ADC SDRUser (DCCaptchaMode CMAudio) -> CMAudio; _ -> CMText
              in sendMemberCaptcha g m (Just ciId) noCaptcha 0 mode
            Just pc@PendingCaptcha {captchaText, sentAt, attempts, captchaMode} -> case cmd of
              ADC SDRUser (DCCaptchaMode CMAudio) -> do
                atomically $ TM.insert gmId pc {captchaMode = CMAudio} $ pendingCaptchas env
                sendVoiceCaptcha sendRef (T.unpack captchaText)
              ADC SDRUser (DCSearchGroup _) -> do
                ts <- getCurrentTime
                if
                  | ts `diffUTCTime` sentAt > captchaTTL -> sendMemberCaptcha g m (Just ciId) captchaExpired (attempts - 1) captchaMode
                  | matchCaptchaStr captchaText msgText -> do
                      sendComposedMessages_ cc sendRef [(Just ciId, MCText $ "Correct, you joined the group " <> n)]
                      approvePendingMember a g m
                  | attempts >= maxCaptchaAttempts -> rejectPendingMember tooManyAttempts
                  | otherwise -> sendMemberCaptcha g m (Just ciId) (wrongCaptcha attempts) attempts captchaMode
              _ -> sendComposedMessages_ cc sendRef [(Just ciId, MCText unknownCommand)]
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

### 3.13 Add imports in Service.hs

**File:** `apps/simplex-directory-service/src/Directory/Service.hs`

**Location:** After existing imports (around line 28)

**Add:**
```haskell
import qualified Data.Attoparsec.Text as A
import Data.Either (fromRight)
```

**Note:** `T.strip` is already available via the existing `import qualified Data.Text as T`.

---

## Test Updates

**File:** `tests/Bots/DirectoryTests.hs`

### 4.1 Update expected output for clickable command

**Location:** Line 1278 (or wherever `"Send /audio"` appears)

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

**Strategy:** Add test that verifies wrong answer after `/audio` sends voice retry (not image).

```haskell
testVoiceCaptchaRetry :: HasCallStack => TestParams -> IO ()
testVoiceCaptchaRetry ps = do
  -- Setup similar to testVoiceCaptchaScreening...
  -- After receiving initial image captcha and switching to audio:
        -- cath requests audio captcha
        cath #> "#privacy (support) /audio"
        cath <# "#privacy (support) 'SimpleX Directory'> voice message (00:05)"
        cath <#. "#privacy (support) 'SimpleX Directory'> sends file "
        cath <##. "use /fr 1"
        -- cath sends WRONG answer after switching to audio mode
        cath #> "#privacy (support) wrong_answer"
        cath <# "#privacy (support) 'SimpleX Directory'!> > cath wrong_answer"
        cath <## "      Incorrect text, please try again."
        -- KEY ASSERTION: retry sends VOICE captcha (not image) because captchaMode=CMAudio
        cath <# "#privacy (support) 'SimpleX Directory'> voice message (00:05)"
        cath <#. "#privacy (support) 'SimpleX Directory'> sends file "
        cath <##. "use /fr 2"
```

---

## Files Changed

| File | Changes |
|------|---------|
| `apps/simplex-directory-service/src/Directory/Events.hs` | Add `CaptchaMode` type; add `DCCaptchaMode_` tag; add `DCCaptchaMode` constructor; add "audio" tag parsing; add `cmdP` case; add `directoryCmdTag` case; export `directoryCmdP`; update exports |
| `apps/simplex-directory-service/src/Directory/Service.hs` | Add imports (`Data.Attoparsec.Text`, `Data.Either.fromRight`); update `PendingCaptcha` with `captchaMode :: CaptchaMode`; update `sendMemberCaptcha` signature; refactor `dePendingMemberMsg` with inverted structure; make `/audio` clickable |
| `tests/Bots/DirectoryTests.hs` | Update expected output (`/'audio'`); add `testVoiceCaptchaRetry` |

---

## Summary of Changes

1. **New type in Events.hs:**
   - `data CaptchaMode = CMText | CMAudio`

2. **New constructor in DirectoryCmd GADT:**
   - `DCCaptchaMode :: CaptchaMode -> DirectoryCmd 'DRUser`
   - Uses existing Attoparsec parsing infrastructure via `directoryCmdP`

3. **State tracking (Service.hs):**
   - `PendingCaptcha { ..., captchaMode :: CaptchaMode }`

4. **Refactored `dePendingMemberMsg` (Service.hs):**
   - Parses command FIRST using `directoryCmdP`
   - Does `TM.lookup` ONCE (inverted structure, no duplication)
   - `Nothing` case: send new captcha in mode derived from command
   - `Just pc` case: switch on command type
     - `DCCaptchaMode CMAudio` → set mode, send voice captcha
     - `DCSearchGroup _` → captcha answer (verify/retry)
     - `_` → unknown command (error message)

5. **Updated `sendMemberCaptcha` (Service.hs):**
   - Takes `CaptchaMode` parameter instead of `Bool`
   - Sends voice or image captcha based on mode

6. **Clickable command:**
   - `"Send /'audio'"` instead of `"Send /audio"`

7. **Test coverage:**
   - `testVoiceCaptchaScreening` (updated): verify clickable command format
   - `testVoiceCaptchaRetry` (new): verify retry behavior with `captchaMode` persistence
