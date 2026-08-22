# Group message rate limiting (anti-spam)

Status: design settled, ready to implement. Started 2026-06-27.

## Goal

Limit how many messages a single group member can send per minute, per hour, and per day, to stop spam, without disrupting normal use.

## Scope

- **p2p groups only.** Relay/channel groups do not need this: ordinary members are observers and cannot send; the senders there are promoted contributors.
- Enforced at three points: the **sender** (rejects before sending), a **forwarding admin** (drops, so it does not fan the message out), and each **recipient** (drops and shows a collapsed indicator).
- **Only direct messages are checked** (`forwarded == False`). A message forwarded by an admin is not re-checked: the admin already applied the limit, and if an admin floods, the user leaves the group. This also means the timestamp used is always the own-server `brokerTs`, never the admin-asserted `fwdBrokerTs`.
- **Counting is by server time** (`brokerTs`), not by local receive time, so catch-up after being offline does not trip the limit.

## Settled decisions

- Limit values are three optional caps: `Nothing` per window = that window uncapped; all `Nothing` = off.
- Stored as a plain field in `GroupPreferences` (not a `GroupFeature`), resolved through `FullGroupPreferences` to the default when unset. Default (Moderate): 20 / minute, 300 / hour, 1500 / day.
- A **global override** in `ChatConfig`, set by a CLI option, not persisted. It replaces the group rule on that client and applies to send, forward, and display alike. Low or zero caps stop forwarding and drop locally, so no separate forwarding switch is needed.
- Moderators and above are exempt. Only `XMsgNew` content counts, and only distinct messages.
- The recipient shows one collapsed "N messages rate-limited" item per burst.

---

## 1. Data model (`src/Simplex/Chat/Types/Preferences.hs`)

New type, next to `TimedMessagesGroupPreference`:

```haskell
data GroupRateLimit = GroupRateLimit
  { perMinute :: Maybe Int
  , perHour   :: Maybe Int
  , perDay    :: Maybe Int
  }
  deriving (Eq, Show)
```

- Add JSON with the existing TH splice near line 1137: `$(J.deriveJSON defaultJSON ''GroupRateLimit)`.
- `GroupPreferences` (line 314): add `rateLimit :: Maybe GroupRateLimit`. `Nothing` = owner set nothing.
- `FullGroupPreferences` (line 410): add `rateLimit :: GroupRateLimit`. Always concrete.
- `defaultGroupPrefs` (line 479): `rateLimit = GroupRateLimit (Just 20) (Just 300) (Just 1500)`.
- `emptyGroupPrefs` (line 497): extra `Nothing`.
- `mergeGroupPreferences` (line 991): resolve `rateLimit` from the `Maybe` field, falling back to `defaultGroupPrefs`.
- `toGroupPreferences` (line 1012): map the resolved value back to `Just`.
- `defaultBusinessGroupPrefs` / `businessGroupPrefs` (lines 500, 517): add the field (business groups keep the default).

Backward compatibility: `rateLimit` is a `Maybe` field, so older serialized preferences omit it and decode as `Nothing`, which resolves to the default. No protocol version gate is needed; the limit takes effect only on clients that include this code, and they enforce the compiled default even when the field is absent.

## 2. Runtime state (`src/Simplex/Chat/Controller.hs`, built in `src/Simplex/Chat.hs`)

```haskell
data MemberRate = MemberRate
  { accepted     :: Seq UTCTime       -- accepted server timestamps, for the window check
  , dropped      :: Int               -- actual drops in the current burst
  , savedDropped :: Int               -- the value last written to droppedItem
  , droppedItem  :: Maybe ChatItemId  -- the open indicator item for the current burst
  }

emptyMemberRate :: MemberRate
emptyMemberRate = MemberRate Seq.empty 0 0 Nothing
```

Add to `ChatController` (line 140+ area, near `timedItemThreads`):

```haskell
msgRates :: TMap (GroupId, GroupMemberId) (TVar MemberRate)
```

Initialize in `newChatController` (`src/Simplex/Chat.hs` ~202, beside `cleanupManagerAsync`): `msgRates <- TM.emptyIO`, and add it to the record (~247).

Own sends use the key `(groupId, membershipMemberId)`; received messages use `(groupId, senderMemberId)`. They never share a key.

## 3. Global override (`ChatConfig` + CLI)

- `ChatConfig` (Controller.hs:140): add `groupRateLimitOverride :: Maybe GroupRateLimit`.
- `defaultChatConfig` (`src/Simplex/Chat.hs` ~110): `groupRateLimitOverride = Nothing`.
- CLI option in `src/Simplex/Chat/Options.hs` (`CoreChatOpts`, line 56): a flag such as `--group-msg-rate-limit "20,300,1500"` and an off form; populate `ChatConfig` when the controller is assembled. Exact flag name and syntax are operator-facing - confirm before finalizing.

## 4. Counting logic (new, e.g. `src/Simplex/Chat/Library/Internal.hs`)

The window check, pure:

```haskell
minute, hour, day :: NominalDiffTime
minute = 60; hour = 3600; day = 86400

-- prune entries older than one day before t, and report whether t is within the caps
checkAccepted :: GroupRateLimit -> UTCTime -> Seq UTCTime -> (Bool, Seq UTCTime)
checkAccepted GroupRateLimit{perMinute, perHour, perDay} t ts =
  let kept  = Seq.dropWhileL (<= addUTCTime (-day) t) ts
      nDay  = Seq.length kept
      nHour = countSince (addUTCTime (-hour)   t) kept
      nMin  = countSince (addUTCTime (-minute) t) kept
      within = not (reached perMinute nMin || reached perHour nHour || reached perDay nDay)
   in (within, kept)
  where
    countSince x = Seq.length . Seq.takeWhileR (> x)   -- entries newer than x are a suffix
    reached Nothing  _ = False
    reached (Just c) n = n >= c

allOff :: GroupRateLimit -> Bool
allOff (GroupRateLimit a b c) = isNothing a && isNothing b && isNothing c
```

Get-or-create the per-member `TVar`, two lookups (IO then STM), combinator form (no `\case` ladders), mirroring `storeNtf` (`simplexmq` `Server/NtfStore.hs:37`):

```haskell
memberRateVar :: TMap (GroupId, GroupMemberId) (TVar MemberRate)
              -> (GroupId, GroupMemberId) -> IO (TVar MemberRate)
memberRateVar rates key =
  TM.lookupIO key rates >>= maybe (atomically getCreate) pure
  where
    getCreate = TM.lookup key rates >>= maybe create pure
    create = do
      tv <- newTVar emptyMemberRate
      TM.insert key tv rates
      pure tv
```

`TM` is `Simplex.Messaging.TMap`, defined in `simplexmq`; no addition to that library is needed.

## 5. Effective limit

```haskell
effectiveLimit :: Maybe GroupRateLimit -> GroupInfo -> GroupRateLimit
effectiveLimit override GroupInfo{fullGroupPreferences} =
  fromMaybe (rateLimit fullGroupPreferences) override
```

`override` = `asks (groupRateLimitOverride . config)`; the group rule is `rateLimit fullGroupPreferences` (Types.hs:480).

## 6. Sender check (`src/Simplex/Chat/Library/Commands.hs`, `sendGroupContentMessages_` ~4457)

Beside `assertGroupContentAllowed` (line 4459), add `assertWithinSendRate`:

- Skip when `memberRole' membership >= GRModerator` (exempt) or the effective limit `allOff`.
- Skip for support scope (messages to admins), keep for the main chat.
- `tv <- memberRateVar rates (groupId, membershipMemberId)`; `now <- getCurrentTime`.
- `atomically $ do r <- readTVar tv; let (within, kept) = checkAccepted lim now (accepted r); when within (writeTVar tv r{accepted = kept Seq.|> now}); pure within`.
- If not within, `throwCmdError` (or a dedicated `CEGroupSendRateExceeded` for the apps to detect - confirm error surface). The message is not recorded when rejected.

Local clock is correct here because the sender is online when it sends.

## 7. Recipient and forwarding admin (`src/Simplex/Chat/Library/Subscriber.hs`, `newGroupContentMessage` ~2142)

In the `Just m` branch, after `mkGetMessageChatScope`, before the existing `blockedByAdmin` branch, add rate handling for `not forwarded` and non-exempt members:

- Compute `lim = effectiveLimit override gInfo'`. If `forwarded`, `memberRole m' >= GRModerator`, or `allOff lim`, skip straight to the existing logic.
- `tv <- memberRateVar rates (groupId, memberId)`.
- One transaction on `tv` using `checkAccepted lim brokerTs (accepted r)`:
  - **within limit (accept):** set `accepted = kept |> brokerTs`. If `droppedItem` is set and `dropped > savedDropped`, return an instruction to finalize the indicator (update to `dropped`), then reset `dropped`, `savedDropped` to 0 and `droppedItem` to `Nothing`. Continue to the existing content path (`blockedByAdmin` / `prohibitedGroupContent` / moderation / `createContentItem`).
  - **over limit (drop):** set `accepted = kept` (pruned), `dropped + 1`. If `droppedItem` is `Nothing`, return "create indicator, count 1"; else return "no write". Return `Nothing` from `newGroupContentMessage`, so no content item is stored and (at an admin) no delivery task is produced, so it is not forwarded.
- Perform the returned database instruction after the transaction (writes cannot run in STM):
  - finalize/update: `updateGroupChatItem` on `droppedItem` to the new count.
  - create: write the indicator item (see section 8), then a second small transaction stores the new `ChatItemId` into `droppedItem` and sets `savedDropped` to the count. One member is one connection is one thread, so there is no race between create and store-id.

## 8. Indicator chat item (`src/Simplex/Chat/Messages/CIContent.hs` + apps)

New received content, mirroring `CIRcvIntegrityError` at every site:

- GADT (line 149): `CIRcvGroupRateLimited :: Int -> CIContent 'MDRcv`.
- `isRcv-info` predicate (line 222): add `-> True`.
- `ciContentToText` (line 293): text like `"N messages rate-limited"`.
- `JCIContent` (line 486) + `toJCIContent` (522) + `aciContentJSON` (558).
- `DBJCIContent` (line 595) + `toDBJCIContent` (631) + `fromDBJCIContent` (667).
- tag text (line 788): `"rcvGroupRateLimited"`.
- `View.hs` (690, 714, 736): a `viewRcvGroupRateLimited` line.

Apps (Kotlin `apps/multiplatform/.../model/ChatModel.kt`, Swift equivalents):
- New `CIContent` variant in the app models with its JSON tag.
- New `CIMergeCategory` value (enum at ChatModel.kt:3498, plus Swift) so consecutive burst items collapse into one group, as `RcvGroupEvent` / `RcvItemDeleted` already do.
- A small informational item view (model on the integrity-error / marked-deleted views).
- Localization strings for the item text.

The count that is displayed is the item's own count; the merge category collapses consecutive per-burst items. Whether interleaved messages from other members should split the indicator into a new row (strict "update only if last") is a UX call, still open.

## 9. Periodic flush (`cleanupManager`, `src/Simplex/Chat/Library/Commands.hs` ~4835)

Add a step that iterates `msgRates` and, for each member with `droppedItem` set and `dropped > savedDropped`, updates the indicator item to `dropped` and sets `savedDropped = dropped`. This is the middle trigger (first drop / periodic / burst end) and also writes the final count when a member floods and then goes silent. `cleanupManagerInterval` is 30 minutes (`src/Simplex/Chat.hs:125`); confirm whether a shorter dedicated interval is wanted, since the first-drop write already shows the indicator immediately and this only updates the count.

## 10. What is counted / exemptions

- Count only `XMsgNew` content (not edits, reactions, deletes, or file descriptions).
- Count only distinct messages: the increment sits after the dedup in `saveGroupRcvMsg` / `saveGroupFwdRcvMsg`, so a message received both forwarded and direct is counted once. Since forwarded messages are not checked, this is naturally the direct path only.
- Moderators and above are exempt at all three points.

## 11. Open items (UX, to confirm before or during implementation)

- Indicator placement: one row per burst updated in place, or split into a new row when other members' messages interleave (strict update-if-last).
- Sender reject error surface: reuse `throwCmdError` text, or a dedicated `ChatErrorType` constructor for the apps.
- CLI flag name and syntax for the override.
- Whether the periodic flush reuses the 30-minute cleanup interval or a shorter one.

## 12. Tests (`tests/ChatTests/Groups.hs` or a new module)

- Over-limit direct messages are dropped and an indicator with the right count appears.
- Catch-up: many messages with `brokerTs` spread over a long span are all accepted (not dropped) even when received in one batch.
- A genuine burst (many messages with `brokerTs` inside one minute) is dropped, including one received during catch-up from a past window.
- Sender is rejected once over the limit.
- Forwarded messages are not rate-limited.
- Moderators and above are exempt.
- The global override replaces the group rule; all-`Nothing` disables limiting.

## Files touched

- `src/Simplex/Chat/Types/Preferences.hs` - `GroupRateLimit`, prefs fields, defaults, conversions, JSON.
- `src/Simplex/Chat/Controller.hs` - `MemberRate`, `msgRates`, `groupRateLimitOverride`.
- `src/Simplex/Chat.hs` - init `msgRates`, `defaultChatConfig`.
- `src/Simplex/Chat/Options.hs` - CLI override flag.
- `src/Simplex/Chat/Library/Internal.hs` - `checkAccepted`, `memberRateVar`, `effectiveLimit`, `allOff`.
- `src/Simplex/Chat/Library/Commands.hs` - sender check, periodic flush step.
- `src/Simplex/Chat/Library/Subscriber.hs` - recipient/admin check and indicator write in `newGroupContentMessage`.
- `src/Simplex/Chat/Messages/CIContent.hs` - `CIRcvGroupRateLimited` at all sites.
- `src/Simplex/Chat/View.hs` - view line.
- `apps/multiplatform/.../model/ChatModel.kt` + Swift - content variant, merge category, view, strings.
- Tests.
