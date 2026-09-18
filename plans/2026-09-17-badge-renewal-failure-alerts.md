# Plan: tell the user when badge renewal is failing

## The problem

When the monthly renewal fails, the worker retries silently — with backoff for network failures, a day later for anything else — and the user learns nothing. Nothing else marks the failure: the badge is retired only when the paid months run out (`retireExpiredBadge`), so a badge whose renewal keeps failing stays on the profile with an expired credential, and contacts see it as expired.

## What changes

**1. Core remembers the failure.** Four new columns on `badge_purchases`. Three are written only by the renewal request path (`requestBadgeIssue`), never by retirement or presentation: `issue_failed_since` (the first failed attempt of the current run, kept until an issuance is stored), `issue_error_at` and `issue_error` (the last failure: when, and what). A renewal request counts as failed when it ends without a new credential stored: the service refused, the request threw (timeout, network, undecodable or unexpected reply), or the issued credential did not verify — that last case stores the ledger row today and is still recorded as a failure. The fourth, `next_wake_at`, is the wake the worker is about to wait for, written at the end of every pass whether it failed or not.

`BadgeState` gains `issueError :: Maybe BadgeIssueError` — `{failedSince, lastAttemptAt, reason}` — and `nextWakeAt :: Maybe UTCTime`. `reason` is a sum of the failure kinds below. That is the whole UI contract; nothing else is added to the wire.

**2. Core decides when it is an alert.** A new alert kind, `BAIssueFailed`, derived from stored state like the others. It is raised when `issueError` is present and either

- the reason is terminal — a service refusal without `retryAfter`, an invalid credential, an unexpected response — at once; or
- the reason is transient — timeout, network, a refusal with `retryAfter` (rate-limited, provider unavailable, payment pending) — and the shown credential's expiry has passed: from that moment contacts see the badge as expired, so the failure is visible and worth a word. Requests start a day before expiry, so this is roughly "failing for a day". Whether the service gave `retryAfter` is the service's own view of transience and holds for codes this version does not know.

Once `paidThrough` has passed, the alert is the usual Support ended, which takes precedence; `derivedBadgeAlert` raises Support ended on `paidThrough <= now` alone, without today's `balanceMonths == 0` condition.

`episode` is `issue_failed_since`, so one alert per run of failures; acknowledging it (the existing `/_badge ack`, snooze included) silences that run and a later run alerts again. Success clears the state and the alert with it.

**3. Chat list: the banner slot, in error dress.** The existing one-slot banner (`SupportSimpleXBanner`) gets a warning variant — the hero replaced by a red `exclamationmark.triangle` / `ic_warning`, title "Badge renewal failed" in red, subtitle "Tap for details" — shown for `BAIssueFailed` under the same conditions as Support ended (same user, same slot before the pitch, also suppresses the onboarding cards). Tapping opens the badges screen as the other banners do — the badge is still shown while renewal fails, so the router lands on Your Badge. Dismiss offers "Remind me later" / "Dismiss" through the same ack calls as Support ended. Not a one-shot modal alert: the failure lasts days and a modal is gone after one tap.

**4. Your Badge: an "Error" section** below "How it works", above the developer-tools section, shown while `issueError` is present — any recorded failure, from the first one; only the banner waits for §2's criteria. Header: red `exclamationmark.triangle` + "Error", the "Connection failed" section of `GroupMemberInfoView` on both platforms. Rows: the reason as a sentence in secondary color; "Since" with `failedSince`; "Next attempt" with `nextWakeAt` when present; and a "Contact SimpleX team" action that opens the team address as "Send questions and ideas" does (iOS: dismiss, then `ChatModel.shared.appOpenUrl = simplexTeamURL`; Kotlin: `uriHandler.openVerifiedSimplexUri(simplexTeamUri)`). No "Retry" button: opening Your Badge already signals the worker (`APIGetBadgeState` → `startBadgeWork`), which retries at once.

The sentences: "The badge service refused the renewal: %@" (the service code, e.g. `unknown_purchase_key`); "The badge service did not respond."; "The badge service could not be reached."; "The badge issued by the service cannot be verified."; "Unexpected response from the badge service: %@".

**5. Developer tools** (the Credential section): "Next check" from `nextWakeAt` when present, and while `issueError` is present "Last attempt" with `lastAttemptAt` and "Last error" with the raw reason (tag plus payload, e.g. `serviceError code_used`). The CLI's `/_badge state` prints the same: `, next check YYYY-MM-DD HH:MM` on the state line, and a second line `renewal failing since YYYY-MM-DD: <reason as stored>`.

## Specifics

**Types** (`Simplex.Chat.Badges.Types`, next to `BadgeAlert`):

```haskell
data BadgeIssueFailure
  = BIFServiceError {code :: BadgeServiceErrorCode, retryable :: Bool} -- retryable = the service gave retryAfter
  | BIFTimeout
  | BIFNetwork
  | BIFInvalidCredential
  | BIFUnexpected {message :: Text} -- undecodable or unexpected reply, or any other throw

data BadgeIssueError = BadgeIssueError {failedSince :: UTCTime, lastAttemptAt :: UTCTime, reason :: BadgeIssueFailure}
```

`BadgeIssueFailure` is stored as text the way `CIStatus` is: a `StrEncoding` instance with a tag and space-separated payload — `service_error <code> retry|final`, `timeout`, `network`, `invalid_credential`, `unexpected <message to end of input>` — and `ToField`/`FromField` through it. To the UI it goes as `sumTypeJSON $ dropPrefix "BIF"`, so both apps decode it without hand-written decoders; `BadgeIssueError` and the new `BadgeState` fields use `defaultJSON`. `BadgeAlertKind` gains `BAIssueFailed`, text `issue_failed`, JSON `issueFailed`.

Classification of a thrown request error, next to `badgeErrorRetry` and by its rule: `AGENT (A_SERVICE ASETimeout)` → `BIFTimeout`; `temporaryOrHostError` → `BIFNetwork`; anything else → `BIFUnexpected` with the error's text. Transient: `BIFTimeout`, `BIFNetwork`, `BIFServiceError` with `retryable`. Terminal: the rest.

**Schema.** `M20260918_badge_issue_errors`, SQLite and Postgres, registered in both `Migrations.hs` and the cabal file, with a down migration:

```sql
ALTER TABLE badge_purchases ADD COLUMN issue_failed_since TEXT; -- TIMESTAMPTZ in Postgres, as alert_snooze_until
ALTER TABLE badge_purchases ADD COLUMN issue_error_at TEXT;
ALTER TABLE badge_purchases ADD COLUMN issue_error TEXT;
ALTER TABLE badge_purchases ADD COLUMN next_wake_at TEXT;
```

**Store** (`Store/Badges.hs`): `UserBadgePurchase` gains `issueError :: Maybe BadgeIssueError` and `nextWakeAt :: Maybe UTCTime`, read by `getBadgePurchase` (`issueError` is present when all three columns are). `setBadgeIssueError db purchaseId now failure`: `SET issue_failed_since = COALESCE(issue_failed_since, ?), issue_error_at = ?, issue_error = ?`. `storeBadgeIssuance` also clears the three columns, so an issuance and the clearing are one transaction. `setBadgeNextWake db purchaseId at_`.

**Worker** (`Library/Commands.hs`):

- `requestBadgeIssue` records every failing outcome through `setBadgeIssueError` before it returns or rethrows: the `BSPError` branch (`BIFServiceError code (isJust retryAfter)`), the unexpected-response branch (`BIFUnexpected`), a `Nothing` from `verifyIssuedCredential` (`BIFInvalidCredential`, alongside the statement it still applies), `applyBadgeStatement` answering `False` (`BIFUnexpected` with the internal error's text), and any throw from the request itself, caught with `catchAllErrors`, recorded by the classification above and rethrown so `retryBadgeError` still decides the retry. A successful issuance clears through `storeBadgeIssuance`.
- `updateUserBadge` writes the wake it returns with `setBadgeNextWake` before emitting, and emits `CEvtBadgeChanged` when `retired || issued || failed`, where `failed` is a `Left` from `requestBadgeIssue` (the invalid-credential case is covered by `issued`). `retryBadgeError` gets the user id and the delay, writes `next_wake_at` (now plus the delay `withRetryInterval` hands the callback, or plus `badgeStalledInterval`) and emits `CEvtBadgeChanged` before it loops or stalls. So the state the apps receive after a failure already carries the next attempt.
- `derivedBadgeAlert now purchase shownCred balance`: Support ended when `paidThrough <= now`; else `BAIssueFailed {episode = strEncode failedSince, date = failedSince}` by §2, with the transient threshold read off `shownBadgeCredential`; else nothing. `unansweredBadgeAlert` and `getUserBadgeState` pass the purchase and the shown credential through; ack, snooze and the emitted-occurrence key work unchanged.
- `getUserBadgeState` fills `issueError` and `nextWakeAt` from the purchase.

**Apps.** `BadgeState` gains the two optional fields; `BadgeIssueError` and `BadgeIssueFailure` are mirrored as ordinary decodable types (iOS: `BadgeState` becomes `Decodable, Hashable`, nothing encodes it; Kotlin: sealed class with `@SerialName` tags `serviceError`, `timeout`, `network`, `invalidCredential`, `unexpected`). `BadgeAlertKind` gains `issueFailed`, and `badgeAlertKindParam` maps it to `issue_failed`. `SupportSimpleXBanner` takes a `warning` flag for §3; the chat list computes `badgeIssueFailed` next to `supportEnded` and shows the banner under the same conditions, with the dismiss alert titled "Badge renewal failed". Your Badge gets §4 and §5. New Kotlin strings: about thirteen keys.

## Tests

`BadgeServiceTests`, with the test clock: a refusal (delete the purchase from the service's table, so it answers `unknown_purchase_key` without `retryAfter`) alerts on the next pass and `/_badge state` shows the error; a stopped service alerts only once the clock passes the credential's expiry, not before (the request timeout may need to be configurable for the test to run in seconds); a restart in between keeps `issue_failed_since`; the service back and a successful renewal clears state and alert; ack silences the run, and a new run after a success alerts again.

## Decisions taken without asking

- Persist in the DB, not in memory: the run's first-failure time must survive a restart, or every restart re-alerts.
- A new migration, not an edit of `M20260915`: 7.1 beta devices have applied it.
- Terminal vs transient is decided from the failure's kind, not from a count of attempts: counts depend on backoff timing, kinds do not.
- The transient threshold is the credential's expiry rather than a fixed 24 h, because that is when the failure becomes visible to contacts; the two nearly coincide.
- The banner, not a modal, for the reason in §3; a modal can be added later if the banner is missed.
- `reason` is typed (not a text) so the sentences in §4 are localizable, and the service code inside it is the `BadgeServiceErrorCode` the apps already decode.
- "Contact SimpleX team" reuses the existing address flow rather than pre-filling a message.

## Not in this plan

The service-side `//purchase <key>` lookup; a user-facing ledger; alerts for the other kinds (`BARenewalApproaching`, `BAPaymentIssue`) that need payments.
