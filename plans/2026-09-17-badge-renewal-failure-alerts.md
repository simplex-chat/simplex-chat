# Plan: tell the user when badge renewal is failing

## The problem

When the monthly renewal fails, the worker retries silently — with backoff for network failures, a day later for anything else — and the user learns nothing. Nothing else marks the failure: the badge is retired only when the paid months run out (`retireExpiredBadge`), so a badge whose renewal keeps failing stays on the profile with an expired credential, and contacts see it as expired. The Support-ended alert never fires, because that needs a zero balance.

## What changes

**1. Core remembers the failure.** Three new columns on `badge_purchases`, written only by the renewal request path (not by retirement or presentation): `issue_failed_since` (the first failed attempt of the current run, kept until a credential is stored), `issue_error_at` and `issue_error` (the last failure: when, and what, as a small typed value stored as JSON). A renewal request counts as failed when it ends without a new credential stored: the service refused, the request threw (timeout, network, unexpected reply), or the issued credential did not verify — that last case stores the ledger row today and must still be recorded as a failure. Passes that request nothing write nothing here. And one more column written at the end of every pass, failing or not: `next_wake_at`, the wake the worker just scheduled — on a retried failure, now plus the retry delay.

`BadgeState` gains `issueError :: Maybe BadgeIssueError` — `{failedSince, lastAttemptAt, reason}` — and `nextWakeAt :: Maybe UTCTime`. `reason` is a sum: service refusal (with the service's code and whether it gave `retryAfter`), timeout, network, credential invalid, unexpected response. That is the whole UI contract; nothing else is added to the wire.

**2. Core decides when it is an alert.** A new alert kind, `BAIssueFailed`, derived from stored state like the others (`derivedBadgeAlert` gets the purchase and the shown credential as inputs). It is raised when `issueError` is present and either

- the reason is terminal — a service refusal without `retryAfter`, an invalid credential, an unexpected response — at once; or
- the reason is transient — timeout, network, a refusal with `retryAfter` (rate-limited, provider unavailable, payment pending) — and the shown credential's expiry has passed: from that moment contacts see the badge as expired, so the failure is visible and worth a word. Requests start a day before expiry, so this is roughly "failing for a day". Whether the service gave `retryAfter` is the service's own view of transience and holds for codes this version does not know.

`episode` is `issue_failed_since`, so one alert per run of failures; acknowledging it (the existing `/_badge ack`, snooze included) silences that run and a later run alerts again. Success clears the state and the alert with it. It cannot coincide with Support-ended, which needs a zero ledger balance, and only issuances — the thing that is failing — bring the balance down.

**3. Chat list: the banner slot, in error dress.** The existing one-slot banner (`SupportSimpleXBanner`) gets a warning variant — `exclamationmark.triangle` in red, title "Badge renewal failed", subtitle "Tap for details" — shown for `BAIssueFailed`, tapping opens Your Badge (the badge is still shown while renewal fails, so the router lands there), dismiss offers "Remind me later"/"Dismiss" as Support-ended does. It takes the one slot before the pitch; it cannot coincide with Support-ended. Not a one-shot modal alert: the failure lasts days and a modal is gone after one tap.

**4. Your Badge: an "Error" section** below "How it works", above the developer-tools section, shown while `issueError` is present — any recorded failure, from the first one; only the banner waits for §2's criteria. Header: red `exclamationmark.triangle` + "Error" (the "Connection failed" section in `GroupMemberInfoView` is the pattern on both platforms). Rows: the reason as a sentence ("The badge service refused the renewal: `code`", "The badge service did not respond", "Cannot verify the badge the service issued", …); "Since" with `failedSince`; "Next attempt" with `nextWakeAt`; and a "Contact SimpleX team" action that opens the team address as "Send questions and ideas" does (`appOpenUrl = simplexTeamURL` / `openVerifiedSimplexUri(simplexTeamUri)`). No "Retry" button: opening Your Badge already signals the worker (`APIGetBadgeState` → `startBadgeWork`), which retries at once.

**5. Developer tools** (the Credential section): "Next check" from `nextWakeAt` always, and "Last error" as the raw reason when `issueError` is present. The CLI's `/_badge state` prints both.

## Decisions taken without asking

- Persist in the DB, not in memory: the run's first-failure time must survive a restart, or every restart re-alerts.
- A new migration, not an edit of `M20260915`: 7.1 beta devices have applied it.
- Terminal vs transient is decided from the failure's kind, not from a count of attempts: counts depend on backoff timing, kinds do not.
- The transient threshold is the credential's expiry rather than a fixed 24 h, because that is when the failure becomes visible to contacts; the two nearly coincide.
- The banner, not a modal, for the reason in §3; a modal can be added later if the banner is missed.
- `reason` is typed (not a text) so the sentences in §4 are localizable, and the service code inside it is the `BadgeServiceErrorCode` the apps already decode.
- "Contact SimpleX team" reuses the existing address flow rather than pre-filling a message.
- Edge left as is: if the paid period ends while renewals are still failing, the badge is retired (today's rule) with the failure state kept, so the banner stays and its tap lands on the Support screen, which has no Error section. The alert still names the failure; showing the error there too is not in this plan.

## Not in this plan

The service-side `//purchase <key>` lookup; a user-facing ledger; alerts for the other kinds (`BARenewalApproaching`, `BAPaymentIssue`) that need payments.

## Tests

`BadgeServiceTests`, with the test clock: a refusal (delete the purchase from the service's table, so it answers `unknown_purchase_key` without `retryAfter`) alerts on the next pass and `/_badge state` shows the error; a stopped service alerts only once the clock passes the credential's expiry, not before (the request timeout may need to be configurable for the test to run in seconds); a restart in between keeps `issue_failed_since`; the service back and a successful renewal clears state and alert; ack silences the run, and a new run after a success alerts again.
