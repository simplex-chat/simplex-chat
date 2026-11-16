# SimpleX Chat iOS -- Known Gaps & Recommendations

> Aggregation of `[GAP]` and `[REC]` annotations discovered during specification analysis. Organized by product area.
>
> **Related spec:** [spec/README.md](../spec/README.md)

---

## UI: Error Feedback

### GAP: No user-visible error on FFI command failure
**Source:** [spec/architecture.md](../spec/architecture.md)
API calls via `chatApiSendCmd` return `APIResult<R>` which can be `.error(ChatError)`. Not all error cases surface user-visible feedback in the UI.

**REC:** Audit all `chatApiSendCmd` call sites and ensure `.error` cases show appropriate alerts or banners.

---

## UI: Loading States

### GAP: No loading indicator during initial chat list population
**Source:** [spec/client/chat-list.md](../spec/client/chat-list.md)
When `ChatModel.chatInitialized` transitions to `true`, the chat list appears fully formed. There is no intermediate loading state for users with large numbers of chats.

**REC:** Add a progress indicator during `apiGetChats` for users with 100+ conversations.

---

## Flows: Group Lifecycle

### GAP: Bulk member role change — API supports batch but UI uses single-member calls
**Source:** [spec/api.md](../spec/api.md)
`APIMembersRole` accepts `NonEmpty GroupMemberId`, supporting batch role changes at the API level. However, the iOS UI (`GroupMemberInfoView.swift`) currently invokes it with a single member at a time.

**REC:** Expose batch role change in the UI for group admins managing large groups.

---

## Security

### GAP: Database passphrase not enforced by default
**Source:** [spec/database.md](../spec/database.md)
Database encryption is optional and requires the user to manually set a passphrase. New installations start with an unencrypted database.

**REC:** Consider prompting users to set a database passphrase during onboarding, especially on devices without hardware encryption.

### GAP: No forward secrecy indicator in UI
**Source:** [product/glossary.md](glossary.md)
While the double-ratchet protocol provides forward secrecy, there is no UI indicator showing whether a specific conversation has achieved forward secrecy (i.e., completed initial key exchange ratcheting).

**REC:** Add a security indicator in contact/group info showing ratchet state.

---

## Documentation

### GAP: Haskell Store layer not fully specified
**Source:** [spec/database.md](../spec/database.md)
The Haskell Store modules (`Store/Direct.hs`, `Store/Groups.hs`, `Store/Messages.hs`, etc.) are referenced by function name but not fully specified with parameter types and return types.

**REC:** Expand database spec with key Store function signatures as the specification matures.

---

## Badges: Store Purchases

### GAP: A badge moves to another device or profile only once in a while, and is bought again only to continue the last purchase
**Source:** [Shared/Views/Badges/BadgeStore.swift](../Shared/Views/Badges/BadgeStore.swift)
`restorePurchases()` hands over `Transaction.currentEntitlements` and `Transaction.unfinished` as restored, and the service moves the subscription to the restoring profile's purchase key. Restore is refused while the restoring profile shows a badge. A payment moves again only once the credential issued to the key it last moved from has expired; until then the move is refused `transfer_limited`. The first Restore's own answer doesn't say so: the app hears of the refusal only from core's purchase-refused event, which core sends only for a record holding an invoice id, and the next Restore is answered `transfer_limited` itself. A badge bought with Stripe or crypto cannot be moved at all. On the same install a lapsed subscriber can subscribe again only to the badge the active profile's newest purchase retired: core resumes that purchase on the new charge, as Apple keeps the original transaction id. The app's gate (`forSale`, comparing the original ids in `Transaction.all` with the purchase's `transactionRef`; the ids are read at the first load and again before each subscription purchase, and extended by every auto-renewable transaction the sweep, its updates or Restore sees and every subscription bought) shuts subscriptions for any other profile, device or badge, and for the rest of the run after a purchase that awaits approval, which StoreKit does not list.

**REC:** Restore by a receipt code for Stripe and crypto purchases, and announce a limited move on the first Restore.

### GAP: A device whose subscription Restore moved away still offers to buy it again
**Source:** [Shared/Views/Badges/BadgeStore.swift](../Shared/Views/Badges/BadgeStore.swift)
Once Restore moves a subscription to another device or profile, the service answers the old purchase key ended, and this device retires its badge. Its ledger tip is then the `transferOut` debit, so core knows the payment moved out (`purchaseMovedOut` in `Store/Badges.hs`), but no field of `BadgeState` says so: the newest purchase still reads as a subscription not shown, of its own tier, with the same `subscription.transactionRef`. So `resumableSubscription` returns that original id, `forSale` opens monthly and annual at that tier when it is the only original id in `priorSubscriptions`, and with nothing open (`canBuy`) the order screen's `payButton` is enabled. A charge Apple makes then keeps the original transaction id, whose payment the service has bound to the key Restore moved it to, so it funds that key; here `handOverStoreReceipt` finds the purchase moved out, answers with its old credential, the app finishes the transaction, and this profile shows no badge and no message.

**REC:** After the first release, carry the move in the badge state, so `forSale` closes subscriptions for a purchase whose payment moved away.

---

