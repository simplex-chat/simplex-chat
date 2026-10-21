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

## Online Search

### GAP: Groups joined without the listed short link still appear as results with Open
**Source:** [spec/client/chat-list.md](../spec/client/chat-list.md#11-online-search)
A directory entry is matched to a local chat by a store-only plan of its link (`resolve=never`), which finds a group only when it was prepared from that short link (every short-link join in current apps, Open included), joined with `/c` and that short link (the join connection stores the full link under it), or joined with the full link the directory sends. A group joined otherwise shows as a result with Open beside its own chat row:

- a group joined with a full link (an older app) when the directory sends the short link: Open's online plan finds it known and shows the "Open group" alert;
- a member added by invitation, or one who joined through a link the owner has since replaced: the online plan is "ok", so Open prepares and opens a second copy of the group without asking (before, the "Open group?" confirm led to the same copy).

Accepted for now: channels, the directory's main content, are always joined through a prepared group. "Already joining" is reachable for a short-link entry with a pending `/c` join and for entries the directory sends with a full link. Core tests pin the store-only contract, including the `/c` and invited-member cases.

**REC:** Recognising invited members needs a link in the group record that matches the listed one, which they don't have today: the owner would have to share the group's current link, a protocol change.

### GAP: The count of results left can include listings the directory never sends
**Source:** [spec/client/chat-list.md](../spec/client/chat-list.md#11-online-search)
The directory's `moreCount` counts the matching listings after the page, including listings without a public link, which it never sends. "Load more · N left" can therefore promise more results than loading brings, and a last page can come back empty.

**REC:** Count only listings with a public link in the directory's count query.

---

