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

### GAP: Groups joined through another link still appear as directory results
**Source:** [spec/client/chat-list.md](../spec/client/chat-list.md#11-online-search)
A directory entry is matched to a local chat by a store-only lookup of its link. An entry the app finds this way opens as a chat: the screen's chat row is drawn in place of the entry. A regular group's entry carries the directory's own group link (short link only), so a group the user joined through a different link, through the directory's full link, or by invitation is not found and still shows as a directory entry with "Join group". Tapping it goes through `planAndConnect`, which handles the known group. Accepted.

Related: the note "All results on this page are already in your chats" is not shown when a page's only results are hidden because a name search resolved to the same link (that row is on screen, but not one of the user's chats); the Directory part then shows only "Show more" when there are more pages, and is not shown otherwise.

**REC:** Have the directory send a stable group identifier the core can match locally, or let the core match a directory short link against groups joined through the corresponding full link.

### GAP: First-search alert does not mention the server hosting a resolved address
**Source:** [spec/client/chat-list.md](../spec/client/chat-list.md#11-online-search)
Resolving a SimpleX name with `PlanResolveMode.unknown` also loads the resolved address's link data from the SMP server that hosts it. The search text itself is not sent there, and the alert names only SimpleX Directory and the name-resolving server.

**REC:** Decide whether the alert copy should also say that the address a name points to is loaded.

### GAP: New chat does not narrow contacts by name while typing
**Source:** [spec/client/navigation.md](../spec/client/navigation.md#12-new-chat-online-search)
The chat list resolves a typed SimpleX name locally on each keystroke to narrow the list to the matching chat; New chat does not. A known contact whose display name does not contain the typed name is therefore not in "Your Contacts" and appears as a Names result instead (tapping it opens the existing chat).

**REC:** Reuse the chat list's local name resolution in `ContactsListSearchBar` if this proves confusing.

---

