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

## Channels (Relays) — In-Progress Implementation

### GAP: ShareAPI sendAsGroup always false
**Source:** `SimpleX SE/ShareAPI.swift` L71
The share extension passes `sendAsGroup: false` with a TODO comment. Channel owners sharing content via the share extension cannot yet send as the channel identity.

**REC:** Implement sendAsGroup logic in the share extension when channel context is available.

### GAP: Server validation warnings not surfaced in UI
**Source:** `Shared/Views/UserSettings/NetworkAndServers/NetworkAndServers.swift` L368
`validateServers_` now returns `(errors, warnings)` tuple but the warnings are assigned to `_warns` (unused) with a TODO comment. `UserServersWarning` values (e.g., `noChatRelays`) are silently discarded.

**REC:** Display server validation warnings in the NetworkAndServers UI alongside errors.

### GAP: GroupShortLinkInfo.direct not used in group link plan
**Source:** `Shared/Views/NewChat/NewChatView.swift` L1335
`GroupLinkPlan.ok` pattern captures `_groupSLinkInfo_` (unused) with a TODO comment. The `direct` flag and relay hostnames from `GroupShortLinkInfo` are not yet consumed in the join flow UI.

**REC:** Use `GroupShortLinkInfo` to display relay information in the pre-join group link UI.
