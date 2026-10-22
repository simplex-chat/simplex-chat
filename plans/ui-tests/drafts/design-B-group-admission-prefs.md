# Design B: group preferences, member admission and review (catalogue 03 §5, §6, parts of §12)

Status: design only, nothing implemented. Worktree `~/wt-uitest` (branch `nd/desktop-ui-tests`, HEAD `8927c1892`).
Paths: `C/` = `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`, `V/` = `C/views/`,
`T/` = `apps/multiplatform/common/src/uiTestShared/kotlin/chat/simplex/app/ui/`, `H/` = `tests/ChatTests/`,
Haskell core paths are under `src/Simplex/Chat/`. UI strings are from
`apps/multiplatform/common/src/commonMain/moko-resources/base/strings.xml` (the `resources/MR/base/strings.xml` path in the
brief does not exist). Every file:line below was read on 2026-10-20 in this worktree.

## 0. Summary

- 35 catalogue rows (§5: 19, §6: 16) plus 4 rows of §12 are reduced to **10 tests + 3 `@Ignore` stubs**:
  5 in `GroupPreferencesTest`, 5 in `MemberAdmissionTest`. 8 of the 10 need exactly one group join; 2 need no peer.
- Peer-side proof everywhere it exists: exact CLI preference lines (`expects`, consecutive), CLI rejections
  (`bad chat command: …`, `#team: you don't have permission to send messages`), support-scope delivery
  (`#team (support) alice> …`), and "next line" sentinels proving a pending member got nothing else.
- Two infrastructure decisions need your answer before implementation (§3): **D1** scoped compose keys for typing in
  a support chat (two compose fields exist at once on both platforms), **D2** a small order-insensitive peer wait.
- One likely app bug found by reading (§6.1): unsaved edits on «Group preferences» / «Member admission» are discarded
  whenever the group's `GroupInfo` changes in the model, e.g. when any message arrives in the group. Proposed as a
  probe test (T5) that becomes `@KnownAppIssue` if it reproduces.
- The admission=all bypass race (no `withGroupLock` in `APIAcceptMember`, `Library/Commands.hs:2922-2940`) is not
  targeted; the review tests avoid it by enabling review before the joiner connects and running `quiesce()` (setup
  only) before accepting.

## 1. Row-by-row decisions

`keep` = has its own test; `merge → Tn` = asserted inside test Tn (catalogue row moves to Tn's name); `drop` = no
test (reason). Corrected file:line in the right column; the catalogue's line numbers are from master `d72f68e6d` and
many drifted.

### §5 Group preferences (19 rows, 1 ignored)

| Row (catalogue name) | Decision | Corrected references / notes |
|---|---|---|
| `groupPrefs_disappearingOnWithTtl` | merge → T2 | `V/chat/group/GroupPreferences.kt:109-118` (toggle), `:292-303` (TTL row), `C/views/helpers/CustomTimePicker.kt:83-140`; TTL list `C/model/SimpleXAPI.kt:5624-5625` (600, 3600, 86400, 1 week, 30 d, 90 d, off). H/Profiles.hs:3559 (not 3584) |
| `groupPrefs_disappearingMessageExpires` | **drop** | Expiry is core timers (`H/Profiles.hs:3559` testEnableTimedMessagesGroup); the app's only part, removing an expired item on the `ChatItemsDeleted` event, is the same path as 02 §D `timed_send30s_bothSidesDelete`; needs `SLOW` + real time. T2 proves the TTL reaches members (`Disappearing messages: on (1 week)`) |
| `groupPrefs_disappearingOff` | merge → T2 (second save) | `GroupPreferences.kt:115` keeps the old TTL with `enable = OFF`; core prints no param when off (`Types/Preferences.hs:951-956`) |
| `groupPrefs_directMessagesOff_memberInfoProhibited` | merge → T3 | member-info branches `V/chat/group/GroupMemberInfoView.kt:497-544` (515 on, 531-543 prohibited), alert `:742-747`. GUI groups start with DM off from the core default (`Types/Preferences.hs:573`), not from `AddGroupView.kt` (its `:164` only sets history on) |
| `groupPrefs_directMessagesForOwnersOnly` | keep = T3 | role row `GroupPreferences.kt:304-313`; core gate `Library/Commands.hs:2632`; H/Profiles.hs:3658 (not 3683) |
| `groupPrefs_fullDeleteOn` | merge → T2 | `:126-131`; the row's whole verification was the CLI line, which T2 asserts |
| `groupPrefs_reactionsOff_hidesReactionMenu` | merge → T2 | menu gate `V/chat/item/ChatItemView.kt:404` (not 157), chip `:159-160`; core `Library/Commands.hs:1014-1015` (not 1004) |
| `groupPrefs_voiceOff_showsNotAllowed` | merge → T2 (owner) | `V/chat/SendMsgView.kt:140-151,377-388`, alert `:587-596` (not 585-594); gate `C/model/ChatModel.kt:2540` `voice.on(membership)` |
| `groupPrefs_voiceForAdminsOnly` | merge → T4 | same code; alice is the member |
| `groupPrefs_filesForOwnersOnly` | merge → T2 (as «admins», role mapping) | `:145-151`; core rejection `Library/Commands.hs:4963-4968`; H/Profiles.hs:3727 (not 3752). The attach-button UI is owned by 02 §F (legend) |
| `groupPrefs_simplexLinksForOwnersOnly` | merge → T2 (owner side, CLI rejection, owner allowed) and T4 (member-side banner) | `:152-158`; banner `V/chat/ComposeView.kt:1751,1754,1757`; core `Library/Commands.hs:839`; H/Profiles.hs:3762 (not 3787) |
| `groupPrefs_reportsToggleDisabled` | merge → T1 (one assertion) | `:159-164` `disabled = true` at `:161` |
| `groupPrefs_historyOff_newMemberGetsNoHistory` | merge → T2 (save mapping only); join-based part **dropped** | Sending history is core (`H/Groups.hs:6344`, not 6040); the app part is the toggle mapping, proved by `Recent history: off`. History **on** delivery is asserted in T6 (`[>>]` after acceptance). A second joiner would add a join and its flake (plan §15.4.1) for core-only coverage |
| `groupPrefs_supportToggleDisabledInGroups` | merge → T1 (one assertion) | `:217` `SupportPreference(disabled = true)` |
| `groupPrefs_signMessages` | keep as `@Ignore` stub | `:219-221` commented out, inside the channel branch (`:218-240`), not the group branch |
| `groupPrefs_resetRestoresSaved` | merge → T1 | `:78-80`, buttons `:241-251,341-350`. `isEnabled` is always true on `SectionItemView` keys (guide §2), so "Reset/Save disabled" is checked by effect: Back closes with no alert |
| `groupPrefs_unsavedChangesAlert` | keep = T1 | `:64-68`, alert `:352-359` |
| `groupPrefs_readOnlyForMember` | keep = T4 | `:282,314-327`; owner-only buttons `:241` |
| `groupPrefs_receivedUpdateShownAsEvent` | merge → T4 | item text `C/model/ChatModel.kt:4311-4312,4361-4371` gives `Voice messages: on (admins)`; feature items merge with adjacent feature items (`ChatModel.kt:3485-3493`, merged view shows icons only, `V/chat/item/CIChatFeatureView.kt:28-42`), so T4 puts a message before the change |

### §6 Member admission and review (16 rows, 2 ignored)

| Row | Decision | Corrected references / notes |
|---|---|---|
| `memberAdmission_enableReviewAll` | merge → T6 | The effect (the next link joiner is pending) is stronger than the existing-member line; enabling uses the screen's Save. `V/chat/group/MemberAdmission.kt:37-49,83-90,117-125`. CLI `/set admission review #team all` is at H/Groups.hs:3728 (not 3684) |
| `memberAdmission_readOnlyForNonOwner` | merge → T4 | `MemberAdmission.kt:91-98,126-131` |
| `memberAdmission_unsavedChangesAlert` | keep = T7 | `MemberAdmission.kt:50-55,148-156` |
| `review_pendingMemberListedInChatsWithMembers` | merge → T6 | status text `V/chat/group/MemberSupportView.kt:174-175` («pending review»). The row flag icons (`MemberSupportView.kt:254-260`, `V/chatlist/ChatPreviewView.kt:393-396`, not 390-391) have `contentDescription = null`: not observable, not asserted, and not to be keyed (guide §4) |
| `review_hostChatsWithPendingMember` | merge → T6 | open `MemberSupportView.kt:134-146`; compose actions `V/chat/ComposeView.kt:1730-1744` (not 1715-1729). "Reaches only him" cannot be shown with one member; T6 proves scope delivery and that a main-group message did not reach the pending member |
| `review_acceptAsMember` | keep = T6 | `V/chat/ComposeContextPendingMemberActionsView.kt:56-68,84-113,115-126` |
| `review_acceptAsObserver` | keep = T8 | `:98-103` |
| `review_rejectPendingMember` | keep = T9 | `:42-54,73-82` |
| `review_acceptFromLongPressMenu` | merge → T8 | `MemberSupportView.kt:147,269-276`; long press = right click via `SectionItemViewLongClickable` `.onRightClick` (`V/helpers/Section.kt:262`) |
| `review_moderatorAcceptsMember` | **drop**, its distinct part → T4 | Accepting as moderator runs the same app code as T6/T8; the only moderator-specific UI is the «Chats with members» row visibility (`V/chat/group/GroupChatInfoView.kt:693-696`), asserted in T4 when bob promotes alice. The full row needs a second CLI peer, alice's join and the pending joiner's introduction to alice (two extra network handshakes, plan §15.4.1). Core path: H/Groups.hs:3819-3820 |
| `joinerPendingReview_supportChatAutoOpens` | merge → T10 | `V/chat/ChatView.kt:248-258` (not 246-256) |
| `joinerPendingReview_messagesAdmins` | merge → T10 | — |
| `joinerPendingReview_acceptedByAdmin` | keep = T10 | `C/model/ChatModel.kt:1726-1729` («reviewed by admins»); auto-close of the support chat on acceptance `C/model/SimpleXAPI.kt:3300-3320` (not in the catalogue; T10 asserts it). CLI `/accept member` parser `Library/Commands.hs:6519` (not 6274) |
| `joinerPendingReview_rejected` | **drop** | After `/rm`, the app shows the generic removed state (`ChatModel.kt:1761`, «removed from group») through the same path as any removed member; core-tested (H/Groups.hs:4011-4034, not 3985). Costs a join for no app code that T10 or other removal tests do not reach. Re-add if a pending-specific removal bug shows up (the Android support-chat dispose rule `V/chat/group/MemberSupportChatView.kt:33-45` would then belong in an Android-only test) |
| `admission_hostPendingApproval` | keep as `@Ignore` stub | H/Groups.hs:3658 (not 3614) |
| `admission_joinRejectedByHook` | keep as `@Ignore` stub | H/Groups.hs:3629 (not 3585) |

### §12 rows used as the review flow's continuation

| Row | Decision |
|---|---|
| `chatsWithMembers_emptyState` | merge → T6 (opposite state before the joiner) and T9 (state after rejection) |
| `chatsWithMembers_listSearchAndOpen` | open part merged → T6; search part stays in §12 |
| `chatsWithMembers_removedMemberChatGone` | merge → T9 (`MemberSupportView.kt:89`) |
| `chatsWithMembers_disabledState` | not touched (needs `/set support off`, §12's own scenario) |

## 2. Test list

| # | Test (class) | Rows covered | Peers | Joins |
|---|---|---|---|---|
| T1 | `groupPrefs_unsavedChangesAlert` (GroupPreferencesTest) | unsavedChangesAlert, resetRestoresSaved, reportsToggleDisabled, supportToggleDisabledInGroups | — | 0 |
| T2 | `groupPrefs_ownerChangesNotifyAndRestrictMembers` (GroupPreferencesTest), new name | disappearingOnWithTtl, disappearingOff, fullDeleteOn, reactionsOff_hidesReactionMenu, voiceOff_showsNotAllowed, filesForOwnersOnly, simplexLinksForOwnersOnly (owner side), historyOff (mapping) | bob | 1 (invitation) |
| T3 | `groupPrefs_directMessagesForOwnersOnly` (GroupPreferencesTest) | directMessagesForOwnersOnly, directMessagesOff_memberInfoProhibited | bob | 1 (link) |
| T4 | `groupPrefs_readOnlyForMember` (GroupPreferencesTest) | readOnlyForMember, receivedUpdateShownAsEvent, voiceForAdminsOnly, simplexLinksForOwnersOnly (member side), memberAdmission_readOnlyForNonOwner, moderator visibility of «Chats with members» | bob (host) | 1 (invitation) |
| T5 | `groupPrefs_unsavedEditsKeptWhenMessageArrives` (GroupPreferencesTest), new row | probe for finding §6.1 | bob | 1 (invitation) |
| T6 | `review_acceptAsMember` (MemberAdmissionTest) | review_acceptAsMember, memberAdmission_enableReviewAll, review_pendingMemberListedInChatsWithMembers, review_hostChatsWithPendingMember, chatsWithMembers_emptyState, chatsWithMembers_listSearchAndOpen (open) | bob (joiner) | 1 (link, pending) |
| T7 | `memberAdmission_unsavedChangesAlert` (MemberAdmissionTest) | memberAdmission_unsavedChangesAlert | — | 0 |
| T8 | `review_acceptAsObserver` (MemberAdmissionTest) | review_acceptAsObserver, review_acceptFromLongPressMenu | bob (joiner) | 1 (link, pending) |
| T9 | `review_rejectPendingMember` (MemberAdmissionTest) | review_rejectPendingMember, chatsWithMembers_removedMemberChatGone, chatsWithMembers_emptyState (after) | bob (joiner) | 1 (link, pending) |
| T10 | `joinerPendingReview_acceptedByAdmin` (MemberAdmissionTest) | joinerPendingReview_acceptedByAdmin, supportChatAutoOpens, messagesAdmins | bob (host) | 1 (alice via link, pending) |
| — | `groupPrefs_signMessages`, `admission_hostPendingApproval`, `admission_joinRejectedByHook` | `@Ignore` stubs with the catalogue reasons | — | 0 |

Files: `T/groups/GroupPreferencesTest.kt`, `T/groups/MemberAdmissionTest.kt` (class map, legend). New screen helpers
go in a new `T/harness/GroupPrefsScreens.kt` (pattern of `ChatList*Screens.kt`), two small additions to
`T/harness/Screens.kt` (`GroupInfoScreen`, `MemberInfoScreen`), one helper in `T/harness/PeerLines.kt` (D2).

Joins are minimised by: no test needs more than one join; tests that only check screen logic (T1, T7) use
`emptyGroup` with no peer; alice joins as a member by invitation (no review applies to invitations,
`src/Simplex/Chat/Types.hs:1459-1464` is reached only from the link-join path `Library/Internal.hs:1059`).

## 3. Decisions needed before implementation

**D1. Typing in a support chat (T6, T9, T10).** A support chat is a second `ChatView` in a modal
(`V/chat/group/MemberSupportChatView.kt:159-173`) while the group's `ChatView` stays composed: desktop shows it in the
center pane next to the end pane (`C/App.kt:390-427,443-452`), Android draws the modal over it. Both compose fields
carry `Ui.Chat.Message` (`C/platform/PlatformTextField.desktop.kt:121`, `…android.kt:101`) and both send buttons
`Ui.Chat.Send` (`V/chat/SendMsgView.kt:467`), so `typeInto`/`pressEnter` fail with two matching nodes, and the Android
harness's Espresso `isAssignableFrom(EditText)` (`android/…/harness/UiTest.kt:249-257,295-303`) is ambiguous.
- Option A (recommended; §12 needs the same for every support-chat send): key the compose field by context.
  - `Ui.kt`: `Message = UiKey("chat.compose")`, `Send = Message.part("send")`, `ScopeMessage = UiKey("chat.compose", "scope")`, `ScopeSend = ScopeMessage.part("send")` (tags `chat.compose.send`, `chat.compose[scope]`, `chat.compose.send[scope]`; tests use the constants, no literal `chat.send` exists).
  - `V/chat/ComposeView.kt:1494`: pass `uiKey = if (chatsCtx.secondaryContextFilter == null) Ui.Chat.Message else Ui.Chat.ScopeMessage` to `SendMsgView`.
  - `V/chat/SendMsgView.kt:37` new `uiKey: UiKey` parameter (before the lambdas); `:81` passes it to `PlatformTextField`; `SendMsgButton` (`:454`, calls at `:138,248,258`) gets `uiKey.part("send")` in place of the constant at `:467`.
  - `C/platform/PlatformTextField.kt:12` expect + both actuals: `uiKey` parameter before `onDone`; Android also sets `editText.tag = uiKey.tag` when `uiTestHooksEnabled`.
  - Android harness: `isMessageField(key) = key.id == Ui.Chat.Message.id`; Espresso matchers add `withTagValue(equalTo(key.tag))`; `pressEnter(key)` clicks `key.part("send")`.
  - 6 app files, keys only, no-ops in release.
- Option B (no app change): drop the GUI→peer support sends; T6/T9/T10 keep only peer→GUI support messages
  (bob's `#team (support) …` shown to alice). Loses the strongest checks of `review_hostChatsWithPendingMember` and
  `joinerPendingReview_messagesAdmins`.

**D2. `PeerLines.eventuallyAll(timeoutMs, vararg lines)`** (T6, T8, T9): the joiner's three pending-join lines come
in no guaranteed order (`<###` in H/Groups.hs:3750-3756). `eventually` on one of them can drop another into
"skipped" and break the next-line sentinel. Sketch (in `T/harness/PeerLines.kt`):
```kotlin
fun eventuallyAll(timeoutMs: Long, vararg want: String) {
  await(timeoutMs, "$name: all of ${want.toList()}") { synchronized(lines) { want.all { w -> lines.any { it == w || stripTime(it) == w } } } }
  synchronized(lines) { dropThrough(want.maxOf { w -> lines.indexOfFirst { it == w || stripTime(it) == w } }) }
}
```
Lines before the last wanted one go to `skipped`, so error lines among them still fail the test.

## 4. Keys to add (all on elements a test clicks, or the support-chat field it types into)

| Key (`C/ui/Ui.kt`) | App element (file:line) | Change |
|---|---|---|
| `Ui.GroupInfo.Preferences` | `V/chat/group/GroupChatInfoView.kt:1033-1039` `GroupPreferencesButton` → `SettingsActionItem` | pass existing `uiKey` |
| `Ui.GroupInfo.MemberSupport` | `GroupChatInfoView.kt:1016-1030` `MemberSupportButton` → `SettingsActionItemWithContent` | add `uiKey: UiKey? = null` before `content` in `V/usersettings/SettingsView.kt:394`, forward to `SectionItemView` |
| `Ui.GroupPrefs.MemberAdmission` | `V/chat/group/GroupPreferences.kt:257-263` | `SettingsActionItem(…, uiKey)` |
| `Ui.GroupPrefs.Toggle(feature)` = `UiKey("groupPrefs.toggle", feature.name)` | call `GroupPreferences.kt:283`; `PreferenceToggleWithIcon` `V/usersettings/SettingsView.kt:456-473` | new `uiKey` before `onChange`, on `DefaultSwitch(modifier = Modifier.ui(uiKey))` (the switch has real Disabled semantics, so `isEnabled` works) |
| `Ui.GroupPrefs.Role(feature)` + `.option(role ?: "null")` | `GroupPreferences.kt:305` `ExposedDropDownSettingRow` | existing `uiKey`; options keyed by `C/views/helpers/ExposedDropDownSettingRow.kt:77` |
| `Ui.GroupPrefs.Ttl` + `.option("DropdownValue(value=604800)")` | `GroupPreferences.kt:294`; `C/views/helpers/CustomTimePicker.kt:83-91,114` | new `uiKey` before `onSelected`, forwarded. The option arg is the private data class `DropdownSelection.DropdownValue`'s `toString`; the helper owns that string (fragile but stable for a data class) |
| `Ui.GroupPrefs.Reset`, `Ui.GroupPrefs.Save` | `GroupPreferences.kt:343,346` | `SectionItemView(…, uiKey)` |
| `Ui.MemberAdmission.Review` + `.option(MemberCriteria.All)` / `.option("null")` | `V/chat/group/MemberAdmission.kt:118-125` | existing `uiKey` |
| `Ui.MemberAdmission.Save` | `MemberAdmission.kt:142` | `SectionItemView(…, uiKey)` |
| `Ui.MemberSupport.Row(groupMemberId)` | `V/chat/group/MemberSupportView.kt:134-150` `SectionItemViewLongClickable` | existing `uiKey` |
| `Ui.MemberSupport.RowMenu(groupMemberId, "accept")` | `MemberSupportView.kt:273` `ItemAction` | existing `uiKey` |
| `Ui.Chat.PendingReject`, `Ui.Chat.PendingAccept` | `V/chat/ComposeContextPendingMemberActionsView.kt:42-49,56-63` | `.ui(…)` first in the `Column` modifiers (as `Ui.Chat.RequestAccept`, `ComposeContextContactRequestActionsView.kt:68,88`) |
| `Ui.Alert.Option(Shared, "acceptAsMember")`, `(Shared, "acceptAsObserver")` | `ComposeContextPendingMemberActionsView.kt:91,98` (buttons-column alert has no keys, `V/helpers/AlertManager.kt:75-105`) | `SectionItemView(…, uiKey = …)` (pattern `GroupMemberInfoView.kt:318`) |
| `Ui.MemberInfo.Message` | `V/chat/ChatInfoView.kt:1065` `InfoViewActionButton`; `GroupMemberInfoView.kt:882-895` `OpenChatButton`; uses at `GroupMemberInfoView.kt:512,517,519,533` | `uiKey` before `onClick` on `InfoViewActionButton` (on its `IconButton`), forwarded by `OpenChatButton`; only one of the four exists at a time |
| `Ui.Chat.ScopeMessage`, `Ui.Chat.ScopeSend` | see D1 | — |

Reused without change: `Ui.Alert.Confirm/Dismiss(Shared)` (dialog `AlertManager.kt:160,164`, stacked `:201,205`,
message `:251`), `Ui.GroupInfo.CreateLink`, `Ui.GroupLink.Share`, `Ui.GroupInfo.Member`, `Ui.Chat.Item`,
`Ui.Chat.ItemMenu(id, "select"|"reply")`, `Ui.Chat.ItemReaction`, `Ui.Chat.ReactionChip`, `Ui.Chat.Message/Send`,
`described("Record voice message")` (the mic `IconButton` merges its icon's description,
`V/chat/SendMsgView.kt:377-388`, string `icon_descr_record_voice_message`), `described("Back")`.

Not keyed on purpose: flag icons, `InfoRow` values (read through unique footer texts instead), «Cancel» in the
accept alert (never clicked).

## 5. New helpers (`T/harness/GroupPrefsScreens.kt` unless noted)

```kotlin
// Screens.kt, GroupInfoScreen
fun preferences(): GroupPreferencesScreen { s.click(Ui.GroupInfo.Preferences); return GroupPreferencesScreen(s) }
fun chatsWithMembers(): MemberSupportScreen { s.click(Ui.GroupInfo.MemberSupport); return MemberSupportScreen(s) }
fun close() { s.escape(); s.poll(what = "group info to close") { !ModalManager.end.hasModalsOpen() } }   // callers close sub-screens first, one Back each
// Screens.kt, MemberInfoScreen
fun message() = s.click(Ui.MemberInfo.Message)

class GroupPreferencesScreen(private val s: UiScope) {
  init { s.poll(what = "the `Group preferences` screen") { s.seesText("Group preferences") && s.exists(Ui.GroupPrefs.MemberAdmission) } }
  fun toggle(f: GroupFeature) = s.click(Ui.GroupPrefs.Toggle(f))
  fun role(f: GroupFeature, r: GroupMemberRole?) { s.click(Ui.GroupPrefs.Role(f)); s.click(Ui.GroupPrefs.Role(f).option(r ?: "null")) }
  fun ttl(seconds: Int) { s.click(Ui.GroupPrefs.Ttl); s.click(Ui.GroupPrefs.Ttl.option("DropdownValue(value=$seconds)")) }
  fun reset() = s.click(Ui.GroupPrefs.Reset)
  fun save() = s.click(Ui.GroupPrefs.Save)
  fun admission(): MemberAdmissionScreen { s.click(Ui.GroupPrefs.MemberAdmission); return MemberAdmissionScreen(s) }
  fun backExpectingAlert(title: String) { s.goBack(); s.poll(what = "the `$title` alert") { s.seesText(title) } }
  fun backClosing() { s.goBack(); s.poll(what = "group preferences to close without an alert") { !s.exists(Ui.GroupPrefs.MemberAdmission) && !s.seesText("Save preferences?") } }
}

class MemberAdmissionScreen(private val s: UiScope) {
  init { s.poll(what = "the `Member admission` screen") { s.seesText("Member admission") && s.seesText("Review members before admitting (\"knocking\").") } }
  fun review(): List<String> = s.textsOf(Ui.MemberAdmission.Review)
  fun select(c: MemberCriteria?) { s.click(Ui.MemberAdmission.Review); s.click(Ui.MemberAdmission.Review.option(c ?: "null")) }
  fun save() = s.click(Ui.MemberAdmission.Save)
}

fun UiScope.enableReviewAll(info: GroupInfoScreen, group: String) {
  val adm = info.preferences().admission()
  adm.select(MemberCriteria.All); adm.save()
  poll(what = "review=all saved for #$group") { groupInfoOf(group).groupProfile.memberAdmission?.review == MemberCriteria.All }
  goBack(); goBack()
}

class MemberSupportScreen(private val s: UiScope) {
  fun empty() = s.poll(what = "`No chats with members`") { s.seesText("No chats with members") }
  fun rowTexts(name: String): List<String> = s.textsOf(Ui.MemberSupport.Row(memberId(name)))
  fun open(name: String): SupportChatScreen { s.click(Ui.MemberSupport.Row(memberId(name))); return SupportChatScreen(s).also { it.waitOpen() } }
  fun menuAccept(name: String) { val id = memberId(name); s.openContextMenu(Ui.MemberSupport.Row(id)); s.click(Ui.MemberSupport.RowMenu(id, "accept")) }
  private fun memberId(name: String): Long // poll chatModel.groupMembers like GroupInfoScreen.memberId (Screens.kt:337-344)
}

class SupportChatScreen(private val s: UiScope) {
  fun waitOpen() = s.poll(what = "the support chat") { chatModel.secondaryChatsContext.value != null && ModalManager.end.hasModalOpen(ModalViewId.SECONDARY_CHAT) }
  fun items(): List<ChatItem> = chatModel.secondaryChatsContext.value?.chatItems?.value.orEmpty()
  fun itemId(text: String): Long // as ChatScreen.itemId (Screens.kt:222-229) over items()
  fun send(text: String) { s.typeInto(Ui.Chat.ScopeMessage, text); s.poll(what = "the support send button to be enabled") { s.isEnabled(Ui.Chat.ScopeSend) }; s.pressEnter(Ui.Chat.ScopeMessage); s.poll(what = "the support message field to clear") { s.editableText(Ui.Chat.ScopeMessage).isNullOrEmpty() }; s.hideKeyboard() }
  fun acceptVia(option: String) { s.click(Ui.Chat.PendingAccept); acceptAlert(option) }
  fun reject() = s.click(Ui.Chat.PendingReject)
}

fun UiScope.acceptAlert(option: String) {
  poll(what = "the `Accept member` alert") { seesText("Accept member") && seesText("Member will join the group, accept member?") && seesText("Accept as member") && seesText("Accept as observer") }
  click(Ui.Alert.Option(Ui.AlertSource.Shared, option))
}

fun UiScope.pendingLinkJoin(app: App, joiner: CliPeer, group: String): GroupInfoScreen {
  val info = emptyGroup(app, group).groupInfo()
  info.chatsWithMembers().empty(); goBack()
  enableReviewAll(info, group)
  val link = info.createLink()
  info.close()
  joiner sends "/c $link"
  joiner expects "connection request sent!"
  joiner.eventuallyAll(CONNECT_TIMEOUT_MS, "#$group: alice accepted you to the group, pending review", "#$group: you joined the group, connecting to group moderators for admission to group")
  quiesce()
  return info
}
```
`groupInfoOf(name)` = `(app.chatList.chat(name).chatInfo as ChatInfo.Group).groupInfo`. Alice's side of the pending
join is checked by each test on the «Chats with members» row (that screen loads the members itself,
`MemberSupportView.kt:44-46`), so the helper waits only for the joiner's lines and app quiet (setup only). The helper
sets up review through the UI, which is part of T6's scenario; T8/T9 reuse it as setup (their own steps diverge
right after, so they are not prefixes of T6).

## 6. Tests

Conventions in pseudo-code: `alice = app("alice")`, `bob = cli("bob")`; `chat` is a `ChatScreen`; every CLI line
listed is exact; "next line" = `expects`/`expectsMessage`, which proves nothing else arrived.

### T1 `groupPrefs_unsavedChangesAlert` (GroupPreferencesTest, no peer, 0 joins)

- Scenario: the owner changes a preference, resets it, leaves without saving, then saves through the alert; the
  hard-coded disabled toggles stay disabled.
- Fails if: Back closes with unsaved changes (`GroupPreferences.kt:66` → `close()` always); Reset does nothing
  (`:79` removed); «Exit without saving» saves (`:357-358` swapped); Save does not reach the core (`:49` replaced by a
  model-only update; caught after `restart()`); the reports or support toggle becomes enabled (`:161`, `:217`).
- Steps:
```kotlin
val alice = app("alice")
val info = emptyGroup(alice, "team").groupInfo()
var prefs = info.preferences()
check(isEnabled(Ui.GroupPrefs.Toggle(GroupFeature.Reactions)))                         // negative control
check(!isEnabled(Ui.GroupPrefs.Toggle(GroupFeature.Reports)) && !isEnabled(Ui.GroupPrefs.Toggle(GroupFeature.Support)))
poll { seesText("Allow message reactions.") }                                         // opposite first
prefs.toggle(Reactions); poll { seesText("Prohibit messages reactions.") && !seesText("Allow message reactions.") }
prefs.reset();           poll { seesText("Allow message reactions.") }
prefs.backClosing()                                                                    // no alert after reset
prefs = info.preferences(); prefs.toggle(Reactions)
prefs.backExpectingAlert("Save preferences?")
check("Save and notify group members" in textsOf(Ui.Alert.Confirm(Shared)) && "Exit without saving" in textsOf(Ui.Alert.Dismiss(Shared)))
click(Ui.Alert.Dismiss(Shared)); poll { !exists(Ui.GroupPrefs.MemberAdmission) }
prefs = info.preferences(); poll { seesText("Allow message reactions.") }              // not saved
prefs.toggle(Reactions); prefs.backExpectingAlert("Save preferences?"); click(Ui.Alert.Confirm(Shared))
poll { !exists(Ui.GroupPrefs.MemberAdmission) && groupInfoOf("team").fullGroupPreferences.reactions.enable == GroupFeatureEnabled.OFF }
info.close(); restart()
alice.chatList.open("team").groupInfo().preferences(); poll { seesText("Prohibit messages reactions.") }
```
- Strings: footers `allow_message_reactions` «Allow message reactions.», `prohibit_message_reactions_group`
  «Prohibit messages reactions.» (sic), `save_preferences_question`, `save_and_notify_group_members`,
  `exit_without_saving`, `group_preferences`.
- Optional extra evidence (catalogue's original check): the command log `chatModel.terminalItems` has no
  `/_group_profile` before the confirm step (legend "Log").
- Platform: `goBack()` = Back arrow (desktop) / system Back (Android); both reach `ModalView`'s `close`
  (`V/helpers/ModalView.kt:42,62`). Flakiness: none (no network).

### T2 `groupPrefs_ownerChangesNotifyAndRestrictMembers` (GroupPreferencesTest, bob, 1 join)

- Scenario: the owner restricts eight preferences in one save; the member is told exactly what changed and is
  refused; the owner's own UI hides reactions and blocks voice; the owner then reverts two of them.
- Fails if: any `applyPrefs(preferences.copy(X = …))` writes the wrong field or value
  (`GroupPreferences.kt:111-169`, e.g. `:135` `copy(reactions=…)` → `copy(fullDelete=…)`); the role dropdown's value is
  dropped (`:310` `onSelected(enableFeature.value, value)` → `null`); the TTL pick is ignored (`:107`); the role row is
  shown while off (`:304`); the reaction menu ignores the preference (`ChatItemView.kt:404`); the mic ignores it
  (`ChatModel.kt:2540` → `true`).
- Setup: `connect(bob)` (Helpers.kt:26-32), `val chat = ownGroup(alice, "team", bob)` (Screens.kt:377-391; bob prints
  `#team: alice invites you to join the group as member`, `use /j team to accept`, `#team: you joined the group`).
  `val link = bob.createOneTimeLink()` (Helpers.kt:56-61), `val file = bob.createFile("p.txt", "x")`
  (`common/src/desktopTest/…/harness/CliPeer.kt:15-16`; Android through the bridge `file` command).
- Steps and assertions:
```kotlin
chat.send("hi");            bob.expectsMessage("#team alice> hi", CONNECT_TIMEOUT_MS)
bob sends "+1 #team hi";    bob expects "added 👍"
val hi = chat.itemId("hi"); poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's 👍 on hi") { exists(Ui.Chat.ReactionChip(hi, "👍")) }
rightClick(Ui.Chat.Item(hi)); waitFor(Ui.Chat.ItemMenu(hi, "select")); check(exists(Ui.Chat.ItemReaction(hi, "👎")))   // opposite first
click(Ui.Chat.ItemMenu(hi, "select")); escape(); poll { !exists(Ui.Chat.Selection.part("title")) }
quiesce()                                                                              // see §6.1
val info = chat.groupInfo(); val p = info.preferences()
check(!exists(Ui.GroupPrefs.Ttl) && !exists(Ui.GroupPrefs.Role(DirectMessages)) && exists(Ui.GroupPrefs.Role(Voice)))
p.toggle(TimedMessages); poll { "1 day" in textsOf(Ui.GroupPrefs.Ttl) }; p.ttl(7 * 86400)
p.toggle(DirectMessages); poll { "all members" in textsOf(Ui.GroupPrefs.Role(DirectMessages)) }; p.role(DirectMessages, Owner)
p.toggle(FullDelete); p.toggle(Reactions); p.toggle(Voice); poll { !exists(Ui.GroupPrefs.Role(Voice)) }
p.role(Files, Admin); p.role(SimplexLinks, Owner); p.toggle(History)
p.save()
bob.expectsMessage("alice updated group #team: (signed)", CONNECT_TIMEOUT_MS)   // next line after `added 👍`: nothing else arrived
bob expects "updated group preferences:"
bob expects "Disappearing messages: on (1 week)"
bob expects "Direct messages: on for owners"
bob expects "Full deletion: on"
bob expects "Message reactions: off"
bob expects "Voice messages: off"
bob expects "Files and media: on for admins"
bob expects "SimpleX links: on for owners"
bob expects "Recent history: off"
p.backClosing(); info.close()
rightClick(Ui.Chat.Item(hi)); waitFor(Ui.Chat.ItemMenu(hi, "select")); check(!exists(Ui.Chat.ItemReaction(hi, "👎")) && exists(Ui.Chat.ItemMenu(hi, "reply")))
click(Ui.Chat.ItemMenu(hi, "select")); escape(); poll { !exists(Ui.Chat.Selection.part("title")) }
click(described("Record voice message"))
poll { seesText("Voice messages prohibited!") && seesText("Only group owners can enable voice messages.") }; click(Ui.Alert.Confirm(Shared))
bob sends "+^ #team hi";        bob expects "bad chat command: feature not allowed Message reactions"
bob sends "#team $link";        bob expects "bad chat command: feature not allowed SimpleX links"
bob sends "/f #team $file";     bob expects "bad chat command: feature not allowed Files and media"
chat.send(link);                bob.expectsMessage("#team alice> $link", CONNECT_TIMEOUT_MS)    // owner allowed (role gate, other branch)
val p2 = chat.groupInfo().preferences()
p2.toggle(TimedMessages); poll { !exists(Ui.GroupPrefs.Ttl) }; p2.toggle(Reactions); p2.save()
bob.expectsMessage("alice updated group #team: (signed)", CONNECT_TIMEOUT_MS); bob expects "updated group preferences:"
bob expects "Disappearing messages: off"; bob expects "Message reactions: on"
p2.backClosing(); /* close group info */
rightClick(Ui.Chat.Item(hi)); waitFor(Ui.Chat.ItemMenu(hi, "select")); check(exists(Ui.Chat.ItemReaction(hi, "👎")))   // both branches
click(Ui.Chat.ItemMenu(hi, "select")); escape()
bob sends "+^ #team hi"; bob expects "added 🚀"
```
- Peer lines: View.hs `viewGroupUpdated` `src/Simplex/Chat/View.hs:2113-2147` (header `:2122`, list `:2129-2137` in
  `regularGroupFeatures` order `Types/Preferences.hs:280-293,318-319`), text `groupPrefStateText`
  `Types/Preferences.hs:951-962`, TTL text `:969-990` («1 week»), `CECommandError` → `bad chat command: …`
  (`View.hs:2879`); reaction lines from H/Groups.hs:4380-4389; files and links from H/Profiles.hs:3747-3748,3778-3781.
  None of these match `PeerLines.ERROR_LINE` (`PeerLines.kt:105`), and all are consumed with `expects`.
- UI strings: `delete_after`, `feature_roles_all_members` «all members», `voice_messages_prohibited`,
  `only_group_owners_can_enable_voice`. `1 day` from `CustomTimeUnit.toText` (`C/model/SimpleXAPI.kt:5836`, `ttl_day`).
- Platform: desktop keeps the chat visible next to group info; Android covers it, so every chat action follows
  closing the modals (as `memberInfo_blockForAllNotifiesMembers`, `T/groups/GroupLinksTest.kt:81-84`). Never tap the
  mic while voice is allowed (desktop records through VLC, `common/src/desktopMain/…/platform/RecAndPlay.desktop.kt:31-40`;
  Android has RECORD_AUDIO granted).
- Flakiness: one invitation join; `quiesce()` before opening the screen so no group event replaces `GroupInfo`
  while editing (finding §6.1); the menu is dismissed through Select + Escape (pattern `T/chat/FilesTest.kt:79-84`).
- If you prefer smaller tests: split off the second save as its own test only at the cost of one more join (it
  needs the first save as setup, so the guide's prefix rule argues for keeping it here).

### T3 `groupPrefs_directMessagesForOwnersOnly` (GroupPreferencesTest, bob, 1 join)

- Scenario: in a group with DMs off the owner cannot message a member from member info; she enables DMs for owners
  only; the member is still refused, the owner can now open a direct chat.
- Fails if: the member-info branch ignores the preference (`GroupMemberInfoView.kt:515` → `false`: alert stays) or
  ignores the role (`.on(groupInfo.membership)` → `.on`, not observable on alice as owner, so the role is checked on
  the peer); the role is not saved (`GroupPreferences.kt:122-123` drops `role`); the «Enabled for» row shows while
  off or not when on (`:304`).
- Setup: `val info = emptyGroup(alice, "team").groupInfo(); val link = info.createLink()`; `bob sends "/c $link"`,
  `bob expects "connection request sent!"`, `bob.eventually("#team: you joined the group", CONNECT_TIMEOUT_MS)`
  (pattern `T/groups/GroupLinksTest.kt:18-28`); bob is not alice's contact, so member info shows the group branches
  (`GroupMemberInfoView.kt:498`). `quiesce()`.
- Steps:
```kotlin
info.member("bob").message()
poll { seesText("Can't message group member") && seesText("Direct messages between members are prohibited in this group.") }
click(Ui.Alert.Confirm(Shared)); goBack()
bob sends "@alice hi"; bob expects "bad chat command: direct messages not allowed"
val p = info.preferences(); check(!exists(Ui.GroupPrefs.Role(DirectMessages)))
p.toggle(DirectMessages); poll { "all members" in textsOf(Ui.GroupPrefs.Role(DirectMessages)) }
p.role(DirectMessages, Owner); p.save()
bob.expectsMessage("alice updated group #team: (signed)", CONNECT_TIMEOUT_MS); bob expects "updated group preferences:"; bob expects "Direct messages: on for owners"
p.backClosing()
bob sends "@alice hi"; bob expects "bad chat command: direct messages not allowed"        // role gate
info.member("bob").message()
poll(what = "a direct chat with bob opened instead of the alert") {
  (chatModel.chats.value.firstOrNull { it.id == chatModel.chatId.value }?.chatInfo as? ChatInfo.Direct)?.contact?.displayName == "bob" && !seesText("Can't message group member")
}
```
- Peer: `@alice` for a non-contact member goes to `SendMemberContactMessage` (`Library/Commands.hs:2597-2611`), refused
  at `:2632`. The last step stops before sending, so §11 `memberContact_createAndSendInvitation` is not duplicated
  (`createMemberContact` `GroupMemberInfoView.kt:109-124` only creates the contact and opens the chat).
- Strings: `cant_send_message_to_member_alert_title` «Can't message group member»,
  `direct_messages_are_prohibited_in_group`.
- Platform: member info is an end-pane modal on desktop, full screen on Android; after `message()` succeeds the app
  calls `closeAll()`. Flakiness: one link join.

### T4 `groupPrefs_readOnlyForMember` (GroupPreferencesTest, bob host, 1 join)

- Scenario: alice, a member of bob's group, sees preferences and admission read-only; host changes appear live and
  as a chat item; voice for admins only blocks her mic; links for owners only blocks her send with a banner until bob
  allows links for all; after bob makes her a moderator «Chats with members» appears and «Chat with admins» goes.
- Fails if: a member gets toggles or Save (`GroupPreferences.kt:282` → `if (true)`, or `:241`); admission is editable
  for members (`MemberAdmission.kt:117` / `:91`); the mic ignores the role (`ChatModel.kt:2540`); the links banner
  ignores the role (`ChatModel.kt:2582` → `p.simplexLinks.on`) or does not clear when allowed (`ComposeView.kt:1751`);
  the moderator gate breaks (`GroupChatInfoView.kt:693` / `:637-639`).
- Setup: `connect(bob); hostGroup(bob, "team", "alice"); joinGroupFromList(alice, bob, "team")` (Helpers.kt:42-48,
  69-78). `val link = bob.createOneTimeLink()`. bob's group has core defaults (voice on, links on, support on,
  `Types/Preferences.hs:568-586`).
- Steps:
```kotlin
val chat = alice.chatList.open("team")
var p = chat.groupInfo().preferences()
check(!exists(Ui.GroupPrefs.Toggle(Voice)) && !seesText("Save and notify group members") && !seesText("Reset"))
check(seesText("Members can send voice messages.") && seesText("Members can send SimpleX links.") && !seesText("admins"))
val adm = p.admission(); check(!exists(Ui.MemberAdmission.Review) && seesText("off") && !seesText("all"))
bob sends "/set admission review #team all"; bob expects "changed member admission rules"
poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice's read-only admission to show all") { seesText("all") }
goBack(); p.backClosing(); /* close group info */
bob sends "#team before voice"; chat.itemId("before voice")                            // breaks feature-item merging
bob sends "/set voice #team on admin"; bob expects "updated group preferences:"; bob expects "Voice messages: on for admins"
val ev = chat.itemId("Voice messages: on (admins)"); check(textsOf(Ui.Chat.Item(ev)).any { "Voice messages: on (admins)" in it })
click(described("Record voice message"))
poll { seesText("Voice messages prohibited!") && seesText("Only group owners can enable voice messages.") }; click(Ui.Alert.Confirm(Shared))
p = chat.groupInfo().preferences(); poll { seesText("Members can send voice messages.") && seesText("Enabled for") && seesText("admins") }
p.backClosing(); /* close group info */
bob sends "/set links #team on owner"; bob expects "updated group preferences:"; bob expects "SimpleX links: on for owners"
poll(timeoutMs = CONNECT_TIMEOUT_MS) { !groupInfoOf("team").groupFeatureEnabled(GroupFeature.SimplexLinks) }
typeInto(Ui.Chat.Message, link)
poll { seesText("SimpleX links not allowed") && !isEnabled(Ui.Chat.Send) }
bob sends "/set links #team on"; bob expects "updated group preferences:"; bob expects "SimpleX links: on"
poll(timeoutMs = CONNECT_TIMEOUT_MS) { !seesText("SimpleX links not allowed") && isEnabled(Ui.Chat.Send) }
pressEnter(Ui.Chat.Message); bob.expectsMessage("#team alice> $link", CONNECT_TIMEOUT_MS)
var gi = chat.groupInfo(); poll { seesText("Chat with admins") && !exists(Ui.GroupInfo.MemberSupport) }
gi.close()
bob sends "/mr #team alice moderator"; bob.eventuallyMatching(Regex("#team: you changed the role of alice to moderator( \\(signed\\))?"))
gi = chat.groupInfo(); poll(timeoutMs = CONNECT_TIMEOUT_MS) { exists(Ui.GroupInfo.MemberSupport) && !seesText("Chat with admins") }
gi.chatsWithMembers().empty()
```
- Peer lines: own `/set` outputs as H/Profiles.hs:3389-3391,3769-3770; admission `View.hs:2139-2141`;
  `/mr` parser `Library/Commands.hs:6520`, output `View.hs:1437` (exact `(signed)` suffix to confirm on the first run,
  then replace the regex by the exact line). `/accept`-free.
- Strings: `group_members_can_send_voice`, `group_members_can_send_simplex_links`, `feature_enabled_for`,
  `feature_roles_admins`, `simplex_links_not_allowed`, `button_support_chat` «Chat with admins», `member_support`,
  `no_support_chats`, `member_criteria_off`/`member_criteria_all`, item text `featureText` (`ChatModel.kt:4361-4371`).
  «off»/«all» are checked only while the admission screen is the top modal (only one modal is composed).
- Platform: the link banner sits above the compose field; on Android `hideKeyboard()` is not needed before reading
  it. Flakiness: one invitation join; each host change is awaited in the model or on screen with
  `CONNECT_TIMEOUT_MS` (event delivery). Optional split: links + moderator visibility into a second test (+1 join).

### T5 `groupPrefs_unsavedEditsKeptWhenMessageArrives` (GroupPreferencesTest, bob, 1 join; probe, then `@KnownAppIssue`)

- Scenario: the owner is editing preferences when a member writes in the group; her unsaved edit stays.
- Why: `GroupPreferencesView` keys both states on `gInfo` (`GroupPreferences.kt:43-44`
  `rememberSaveable(gInfo, …)`), and `gInfo` is re-derived from the model (`:37-41`). Every new item replaces the
  chat's `ChatInfo` (`ChatModel.kt:573-582` → `updateChatInfo` `:466-489`), and `GroupInfo` carries `chatTs`,
  `updatedAt`, `groupSummary`, `membersRequireAttention` (`ChatModel.kt:2505-2525`). If the event's `GroupInfo`
  differs from the model's, the edit silently resets. `MemberAdmission.kt:34-35` has the same pattern.
- Steps: `ownGroup(alice, "team", bob)`; `quiesce()`; open preferences; `toggle(Reactions)`;
  `poll { seesText("Prohibit messages reactions.") }`; `bob sends "#team hello"`;
  `poll(CONNECT) { chatModel.chatsContext.chatItems.value.any { it.content.text == "hello" } }`; `settle()`;
  `check(seesText("Prohibit messages reactions."))`; `save()`; bob expects the three lines ending
  `Message reactions: off`.
- Fails if the bug exists (that is the point). Run it once unannotated: if it fails at the `check`, add
  `@KnownAppIssue("15.5 item 7", "<exact check message>")` and a plan §15.5 entry; if it passes, keep it unannotated
  as a regression test for any future `rememberSaveable(gInfo)` change, or drop it if you think the trigger is too
  narrow. A stronger trigger if a message does not change `GroupInfo`: a member leaving (`bob sends "/l team"`,
  updates `groupSummary`), then check the footer only.

### T6 `review_acceptAsMember` (MemberAdmissionTest, bob joiner, 1 join)

- Scenario: the owner turns on review, a link joiner waits as «pending review» in «Chats with members», the owner
  talks to him there, accepts him as member, and he gets history and later messages.
- Fails if: Save on the admission screen does nothing (`MemberAdmission.kt:37-49`; bob would join without review and
  print `#team: you joined the group` only); the pending member is not listed (`MemberSupportView.kt:89`) or shows his
  role instead of status (`:174`); the compose actions do not appear for a pending member or stay for an accepted one
  (`ComposeView.kt:1735`); «Accept as member» sends the wrong role (`ComposeContextPendingMemberActionsView.kt:93` →
  `Observer`: row shows «observer») or nothing (`:62`).
- Steps:
```kotlin
val alice = app("alice"); val bob = cli("bob")
val info = pendingLinkJoin(alice, bob, "team")       // includes: «No chats with members» first, enableReviewAll, link
val chat = ChatScreen(this)
chat.send("before review")                            // main group; bob is pending
val cwm = chat.groupInfo().chatsWithMembers()
poll(timeoutMs = CONNECT_TIMEOUT_MS) { cwm.rowTexts("bob").let { "bob" in it && "pending review" in it } }
val sc = cwm.open("bob")
poll { sc.items().any { it.content.text == "New member wants to join the group." } && "Accept" in textsOf(Ui.Chat.PendingAccept) && "Reject" in textsOf(Ui.Chat.PendingReject) }
sc.send("hi")
bob.expectsMessage("#team (support) alice> hi", CONNECT_TIMEOUT_MS)   // next line: "before review" never reached bob
bob sends "#team (support) hello"; sc.itemId("hello")
sc.acceptVia("acceptAsMember")
bob.eventuallyAll(CONNECT_TIMEOUT_MS, "#team: you joined the group", "#team alice> before review [>>]")
poll(timeoutMs = CONNECT_TIMEOUT_MS) { chatModel.secondaryChatsContext.value == null && cwm.rowTexts("bob").let { "member" in it && "pending review" !in it } }
cwm.open("bob"); sc.itemId("hello"); check(!exists(Ui.Chat.PendingAccept) && !exists(Ui.Chat.PendingReject))   // other branch
/* close modals */ chat.send("after accept"); bob.expectsMessage("#team alice> after accept", CONNECT_TIMEOUT_MS)
```
- Peer lines: `View.hs:1387` (accepted you, pending review), `:1350-1363` (`connecting to group moderators…`), support
  echo format H/Groups.hs:3787-3800, history `[>>]` H/Groups.hs:3823-3826. With alice as the only moderator bob prints
  no `member … is connected` lines. The history order after acceptance is deterministic on one connection but D2 makes
  the test independent of it; `after accept` as the next line proves no duplicate history or stray group line.
- Strings: `member_support`, `no_support_chats`, `group_member_status_pending_review`, `group_member_role_member`,
  `rcv_group_event_new_member_pending_review` «New member wants to join the group.», `accept_pending_member_button`,
  `reject_pending_member_button`, `accept_pending_member_alert_title` «Accept member»,
  `accept_pending_member_alert_question`, `…_confirmation_as_member`, `…_confirmation_as_observer`.
- Platform: on Android `SupportChatScreen.send` hides the soft keyboard (the actions sit right above the field);
  on desktop the main chat is visible beside the support chat, which is why D1 is needed. Desktop plan §15.5 item 2
  (first tap after typing lost) is avoided because `click` focuses a focusable target first
  (`common/src/desktopTest/…/harness/UiTest.kt:229-235`); if `PendingAccept` turns out not focusable, record it.
- Flakiness: one link join with review (plan §15.4.1 applies); `quiesce()` once after the join (setup), never between
  user actions; accept happens with no profile update in flight (the known race needs a concurrent profile update).

### T7 `memberAdmission_unsavedChangesAlert` (MemberAdmissionTest, no peer, 0 joins)

- Scenario: the owner selects «all», leaves without saving, comes back, selects again and saves through the alert.
- Fails if: Back closes with changes (`MemberAdmission.kt:52`); the selection is not applied (`:85-89`); the alert's
  buttons are swapped (`:153-154`).
- Steps:
```kotlin
val info = emptyGroup(alice, "team").groupInfo(); val p = info.preferences()
var a = p.admission(); poll { "off" in a.review() }
a.select(MemberCriteria.All); poll { "all" in a.review() }
goBack(); poll { seesText("Save admission settings?") && "Save and notify group members" in textsOf(Ui.Alert.Confirm(Shared)) && "Exit without saving" in textsOf(Ui.Alert.Dismiss(Shared)) }
click(Ui.Alert.Dismiss(Shared)); poll { !seesText("Review members before admitting (\"knocking\").") }
check(groupInfoOf("team").groupProfile.memberAdmission?.review == null)
a = p.admission(); poll { "off" in a.review() }
a.select(MemberCriteria.All); goBack(); poll { seesText("Save admission settings?") }; click(Ui.Alert.Confirm(Shared))
poll { groupInfoOf("team").groupProfile.memberAdmission?.review == MemberCriteria.All && !seesText("Save admission settings?") }
a = p.admission(); poll { "all" in a.review() }
```
- Strings: `member_admission`, `admission_stage_review` «Review members», `admission_stage_review_descr`,
  `save_admission_question`, `member_criteria_off`/`_all`. Admission changes create no chat item for alice, so the
  model and the reopened screen are the evidence; T6 proves a saved «all» takes effect.

### T8 `review_acceptAsObserver` (MemberAdmissionTest, bob joiner, 1 join)

- Scenario: the owner accepts a pending joiner from the row's context menu as observer; he can read but not post.
- Fails if: the menu item is missing (`MemberSupportView.kt:272-276`); «Accept as observer» sends `Member`
  (`ComposeContextPendingMemberActionsView.kt:100`: bob's post is accepted, `expects` fails).
- Steps: `val info = pendingLinkJoin(alice, bob, "team")`; `val cwm = ChatScreen(this).groupInfo().chatsWithMembers()`;
  `poll { "pending review" in cwm.rowTexts("bob") }`; `cwm.menuAccept("bob")`; `acceptAlert("acceptAsObserver")`;
  `bob.eventually("#team: you joined the group", CONNECT_TIMEOUT_MS)`;
  `poll(CONNECT) { "observer" in cwm.rowTexts("bob") && "pending review" !in cwm.rowTexts("bob") }`;
  `bob sends "#team hi"`; `bob expects "#team: you don't have permission to send messages"` (`View.hs:2831-2833`);
  close modals; `chat.send("for observers")`; `bob.expectsMessage("#team alice> for observers", CONNECT_TIMEOUT_MS)`.
  The GUI group has no prior messages, so no `[>>]` lines precede (history on, nothing to send).
- Platform: `openContextMenu` right click (desktop) / long press (Android), `Section.kt:262`.

### T9 `review_rejectPendingMember` (MemberAdmissionTest, bob joiner, 1 join)

- Scenario: the owner opens the pending joiner's chat, cancels a reject, then rejects; he is removed and his chat
  disappears from «Chats with members».
- Fails if: «Reject» opens the accept flow or nothing (`ComposeContextPendingMemberActionsView.kt:48`); Cancel rejects
  (`AlertManager.kt:160` wiring); the removed member's row stays (`MemberSupportView.kt:89`).
- Steps:
```kotlin
pendingLinkJoin(alice, bob, "team"); val cwm = ChatScreen(this).groupInfo().chatsWithMembers()
val sc = cwm.open("bob")
sc.reject(); poll { seesText("Reject member?") && "Reject" in textsOf(Ui.Alert.Confirm(Shared)) && "Cancel" in textsOf(Ui.Alert.Dismiss(Shared)) }
click(Ui.Alert.Dismiss(Shared)); poll { !seesText("Reject member?") && exists(Ui.Chat.PendingReject) }
sc.send("still pending"); bob.expectsMessage("#team (support) alice> still pending", CONNECT_TIMEOUT_MS)   // Cancel sent nothing
sc.reject(); poll { seesText("Reject member?") }; click(Ui.Alert.Confirm(Shared))
bob.expectsMessage("#team: alice removed you from the group (signed)", CONNECT_TIMEOUT_MS); bob expects "use /d #team to delete the group"
poll(CONNECT) { chatModel.secondaryChatsContext.value == null }; cwm.empty()
```
- Peer: `View.hs:537` + `groupPreserved`; H/Groups.hs:4029-4032. The removal is the next line after `still pending`
  (`expectsMessage` with `CONNECT_TIMEOUT_MS`; `stripTime` is a no-op on event lines), so nothing else reached bob.
- Strings: `reject_pending_member_alert_title` «Reject member?», `reject_pending_member_button`, `cancel_verb`,
  `no_support_chats`. With D1 option B, replace the `still pending` sentinel by `bob sends "#team (support) ping"` and
  check that the next bob line after Confirm is the removal.

### T10 `joinerPendingReview_acceptedByAdmin` (MemberAdmissionTest, bob host, 1 join)

- Scenario: alice joins a group whose host reviews members; the support chat opens by itself, the main compose says
  «reviewed by admins», she writes to the admins and gets their reply; group messages sent while she waits do not
  reach her; after the host accepts her the support chat closes and she can post.
- Fails if: the support chat does not open (`ChatView.kt:248-258`); the compose is not blocked while pending
  (`ChatModel.kt:1726-1729`); the support chat does not close on acceptance (`SimpleXAPI.kt:3306-3319`); the app
  shows the pending main-group message.
- Setup (bob): `val link = bob.createGroupLink("team")` (`T/harness/ChatListFilterScreens.kt:88-94`: `/g team`,
  `/create link #team`, `Group link is created!`); `bob sends "/set admission review #team all"`;
  `bob expects "changed member admission rules"`. Alice joins with `joinLink(link)`
  (`ChatListFilterScreens.kt:62-67`; API precondition, joining by link is owned by §8);
  `bob.eventuallyAll(CONNECT_TIMEOUT_MS, "alice: accepting request to join group #team...", "#team: alice connected and pending review")`
  (`View.hs:542,1371`); `poll(CONNECT) { groupInfoOf("team").membership.memberStatus == GroupMemberStatus.MemPendingReview }`; `quiesce()`.
- Steps:
```kotlin
val chat = alice.chatList.open("team")
val sc = SupportChatScreen(this); sc.waitOpen()
poll { seesText("Chat with admins") && sc.items().any { it.content.text == "Please wait for group moderators to review your request to join the group." } }
if (appPlatform.isDesktop) poll { seesText("reviewed by admins") }       // main chat stays visible only on desktop
sc.send("hello admins"); bob.expectsMessage("#team (support: alice) alice> hello admins", CONNECT_TIMEOUT_MS)
bob sends "#team while pending"
bob sends "#team (support: alice) hi alice"; sc.itemId("hi alice")
check(chatModel.chatsContext.chatItems.value.none { it.content.text == "while pending" })   // later message arrived, earlier group one did not
bob sends "/accept member #team alice"; bob expects "#team: alice accepted"
poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the support chat to close after acceptance") { chatModel.secondaryChatsContext.value == null && !ModalManager.end.hasModalsOpen() }
poll { !seesText("reviewed by admins") }
chat.send("hi all"); bob.expectsMessage("#team alice> hi all", CONNECT_TIMEOUT_MS)
```
- Notes: bob's group has history off (core default), so `while pending` never arrives; the check after acceptance
  could repeat after `hi all`. The auto-close uses a real-time `delay(1000)` on `Dispatchers.Default`
  (`SimpleXAPI.kt:3313-3317`), not tracked by `UiIdle`; `poll` waits for it in real time.
- Strings: `support_chat` «Chat with admins» (title, `MemberSupportChatView.kt:111`), `reviewed_by_admins`,
  `snd_group_event_user_pending_review`.
- Platform: on Android the support chat covers the group chat and closing it while pending also closes the group
  chat (`MemberSupportChatView.kt:33-45`), so the test never closes it while pending; the «reviewed by admins» check
  before acceptance is desktop-only (real UI difference, guide §3). Flakiness: one link join.

## 6.1 Suspected app findings (report, do not fix)

1. **Unsaved group preference / admission edits are discarded when the group's `GroupInfo` changes**
   (`GroupPreferences.kt:43-44`, `MemberAdmission.kt:34-35`, model replacement `ChatModel.kt:466-489,573-582`).
   Realistic in any active group: the owner toggles several preferences, a message or member event arrives, the
   toggles snap back with no notice, and Save sends only what was changed after the reset. T5 probes it.
2. **Save through the unsaved-changes alert closes the screen even when the save fails**
   (`GroupPreferences.kt:46-60`, `MemberAdmission.kt:37-48`: `afterSave()` runs whether or not `apiUpdateGroup`
   returned a group). Minor; the API error alert is probably shown, but the edits are lost. Not tested (needs a
   failing core call).
3. **The «SimpleX links not allowed» state can be stale when link previews are off**
   (`ComposeView.kt:1087`: `hasSimplexLink` includes the group permission at typing time, so a later prohibition is
   not shown until the text changes; with previews on, `:509` does not include it and `:1751` re-checks). Minor; not
   tested (the harness keeps previews on).
4. Not a bug, catalogue correction: «Direct messages» off in GUI groups comes from the core default
   (`Types/Preferences.hs:573`), not from `AddGroupView.kt` (it sets only history on, `:164`).
5. Cosmetic: `strings.xml:2317` `<string name="chat_preferences_off">off</string>`` has a stray backtick after the
   element (outside the string value, harmless).

## 7. Catalogue edits to apply after the tests pass

- §5: rows merged as in §1 (names moved into T1–T4's `Test case(s)` cells); new names
  `groupPrefs_ownerChangesNotifyAndRestrictMembers` and `groupPrefs_unsavedEditsKeptWhenMessageArrives`; removed
  `groupPrefs_disappearingMessageExpires` and the join-based history test; Needs: T1 `—`, T2 `P, XFTP`, T3–T5 `P`.
- §6: `memberAdmission_enableReviewAll` → T6, `memberAdmission_readOnlyForNonOwner` → T4, `review_*` merges as in §1,
  `review_moderatorAcceptsMember` and `joinerPendingReview_rejected` removed with the reasons above;
  `joinerPendingReview_*` Needs become `P` (no `CB`: the link comes from the CLI and `joinLink` uses the API).
- §12: `chatsWithMembers_emptyState` and `chatsWithMembers_removedMemberChatGone` move into T6/T9.
- All corrected file:line references from §1; Haskell line numbers as listed.
- Recount with `tools/recount.py` and regenerate the class map (legend "Count rules").

## 8. Mutation checklist for the PR (one per test, must turn the test red)

| Test | Mutation |
|---|---|
| T1 | `GroupPreferences.kt:66` `if (preferences == currentPreferences) close() else …` → `close()` |
| T2 | `GroupPreferences.kt:135` `copy(reactions = …)` → `copy(fullDelete = …)` |
| T3 | `GroupMemberInfoView.kt:515` condition → `false` |
| T4 | `GroupPreferences.kt:282` `if (groupInfo.isOwner)` → `if (true)` |
| T5 | (probe) remove `gInfo` from `rememberSaveable` keys at `GroupPreferences.kt:43-44` should make it pass if it failed |
| T6 | `ComposeContextPendingMemberActionsView.kt:93` `GroupMemberRole.Member` → `Observer` |
| T7 | `MemberAdmission.kt:153-154` swap `onConfirm`/`onDismiss` |
| T8 | `ComposeContextPendingMemberActionsView.kt:100` `Observer` → `Member` |
| T9 | `MemberSupportView.kt:89` drop the `MemRemoved` filter |
| T10 | `SimpleXAPI.kt:3306-3319` remove the auto-close block |
