package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.createOneTimeLink
import chat.simplex.app.ui.harness.described
import chat.simplex.app.ui.harness.emptyGroup
import chat.simplex.app.ui.harness.goBack
import chat.simplex.app.ui.harness.groupInfoOf
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.joinGroupFromList
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.GroupFeature
import chat.simplex.common.model.GroupFeatureEnabled
import chat.simplex.common.model.GroupMemberRole
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Ignore
import org.junit.Test

class GroupPreferencesTest : UiTest() {
  @Test fun groupPrefs_unsavedChangesAlert() = uiTest {
    val alice = app("alice")
    val info = emptyGroup(alice, "team").groupInfo()
    var prefs = info.preferences()
    waitFor(Ui.GroupPrefs.Toggle(GroupFeature.Reports))
    waitFor(Ui.GroupPrefs.Toggle(GroupFeature.Support))
    check(isEnabled(Ui.GroupPrefs.Toggle(GroupFeature.Reactions))) { "the `Message reactions` toggle is disabled" }
    check(!isEnabled(Ui.GroupPrefs.Toggle(GroupFeature.Reports)) && !isEnabled(Ui.GroupPrefs.Toggle(GroupFeature.Support))) { "the `Member reports` or `Chat with admins` toggle is enabled" }
    poll(what = "reactions allowed before the change") { seesText("Allow message reactions.") }
    prefs.toggle(GroupFeature.Reactions)
    poll(what = "reactions prohibited after the toggle") { seesText("Prohibit messages reactions.") && !seesText("Allow message reactions.") }
    prefs.reset()
    poll(what = "reactions allowed again after `Reset`") { seesText("Allow message reactions.") && !seesText("Prohibit messages reactions.") }
    prefs.backClosing()
    prefs = info.preferences()
    poll(what = "reactions allowed when reopened after `Reset`") { seesText("Allow message reactions.") }
    prefs.toggle(GroupFeature.Reactions)
    poll(what = "reactions prohibited after the toggle") { seesText("Prohibit messages reactions.") && !seesText("Allow message reactions.") }
    prefs.backExpectingAlert("Save preferences?")
    check("Save and notify group members" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) && "Exit without saving" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared))) { "the `Save preferences?` alert has no `Save and notify group members` / `Exit without saving`" }
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "group preferences to close after `Exit without saving`") { !exists(Ui.GroupPrefs.MemberAdmission) && !seesText("Save preferences?") }
    check(groupInfoOf("team").fullGroupPreferences.reactions.enable == GroupFeatureEnabled.ON) { "`Exit without saving` saved the reactions change" }
    prefs = info.preferences()
    poll(what = "reactions still allowed after `Exit without saving`") { seesText("Allow message reactions.") }
    prefs.toggle(GroupFeature.Reactions)
    poll(what = "reactions prohibited after the toggle") { seesText("Prohibit messages reactions.") && !seesText("Allow message reactions.") }
    prefs.backExpectingAlert("Save preferences?")
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "group preferences to close with reactions saved off") {
      !exists(Ui.GroupPrefs.MemberAdmission) && groupInfoOf("team").fullGroupPreferences.reactions.enable == GroupFeatureEnabled.OFF
    }
    info.close()
    restart().chatList.open("team").groupInfo().preferences()
    poll(what = "reactions prohibited after restart") { seesText("Prohibit messages reactions.") && !seesText("Allow message reactions.") }
  }

  @Test fun groupPrefs_ownerChangesNotifyAndRestrictMembers() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val link = bob.createOneTimeLink()
    bob.eventually("and ask them to connect: /c <invitation_link_above>")
    val file = bob.createFile("p.txt", "x")
    val chat = ownGroup(alice, "team", bob)
    chat.send("hi")
    bob.expectsMessage("#team alice> hi", CONNECT_TIMEOUT_MS)
    bob sends "+1 #team hi"
    bob expects "added 👍"
    val hi = chat.itemId("hi")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's 👍 on `hi`") { exists(Ui.Chat.ReactionChip(hi, "👍")) }
    check(menuOffersReaction(hi, "👎")) { "the menu of `hi` offers no 👎 while reactions are allowed" }
    var info = chat.groupInfo()
    val p = info.preferences()
    check(!exists(Ui.GroupPrefs.Ttl) && !exists(Ui.GroupPrefs.Role(GroupFeature.DirectMessages)) && exists(Ui.GroupPrefs.Role(GroupFeature.Voice))) { "`Delete after` / `Enabled for` rows do not match the default preferences" }
    p.toggle(GroupFeature.TimedMessages)
    poll(what = "`Delete after` shown with `1 day`") { exists(Ui.GroupPrefs.Ttl) && "1 day" in textsOf(Ui.GroupPrefs.Ttl) }
    p.ttl(7 * 86400)
    p.toggle(GroupFeature.DirectMessages)
    poll(what = "`Enabled for` `all members` under `Direct messages`") {
      exists(Ui.GroupPrefs.Role(GroupFeature.DirectMessages)) && "all members" in textsOf(Ui.GroupPrefs.Role(GroupFeature.DirectMessages))
    }
    p.role(GroupFeature.DirectMessages, GroupMemberRole.Owner)
    p.toggle(GroupFeature.FullDelete)
    p.toggle(GroupFeature.Reactions)
    p.toggle(GroupFeature.Voice)
    poll(what = "`Enabled for` hidden under voice messages turned off") { !exists(Ui.GroupPrefs.Role(GroupFeature.Voice)) }
    p.role(GroupFeature.Files, GroupMemberRole.Admin)
    p.role(GroupFeature.SimplexLinks, GroupMemberRole.Owner)
    p.toggle(GroupFeature.History)
    p.save()
    bob.expectsMessage("alice updated group #team: (signed)", CONNECT_TIMEOUT_MS)
    bob expects "updated group preferences:"
    bob expects "Disappearing messages: on (1 week)"
    bob expects "Direct messages: on for owners"
    bob expects "Full deletion: on"
    bob expects "Message reactions: off"
    bob expects "Voice messages: off"
    bob expects "Files and media: on for admins"
    bob expects "SimpleX links: on for owners"
    bob expects "Recent history: off"
    poll(what = "the saved preferences in team's model") { groupInfoOf("team").fullGroupPreferences.history.enable == GroupFeatureEnabled.OFF }
    p.backClosing()
    info.close()
    check(!menuOffersReaction(hi, "👎")) { "the menu of `hi` still offers 👎 with reactions prohibited" }
    click(described("Record voice message"))
    poll(what = "the `Voice messages prohibited!` alert") { seesText("Voice messages prohibited!") && seesText("Only group owners can enable voice messages.") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    bob sends "+^ #team hi"
    bob expects "bad chat command: feature not allowed Message reactions"
    bob sends "#team $link"
    bob expects "bad chat command: feature not allowed SimpleX links"
    bob sends "/f #team $file"
    bob expects "bad chat command: feature not allowed Files and media"
    chat.send(link)
    bob.expectsMessage("#team alice> $link", CONNECT_TIMEOUT_MS)
    hideKeyboard()
    info = chat.groupInfo()
    val p2 = info.preferences()
    p2.toggle(GroupFeature.TimedMessages)
    poll(what = "`Delete after` hidden with disappearing messages off") { !exists(Ui.GroupPrefs.Ttl) }
    p2.toggle(GroupFeature.Reactions)
    p2.save()
    bob.expectsMessage("alice updated group #team: (signed)", CONNECT_TIMEOUT_MS)
    bob expects "updated group preferences:"
    bob expects "Disappearing messages: off"
    bob expects "Message reactions: on"
    poll(what = "reactions allowed again in team's model") { groupInfoOf("team").fullGroupPreferences.reactions.enable == GroupFeatureEnabled.ON }
    p2.backClosing()
    info.close()
    check(menuOffersReaction(hi, "👎")) { "the menu of `hi` offers no 👎 after reactions were allowed again" }
    bob sends "+^ #team hi"
    bob expects "added 🚀"
  }

  @Test fun groupPrefs_directMessagesForOwnersOnly() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val info = emptyGroup(alice, "team").groupInfo()
    val link = info.createLink()
    bob sends "/c $link"
    bob expects "connection request sent!"
    bob.eventually("#team: you joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob listed as connected in team's members") { "connected" in info.memberTexts("bob") }
    quiesce()
    info.member("bob").message()
    poll(what = "the `Can't message group member` alert") { seesText("Can't message group member") && seesText("Direct messages between members are prohibited in this group.") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    goBack()
    poll(what = "member info to close to group info") { !exists(Ui.MemberInfo.Message) && exists(Ui.GroupInfo.Preferences) }
    bob sends "@alice hi"
    bob expects "bad chat command: direct messages not allowed"
    val p = info.preferences()
    check(!exists(Ui.GroupPrefs.Role(GroupFeature.DirectMessages))) { "`Enabled for` shown under `Direct messages` while they are off" }
    p.toggle(GroupFeature.DirectMessages)
    poll(what = "`Enabled for` `all members` under `Direct messages`") {
      exists(Ui.GroupPrefs.Role(GroupFeature.DirectMessages)) && "all members" in textsOf(Ui.GroupPrefs.Role(GroupFeature.DirectMessages))
    }
    p.role(GroupFeature.DirectMessages, GroupMemberRole.Owner)
    p.save()
    bob.expectsMessage("alice updated group #team: (signed)", CONNECT_TIMEOUT_MS)
    bob expects "updated group preferences:"
    bob expects "Direct messages: on for owners"
    poll(what = "direct messages for owners saved in team's model") { groupInfoOf("team").fullGroupPreferences.directMessages.role == GroupMemberRole.Owner }
    p.backClosing()
    bob sends "@alice hi"
    bob expects "bad chat command: direct messages not allowed"
    info.member("bob").message()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "a direct chat with bob opened instead of the alert") {
      (chatModel.chats.value.firstOrNull { it.id == chatModel.chatId.value }?.chatInfo as? ChatInfo.Direct)?.contact?.displayName == "bob" && !seesText("Can't message group member")
    }
  }

  @Test fun groupPrefs_readOnlyForMember() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val link = bob.createOneTimeLink()
    bob.eventually("and ask them to connect: /c <invitation_link_above>")
    hostGroup(bob, "team", "alice")
    joinGroupFromList(alice, bob, "team")
    val chat = alice.chatList.open("team")
    var info = chat.groupInfo()
    var p = info.preferences()
    check(!exists(Ui.GroupPrefs.Toggle(GroupFeature.Voice)) && !seesText("Save and notify group members") && !seesText("Reset")) { "a member's group preferences offer toggles, `Reset` or `Save and notify group members`" }
    check(seesText("Members can send voice messages.") && seesText("Members can send SimpleX links.") && !seesText("admins")) { "a member's group preferences do not show the host's defaults" }
    p.admission()
    check(!exists(Ui.MemberAdmission.Review) && seesText("off") && !seesText("all")) { "a member's `Review members` is editable or not `off`" }
    bob sends "/set admission review #team all"
    bob expects "changed member admission rules"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice's read-only `Review members` to show `all`") { seesText("all") && !seesText("off") }
    goBack()
    poll(what = "member admission to close to group preferences") { !seesText("Review members before admitting (\"knocking\").") && exists(Ui.GroupPrefs.MemberAdmission) }
    p.backClosing()
    info.close()
    bob sends "#team before voice"
    chat.itemId("before voice")
    bob sends "/set voice #team on admin"
    bob expects "updated group preferences:"
    bob expects "Voice messages: on for admins"
    val ev = chat.itemId("Voice messages: on (admins)")
    check(textsOf(Ui.Chat.Item(ev)).any { "Voice messages: on (admins)" in it }) { "the `Voice messages: on (admins)` item is not shown" }
    click(described("Record voice message"))
    poll(what = "the `Voice messages prohibited!` alert") { seesText("Voice messages prohibited!") && seesText("Only group owners can enable voice messages.") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    info = chat.groupInfo()
    p = info.preferences()
    poll(what = "voice messages enabled for admins in alice's read-only preferences") { seesText("Members can send voice messages.") && seesText("Enabled for") && seesText("admins") }
    p.backClosing()
    info.close()
    bob sends "/set links #team on owner"
    bob expects "updated group preferences:"
    bob expects "SimpleX links: on for owners"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "SimpleX links prohibited for alice in team's model") { !groupInfoOf("team").groupFeatureEnabled(GroupFeature.SimplexLinks) }
    typeInto(Ui.Chat.Message, link)
    poll(what = "`SimpleX links not allowed` with the send button disabled") { seesText("SimpleX links not allowed") && exists(Ui.Chat.Send) && !isEnabled(Ui.Chat.Send) }
    bob sends "/set links #team on"
    bob expects "updated group preferences:"
    bob expects "SimpleX links: on"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`SimpleX links not allowed` gone with the send button enabled") { !seesText("SimpleX links not allowed") && isEnabled(Ui.Chat.Send) }
    pressEnter(Ui.Chat.Message)
    poll(what = "the message field to clear after one Enter") { editableText(Ui.Chat.Message).isNullOrEmpty() }
    bob.expectsMessage("#team alice> $link", CONNECT_TIMEOUT_MS)
    hideKeyboard()
    info = chat.groupInfo()
    poll(what = "`Chat with admins` shown and `Chats with members` hidden for a member") { seesText("Chat with admins") && !exists(Ui.GroupInfo.MemberSupport) }
    info.close()
    bob sends "/mr #team alice moderator"
    bob.eventuallyMatching(Regex("#team: you changed the role of alice to moderator( \\(signed\\))?"))
    info = chat.groupInfo()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Chats with members` shown and `Chat with admins` hidden for a moderator") { exists(Ui.GroupInfo.MemberSupport) && !seesText("Chat with admins") }
    info.chatsWithMembers().empty()
  }

  @KnownAppIssue("15.5 item 8", "the unsaved `Message reactions` change was discarded when bob's message arrived")
  @Test fun groupPrefs_unsavedEditsKeptWhenMessageArrives() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = ownGroup(alice, "team", bob)
    val p = chat.groupInfo().preferences()
    poll(what = "reactions allowed before the change") { seesText("Allow message reactions.") }
    p.toggle(GroupFeature.Reactions)
    poll(what = "reactions prohibited after the toggle") { seesText("Prohibit messages reactions.") }
    bob sends "#team hello"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's `hello` in team's chat") { chatModel.chatsContext.chatItems.value.any { it.content.text == "hello" } }
    settle()
    check(seesText("Prohibit messages reactions.")) { "the unsaved `Message reactions` change was discarded when bob's message arrived" }
    p.save()
    bob.expectsMessage("alice updated group #team: (signed)", CONNECT_TIMEOUT_MS)
    bob expects "updated group preferences:"
    bob expects "Message reactions: off"
  }

  @Ignore("commented-out code (\"Sign messages\", GroupPreferences.kt:219-221; plan §11.3)")
  @Test fun groupPrefs_signMessages() {}

  private fun UiScope.menuOffersReaction(id: Long, emoji: String): Boolean {
    rightClick(Ui.Chat.Item(id))
    waitFor(Ui.Chat.ItemMenu(id, "reply"))
    val offered = exists(Ui.Chat.ItemReaction(id, emoji))
    click(Ui.Chat.ItemMenu(id, "select"))
    waitFor(Ui.Chat.Selection.part("title"))
    escape()
    poll(what = "selection mode to end") { !exists(Ui.Chat.Selection.part("title")) }
    return offered
  }
}
