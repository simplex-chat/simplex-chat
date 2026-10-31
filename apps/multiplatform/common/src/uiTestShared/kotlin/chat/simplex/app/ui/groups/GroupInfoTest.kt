package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.emptyGroup
import chat.simplex.app.ui.harness.groupInfoOf
import chat.simplex.app.ui.harness.groupInfoRow
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.joinGroupFromList
import chat.simplex.app.ui.harness.newGroupsViaApi
import chat.simplex.common.model.MsgFilter
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

class GroupInfoTest : UiTest() {
  @Test fun groupInfo_setLocalAlias() = uiTest {
    val alice = app("alice")
    var info = emptyGroup(alice, "team").groupInfo()
    poll(what = "group info of `team` with an empty `Set chat name…` field") {
      exists(Ui.Info.Alias) && editableText(Ui.Info.Alias).isNullOrEmpty() && seesText("Set chat name…") && seesText("team")
    }
    typeInto(Ui.Info.Alias, "Work")
    poll(what = "`Work` saved as team's local name") { groupInfoOf("Work").localAlias == "Work" }
    hideKeyboard()
    info.close()
    backToChatList()
    poll(what = "the row reading `Work` instead of `team`") { alice.chatList.rowTexts("Work").let { "Work" in it && "team" !in it } }
    val after = restart()
    poll(what = "the group listed as `Work` after the restart") {
      chatModel.chatRunning.value == true && chatModel.chats.value.any { it.chatInfo.chatViewName == "Work" }
    }
    poll(what = "the row still reading `Work` after the restart") { after.chatList.rowTexts("Work").let { "Work" in it && "team" !in it } }
    info = after.chatList.open("Work").groupInfo()
    poll(what = "group info showing the profile name `team` with the local name `Work`") { editableText(Ui.Info.Alias) == "Work" && seesText("team") }
    replaceText(Ui.Info.Alias, "")
    poll(what = "team's local name cleared") { groupInfoOf("team").localAlias.isEmpty() }
    hideKeyboard()
    info.close()
    backToChatList()
    poll(what = "the row reading `team` again") { alice.chatList.rowTexts("team").let { "team" in it && "Work" !in it } }
  }

  @Test fun groupInfo_muteCycleWithMentions() = uiTest {
    val alice = app("alice")
    var info = emptyGroup(alice, "team").groupInfo()
    fun mode() = groupInfoOf("team").chatSettings.enableNtfs
    poll(what = "`Mute` on the group info button with all notifications on") { exists(Ui.GroupInfo.Mute) && seesText("Mute") && mode() == MsgFilter.All }
    click(Ui.GroupInfo.Mute)
    poll(what = "mentions-only notifications and `Mute all` on the button") { mode() == MsgFilter.Mentions && seesText("Mute all") && !seesText("Mute") }
    click(Ui.GroupInfo.Mute)
    poll(what = "notifications off and `Unmute` on the button") { mode() == MsgFilter.None && seesText("Unmute") && !seesText("Mute all") }
    info.close()
    backToChatList()
    poll(what = "the mute icon on team's row") { "Notifications" in alice.chatList.rowDescriptions("team") }
    info = alice.chatList.open("team").groupInfo()
    poll(what = "`Unmute` on the button after reopening group info") { exists(Ui.GroupInfo.Mute) && seesText("Unmute") }
    click(Ui.GroupInfo.Mute)
    poll(what = "all notifications and `Mute` on the button") { mode() == MsgFilter.All && seesText("Mute") && !seesText("Unmute") }
    info.close()
    backToChatList()
    poll(what = "no mute icon on team's row") { "Notifications" !in alice.chatList.rowDescriptions("team") }
  }

  @Test fun groupInfo_actionButtonsByRole() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    joinGroupFromList(alice, bob, "team")
    backToChatList()
    val info = alice.chatList.open("team").groupInfo()
    poll(what = "a member's group info with `Search` and `Mute`") { exists(Ui.GroupInfo.Mute) && exists(Ui.GroupInfo.Preferences) && seesText("Search") && seesText("Mute") }
    check(!seesText("Invite")) { "a member's group info offers `Invite`" }
    check(listOf(Ui.GroupInfo.CreateLink, Ui.GroupInfo.GroupLink, Ui.GroupInfo.AddMembers, Ui.GroupInfo.MemberSupport, Ui.GroupInfo.Reports).none { exists(it) }) {
      "a member's group info shows a link, `Invite members`, `Chats with members` or `Member reports` row"
    }
    bob sends "/mr team alice admin"
    bob.eventuallyMatching(Regex("#team: you changed the role of alice to admin( \\(signed\\))?"))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "an admin's group info with `Invite`, `Create group link`, `Invite members`, `Chats with members` and `Member reports`") {
      seesText("Invite") && listOf(Ui.GroupInfo.CreateLink, Ui.GroupInfo.AddMembers, Ui.GroupInfo.MemberSupport, Ui.GroupInfo.Reports).all { exists(it) }
    }
    check(!exists(Ui.GroupInfo.GroupLink)) { "`Group link` shown before a link was created" }
    info.createLink()
    poll(what = "`Group link` in place of `Create group link`") { exists(Ui.GroupInfo.GroupLink) && !exists(Ui.GroupInfo.CreateLink) }
    groupInfoRow(Ui.GroupInfo.GroupLink)
    waitFor(Ui.GroupLink.Share, CONNECT_TIMEOUT_MS)
    escape()
    poll(what = "the group link screen to close to group info") { !exists(Ui.GroupLink.Share) && exists(Ui.GroupInfo.GroupLink) }
  }

  @Test fun groupInfo_sendReceiptsOption() = uiTest {
    val alice = app("alice")
    newGroupsViaApi("club")
    var info = emptyGroup(alice, "team").groupInfo()
    fun receipts(): List<String> = textsOf(Ui.GroupInfo.SendReceipts)
    poll(what = "team's `Send receipts` at `default (…)`") { receipts().any { it.startsWith("default (") } }
    click(Ui.GroupInfo.SendReceipts)
    click(Ui.GroupInfo.SendReceipts.option(false))
    poll(what = "`no` selected and saved for team") { "no" in receipts() && groupInfoOf("team").chatSettings.sendRcpts == false }
    info.close()
    val after = restart()
    poll(what = "team and club listed after the restart") {
      chatModel.chatRunning.value == true && chatModel.chats.value.map { it.chatInfo.chatViewName }.containsAll(listOf("team", "club"))
    }
    info = after.chatList.open("team").groupInfo()
    poll(what = "team's `Send receipts` still `no` after the restart") { "no" in receipts() }
    info.close()
    backToChatList()
    info = after.chatList.open("club").groupInfo()
    poll(what = "club's `Send receipts` still at `default (…)`") { receipts().any { it.startsWith("default (") } && groupInfoOf("club").chatSettings.sendRcpts == null }
  }
}
