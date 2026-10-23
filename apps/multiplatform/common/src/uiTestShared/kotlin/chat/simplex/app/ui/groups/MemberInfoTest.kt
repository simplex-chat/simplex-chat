package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.ChatScreen
import chat.simplex.app.ui.harness.GroupPreferencesScreen
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.alertSaying
import chat.simplex.app.ui.harness.awaitAppIdle
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.blockForAllViaApi
import chat.simplex.app.ui.harness.closeMemberInfo
import chat.simplex.app.ui.harness.confirmAlert
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.copiedServer
import chat.simplex.app.ui.harness.createGroup
import chat.simplex.app.ui.harness.described
import chat.simplex.app.ui.harness.emptyGroup
import chat.simplex.app.ui.harness.goBack
import chat.simplex.app.ui.harness.groupInfoOf
import chat.simplex.app.ui.harness.groupInfoRow
import chat.simplex.app.ui.harness.groupMemberId
import chat.simplex.app.ui.harness.inviteMembers
import chat.simplex.app.ui.harness.isServer
import chat.simplex.app.ui.harness.joinGroupFromList
import chat.simplex.app.ui.harness.joinsViaLink
import chat.simplex.app.ui.harness.localSmpServer
import chat.simplex.app.ui.harness.memberConnected
import chat.simplex.app.ui.harness.memberConnectionLoaded
import chat.simplex.app.ui.harness.memberOfHostedGroup
import chat.simplex.app.ui.harness.openMemberMenu
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.app.ui.harness.ownGroupWithTwo
import chat.simplex.app.ui.harness.shownSecurityCode
import chat.simplex.common.model.CIContent
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.GroupFeature
import chat.simplex.common.model.GroupFeatureEnabled
import chat.simplex.common.model.GroupMemberRole
import chat.simplex.common.model.RcvGroupEvent
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.ModalManager
import org.junit.Assert.assertEquals
import org.junit.Test

class MemberInfoTest : UiTest() {
  @Test fun memberInfo_changeRoleToAdmin() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    ownGroup(alice, "team", bob).groupInfo().member("bob").changeRole(GroupMemberRole.Admin)
    bob.eventually("#team: alice changed your role from member to admin (signed)", CONNECT_TIMEOUT_MS)
    poll(what = "bob is admin") { chatModel.groupMembers.value.any { it.memberProfile.displayName == "bob" && it.memberRole == GroupMemberRole.Admin } }
  }

  @Test fun memberInfo_removeMember() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    ownGroup(alice, "team", bob, cath).groupInfo().member("bob").remove()
    bob.eventually("#team: alice removed you from the group (signed)", CONNECT_TIMEOUT_MS)
    cath.eventually("#team: alice removed bob from the group (signed)", CONNECT_TIMEOUT_MS)
    poll(what = "bob no longer a current member") { chatModel.groupMembers.value.none { it.memberProfile.displayName == "bob" && it.memberCurrent } }
  }

  @Test fun memberInfo_changeRoleToObserverCannotSend() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val info = ownGroup(alice, "team", bob).groupInfo()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob listed in team's members without a role label") { info.memberTexts("bob").let { "bob" in it && "observer" !in it } }
    val member = info.member("bob")
    member.pickRole(GroupMemberRole.Observer)
    check(seesText("The role will be changed to \"observer\". Everyone in the group will be notified.")) { "the `Change role?` alert does not name `observer`" }
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the role picker back at `member` after dismissing `Change role?`") {
      !seesText("Change role?") && "member" in textsOf(Ui.MemberInfo.Role) && "observer" !in textsOf(Ui.MemberInfo.Role)
    }
    member.changeRole(GroupMemberRole.Observer)
    bob.expectsMessage("#team: alice changed your role from member to observer (signed)", CONNECT_TIMEOUT_MS)
    escape()
    poll(what = "bob listed as `observer` in group info") { !exists(Ui.MemberInfo.Role) && "observer" in info.memberTexts("bob") }
    bob sends "#team hi"
    bob expects "#team: you don't have permission to send messages"
  }

  @Test fun memberInfo_blockForMeHidesMessages() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = memberOfHostedGroup(alice, bob, cath)
    cath sends "#team before block"
    bob.eventuallyMessage("#team cath> before block", CONNECT_TIMEOUT_MS)
    val before = chat.itemId("cath's `before block`") { it.content.text == "before block" }
    var info = chat.groupInfo()
    poll(what = "cath listed in team's members, not blocked") { info.memberTexts("cath").let { "cath" in it && "blocked" !in it } }
    info.member("cath").block("cath")
    poll(what = "cath's member info offering `Unblock member`") { exists(Ui.MemberInfo.Unblock) && !exists(Ui.MemberInfo.Block) }
    escape()
    poll(what = "member info to close to group info with cath listed as `blocked`") { !exists(Ui.MemberInfo.Unblock) && "blocked" in info.memberTexts("cath") }
    info.close()
    chat.send("after block")
    bob.expectsMessage("#team alice> after block", CONNECT_TIMEOUT_MS)
    cath.expectsMessage("#team alice> after block", CONNECT_TIMEOUT_MS)
    cath sends "#team while blocked"
    bob.expectsMessage("#team cath> while blocked", CONNECT_TIMEOUT_MS)
    val hidden = chat.itemId("cath's `while blocked`") { it.content.text == "while blocked" }
    poll(what = "cath's `while blocked` shown as `blocked`, her `before block` unchanged") {
      textsOf(Ui.Chat.Item(hidden)).let { t -> "blocked" in t && t.none { "while blocked" in it } } && textsOf(Ui.Chat.Item(before)).any { "before block" in it }
    }
    info = chat.groupInfo()
    info.member("cath").unblock("cath")
    poll(what = "cath's member info offering `Block member` again") { exists(Ui.MemberInfo.Block) && !exists(Ui.MemberInfo.Unblock) }
    escape()
    poll(what = "member info to close to group info with cath not blocked") { !exists(Ui.MemberInfo.Block) && "blocked" !in info.memberTexts("cath") }
    info.close()
    cath sends "#team after unblock"
    bob.expectsMessage("#team cath> after unblock", CONNECT_TIMEOUT_MS)
    val shown = chat.itemId("cath's `after unblock`") { it.content.text == "after unblock" }
    poll(what = "cath's `after unblock` shown with its text") { textsOf(Ui.Chat.Item(shown)).let { t -> t.any { "after unblock" in it } && "blocked" !in t } }
    chat.send("end")
    cath.expectsMessage("#team alice> end", CONNECT_TIMEOUT_MS)
  }

  @Test fun memberInfo_unblockForAll() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = ownGroupWithTwo(alice, bob, cath)
    blockForAllViaApi("team", "bob")
    cath.eventually("#team: alice blocked bob (signed)", CONNECT_TIMEOUT_MS)
    bob sends "#team while blocked"
    val blocked = chat.itemId("bob's `while blocked`") { it.content.text == "while blocked" }
    poll(what = "bob's `while blocked` shown as `blocked by admin`") { textsOf(Ui.Chat.Item(blocked)).let { t -> "blocked by admin" in t && t.none { "while blocked" in it } } }
    val info = chat.groupInfo()
    val member = info.member("bob")
    poll(what = "bob's member info offering `Unblock for all`, not `Block for all`") { exists(Ui.MemberInfo.UnblockForAll) && !exists(Ui.MemberInfo.BlockForAll) }
    member.unblockForAll("bob")
    cath.expectsMessage("#team: alice unblocked bob (signed)", CONNECT_TIMEOUT_MS)
    poll(what = "bob's member info offering `Block for all` again") { exists(Ui.MemberInfo.BlockForAll) && !exists(Ui.MemberInfo.UnblockForAll) }
    escape()
    poll(what = "member info to close to group info") { !exists(Ui.MemberInfo.Remove) }
    info.close()
    bob sends "#team after unblock"
    cath.expectsMessage("#team bob> after unblock", CONNECT_TIMEOUT_MS)
    val after = chat.itemId("bob's `after unblock`") { it.content.text == "after unblock" }
    poll(what = "bob's `after unblock` shown with its text") { textsOf(Ui.Chat.Item(after)).let { t -> t.any { "after unblock" in it } && "blocked by admin" !in t } }
  }

  @Test fun memberInfo_removeMemberWithMessages() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = ownGroupWithTwo(alice, bob, cath)
    bob sends "#team bob was here"
    cath.eventuallyMessage("#team bob> bob was here", CONNECT_TIMEOUT_MS)
    val msg = chat.itemId("bob's `bob was here`") { it.content.text == "bob was here" }
    val info = chat.groupInfo()
    info.member("bob").removeWithMessages()
    bob.expectsMessage("#team: alice removed you from the group with all messages (signed)", CONNECT_TIMEOUT_MS)
    bob expects "use /d #team to delete the group"
    cath.expectsMessage("#team: alice removed bob from the group with all messages (signed)", CONNECT_TIMEOUT_MS)
    poll(what = "member info to close to group info after removing bob") { !exists(Ui.MemberInfo.Remove) && exists(Ui.GroupInfo.Preferences) }
    info.close()
    poll(what = "bob's `bob was here` shown as `moderated by alice`") { textsOf(Ui.Chat.Item(msg)).let { t -> "moderated by alice" in t && t.none { "bob was here" in it } } }
  }

  @Test fun memberInfo_messageOpensKnownContact() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val team = ownGroup(alice, "team", bob)
    val teamId = alice.chatList.chatId("team")
    val bobId = alice.chatList.chatId("bob")
    poll(what = "team's chat open") { chatModel.chatId.value == teamId }
    team.groupInfo().member("bob").message()
    poll(what = "bob's direct chat open with group and member info closed") { chatModel.chatId.value == bobId && !ModalManager.end.hasModalsOpen() }
    check(chatModel.chats.value.count { (it.chatInfo as? ChatInfo.Direct)?.contact?.displayName == "bob" } == 1) { "`message` created a second direct chat with bob" }
    ChatScreen(this).send("hi from member info")
    bob.eventuallyMessage("alice> hi from member info", CONNECT_TIMEOUT_MS)
  }

  @Test fun memberInfo_verifyCodeMatchesCli() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val info = ownGroup(alice, "team", bob).groupInfo()
    memberConnected(info, "bob")
    fun verified() = chatModel.groupMembers.value.first { it.memberProfile.displayName == "bob" }.verified
    info.member("bob")
    memberConnectionLoaded()
    poll(what = "`Verify security code` in bob's member info with bob not verified") {
      textOf(Ui.Info.VerifyCode) == "Verify security code" && !verified()
    }
    val shown = shownSecurityCode()
    poll(what = "`bob is not verified` with `Mark verified`") { seesText("bob is not verified") && exists(Ui.VerifyCode.Mark) }
    bob sends "/code #team alice"
    bob.eventually("#team alice security code:")
    assertEquals(bob.nextLine().filterNot(Char::isWhitespace), shown)
    click(Ui.VerifyCode.Mark)
    poll(what = "the security code screen closed with bob verified and `View security code` in his member info") {
      !exists(Ui.VerifyCode.Mark) && verified() && exists(Ui.Info.VerifyCode) && textOf(Ui.Info.VerifyCode) == "View security code"
    }
    click(Ui.Info.VerifyCode)
    poll(what = "`bob is verified` with `Clear verification`") {
      seesText("bob is verified") && exists(described("Clear verification")) && !exists(Ui.VerifyCode.Mark)
    }
    click(described("Clear verification"))
    poll(what = "`bob is not verified` and `Mark verified` again, with bob not verified") {
      seesText("bob is not verified") && exists(Ui.VerifyCode.Mark) && !exists(described("Clear verification")) && !verified()
    }
    goBack()
    poll(what = "`Verify security code` back in bob's member info") {
      !exists(Ui.VerifyCode.Mark) && exists(Ui.Info.VerifyCode) && textOf(Ui.Info.VerifyCode) == "Verify security code"
    }
  }

  @Test fun memberInfo_changeRoleOfInvitedMember() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val info = emptyGroup(alice, "team").groupInfo()
    val inv = inviteMembers()
    poll(what = "bob offered") { inv.listed("bob") }
    inv.role(GroupMemberRole.Admin)
    poll(what = "`New member role` set to `admin`") { "admin" in inv.roleTexts() }
    inv.toggle("bob")
    inv.invite()
    bob.expectsMessage("#team: alice invites you to join the group as admin", CONNECT_TIMEOUT_MS)
    bob expects "use /j team to accept"
    poll(what = "bob listed as an invited admin") { info.memberTexts("bob").let { "invited" in it && "admin" in it } }
    val member = info.member("bob")
    poll(what = "bob's role picker at `admin`") { exists(Ui.MemberInfo.Role) && "admin" in textsOf(Ui.MemberInfo.Role) }
    member.pickRole(GroupMemberRole.Member)
    check(seesText("The role will be changed to \"member\". The member will receive a new invitation.")) {
      "the `Change role?` alert for an invited member does not say that a new invitation is sent"
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    bob.expectsMessage("#team: alice invites you to join the group as member", CONNECT_TIMEOUT_MS)
    bob expects "use /j team to accept"
    closeMemberInfo("bob")
    poll(what = "bob listed as an invited member without the `admin` label") { info.memberTexts("bob").let { "invited" in it && "admin" !in it } }
  }

  @Test fun memberInfo_roleReadOnlyForHigherRole() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob)
    connect(cath)
    bob.createGroup("team")
    bob sends "/a team alice admin"
    bob.eventually("invitation to join the group #team sent to alice")
    joinGroupFromList(alice, bob, "team")
    backToChatList()
    val info = alice.chatList.open("team").groupInfo()
    poll(what = "alice an admin of team") { groupInfoOf("team").membership.memberRole == GroupMemberRole.Admin }
    info.member("bob")
    poll(what = "the owner bob's role shown read-only, with no role picker and no `Remove member`") {
      seesText("Role") && seesText("owner") && !exists(Ui.MemberInfo.Role) && !exists(Ui.MemberInfo.Remove)
    }
    closeMemberInfo("bob")
    val inv = inviteMembers()
    poll(what = "cath offered") { inv.listed("cath") }
    click(Ui.InviteMembers.Role)
    poll(what = "`New member role` offering roles up to `admin`, without `owner`") {
      exists(Ui.InviteMembers.Role.option(GroupMemberRole.Admin)) && exists(Ui.InviteMembers.Role.option(GroupMemberRole.Observer)) &&
        !exists(Ui.InviteMembers.Role.option(GroupMemberRole.Owner))
    }
    click(Ui.InviteMembers.Role.option(GroupMemberRole.Admin))
    poll(what = "`New member role` set to `admin`") { "admin" in inv.roleTexts() }
    inv.toggle("cath")
    inv.invite()
    cath.expectsMessage("#team: alice invites you to join the group as admin", CONNECT_TIMEOUT_MS)
    cath expects "use /j team to accept"
    info.member("cath")
    poll(what = "a role picker at `admin` for the invited cath") { exists(Ui.MemberInfo.Role) && "admin" in textsOf(Ui.MemberInfo.Role) }
    click(Ui.MemberInfo.Role)
    poll(what = "cath's roles offered up to `admin`, without `owner`") {
      exists(Ui.MemberInfo.Role.option(GroupMemberRole.Member)) && exists(Ui.MemberInfo.Role.option(GroupMemberRole.Admin)) &&
        !exists(Ui.MemberInfo.Role.option(GroupMemberRole.Owner))
    }
    click(Ui.MemberInfo.Role.option(GroupMemberRole.Admin))
    poll(what = "the role list closed without a `Change role?` alert") { !exists(Ui.MemberInfo.Role.option(GroupMemberRole.Member)) && !seesText("Change role?") }
  }

  @Test fun memberInfo_blockedByAdminShownToMember() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = memberOfHostedGroup(alice, bob, cath)
    cath sends "#team spam from cath"
    bob.eventuallyMessage("#team cath> spam from cath", CONNECT_TIMEOUT_MS)
    val spam = chat.itemId("cath's `spam from cath`") { it.content.text == "spam from cath" }
    poll(what = "cath's message shown with its text") { textsOf(Ui.Chat.Item(spam)).let { t -> t.any { "spam from cath" in it } && "moderated by bob" !in t } }
    bob sends "\\\\ #team @cath spam from cath"
    bob.eventually("message marked deleted by you")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "cath's message shown as `moderated by bob`") {
      textsOf(Ui.Chat.Item(spam)).let { t -> "moderated by bob" in t && t.none { "spam from cath" in it } }
    }
    val info = chat.groupInfo()
    poll(what = "cath listed in team's members, not blocked") { info.memberTexts("cath").let { "cath" in it && "blocked" !in it } }
    val cathId = openMemberMenu("cath")
    poll(what = "a member's menu for cath with `Block member` and without the moderator actions") {
      exists(Ui.GroupInfo.MemberMenu(cathId, "block")) && "Block member" in textsOf(Ui.GroupInfo.MemberMenu(cathId, "block")) &&
        !exists(Ui.GroupInfo.MemberMenu(cathId, "blockForAll")) && !exists(Ui.GroupInfo.MemberMenu(cathId, "remove"))
    }
    click(Ui.GroupInfo.MemberMenu(cathId, "block"))
    confirmAlert("Block member?", "All new messages from cath will be hidden!", "Block")
    poll(what = "cath listed as `blocked`") { "blocked" in info.memberTexts("cath") }
    openMemberMenu("cath")
    poll(what = "`Unblock member` in place of `Block member` in cath's menu") {
      exists(Ui.GroupInfo.MemberMenu(cathId, "unblock")) && !exists(Ui.GroupInfo.MemberMenu(cathId, "block"))
    }
    click(Ui.GroupInfo.MemberMenu(cathId, "unblock"))
    confirmAlert("Unblock member?", "Messages from cath will be shown!", "Unblock")
    poll(what = "cath no longer listed as `blocked`") { "blocked" !in info.memberTexts("cath") }
    info.member("cath")
    poll(what = "`Block member` in cath's member info") { exists(Ui.MemberInfo.Block) && !seesText("Blocked by admin") }
    bob sends "/block for all #team cath"
    bob.eventually("#team: you blocked cath (signed)")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "cath's member info showing `Blocked by admin` in place of `Block member`") {
      seesText("Blocked by admin") && !exists(Ui.MemberInfo.Block) && !exists(Ui.MemberInfo.Unblock)
    }
    closeMemberInfo("cath")
    poll(what = "cath listed as `blocked`") { "blocked" in info.memberTexts("cath") }
    openMemberMenu("cath")
    check(!exists(Ui.GroupInfo.MemberMenu(cathId, "block")) && !exists(Ui.GroupInfo.MemberMenu(cathId, "unblock"))) {
      "a member's menu offers blocking cath, who is blocked by admin"
    }
  }

  @Test fun memberInfo_callButtonsDirectMessagesProhibitedAlert() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val info = emptyGroup(alice, "team").groupInfo()
    check(groupInfoOf("team").fullGroupPreferences.directMessages.enable == GroupFeatureEnabled.OFF) { "direct messages are on in a new group" }
    val link = info.createLink()
    bob.joinsViaLink(link, "team")
    memberConnected(info, "bob")
    info.member("bob")
    for (button in listOf(Ui.MemberInfo.Call, Ui.MemberInfo.Video)) {
      click(button)
      alertSaying("Can't call group member", "Direct messages between members are prohibited in this group.")
    }
    closeMemberInfo("bob")
    groupInfoRow(Ui.GroupInfo.Preferences)
    val prefs = GroupPreferencesScreen(this)
    prefs.toggle(GroupFeature.DirectMessages)
    prefs.save()
    poll(what = "direct messages allowed in team") { groupInfoOf("team").fullGroupPreferences.directMessages.enable == GroupFeatureEnabled.ON }
    prefs.backClosing()
    info.member("bob")
    for (button in listOf(Ui.MemberInfo.Call, Ui.MemberInfo.Video)) {
      click(button)
      alertSaying("Can't call group member", "Send message to enable calls.")
    }
  }

  @Test fun memberInfo_switchAddress() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = ownGroup(alice, "team", bob)
    val info = chat.groupInfo()
    memberConnected(info, "bob")
    val smp = localSmpServer()
    info.member("bob")
    memberConnectionLoaded()
    poll(what = "`Receiving via` and `Sending via` the local server, `Network status` and `Change receiving address` in bob's member info") {
      exists(Ui.Info.SendingVia) && exists(described("Network status")) && exists(Ui.Info.SwitchAddress) && !seesText(ABORT_SWITCH) &&
        textsOf(Ui.Info.ReceivingVia).let { "Receiving via" in it && it.any { t -> smp.hostnames.first() in t } } &&
        textsOf(Ui.Info.SendingVia).let { "Sending via" in it && it.any { t -> smp.hostnames.first() in t } }
    }
    val receiving = copiedServer(Ui.Info.ReceivingVia)
    check(isServer(receiving, smp)) { "`Receiving via` copied `$receiving`, not the local server $smp" }
    val sending = copiedServer(Ui.Info.SendingVia)
    check(isServer(sending, smp)) { "`Sending via` copied `$sending`, not the local server $smp" }
    check(!seesText(NETWORK_STATUS_TEXT)) { "the network status explanation is shown before `Network status` is clicked" }
    click(described("Network status"))
    alertSaying("Network status", NETWORK_STATUS_TEXT)
    click(Ui.Info.SwitchAddress)
    poll(what = "the `Change receiving address?` alert with `Change`") {
      seesText("Change receiving address?") &&
        seesText("Receiving address will be changed to a different server. Address change will complete after sender comes online.") &&
        "Change" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    val bobRow = Ui.GroupInfo.Member(groupMemberId("bob"))
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "member info closed to group info after `Change`") { !exists(Ui.Info.SwitchAddress) && exists(bobRow) }
    bob.eventually("#team: alice started changing address for you", CONNECT_TIMEOUT_MS)
    bob.eventually("#team: alice changed address for you", CONNECT_TIMEOUT_MS)
    info.close()
    chat.itemId("`you changed address for bob` in team") { it.content is CIContent.SndConnEventContent && it.content.text == "you changed address for bob" }
    bob sends "#team after switch"
    chat.itemId("bob's `after switch` on the new address") { it.content.text == "after switch" }
  }

  @KnownAppIssue("15.5 item 21", "cath's loaded member info offering memberInfo.deleteMessages and not memberInfo.remove")
  @Test fun memberInfo_deleteMessagesOfLeftMember() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = ownGroupWithTwo(alice, bob, cath)
    bob sends "#team bob stays"
    cath.eventuallyMessage("#team bob> bob stays", CONNECT_TIMEOUT_MS)
    cath sends "#team cath was here"
    bob.eventuallyMessage("#team cath> cath was here", CONNECT_TIMEOUT_MS)
    val stays = chat.itemId("bob's `bob stays`") { it.content.text == "bob stays" }
    val msg = chat.itemId("cath's `cath was here`") { it.content.text == "cath was here" }
    cath sends "/leave #team"
    cath.eventually("#team: you left the group")
    bob.eventually("#team: cath left the group (signed)", CONNECT_TIMEOUT_MS)
    val left = chat.itemId("cath's `left` event in team") { (it.content as? CIContent.RcvGroupEventContent)?.rcvGroupEvent is RcvGroupEvent.MemberLeft }
    poll(what = "cath's `left` event shown, her message still shown with its text") {
      textsOf(Ui.Chat.Item(left)).any { "left" in it } && textsOf(Ui.Chat.Item(msg)).any { "cath was here" in it }
    }
    click(Ui.Chat.Item(msg).part("avatar"))
    awaitAppIdle("opening cath's member info")
    poll(what = "cath's loaded member info offering ${Ui.MemberInfo.DeleteMessages.tag} and not ${Ui.MemberInfo.Remove.tag}") {
      exists(Ui.MemberInfo.DeleteMessages) && !exists(Ui.MemberInfo.Remove)
    }
    click(Ui.MemberInfo.DeleteMessages)
    confirmAlert("Delete member messages?", "Member messages will be deleted - this cannot be undone!", "Delete messages")
    bob.eventually("#team: alice removed cath from the group with all messages (signed)", CONNECT_TIMEOUT_MS)
    poll(what = "member info closed with cath's message shown as `moderated by alice` and bob's kept") {
      !exists(Ui.MemberInfo.DeleteMessages) && !ModalManager.end.hasModalsOpen() &&
        textsOf(Ui.Chat.Item(msg)).let { t -> "moderated by alice" in t && t.none { "cath was here" in it } } &&
        textsOf(Ui.Chat.Item(stays)).any { "bob stays" in it }
    }
  }

  private companion object {
    const val NETWORK_STATUS_TEXT = "You are connected to the server used to receive messages from this connection."
    const val ABORT_SWITCH = "Abort changing address"
  }
}
