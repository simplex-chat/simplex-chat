package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.PeerFlags
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.connectionRequestSentToGroup
import chat.simplex.app.ui.harness.createFullGroupLink
import chat.simplex.app.ui.harness.createGroupLink
import chat.simplex.app.ui.harness.createShortAndFullGroupLink
import chat.simplex.app.ui.harness.createWelcomeGroupLink
import chat.simplex.app.ui.harness.emptyGroup
import chat.simplex.app.ui.harness.enterLinkInNewChatSearch
import chat.simplex.app.ui.harness.enterLinkInSearch
import chat.simplex.app.ui.harness.groupChatIds
import chat.simplex.app.ui.harness.groupInfoOf
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.openChatById
import chat.simplex.app.ui.harness.openPreparedGroupFromSearch
import chat.simplex.app.ui.harness.pendingConnectionIds
import chat.simplex.app.ui.harness.planConfirm
import chat.simplex.app.ui.harness.planProfileName
import chat.simplex.app.ui.harness.planOption
import chat.simplex.app.ui.harness.switchAwayFromChat
import chat.simplex.common.model.CIDirection
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.GroupMemberStatus
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotEquals
import org.junit.Test

class JoiningTest : UiTest() {
  @Test fun joinGroup_pasteShortLinkPrepareAndJoin() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = bob.createWelcomeGroupLink("team", "Welcome!")
    bob.eventually("to delete it: /delete link #team (joined members will remain connected to you)")
    alice.chatList.row("Private notes")
    check(groupChatIds("team").isEmpty()) { "a group named team exists before the link is opened" }
    val chat = openPreparedGroupFromSearch(link, "team")
    val teamId = chatModel.chatId.value!!
    chat.itemId("Welcome!")
    poll(what = "the banner `Tap Join group` and a `Join group` button with no message field") {
      seesText("Tap Join group") && exists(Ui.Chat.Connect) && "Join group" in textsOf(Ui.Chat.Connect) && !exists(Ui.Chat.Message)
    }
    click(Ui.Chat.Connect)
    bob.eventually("alice: accepting request to join group #team...", CONNECT_TIMEOUT_MS)
    bob.eventually("#team: alice joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the message field in place of `Join group` with alice connected") {
      exists(Ui.Chat.Message) && !exists(Ui.Chat.Connect) && !seesText("Tap Join group") &&
        groupInfoOf("team").membership.memberStatus == GroupMemberStatus.MemConnected
    }
    chat.send("hi team")
    bob.eventuallyMessage("#team alice> hi team", CONNECT_TIMEOUT_MS)
    switchAwayFromChat(alice)
    check(chatModel.chatId.value != teamId) { "team is still the open chat before the link is pasted again" }
    enterLinkInNewChatSearch(alice, link)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the known-group alert for team saying `You are a member` with `Open group`") {
      exists(planConfirm) && "Open group" in textsOf(planConfirm) && textOf(planProfileName) == "team" && seesText("You are a member")
    }
    click(planConfirm)
    poll(what = "the New chat sheet closed with team's chat open and still one group named team") {
      !exists(planConfirm) && !exists(Ui.NewChat.Search) && chatModel.chatId.value == teamId && groupChatIds("team") == listOf(teamId)
    }
  }

  @Test fun joinGroup_preparedJoinIncognito() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = bob.createGroupLink("team")
    bob.eventually("to delete it: /delete link #team (joined members will remain connected to you)")
    val aliceId = chatModel.currentUser.value!!.userId
    alice.chatList.row("Private notes")
    val chat = openPreparedGroupFromSearch(link, "team")
    poll(what = "`Your profile` with alice selected and no `Incognito` option") {
      seesText("Your profile") && exists(Ui.Chat.PickerUser(aliceId)) && !exists(Ui.Chat.PickerIncognito)
    }
    click(Ui.Chat.PickerUser(aliceId))
    poll(what = "the profile list with `Incognito`") { exists(Ui.Chat.PickerIncognito) }
    click(Ui.Chat.PickerIncognito)
    poll(what = "the list collapsed to `Incognito`") { exists(Ui.Chat.PickerIncognito) && !exists(Ui.Chat.PickerUser(aliceId)) }
    click(Ui.Chat.Connect)
    val incognito = bob.eventuallyMatching(Regex("(\\S+): accepting request to join group #team\\.\\.\\."), CONNECT_TIMEOUT_MS).groupValues[1]
    assertNotEquals("alice", incognito)
    bob.eventually("#team: $incognito joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice connected to team with an incognito membership") {
      groupInfoOf("team").membership.let { it.memberStatus == GroupMemberStatus.MemConnected && it.memberIncognito } && exists(Ui.Chat.Message)
    }
    val info = chat.groupInfo()
    poll(what = "alice's own member row named `$incognito`, not `alice`") {
      info.ownMemberTexts().let { incognito in it && "alice" !in it }
    }
  }

  @Test fun joinGroup_fullLinkCurrentProfile() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(fullLinks = true))
    val full = bob.createFullGroupLink("team")
    alice.chatList.row("Private notes")
    enterLinkInSearch(full)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Join group?` alert for the full group link") {
      seesText("Join group?") && seesText("You will connect to all group members.", substring = true) &&
        exists(planOption("useCurrentProfile")) && exists(planOption("useIncognito")) && exists(planOption("cancel"))
    }
    click(planOption("cancel"))
    poll(what = "the alert to close and the search to clear") {
      !seesText("Join group?") && editableText(Ui.ChatList.Search).isNullOrEmpty() && isEnabled(Ui.ChatList.Search)
    }
    check(pendingConnectionIds().isEmpty() && chatModel.chats.value.none { it.chatInfo is ChatInfo.Group }) { "Cancel created a pending connection or a group" }
    enterLinkInSearch(full)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Join group?` alert again") { seesText("Join group?") }
    click(planOption("useCurrentProfile"))
    connectionRequestSentToGroup()
    assertEquals("alice: accepting request to join group #team...", bob.nextLine(CONNECT_TIMEOUT_MS))
    bob.eventually("#team: alice joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice connected to team with no pending connection left") {
      groupChatIds("team").size == 1 && groupInfoOf("team").membership.memberStatus == GroupMemberStatus.MemConnected && pendingConnectionIds().isEmpty()
    }
    backToChatList()
    alice.chatList.open("team").send("joined via the full link")
    bob.eventuallyMessage("#team alice> joined via the full link", CONNECT_TIMEOUT_MS)
  }

  @Test fun connectPlan_ownGroupLinkJoinAsSecondMember() = uiTest {
    val alice = app("alice")
    val info = emptyGroup(alice, "team").groupInfo()
    val link = info.createLink()
    info.close()
    val teamId = alice.chatList.chatId("team")
    switchAwayFromChat(alice)
    check(chatModel.chatId.value != teamId) { "team is still the open chat before its link is pasted" }
    enterLinkInNewChatSearch(alice, link)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Join your group?` alert with `Open group` and `Use current profile`") {
      seesText("Join your group?") && seesText("This is your link for group", substring = true) &&
        exists(planOption("openGroup")) && exists(planOption("useCurrentProfile"))
    }
    click(planOption("openGroup"))
    poll(what = "the alert and the New chat sheet to close with team's chat open") {
      !seesText("Join your group?") && !exists(Ui.NewChat.Search) && chatModel.chatId.value == teamId
    }
    check(groupChatIds("team") == listOf(teamId)) { "`Open group` created another group named team" }
    backToChatList()
    enterLinkInNewChatSearch(alice, link)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Join your group?` alert again") { seesText("Join your group?") && exists(planOption("useCurrentProfile")) }
    click(planOption("useCurrentProfile"))
    connectionRequestSentToGroup()
    var secondId: String? = null
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "a second group named team, listed, with alice connected as herself") {
      secondId = groupChatIds("team").singleOrNull { it != teamId }
      val second = chatModel.chats.value.firstOrNull { it.id == secondId }?.chatInfo as? ChatInfo.Group
      second != null && second.groupInfo.membership.memberStatus == GroupMemberStatus.MemConnected && !second.groupInfo.membership.memberIncognito &&
        exists(Ui.ChatList.Row(secondId!!))
    }
    backToChatList()
    openChatById(teamId).send("to my other self")
    backToChatList()
    openChatById(secondId!!).itemId("`to my other self` received in the second team") { it.content.text == "to my other self" && it.chatDir is CIDirection.GroupRcv }
  }

  @Test fun connectPlan_connectingAllowsReconnect() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(fullLinks = true))
    val full = bob.createFullGroupLink("team")
    bob.stop()
    alice.chatList.row("Private notes")
    enterLinkInSearch(full)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Join group?` alert for the full group link") { seesText("Join group?") && exists(planOption("useCurrentProfile")) }
    click(planOption("useCurrentProfile"))
    connectionRequestSentToGroup()
    poll(what = "one pending connection") { pendingConnectionIds().size == 1 }
    enterLinkInSearch(full)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Repeat join request?` with both profile options while bob is offline") {
      seesText("Repeat join request?") && seesText("You are already joining the group via this link.", substring = true) &&
        exists(planOption("useCurrentProfile")) && exists(planOption("useIncognito")) && !seesText("Join group?")
    }
    click(planOption("cancel"))
    poll(what = "the alert to close with still one pending connection") {
      !seesText("Repeat join request?") && editableText(Ui.ChatList.Search).isNullOrEmpty() && pendingConnectionIds().size == 1
    }
    bob.start()
    bob.eventually("alice: accepting request to join group #team...", CONNECT_TIMEOUT_MS)
    bob.eventually("#team: alice joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "one group team with alice connected and no pending connection") {
      groupChatIds("team").size == 1 && groupInfoOf("team").membership.memberStatus == GroupMemberStatus.MemConnected && pendingConnectionIds().isEmpty()
    }
  }

  @Test fun groupInvitation_deleteInvitation() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    hostGroup(bob, "club", "alice")
    alice.chatList.row("team")
    alice.chatList.row("club")
    poll(what = "alice invited to team and club") {
      listOf("team", "club").all { groupInfoOf(it).membership.memberStatus == GroupMemberStatus.MemInvited }
    }
    alice.chatList.rowMenu("team", "delete")
    poll(what = "the `Delete group?` alert for team saying it is deleted only for alice, with `Delete`") {
      seesText("Delete group?") && seesText("team\n\nGroup will be deleted for you - this cannot be undone!") &&
        "Delete" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    alice.chatList.gone("team")
    check(alice.chatList.shown("club") && groupInfoOf("club").membership.memberStatus == GroupMemberStatus.MemInvited) { "deleting team's invitation removed or changed club's" }
    alice.chatList.rowMenu("club", "join", "Join")
    assertEquals("#club: alice joined the group", bob.nextLine(CONNECT_TIMEOUT_MS))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice connected to club") { groupInfoOf("club").membership.memberStatus == GroupMemberStatus.MemConnected }
  }

  @Test fun joinGroup_fullLinkIncognito() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(fullLinks = true))
    val full = bob.createFullGroupLink("team")
    alice.chatList.row("Private notes")
    enterLinkInSearch(full)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Join group?` alert with `Use new incognito profile`") {
      seesText("Join group?") && exists(planOption("useIncognito")) && exists(planOption("useCurrentProfile"))
    }
    click(planOption("useIncognito"))
    connectionRequestSentToGroup()
    val incognito = bob.eventuallyMatching(Regex("(\\S+): accepting request to join group #team\\.\\.\\."), CONNECT_TIMEOUT_MS).groupValues[1]
    assertNotEquals("alice", incognito)
    bob.eventually("#team: $incognito joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice connected to team with the incognito profile `$incognito`") {
      groupChatIds("team").size == 1 && groupInfoOf("team").membership.let {
        it.memberStatus == GroupMemberStatus.MemConnected && it.memberIncognito && it.memberProfile.displayName == incognito
      }
    }
    backToChatList()
    alice.chatList.open("team").send("hi from incognito")
    bob.eventuallyMessage("#team $incognito> hi from incognito", CONNECT_TIMEOUT_MS)
  }

  @KnownAppIssue("15.5 item 20", "the `Group already exists!` alert for the group being joined")
  @Test fun connectPlan_connectingProhibit() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(fullLinks = true))
    val (short, full) = bob.createShortAndFullGroupLink("team")
    bob.stop()
    alice.chatList.row("Private notes")
    openPreparedGroupFromSearch(short, "team")
    val teamId = chatModel.chatId.value!!
    poll(what = "`Join group` in the prepared group") { exists(Ui.Chat.Connect) && "Join group" in textsOf(Ui.Chat.Connect) }
    click(Ui.Chat.Connect)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the join request sent while bob is offline, with alice not yet connected") {
      groupInfoOf("team").let { !it.nextConnectPrepared && it.membership.memberStatus != GroupMemberStatus.MemConnected }
    }
    switchAwayFromChat(alice)
    check(chatModel.chatId.value != teamId) { "team is still the open chat before the full link is pasted" }
    enterLinkInSearch(full)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Group already exists!` alert for the group being joined") {
      seesText("Group already exists!") && seesText("You are already joining the group", substring = true) && exists(planConfirm) &&
        !seesText("Join group?") && !seesText("Repeat join request?")
    }
    click(planConfirm)
    poll(what = "the alert closed with still one team and no pending connection") {
      !seesText("Group already exists!") && editableText(Ui.ChatList.Search).isNullOrEmpty() && groupChatIds("team") == listOf(teamId) &&
        pendingConnectionIds().isEmpty()
    }
    bob.start()
    bob.eventually("alice: accepting request to join group #team...", CONNECT_TIMEOUT_MS)
    bob.eventually("#team: alice joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice connected to the one team") {
      groupChatIds("team") == listOf(teamId) && groupInfoOf("team").membership.memberStatus == GroupMemberStatus.MemConnected
    }
  }
}
