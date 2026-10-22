package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.ChatScreen
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.closeGroupLink
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.connectIncognito
import chat.simplex.app.ui.harness.contactOf
import chat.simplex.app.ui.harness.coreGroupLinkRole
import chat.simplex.app.ui.harness.coreMemberContactId
import chat.simplex.app.ui.harness.described
import chat.simplex.app.ui.harness.emptyGroup
import chat.simplex.app.ui.harness.groupInfoOf
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.openNewGroupLink
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.app.ui.harness.pickGroupLinkRole
import chat.simplex.app.ui.harness.sharedGroupLink
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.GroupMemberRole
import chat.simplex.common.model.GroupMemberStatus
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.ModalManager
import org.junit.Assert.assertNotEquals
import org.junit.Test

class GroupLinksTest : UiTest() {
  @Test fun groupLink_cliJoinsViaShortLink() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val info = emptyGroup(alice, "team").groupInfo()
    poll(what = "alice alone in team's members") { "you: creator" in info.ownMemberTexts() && info.memberTexts("bob").isEmpty() }
    val link = info.createLink()
    bob sends "/c $link"
    bob expects "connection request sent!"
    bob.eventually("#team: joining the group...", CONNECT_TIMEOUT_MS)
    bob.eventually("#team: you joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob listed as connected in team's members") { "connected" in info.memberTexts("bob") }
  }

  @Test fun groupInvitation_incognitoContactJoinsIncognito() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val incognito = connectIncognito(bob)
    assertNotEquals("alice", incognito)
    hostGroup(bob, "team", incognito)
    alice.chatList.open("team")
    poll(what = "the `Join group?` alert with `Join incognito`") { seesText("Join group?") && "Join incognito" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    bob.eventually("#team: $incognito joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "membership of #team to be connected") {
      (alice.chatList.chat("team").chatInfo as ChatInfo.Group).groupInfo.membership.memberStatus == GroupMemberStatus.MemConnected
    }
    val info = alice.chatList.open("team").groupInfo()
    poll(what = "alice's own member row named `$incognito`, not `alice`") {
      info.ownMemberTexts().let { incognito in it && "alice" !in it && "you: connected" in it }
    }
  }

  @Test fun moderate_markDeletedForAll() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    val chat = ownGroup(alice, "team", bob, cath)
    bob.eventuallyMatching(Regex("#team: (new )?member cath is connected"), CONNECT_TIMEOUT_MS)
    cath.eventuallyMatching(Regex("#team: (new )?member bob is connected"), CONNECT_TIMEOUT_MS)
    bob sends "#team spam"
    cath.eventuallyMessage("#team bob> spam", CONNECT_TIMEOUT_MS)
    val id = chat.itemId("spam")
    poll(what = "bob's `spam` shown, not moderated") { textsOf(Ui.Chat.Item(id)).let { t -> t.any { "spam" in it } && t.none { "moderated by" in it } } }
    chat.menu("spam", "moderate")
    poll(what = "the `Delete member message?` alert saying the message will be marked as moderated, with `Delete`") {
      seesText("Delete member message?") && seesText("The message will be marked as moderated for all members.") &&
        "Delete" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    cath.eventuallyMessage("#team bob> [marked deleted by alice] spam", CONNECT_TIMEOUT_MS)
    bob.eventuallyMessage("#team bob> [marked deleted by alice] spam", CONNECT_TIMEOUT_MS)
    poll(what = "`moderated by alice` in place of `spam`") { textsOf(Ui.Chat.Item(id)).let { t -> "moderated by alice" in t && t.none { "spam" in it } } }
  }

  @Test fun memberInfo_blockForAllNotifiesMembers() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    val chat = ownGroup(alice, "team", bob, cath)
    bob.eventuallyMatching(Regex("#team: (new )?member cath is connected"), CONNECT_TIMEOUT_MS)
    cath.eventuallyMatching(Regex("#team: (new )?member bob is connected"), CONNECT_TIMEOUT_MS)
    bob sends "#team before block"
    cath.eventuallyMessage("#team bob> before block", CONNECT_TIMEOUT_MS)
    val before = chat.itemId("before block")
    chat.groupInfo().member("bob").blockForAll()
    cath.eventually("#team: alice blocked bob (signed)", CONNECT_TIMEOUT_MS)
    poll(what = "bob's member info offering `Unblock for all`") { seesText("Unblock for all") }
    escape()
    poll(what = "member info to close to group info") { !exists(Ui.MemberInfo.Remove) }
    escape()
    poll(what = "group info to close to team's chat") { !ModalManager.end.hasModalsOpen() }
    bob sends "#team after block"
    val after = chat.itemId("after block")
    poll(what = "bob's new message shown as `blocked by admin`, his earlier one unchanged") {
      textsOf(Ui.Chat.Item(after)).let { t -> "blocked by admin" in t && t.none { "after block" in it } } &&
        textsOf(Ui.Chat.Item(before)).any { "before block" in it }
    }
    bob sends "#team still blocked"
    val again = chat.itemId("still blocked")
    poll(what = "bob's two blocked messages collapsed into `2 messages blocked by admin`") {
      "2 messages blocked by admin" in textsOf(Ui.Chat.Item(again)) && !exists(Ui.Chat.Item(after))
    }
    chat.send("after the block")
    bob.expectsMessage("#team alice> after the block", CONNECT_TIMEOUT_MS)
    cath.expectsMessage("#team alice> after the block", CONNECT_TIMEOUT_MS)
  }

  @Test fun groupLink_cliJoinsViaFullLink() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val info = emptyGroup(alice, "team").groupInfo()
    poll(what = "`Create group link` in team's info, without a `Group link` row") { exists(Ui.GroupInfo.CreateLink) && !exists(Ui.GroupInfo.GroupLink) }
    openNewGroupLink()
    poll(what = "the link created by itself, with the QR code, `Initial role`, `Share link` and `Delete link`, and no `Full link` toggle") {
      seesDescription("QR Code") && seesText("Initial role") && exists(Ui.GroupLink.Role) && exists(described("Delete link")) && !exists(Ui.Link.ShortLinkToggle)
    }
    val short = sharedGroupLink()
    check(FULL_CONTACT_LINK !in short) { "`Share link` shared the full link `$short` by default" }
    enableDeveloperToolsViaApi()
    poll(what = "the `Full link` toggle with developer tools on") { exists(Ui.Link.ShortLinkToggle) && "Full link" in textsOf(Ui.Link.ShortLinkToggle) }
    click(Ui.Link.ShortLinkToggle)
    poll(what = "the toggle reading `Short link`") { "Short link" in textsOf(Ui.Link.ShortLinkToggle) }
    val full = sharedGroupLink(previous = short)
    check(FULL_CONTACT_LINK in full) { "`Share link` with `Full link` shared `$full`, not the full link" }
    bob sends "/c $full"
    bob expects "connection request sent!"
    bob.eventually("#team: joining the group...", CONNECT_TIMEOUT_MS)
    bob.eventually("#team: you joined the group", CONNECT_TIMEOUT_MS)
    closeGroupLink()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob listed as connected in team's members") { "connected" in info.memberTexts("bob") }
  }

  @Test fun groupLink_initialRoleObserver() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = emptyGroup(alice, "team")
    val info = chat.groupInfo()
    val groupId = groupInfoOf("team").groupId
    openNewGroupLink()
    poll(what = "`Initial role` member") { "member" in textsOf(Ui.GroupLink.Role) && coreGroupLinkRole(groupId) == GroupMemberRole.Member }
    pickGroupLinkRole(GroupMemberRole.Observer)
    poll(what = "`observer` as the link's initial role in the core") { "observer" in textsOf(Ui.GroupLink.Role) && coreGroupLinkRole(groupId) == GroupMemberRole.Observer }
    val link = sharedGroupLink()
    bob sends "/c $link"
    bob expects "connection request sent!"
    bob.eventually("#team: you joined the group", CONNECT_TIMEOUT_MS)
    bob sends "#team hi there"
    bob expects "#team: you don't have permission to send messages"
    pickGroupLinkRole(GroupMemberRole.Member)
    poll(what = "`member` as the link's initial role in the core again") { "member" in textsOf(Ui.GroupLink.Role) && coreGroupLinkRole(groupId) == GroupMemberRole.Member }
    cath sends "/c $link"
    cath expects "connection request sent!"
    cath.eventually("#team: you joined the group", CONNECT_TIMEOUT_MS)
    cath sends "#team hello from cath"
    closeGroupLink()
    info.close()
    chat.itemId("cath's `hello from cath`") { it.content.text == "hello from cath" }
    check(chatModel.chatsContext.chatItems.value.none { it.content.text == "hi there" }) { "the observer bob's message reached the group" }
  }

  @Test fun groupLink_deleteKeepsMembers() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = emptyGroup(alice, "team")
    val info = chat.groupInfo()
    openNewGroupLink()
    val link = sharedGroupLink()
    bob sends "/c $link"
    bob expects "connection request sent!"
    bob.eventually("#team: you joined the group", CONNECT_TIMEOUT_MS)
    check(!seesText("Delete link?")) { "`Delete link?` is shown before `Delete link` is clicked" }
    click(described("Delete link"))
    poll(what = "the `Delete link?` alert saying members stay, with `Delete`") {
      seesText("Delete link?") && seesText("All group members will remain connected.") && "Delete" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the group link screen offering `Create link` after the deletion") { !exists(Ui.GroupLink.Share) && seesText("Create link") }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the deleted group link to fail its plan on the server") {
      cath sends "/_connect plan 1 $link"
      cath.eventuallyMatching(GROUP_LINK_PLAN).value.startsWith("error")
    }
    cath sends "/c $link"
    cath.eventuallyMatching(AUTH_FAILED)
    closeGroupLink()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob still connected in team's members and cath not listed") {
      "connected" in info.memberTexts("bob") && info.memberTexts("cath").isEmpty()
    }
    info.close()
    chat.send("still here")
    bob.expectsMessage("#team alice> still here", CONNECT_TIMEOUT_MS)
  }

  @Test fun groupLink_existingContactMergedWithMember() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val bobChatId = alice.chatList.chatId("bob")
    val chat = emptyGroup(alice, "team")
    val link = chat.groupInfo().createLink()
    bob sends "/c $link"
    bob expects "connection request sent!"
    bob.eventually("#team: you joined the group", CONNECT_TIMEOUT_MS)
    bob.eventually("contact and member are merged: alice, #team alice_1", CONNECT_TIMEOUT_MS)
    bob expects "use @alice <message> to send messages"
    val bobContactId = contactOf("bob").contactId
    val groupId = groupInfoOf("team").groupId
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice's core to link bob's member to his existing contact") { coreMemberContactId(groupId, "bob") == bobContactId }
    escape()
    poll(what = "group info to close") { !ModalManager.end.hasModalsOpen() }
    val info = chat.groupInfo()
    poll(what = "bob's member in reopened group info linked to his existing contact, with no second chat for bob") {
      chatModel.groupMembers.value.firstOrNull { it.memberProfile.displayName == "bob" }?.memberContactId == bobContactId &&
        chatModel.chats.value.count { (it.chatInfo as? ChatInfo.Direct)?.contact?.profile?.displayName == "bob" } == 1
    }
    info.member("bob").message()
    poll(what = "bob's existing chat open from his member info") { chatModel.chatId.value == bobChatId && !ModalManager.end.hasModalsOpen() }
    ChatScreen(this).send("via member info")
    bob.expectsMessage("alice> via member info", CONNECT_TIMEOUT_MS)
  }

  private fun enableDeveloperToolsViaApi() = appPrefs.developerTools.set(true)

  private companion object {
    const val FULL_CONTACT_LINK = "/contact#/?v="
    val GROUP_LINK_PLAN = Regex("group link: ok to connect.*|error: connection authorization failed.*")
    val AUTH_FAILED = Regex("error: connection authorization failed - .*")
  }
}
