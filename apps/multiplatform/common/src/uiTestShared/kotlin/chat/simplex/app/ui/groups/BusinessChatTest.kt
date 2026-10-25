package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.closeMemberInfo
import chat.simplex.app.ui.harness.businessChat
import chat.simplex.app.ui.harness.businessTypeOf
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.connectPeers
import chat.simplex.app.ui.harness.createAddress
import chat.simplex.app.ui.harness.createBusinessAddress
import chat.simplex.app.ui.harness.deleteBusinessChatFromInfo
import chat.simplex.app.ui.harness.enterLinkInNewChatSearch
import chat.simplex.app.ui.harness.groupInfoOf
import chat.simplex.app.ui.harness.inviteMembers
import chat.simplex.app.ui.harness.leaveBusinessChatFromInfo
import chat.simplex.app.ui.harness.openNewChatFromSearch
import chat.simplex.app.ui.harness.planConfirm
import chat.simplex.app.ui.harness.planProfileName
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.app.ui.harness.switchAwayFromChat
import chat.simplex.common.model.BusinessChatType
import chat.simplex.common.model.CIContent
import chat.simplex.common.model.GroupMemberRole
import chat.simplex.common.model.GroupMemberStatus
import chat.simplex.common.model.RcvGroupEvent
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

class BusinessChatTest : UiTest() {
  @Test fun business_addTeamMember() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val address = createBusinessAddress(alice)
    connect(cath)
    bob sends "/c $address"
    bob expects "connection request sent!"
    bob.eventually("#alice: joining the group...", CONNECT_TIMEOUT_MS)
    bob.eventually("#alice: you joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "a customer business chat named bob") { businessTypeOf("bob") == BusinessChatType.Customer }
    quiesce()
    val chat = alice.chatList.open("bob")
    val info = chat.groupInfo()
    poll(what = "`Chat preferences` with the chat owners footer in the business chat's info") {
      seesText("Chat preferences") && seesText("Only chat owners can change preferences.") && !seesText("Group preferences")
    }
    check(!exists(Ui.GroupInfo.CreateLink) && !exists(Ui.GroupInfo.GroupLink) && !exists(Ui.GroupInfo.MemberSupport) &&
      !exists(Ui.GroupInfo.EditProfile) && !exists(Ui.GroupInfo.WelcomeMessage)) { "the business chat's info offers a group link, `Chats with members`, profile or welcome editing" }
    scrollTo(Ui.GroupInfo.AddMembers)
    check("Add team members" in textsOf(Ui.GroupInfo.AddMembers)) { "the business owner's add button is not `Add team members`: ${textsOf(Ui.GroupInfo.AddMembers)}" }
    val invite = inviteMembers()
    invite.toggle("cath")
    invite.invite()
    cath.eventually("#bob: alice invites you to join the group as member", CONNECT_TIMEOUT_MS)
    cath expects "use /j bob to accept"
    cath sends "/j bob"
    cath.eventuallyAll(lines = listOf("#bob: you joined the group", "#bob: member bob_1 is connected"), timeoutMs = CONNECT_TIMEOUT_MS)
    bob.eventuallyAll(lines = listOf("#alice: alice_1 added cath to the group (connecting...)", "#alice: new member cath is connected"), timeoutMs = CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "cath listed as connected in the business chat") { info.memberTexts("cath").let { "connected" in it || "complete" in it } }
    info.close()
    chat.send("welcome from the team")
    bob.eventuallyMessage("#alice alice_1> welcome from the team", CONNECT_TIMEOUT_MS)
    cath.eventuallyMessage("#bob alice> welcome from the team", CONNECT_TIMEOUT_MS)
    bob sends "#alice thanks"
    cath.eventuallyMessage("#bob bob_1> thanks", CONNECT_TIMEOUT_MS)
    chat.itemId("bob's `thanks` in the business chat") { it.content.text == "thanks" }
    chat.groupInfo()
    deleteBusinessChatFromInfo("bob")
    bob.eventuallyMatching(Regex("#alice: alice_1 deleted the group( \\(signed\\))?"), CONNECT_TIMEOUT_MS)
    cath.eventuallyMatching(Regex("#bob: alice deleted the group( \\(signed\\))?"), CONNECT_TIMEOUT_MS)
    alice.chatList.gone("bob")
  }

  @Test fun customer_connectToBusinessAddress() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = bob.createAddress(business = true)
    alice.chatList.row("Private notes")
    check(chatModel.chats.value.none { it.chatInfo.chatViewName == "bob" }) { "a chat named bob exists before the address is opened" }
    val chat = openNewChatFromSearch(address, "bob")
    val bizId = chatModel.chatId.value!!
    poll(what = "a prepared business chat with `Tap Connect to chat`, a message field and `Connect`") {
      businessTypeOf("bob") == BusinessChatType.Business && seesText("Tap Connect to chat") &&
        exists(Ui.Chat.Message) && exists(Ui.Chat.Connect) && "Connect" in textsOf(Ui.Chat.Connect)
    }
    click(Ui.Chat.Connect)
    bob.eventually("#alice: accepting business address request...", CONNECT_TIMEOUT_MS)
    bob.eventually("#alice: alice_1 joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the banner `Business connection` with `Connect` gone and alice connected") {
      seesText("Business connection") && !seesText("Tap Connect to chat") && !exists(Ui.Chat.Connect) && exists(Ui.Chat.Message) &&
        groupInfoOf("bob").membership.memberStatus == GroupMemberStatus.MemConnected
    }
    chat.send("hello business")
    bob.eventuallyMessage("#alice alice_1> hello business", CONNECT_TIMEOUT_MS)
    bob sends "#alice hi alice"
    chat.itemId("bob's `hi alice` in the business chat") { it.content.text == "hi alice" }
    switchAwayFromChat(alice)
    check(chatModel.chatId.value != bizId) { "the business chat is still open before the address is pasted again" }
    enterLinkInNewChatSearch(alice, address)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the known business alert for bob with `Open chat` and no member role") {
      exists(planConfirm) && "Open chat" in textsOf(planConfirm) && textOf(planProfileName) == "bob" && !seesText("You are a member")
    }
    click(planConfirm)
    poll(what = "the New chat sheet closed with the business chat open and still one chat named bob") {
      !exists(planConfirm) && !exists(Ui.NewChat.Search) && chatModel.chatId.value == bizId && chatModel.chats.value.count { it.chatInfo.chatViewName == "bob" } == 1
    }
  }

  @KnownAppIssue("15.5 item 14", "waiting for the customer's membership left in the model", intermittent = true)
  @Test fun customer_seesTeamMemberAdded() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    businessChat(bob)
    connectPeers(bob, cath)
    val chat = alice.chatList.open("bob")
    bob sends "/a #alice cath"
    bob.eventually("invitation to join the group #alice sent to cath")
    cath.eventually("#alice: bob invites you to join the group as member", CONNECT_TIMEOUT_MS)
    cath expects "use /j alice to accept"
    cath sends "/j alice"
    cath.eventuallyAll(lines = listOf("#alice: you joined the group", "#alice: member alice_1 is connected"), timeoutMs = CONNECT_TIMEOUT_MS)
    bob.eventually("#alice: cath joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's `invited cath` event in the business chat") {
      chatModel.chatsContext.chatItems.value.any { ((it.content as? CIContent.RcvGroupEventContent)?.rcvGroupEvent as? RcvGroupEvent.MemberAdded)?.profile?.displayName == "cath" }
    }
    cath sends "#alice hello from the team"
    chat.itemId("cath's `hello from the team` in the business chat") { it.content.text == "hello from the team" }
    val info = chat.groupInfo()
    poll(what = "`Chat preferences` with the chat owners footer in the customer's chat info") {
      seesText("Chat preferences") && seesText("Only chat owners can change preferences.") && !seesText("Group preferences")
    }
    check(!exists(Ui.GroupInfo.EditProfile) && !exists(Ui.GroupInfo.MemberSupport) && !exists(Ui.GroupInfo.CreateLink)) { "a customer's chat info offers profile editing, `Chats with members` or a group link" }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "cath listed as connected in the business chat") { info.memberTexts("cath").let { "connected" in it || "complete" in it } }
    check(info.ownMemberTexts().isNotEmpty() && !exists(Ui.GroupInfo.AddMembers)) { "a customer's member list offers `Add friends`" }
    leaveBusinessChatFromInfo("bob")
    bob.eventuallyMatching(Regex("#alice: alice_1 left the group( \\(signed\\))?"), CONNECT_TIMEOUT_MS)
    cath.eventuallyMatching(Regex("#alice: alice_1 left the group( \\(signed\\))?"), CONNECT_TIMEOUT_MS)
    poll(what = "`you left` in the business chat's compose field") { seesText("you left") }
    poll(what = "the customer's membership left in the model") { groupInfoOf("bob").membership.memberStatus == GroupMemberStatus.MemLeft }
    click(Ui.Chat.Message)
    poll(what = "the `You can't send messages!` alert for the left customer") { seesText("You can't send messages!") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  }

  @Test fun business_promoteCustomerToAdmin() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = createBusinessAddress(alice)
    bob sends "/c $address"
    bob expects "connection request sent!"
    bob.eventually("#alice: you joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "a customer business chat named bob") { businessTypeOf("bob") == BusinessChatType.Customer }
    quiesce()
    val info = alice.chatList.open("bob").groupInfo()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the customer bob listed as connected, without a role label") {
      info.memberTexts("bob").let { ("connected" in it || "complete" in it) && "admin" !in it }
    }
    val member = info.member("bob")
    member.pickRole(GroupMemberRole.Admin)
    check(seesText("The role will be changed to \"admin\". Everyone in the chat will be notified.")) {
      "the `Change role?` alert of a business chat does not use the chat wording"
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    bob.eventuallyMatching(Regex("#alice: \\S+ changed your role from member to admin \\(signed\\)"), CONNECT_TIMEOUT_MS)
    poll(what = "the `Change role?` alert closed") { !seesText("Change role?") }
    closeMemberInfo("bob")
    poll(what = "bob listed as `admin` in the business chat's members") { "admin" in info.memberTexts("bob") }
    info.member("bob")
    click(Ui.MemberInfo.Remove)
    poll(what = "the `Remove member?` alert saying bob will be removed from the chat") {
      seesText("Remove member?") && seesText("Member will be removed from chat - this cannot be undone!") &&
        exists(Ui.Alert.Option(Ui.AlertSource.Shared, "remove"))
    }
    click(Ui.Alert.Option(Ui.AlertSource.Shared, "remove"))
    bob.eventuallyMatching(Regex("#alice: \\S+ removed you from the group( \\(signed\\))?"), CONNECT_TIMEOUT_MS)
    poll(what = "bob no longer a current member of the business chat") {
      chatModel.groupMembers.value.none { it.memberProfile.displayName == "bob" && it.memberCurrent }
    }
  }
}
