package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.SupportChatScreen
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.acceptAlert
import chat.simplex.app.ui.harness.createGroupLink
import chat.simplex.app.ui.harness.emptyGroup
import chat.simplex.app.ui.harness.goBack
import chat.simplex.app.ui.harness.groupInfoOf
import chat.simplex.app.ui.harness.joinLink
import chat.simplex.app.ui.harness.pendingLinkJoin
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.common.model.GroupMemberStatus
import chat.simplex.common.model.MemberCriteria
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.ModalManager
import org.junit.Ignore
import org.junit.Test

class MemberAdmissionTest : UiTest() {
  @Test fun memberAdmission_unsavedChangesAlert() = uiTest {
    val alice = app("alice")
    val p = emptyGroup(alice, "team").groupInfo().preferences()
    var a = p.admission()
    poll(what = "`Review members` off") { "off" in a.review() }
    a.select(MemberCriteria.All)
    poll(what = "`Review members` all after selecting it") { "all" in a.review() }
    goBack()
    poll(what = "the `Save admission settings?` alert with `Save and notify group members` and `Exit without saving`") {
      seesText("Save admission settings?") && exists(Ui.Alert.Confirm(Ui.AlertSource.Shared)) &&
        "Save and notify group members" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) && "Exit without saving" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "member admission to close to group preferences after `Exit without saving`") {
      !seesText("Review members before admitting (\"knocking\").") && exists(Ui.GroupPrefs.MemberAdmission)
    }
    check(groupInfoOf("team").groupProfile.memberAdmission?.review == null) { "`Exit without saving` saved the admission setting" }
    a = p.admission()
    poll(what = "`Review members` still off after `Exit without saving`") { "off" in a.review() }
    a.select(MemberCriteria.All)
    goBack()
    poll(what = "the `Save admission settings?` alert") { seesText("Save admission settings?") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "member admission to close with review of all members saved") {
      groupInfoOf("team").groupProfile.memberAdmission?.review == MemberCriteria.All && !seesText("Save admission settings?") &&
        !seesText("Review members before admitting (\"knocking\").") && exists(Ui.GroupPrefs.MemberAdmission)
    }
    a = p.admission()
    poll(what = "`Review members` all when reopened") { "all" in a.review() }
  }

  @KnownAppIssue("15.5 item 30", "Compose-Espresso link", intermittent = true)
  @Test fun review_acceptAsMember() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val chat = pendingLinkJoin(alice, bob, "team")
    chat.send("before review")
    hideKeyboard()
    val info = chat.groupInfo()
    val cwm = info.chatsWithMembers()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob listed in `Chats with members` as `pending review`") { cwm.rowTexts("bob").let { "bob" in it && "pending review" in it } }
    val sc = cwm.open("bob")
    poll(what = "bob's join request with `Accept` and `Reject` in his support chat") {
      sc.items().any { it.content.text == "New member wants to join the group." } &&
        exists(Ui.Chat.PendingAccept) && "Accept" in textsOf(Ui.Chat.PendingAccept) && exists(Ui.Chat.PendingReject) && "Reject" in textsOf(Ui.Chat.PendingReject)
    }
    sc.send("hi")
    bob.expectsMessage("#team (support) alice> hi", CONNECT_TIMEOUT_MS)
    check("hi" !in chat.itemTexts()) { "alice's support message `hi` was added to team's main chat" }
    bob sends "#team (support) hello"
    sc.itemId("hello")
    sc.acceptVia("acceptAsMember")
    val joined = listOf(bob.nextLine(CONNECT_TIMEOUT_MS), bob.nextLine(CONNECT_TIMEOUT_MS))
    check("#team: you joined the group" in joined && joined.any { it.endsWith("#team alice> before review [>>]") }) { "bob's lines after acceptance are not exactly the join and the `before review` history: $joined" }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the support chat to close and bob listed as `member`") {
      chatModel.secondaryChatsContext.value == null && cwm.rowTexts("bob").let { "member" in it && "pending review" !in it }
    }
    val accepted = cwm.open("bob")
    accepted.itemId("hello")
    check(!exists(Ui.Chat.PendingAccept) && !exists(Ui.Chat.PendingReject)) { "`Accept` / `Reject` still shown for an accepted member" }
    accepted.close()
    cwm.close()
    info.close()
    chat.send("after accept")
    bob.expectsMessage("#team alice> after accept", CONNECT_TIMEOUT_MS)
  }

  @Test fun review_acceptAsObserver() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val chat = pendingLinkJoin(alice, bob, "team")
    val info = chat.groupInfo()
    val cwm = info.chatsWithMembers()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob listed in `Chats with members` as `pending review`") { "pending review" in cwm.rowTexts("bob") }
    cwm.menuAccept("bob")
    acceptAlert("acceptAsObserver")
    bob.eventually("#team: you joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob listed as `observer`") { cwm.rowTexts("bob").let { "observer" in it && "pending review" !in it } }
    bob sends "#team hi"
    bob expects "#team: you don't have permission to send messages"
    cwm.close()
    info.close()
    chat.send("for observers")
    bob.expectsMessage("#team alice> for observers", CONNECT_TIMEOUT_MS)
  }

  @Test fun review_rejectPendingMember() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val chat = pendingLinkJoin(alice, bob, "team")
    val cwm = chat.groupInfo().chatsWithMembers()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob listed in `Chats with members` as `pending review`") { "pending review" in cwm.rowTexts("bob") }
    val sc = cwm.open("bob")
    sc.reject()
    poll(what = "the `Reject member?` alert with `Reject` and `Cancel`") {
      seesText("Reject member?") && exists(Ui.Alert.Confirm(Ui.AlertSource.Shared)) &&
        "Reject" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) && "Cancel" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the alert to close with bob still pending") { !seesText("Reject member?") && exists(Ui.Chat.PendingReject) }
    sc.send("still pending")
    bob.expectsMessage("#team (support) alice> still pending", CONNECT_TIMEOUT_MS)
    sc.reject()
    poll(what = "the `Reject member?` alert") { seesText("Reject member?") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    bob.expectsMessage("#team: alice removed you from the group (signed)", CONNECT_TIMEOUT_MS)
    bob expects "use /d #team to delete the group"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the support chat to close after rejecting bob") { chatModel.secondaryChatsContext.value == null }
    cwm.empty()
  }

  @Test fun joinerPendingReview_acceptedByAdmin() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = bob.createGroupLink("team")
    bob.eventually("to delete it: /delete link #team (joined members will remain connected to you)")
    bob sends "/set admission review #team all"
    bob expects "changed member admission rules"
    joinLink(link)
    bob.eventuallyAll(lines = listOf("alice: accepting request to join group #team...", "#team: alice connected and pending review"), timeoutMs = CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice pending review in #team") { groupInfoOf("team").membership.memberStatus == GroupMemberStatus.MemPendingReview }
    quiesce()
    val chat = alice.chatList.open("team")
    val sc = SupportChatScreen(this)
    sc.waitOpen()
    poll(what = "`Chat with admins` opened with the request to wait for review") {
      seesText("Chat with admins") && sc.items().any { it.content.text == "Please wait for group moderators to review your request to join the group." }
    }
    if (appPlatform.isDesktop) poll(what = "`reviewed by admins` in the group's compose field") { seesText("reviewed by admins") }
    sc.send("hello admins")
    bob.expectsMessage("#team (support: alice) alice> hello admins", CONNECT_TIMEOUT_MS)
    bob sends "#team while pending"
    bob sends "#team (support: alice) hi alice"
    sc.itemId("hi alice")
    check(chatModel.chatsContext.chatItems.value.none { it.content.text == "while pending" }) { "bob's group message sent while alice was pending reached her" }
    bob sends "/accept member #team alice"
    bob expects "#team: alice accepted"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the support chat to close after acceptance") { chatModel.secondaryChatsContext.value == null && !ModalManager.end.hasModalsOpen() }
    poll(what = "`reviewed by admins` gone from the group's compose field") { exists(Ui.Chat.Message) && !seesText("reviewed by admins") }
    chat.send("hi all")
    bob.expectsMessage("#team alice> hi all", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's `while pending` delivered once as group history after acceptance") {
      chatModel.chatsContext.chatItems.value.count { it.content.text == "while pending" } == 1
    }
  }

  @Ignore("host \"pending approval\" needs Haskell `chatHooks` (plan §11.3)")
  @Test fun admission_hostPendingApproval() {}

  @Ignore("\"join rejected\" needs Haskell `chatHooks` (plan §11.3)")
  @Test fun admission_joinRejectedByHook() {}
}
