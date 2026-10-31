package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.composeReport
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.groupInfoOf
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.joinGroupFromList
import chat.simplex.app.ui.harness.memberOfHostedGroup
import chat.simplex.app.ui.harness.menuOffers
import chat.simplex.app.ui.harness.openReportReasons
import chat.simplex.app.ui.harness.receivesReport
import chat.simplex.app.ui.harness.reportSendFinished
import chat.simplex.app.ui.harness.reportSentAlert
import chat.simplex.app.ui.harness.sendReport
import chat.simplex.common.model.GroupFeature
import chat.simplex.common.model.GroupMemberRole
import chat.simplex.common.ui.Ui
import org.junit.Test

class MemberReportTest : UiTest() {
  @KnownAppIssue("15.5 item 12", "bob: expected `      report content`, got `      report illegal`")
  @KnownAppIssue("15.5 item 29", "waiting for cath's `inappropriate joke`", intermittent = true)
  @Test fun report_member_spam_moderatorsReceive() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = memberOfHostedGroup(alice, bob, cath)
    cath sends "#team inappropriate joke"
    bob.eventuallyMessage("#team cath> inappropriate joke", CONNECT_TIMEOUT_MS)
    val joke = chat.itemId("cath's `inappropriate joke`") { it.content.text == "inappropriate joke" }
    openReportReasons(joke)
    for (reason in listOf("Spam", "Inappropriate content", "Community guidelines violation", "Inappropriate profile", "Another reason")) {
      check(exists(Ui.Chat.ReportReason(reason)) && reason in textsOf(Ui.Chat.ReportReason(reason))) { "`Report reason?` does not offer `$reason`" }
    }
    check("Cancel" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared))) { "`Report reason?` has no `Cancel`" }
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "`Report reason?` closed by `Cancel` with no report in compose") {
      !seesText("Report reason?") && exists(Ui.Chat.Message) && !seesText("only group moderators will see it.", substring = true)
    }

    val spam = "Report spam: only group moderators will see it."
    composeReport(joke, "Spam", spam)
    check(editableText(Ui.Chat.Message).isNullOrEmpty() && isEnabled(Ui.Chat.Send)) { "a `Spam` report cannot be sent without a comment" }
    sendReport(spam)
    reportSentAlert()
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    bob.receivesReport("team", "alice", "cath", "inappropriate joke", "report spam")

    val violation = "Report violation: only group moderators will see it."
    composeReport(joke, "Community guidelines violation", violation)
    sendReport(violation)
    reportSentAlert()
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    bob.receivesReport("team", "alice", "cath", "inappropriate joke", "report community")

    val profile = "Report member profile: only group moderators will see it."
    composeReport(joke, "Inappropriate profile", profile)
    sendReport(profile)
    bob.receivesReport("team", "alice", "cath", "inappropriate joke", "report profile")
    reportSendFinished()
    check(!seesText("Report sent to moderators")) { "`Report sent to moderators` shown again after `Don't show again`" }

    val other = "Report other: only group moderators will see it."
    composeReport(joke, "Another reason", other)
    waitFor(Ui.Chat.Send)
    check(!isEnabled(Ui.Chat.Send)) { "an `Another reason` report can be sent without a comment" }
    typeInto(Ui.Chat.Message, "fake giveaway")
    hideKeyboard()
    poll(what = "the send button enabled once the comment is typed") { isEnabled(Ui.Chat.Send) }
    sendReport(other)
    bob.receivesReport("team", "alice", "cath", "inappropriate joke", "report other: fake giveaway")

    chat.send("after reports")
    bob.expectsMessage("#team alice> after reports", CONNECT_TIMEOUT_MS)
    cath.expectsMessage("#team alice> after reports", CONNECT_TIMEOUT_MS)

    val content = "Report content: only group moderators will see it."
    composeReport(joke, "Inappropriate content", content)
    sendReport(content)
    bob.receivesReport("team", "alice", "cath", "inappropriate joke", "report content")
  }

  @Test fun report_notOffered_forOwnModeratorOrPrefOff() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    joinGroupFromList(alice, bob, "team")
    backToChatList()
    val chat = alice.chatList.open("team")
    bob sends "#team from the owner"
    val owners = chat.itemId("bob's `from the owner`") { it.content.text == "from the owner" }
    chat.send("mine")
    bob.eventuallyMessage("#team alice> mine", CONNECT_TIMEOUT_MS)
    val mine = chat.itemId("mine")
    check(menuOffers(owners, "report")) { "`Report` not offered to a member on the owner's message" }
    check(!menuOffers(mine, "report")) { "`Report` offered on alice's own message" }

    bob sends "/set reports #team off"
    bob expects "updated group preferences:"
    bob expects "Member reports: off"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "member reports off in team's model") { !groupInfoOf("team").groupFeatureEnabled(GroupFeature.Reports) }
    check(!menuOffers(owners, "report")) { "`Report` offered with member reports off" }

    bob sends "/set reports #team on"
    bob expects "updated group preferences:"
    bob expects "Member reports: on"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "member reports on in team's model") { groupInfoOf("team").groupFeatureEnabled(GroupFeature.Reports) }
    check(menuOffers(owners, "report")) { "`Report` not offered again with member reports back on" }

    bob sends "/mr team alice moderator"
    bob.eventuallyMatching(Regex("#team: you changed the role of alice to moderator( \\(signed\\))?"))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice a moderator of team") { groupInfoOf("team").membership.memberRole == GroupMemberRole.Moderator }
    check(!menuOffers(owners, "report")) { "`Report` offered to a moderator" }
    check(!menuOffers(owners, "moderate")) { "`Moderate` offered to a moderator on the owner's message" }
  }
}
