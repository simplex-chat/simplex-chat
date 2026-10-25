package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.openReportsFromBanner
import chat.simplex.app.ui.harness.ownGroupWithTwo
import chat.simplex.app.ui.harness.receivesReport
import chat.simplex.app.ui.harness.sendsReport
import chat.simplex.app.ui.harness.reportsBanner
import chat.simplex.app.ui.harness.setRoleViaApi
import chat.simplex.common.model.GroupMemberRole
import chat.simplex.common.ui.Ui
import org.junit.Test

class ModeratorReportTest : UiTest() {
  @Test fun reports_moderatorBannerAndCounts() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = ownGroupWithTwo(alice, bob, cath)
    cath sends "#team joke one"
    cath sends "#team joke two"
    bob.eventuallyMessage("#team cath> joke two", CONNECT_TIMEOUT_MS)
    chat.itemId("cath's `joke two`") { it.content.text == "joke two" }
    check(!exists(Ui.Chat.Reports)) { "a reports banner shown before any report" }
    bob.sendsReport("team", "spam", "cath", "joke one")
    reportsBanner("1 report")
    bob.sendsReport("team", "community", "cath", "joke two")
    reportsBanner("2 reports")
    val info = chat.groupInfo()
    poll(what = "`Member reports` with the count 2 in group info") { info.reportsTexts().let { "Member reports" in it && "2" in it } }
    info.close()
    val view = openReportsFromBanner()
    val one = view.reportId("joke one")
    val two = view.reportId("joke two")
    check(view.texts(one).any { "Spam" in it } && view.texts(two).any { "Community guidelines violation" in it }) {
      "the reports do not show their reasons: ${view.texts(one)}, ${view.texts(two)}"
    }
    view.menu(two, "delete")
    val forMe = Ui.Alert.Option(Ui.AlertSource.Shared, "forMe")
    poll(what = "the `Delete message?` alert for a report offering only `Delete for me`") {
      seesText("Delete message?") && exists(forMe) && "Delete for me" in textsOf(forMe) && !exists(Ui.Alert.Option(Ui.AlertSource.Shared, "forEveryone"))
    }
    click(forMe)
    poll(what = "the `joke two` report gone from `Member reports`, the `joke one` report kept") {
      view.items().none { it.id == two } && !exists(Ui.Chat.Item(two)) && exists(Ui.Chat.Item(one))
    }
    view.close()
    reportsBanner("1 report")
    chat.send("after delete")
    bob.expectsMessage("#team alice> after delete", CONNECT_TIMEOUT_MS)
    cath.expectsMessage("#team alice> after delete", CONNECT_TIMEOUT_MS)
  }

  @Test fun reports_archiveForMeForAllModeratorsAndAll() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = ownGroupWithTwo(alice, bob, cath)
    setRoleViaApi("team", "cath", GroupMemberRole.Moderator)
    cath.eventually("#team: alice changed your role from member to moderator (signed)", CONNECT_TIMEOUT_MS)
    bob.eventually("#team: alice changed the role of cath from member to moderator (signed)", CONNECT_TIMEOUT_MS)
    cath sends "#team joke one"
    cath sends "#team joke two"
    cath sends "#team joke three"
    bob.eventuallyMessage("#team cath> joke three", CONNECT_TIMEOUT_MS)
    chat.itemId("cath's `joke three`") { it.content.text == "joke three" }
    bob.sendsReport("team", "spam", "cath", "joke one")
    cath.receivesReport("team", "bob", "cath", "joke one", "report spam")
    bob.sendsReport("team", "community", "cath", "joke two")
    cath.receivesReport("team", "bob", "cath", "joke two", "report community")
    bob.sendsReport("team", "profile", "cath", "joke three")
    cath.receivesReport("team", "bob", "cath", "joke three", "report profile")
    reportsBanner("3 reports")
    val view = openReportsFromBanner()
    val one = view.reportId("joke one")
    val two = view.reportId("joke two")
    val three = view.reportId("joke three")
    check(listOf(one, two, three).none { id -> view.texts(id).any { "archived report" in it } }) { "a report shown as archived before archiving" }
    view.archive(one, "archiveForMe")
    poll(what = "the `joke one` report shown as `archived report`") { view.texts(one).any { "archived report" in it } }
    view.archive(three, "archiveForAll")
    cath.expectsMessage("#team (support: bob) bob> [marked deleted by alice] report profile", CONNECT_TIMEOUT_MS)
    poll(what = "the `joke three` report shown as `archived report`") { view.texts(three).any { "archived report" in it } }
    check(view.texts(two).none { "archived report" in it }) { "the `joke two` report was archived too" }
    view.close()
    reportsBanner("1 report")
    backToChatList()
    alice.chatList.rowMenu("team", "archiveReports")
    poll(what = "the `Archive all reports?` alert with `Archive`") {
      seesText("Archive all reports?") && seesText("All reports will be archived for you.") && "Archive" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "no active reports left in team") { alice.chatList.chat("team").chatStats.reportsCount == 0 }
    alice.chatList.open("team")
    chat.itemId("joke three")
    check(!exists(Ui.Chat.Reports)) { "the reports banner still shown after `Archive all reports`" }
    chat.send("after archive")
    cath.expectsMessage("#team alice> after archive", CONNECT_TIMEOUT_MS)
    bob.expectsMessage("#team alice> after archive", CONNECT_TIMEOUT_MS)
  }

  @Test fun reports_moderateReportedMessageMarksReport() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = ownGroupWithTwo(alice, bob, cath)
    cath sends "#team bad joke"
    bob.eventuallyMessage("#team cath> bad joke", CONNECT_TIMEOUT_MS)
    val joke = chat.itemId("cath's `bad joke`") { it.content.text == "bad joke" }
    bob.sendsReport("team", "spam", "cath", "bad joke")
    reportsBanner("1 report")
    poll(what = "cath's `bad joke` shown, not moderated") { textsOf(Ui.Chat.Item(joke)).let { t -> t.any { "bad joke" in it } && t.none { "moderated by" in it } } }
    chat.menu("bad joke", "moderate")
    poll(what = "the `Delete member message?` alert saying the message will be marked as moderated, with `Delete`") {
      seesText("Delete member message?") && seesText("The message will be marked as moderated for all members.") &&
        "Delete" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    cath.expectsMessage("#team cath> [marked deleted by alice] bad joke", CONNECT_TIMEOUT_MS)
    bob.expectsMessage("#team cath> [marked deleted by alice] bad joke", CONNECT_TIMEOUT_MS)
    bob expects "#team: 1 messages deleted by member alice"
    poll(what = "`moderated by alice` in place of `bad joke` and the reports banner gone") {
      textsOf(Ui.Chat.Item(joke)).let { t -> "moderated by alice" in t && t.none { "bad joke" in it } } && !exists(Ui.Chat.Reports)
    }
    val info = chat.groupInfo()
    poll(what = "`Member reports` in group info without a count") { info.reportsTexts().let { "Member reports" in it && "1" !in it } }
    val view = info.reports()
    val report = view.reportId("bad joke")
    poll(what = "the `bad joke` report shown as `archived report`") { view.texts(report).any { "archived report" in it } }
    view.close()
    info.close()
  }
}
