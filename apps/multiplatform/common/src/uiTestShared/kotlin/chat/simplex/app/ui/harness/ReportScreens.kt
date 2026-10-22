package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatItem
import chat.simplex.common.model.GroupMemberRole
import chat.simplex.common.model.MsgContentTag
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.views.helpers.ModalViewId
import kotlinx.coroutines.runBlocking

fun UiScope.memberOfHostedGroup(app: App, host: CliPeer, member: CliPeer, group: String = "team"): ChatScreen {
  val me = chatModel.currentUser.value!!.displayName
  connect(host)
  connectPeers(host, member)
  hostGroup(host, group, me, member.name)
  joinGroupFromList(app, host, group)
  member sends "/j $group"
  member.eventually("#$group: you joined the group", CONNECT_TIMEOUT_MS)
  host.eventually("#$group: ${member.name} joined the group", CONNECT_TIMEOUT_MS)
  member.eventually("#$group: member $me is connected", CONNECT_TIMEOUT_MS)
  quiesce()
  backToChatList()
  return app.chatList.open(group)
}

fun UiScope.ownGroupWithTwo(app: App, a: CliPeer, b: CliPeer, group: String = "team"): ChatScreen {
  connect(a)
  connect(b)
  val chat = ownGroup(app, group, a, b)
  a.eventuallyMatching(Regex("#$group: (new )?member ${b.name} is connected"), CONNECT_TIMEOUT_MS)
  b.eventuallyMatching(Regex("#$group: (new )?member ${a.name} is connected"), CONNECT_TIMEOUT_MS)
  quiesce()
  return chat
}

fun CliPeer.sendsReport(group: String, reason: String, author: String, text: String) {
  this sends "/report #$group $reason $text"
  expectsMessage("#$group (support) > $author $text")
  this expects "      report $reason"
}

fun CliPeer.receivesReport(group: String, reporter: String, author: String, text: String, report: String) {
  val line = nextLine(CONNECT_TIMEOUT_MS)
  val expected = Regex("""^(\d{2}:\d{2} )?""" + Regex.escape("#$group (support: $reporter) $reporter") + """!?""" + Regex.escape("> > $author $text") + "$")
  check(expected.matches(line)) { "$name: expected the report of `$text` from $reporter, got `$line`; next: ${pending()}" }
  this expects "      $report"
}

private fun memberIdViaApi(group: String, name: String): Long =
  runBlocking { chatController.apiListMembers(null, groupInfoOf(group).groupId) }.first { it.memberProfile.displayName == name }.groupMemberId

fun setRoleViaApi(group: String, name: String, role: GroupMemberRole) {
  val changed = runBlocking { chatController.apiMembersRole(null, groupInfoOf(group).groupId, listOf(memberIdViaApi(group, name)), role) }
  check(changed.any { it.memberRole == role }) { "apiMembersRole did not make $name $role" }
}

fun blockForAllViaApi(group: String, name: String) {
  val blocked = runBlocking { chatController.apiBlockMembersForAll(null, groupInfoOf(group).groupId, listOf(memberIdViaApi(group, name)), true) }
  check(blocked.any { it.blockedByAdmin }) { "apiBlockMembersForAll did not block $name" }
}

fun UiScope.menuOffers(id: Long, action: String): Boolean {
  openContextMenu(Ui.Chat.Item(id))
  waitFor(Ui.Chat.ItemMenu(id, "select"))
  val offered = exists(Ui.Chat.ItemMenu(id, action))
  click(Ui.Chat.ItemMenu(id, "select"))
  waitFor(Ui.Chat.Selection.part("title"))
  escape()
  poll(what = "selection mode to end") { !exists(Ui.Chat.Selection.part("title")) }
  return offered
}

fun UiScope.openReportReasons(id: Long) {
  openContextMenu(Ui.Chat.Item(id))
  click(Ui.Chat.ItemMenu(id, "report"))
  poll(what = "the `Report reason?` alert") { seesText("Report reason?") && exists(Ui.Alert.Dismiss(Ui.AlertSource.Shared)) }
}

fun UiScope.composeReport(id: Long, reason: String, header: String) {
  openReportReasons(id)
  click(Ui.Chat.ReportReason(reason))
  poll(what = "the `$reason` report in compose with `$header`") { !seesText("Report reason?") && seesText(header) }
}

fun UiScope.sendReport(header: String) {
  click(Ui.Chat.Send)
  poll(what = "the report with `$header` to leave compose after sending") { !seesText(header) && editableText(Ui.Chat.Message).isNullOrEmpty() }
}

fun UiScope.reportSentAlert() =
  poll(what = "the `Report sent to moderators` alert with `OK` and `Don't show again`") {
    seesText("Report sent to moderators") && seesText("You can view your reports in Chat with admins.") &&
      "OK" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) && "Don't show again" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
  }

fun UiScope.reportSendFinished() = poll(what = "the report send to finish") { UiIdle.inFlight().isEmpty() }

fun UiScope.reportsBanner(count: String) =
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `$count` banner in the group chat") { exists(Ui.Chat.Reports) && count in textsOf(Ui.Chat.Reports) }

fun UiScope.openReportsFromBanner(): MemberReportsScreen {
  click(Ui.Chat.Reports)
  return MemberReportsScreen(this)
}

class MemberReportsScreen(private val s: UiScope) {
  init {
    s.poll(what = "the `Member reports` view") {
      chatModel.secondaryChatsContext.value?.contentTag == MsgContentTag.Report && ModalManager.end.hasModalOpen(ModalViewId.SECONDARY_CHAT) && s.seesText("Member reports")
    }
  }

  fun items(): List<ChatItem> = chatModel.secondaryChatsContext.value?.chatItems?.value.orEmpty()

  fun reportId(quoted: String): Long {
    var id: Long? = null
    s.poll(what = "the report of `$quoted` in `Member reports`") {
      id = items().lastOrNull { it.isReport && it.quotedItem?.text == quoted }?.id
      id != null && s.exists(Ui.Chat.Item(id!!))
    }
    return id!!
  }

  fun texts(id: Long): List<String> = s.textsOf(Ui.Chat.Item(id))

  fun menu(id: Long, action: String) {
    s.openContextMenu(Ui.Chat.Item(id))
    s.click(Ui.Chat.ItemMenu(id, action))
  }

  fun archive(id: Long, option: String) {
    menu(id, "archive")
    val forMe = Ui.Alert.Option(Ui.AlertSource.Shared, "archiveForMe")
    val forAll = Ui.Alert.Option(Ui.AlertSource.Shared, "archiveForAll")
    s.poll(what = "the `Archive report?` alert with `For me` and `For all moderators`") {
      s.seesText("Archive report?") && s.exists(forMe) && "For me" in s.textsOf(forMe) && s.exists(forAll) && "For all moderators" in s.textsOf(forAll)
    }
    s.click(Ui.Alert.Option(Ui.AlertSource.Shared, option))
  }

  fun close() {
    s.goBack()
    s.poll(what = "`Member reports` to close") { chatModel.secondaryChatsContext.value == null && !ModalManager.end.hasModalOpen(ModalViewId.SECONDARY_CHAT) }
  }
}
