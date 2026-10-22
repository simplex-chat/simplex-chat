package chat.simplex.app.ui.harness

import chat.simplex.common.ui.Ui

fun UiScope.memberConnectionLoaded() {
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the member's servers loaded in member info") { exists(Ui.Info.ReceivingVia) }
  waitStable(Ui.Info.ReceivingVia)
}

fun UiScope.shownSecurityCode(): String {
  click(Ui.Info.VerifyCode)
  waitFor(Ui.VerifyCode.Code)
  return textOf(Ui.VerifyCode.Code)!!.filterNot(Char::isWhitespace)
}

fun UiScope.closeMemberInfo(name: String) {
  val row = Ui.GroupInfo.Member(groupMemberId(name))
  escape()
  poll(what = "$name's member info to close to group info") { !exists(Ui.MemberInfo.Role) && !exists(Ui.Info.VerifyCode) && exists(row) }
}

fun UiScope.alertSaying(title: String, text: String) {
  poll(what = "the `$title` alert saying `$text`") { seesText(title) && seesText(text) && exists(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
  click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  poll(what = "the `$title` alert to close") { !seesText(text) }
}

fun CliPeer.createShortAndFullGroupLink(group: String): Pair<String, String> {
  createGroup(group)
  this sends "/create link #$group"
  this expects "Group link is created!"
  this expects ""
  val short = nextLine()
  val full = eventuallyMatching(Regex("The group link for old clients: (\\S+)")).groupValues[1]
  return short to full
}

fun CliPeer.joinsViaLink(link: String, group: String) {
  this sends "/c $link"
  this expects "connection request sent!"
  eventually("#$group: you joined the group", CONNECT_TIMEOUT_MS)
}

fun UiScope.memberConnected(info: GroupInfoScreen, name: String) =
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "$name listed as connected in the group's members") {
    info.memberTexts(name).let { "connected" in it || "complete" in it }
  }
