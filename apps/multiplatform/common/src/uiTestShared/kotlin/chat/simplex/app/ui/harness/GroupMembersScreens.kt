package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatItem
import chat.simplex.common.model.GroupFeature
import chat.simplex.common.model.GroupFeatureEnabled
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui

fun UiScope.groupMemberId(name: String): Long {
  var id: Long? = null
  poll(what = "member `$name` in the model") {
    id = chatModel.groupMembers.value.firstOrNull { it.memberProfile.displayName == name }?.groupMemberId
    id != null
  }
  return id!!
}

fun UiScope.membersTitleShown(count: Int): Boolean = seesText("$count members")

fun UiScope.scrollToMembersEnd() = scrollTo(Ui.GroupInfo.Delete)

fun UiScope.openMemberMenu(name: String): Long {
  val id = groupMemberId(name)
  scrollTo(Ui.GroupInfo.Member(id))
  openContextMenu(Ui.GroupInfo.Member(id))
  return id
}

fun UiScope.selectMembers(first: String, vararg more: String) {
  val id = openMemberMenu(first)
  click(Ui.GroupInfo.MemberMenu(id, "select"))
  poll(what = "`Selected 1` after `Select` on $first") { ChatScreen(this).selectionTitle() == "Selected 1" }
  more.forEachIndexed { i, name ->
    click(Ui.GroupInfo.Member(groupMemberId(name)))
    poll(what = "`Selected ${i + 2}` after tapping $name") { ChatScreen(this).selectionTitle() == "Selected ${i + 2}" }
  }
}

fun UiScope.selectionEnded() = poll(what = "member selection to end") { ChatScreen(this).selectionTitle() == null }

fun UiScope.confirmAlert(title: String, text: String, action: String) {
  poll(what = "the `$title` alert saying `$text` with `$action`") {
    seesText(title) && seesText(text) && action in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  }
  click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
}

fun UiScope.ownGroupOfThree(app: App, a: CliPeer, b: CliPeer, c: CliPeer, group: String = "team"): ChatScreen {
  connect(a)
  connect(b)
  connect(c)
  val chat = ownGroup(app, group, a, b, c)
  c.eventuallyAll(lines = listOf("#$group: member ${a.name} is connected", "#$group: member ${b.name} is connected"), timeoutMs = CONNECT_TIMEOUT_MS)
  quiesce()
  return chat
}

fun UiScope.linkGroupWithDirectMessages(app: App, group: String, vararg joiners: CliPeer): GroupInfoScreen {
  val info = emptyGroup(app, group).groupInfo()
  val prefs = info.preferences()
  prefs.toggle(GroupFeature.DirectMessages)
  prefs.save()
  poll(what = "direct messages allowed in #$group") { groupInfoOf(group).fullGroupPreferences.directMessages.enable == GroupFeatureEnabled.ON }
  prefs.backClosing()
  val link = info.createLink()
  val joined = mutableListOf<CliPeer>()
  for (j in joiners) {
    j sends "/c $link"
    j expects "connection request sent!"
    j.eventuallyAll(lines = listOf("#$group: you joined the group") + joined.map { "#$group: member ${it.name} is connected" }, timeoutMs = CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "${j.name} listed as connected in #$group") { "connected" in info.memberTexts(j.name) }
    joined += j
  }
  quiesce()
  return info
}

fun UiScope.eventIds(what: String, count: Int, match: (ChatItem) -> Boolean): List<Long> {
  var ids = emptyList<Long>()
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = what) {
    ids = chatModel.chatsContext.chatItems.value.filter(match).map { it.id }
    ids.size == count
  }
  return ids
}

fun UiScope.itemMenu(id: Long, action: String) {
  openContextMenu(Ui.Chat.Item(id))
  click(Ui.Chat.ItemMenu(id, action))
}
