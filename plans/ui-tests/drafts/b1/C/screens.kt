package chat.simplex.app.ui.harness

import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.model.BusinessChatType
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle
import chat.simplex.common.views.chatlist.ActiveFilter
import chat.simplex.common.views.chatlist.PresetTagKind
import kotlinx.coroutines.runBlocking

fun UiScope.presetFilter(kind: PresetTagKind, label: String) {
  quiesce()
  val chip = Ui.ChatList.PresetTag(kind.name)
  if (exists(chip)) {
    poll(what = "the `$label` filter chip") { label in textsOf(chip) }
    click(chip)
  } else {
    click(Ui.ChatList.PresetMenu)
    val item = Ui.ChatList.PresetMenuItem(kind.name)
    poll(what = "the `$label` item in the filter menu") { exists(item) && label in textsOf(item) }
    click(item)
  }
  poll(what = "the `$label` filter to be active") { chatModel.activeChatTagFilter.value == ActiveFilter.PresetTag(kind) }
}

fun UiScope.backToChatList() {
  if (!exists(Ui.ChatList.Search)) escape()
  waitFor(Ui.ChatList.Search)
}

fun UiScope.keepOneTimeLink(app: App): String {
  val link = app.newChat().createOneTimeLink()
  escape()
  poll(what = "the `Keep unused invitation?` alert") { seesText("Keep unused invitation?") }
  click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
  escape()
  poll(what = "the New chat sheet to close with the pending connection kept") {
    !chatModel.newChatSheetVisible.value && chatModel.chats.value.any { it.chatInfo is ChatInfo.ContactConnection }
  }
  return link
}

fun UiScope.namePendingConnection(alias: String) {
  var id: String? = null
  poll(what = "a pending connection in the model") {
    id = chatModel.chats.value.firstOrNull { it.chatInfo is ChatInfo.ContactConnection }?.id
    id != null
  }
  poll(what = "API calls to finish before opening the menu of the pending connection") { UiIdle.inFlight().isEmpty() }
  settle()
  rightClick(Ui.ChatList.Row(id!!))
  click(Ui.ChatList.RowMenu(id!!, "setName"))
  typeInto(Ui.Info.Alias, alias)
  poll(what = "the pending connection alias `$alias` in the model") {
    (chatModel.chats.value.firstOrNull { it.id == id }?.chatInfo as? ChatInfo.ContactConnection)?.contactConnection?.localAlias == alias
  }
  escape()
  poll(what = "the contact name editor to close") { !exists(Ui.Info.Alias) }
}

fun UiScope.joinLink(link: String) {
  val plan = runBlocking { chatController.apiConnectPlan(null, link, inProgress = mutableStateOf(true)) }
    ?: error("apiConnectPlan failed for $link")
  runBlocking { chatController.apiConnect(null, incognito = false, connLink = plan.connLink) }
    ?: error("apiConnect failed for $link")
}

fun UiScope.businessChat(biz: CliPeer) {
  joinLink(biz.createAddress(business = true))
  biz.eventuallyMatching(Regex("#\\S+: \\S+ joined the group"), CONNECT_TIMEOUT_MS)
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the business chat `${biz.name}`") {
    chatModel.chats.value.any { c ->
      c.chatInfo.chatViewName == biz.name && (c.chatInfo as? ChatInfo.Group)?.groupInfo?.businessChat?.chatType == BusinessChatType.Business
    }
  }
  quiesce()
}

fun CliPeer.createAddress(business: Boolean = false): String {
  this sends "/ad"
  this expects "Your new chat address is created!"
  this expects ""
  val link = nextLine()
  this sends (if (business) "/auto_accept on business" else "/auto_accept on")
  eventually(if (business) "auto_accept on, business" else "auto_accept on")
  return link
}

fun CliPeer.createGroupLink(group: String): String {
  this sends "/g $group"
  this expects "group #$group is created"
  this expects "to add members use /a $group <name> or /create link #$group"
  this sends "/create link #$group"
  this expects "Group link is created!"
  this expects ""
  return nextLine()
}
