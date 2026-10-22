package chat.simplex.app.ui.harness

import chat.simplex.common.model.BusinessChatType
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.ModalManager

fun businessTypeOf(name: String): BusinessChatType? =
  chatModel.chats.value.firstNotNullOfOrNull { c -> (c.chatInfo as? ChatInfo.Group)?.groupInfo?.takeIf { c.chatInfo.chatViewName == name }?.businessChat?.chatType }

fun UiScope.createBusinessAddress(app: App): String {
  val address = app.userPicker().address().create()
  waitFor(Ui.Address.Business)
  click(Ui.Address.Business)
  poll(what = "a business address with auto-accept saved") { savedAddressSettings()?.let { it.businessAddress && it.autoAccept != null } == true }
  AddressScreen(this).close()
  return address
}

fun UiScope.leaveBusinessChatFromInfo(name: String) {
  groupInfoRow(Ui.GroupInfo.Leave)
  confirmAlert("Leave chat?", "$name\n\nYou will stop receiving messages from this chat. Chat history will be preserved.", "Leave")
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "group info to close after leaving `$name`") { !ModalManager.end.hasModalsOpen() }
}

fun UiScope.deleteBusinessChatFromInfo(name: String) {
  groupInfoRow(Ui.GroupInfo.Delete)
  confirmAlert("Delete chat?", "$name\n\nChat will be deleted for all members - this cannot be undone!", "Delete")
}
