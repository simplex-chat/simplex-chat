package chat.simplex.app.ui.harness

import chat.simplex.common.model.CIFile
import chat.simplex.common.model.MsgReaction
import chat.simplex.common.model.readCryptoFile
import chat.simplex.common.platform.chatModel
import chat.simplex.common.platform.getLoadedFilePath
import chat.simplex.common.ui.Ui
import java.io.File

fun UiScope.openAttachmentSheet() {
  hideKeyboard()
  click(described("Attach"))
  poll(what = "the attachment sheet over the chat with Image, Video and File") {
    seesDescription("Close sheet") && exists(Ui.Chat.Attach("image")) && exists(Ui.Chat.Attach("video")) && exists(Ui.Chat.Attach("file"))
  }
}

fun UiScope.attachFile(file: File) {
  openAttachmentSheet()
  openFile(file) { click(Ui.Chat.Attach("file")) }
  poll(what = "the preview of ${file.name} with `Cancel file preview`") { seesDescription("Cancel file preview") && seesText(file.name) }
}

fun UiScope.closeItemMenu(id: Long) {
  click(Ui.Chat.ItemMenu(id, "select"))
  waitFor(Ui.Chat.Selection.part("title"))
  escape()
  poll(what = "selection mode to end") { !exists(Ui.Chat.Selection.part("title")) }
}

fun UiScope.offeredReactions(id: Long): List<String> {
  rightClick(Ui.Chat.Item(id))
  waitFor(Ui.Chat.ItemMenu(id, "reply"))
  val offered = MsgReaction.supported.map { it.text }.filter { exists(Ui.Chat.ItemReaction(id, it)) }
  closeItemMenu(id)
  return offered
}

fun itemFileOf(id: Long): CIFile? = chatModel.chatsContext.chatItems.value.firstOrNull { it.id == id }?.file

fun storedBytesOf(file: CIFile): ByteArray {
  val path = getLoadedFilePath(file) ?: throw AssertionError("${file.fileName} is not on disk (status ${file.fileStatus})")
  return File(path).readBytes()
}

fun plainBytesOf(file: CIFile): ByteArray {
  val path = getLoadedFilePath(file) ?: throw AssertionError("${file.fileName} is not on disk (status ${file.fileStatus})")
  val args = file.fileSource?.cryptoArgs
  return if (args != null) readCryptoFile(path, args) else File(path).readBytes()
}
