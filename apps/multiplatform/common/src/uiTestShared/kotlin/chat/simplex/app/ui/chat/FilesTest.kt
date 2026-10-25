package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.CliPeer
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowItem
import chat.simplex.app.ui.harness.connect
import chat.simplex.common.model.CIFile
import chat.simplex.common.model.CIFileStatus
import chat.simplex.common.model.ChatItem
import chat.simplex.common.model.MsgContent
import chat.simplex.common.model.readCryptoFile
import chat.simplex.common.platform.chatModel
import chat.simplex.common.platform.getLoadedFilePath
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Test
import java.io.File

class FilesTest : UiTest() {
  @Test fun rcvFile_clickIcon_receives() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob.sendsFile(bob.createFile("notes.md", NOTES), 1, "notes.md")
    alice.chatList.open("bob")
    val id = fileItemId("notes.md")
    val texts = textsOf(Ui.Chat.Item(id))
    check("notes.md" in texts && texts.any { it.startsWith("${NOTES.length} bytes") }) { "the file item shows no name and size: $texts" }
    check(fileOf(id)?.fileStatus is CIFileStatus.RcvInvitation && !seesText(NOTES, substring = true)) { "notes.md was received before the click: ${fileOf(id)?.fileStatus}" }
    click(Ui.Chat.ItemFile(id))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "notes.md to be received and opened with bob's text") {
      fileOf(id)?.fileStatus is CIFileStatus.RcvComplete && seesText(NOTES, substring = true)
    }
    assertEquals(NOTES, savedBytes(fileOf(id)!!).toString(Charsets.UTF_8))
  }

  @Test fun rcvImage_autoReceived_renders() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob.sendsFile(FIXTURE_JPG, 1, "test.jpg")
    bob sends "/img @alice $FIXTURE_JPG"
    bob expectsMessage "/f @alice $FIXTURE_JPG"
    bob expects "use /fc 2 to cancel sending"
    bob.eventually("completed uploading file 2 (test.jpg) for alice", CONNECT_TIMEOUT_MS)
    awaitRowItem(alice, "bob", "test.jpg as an image") { it.content.msgContent is MsgContent.MCImage && it.file?.fileName == "test.jpg" }
    alice.chatList.open("bob")
    val asFile = itemId("test.jpg sent as a file") { it.content.msgContent is MsgContent.MCFile && it.file?.fileName == "test.jpg" }
    val asImage = itemId("test.jpg sent as an image") { it.content.msgContent is MsgContent.MCImage && it.file?.fileName == "test.jpg" }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the image to be received without a tap") { fileOf(asImage)?.fileStatus is CIFileStatus.RcvComplete }
    poll(what = "the received image shown in full, not its preview, without the `Asked to receive the image` or `Waiting for image` icon") {
      exists(Ui.Chat.ItemFile(asImage)) && !exists(Ui.Chat.ItemFile(asImage).part("preview")) &&
        descriptionsOf(Ui.Chat.Item(asImage)).let { "Asked to receive the image" !in it && "Waiting for image" !in it }
    }
    check(fileOf(asFile)?.fileStatus is CIFileStatus.RcvInvitation) { "test.jpg sent as a file was received without a tap: ${fileOf(asFile)?.fileStatus}" }
    val image = savedBytes(fileOf(asImage)!!)
    assertEquals(FIXTURE_JPG_SIZE, image.size)
    check(image.take(3) == JPEG_MAGIC) { "the saved image is not the JPEG bob sent: ${image.take(3)}" }
  }

  @Test fun rcvFile_senderRevoked_noDownload() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob.sendsFile(bob.createFile("revoked.txt", REVOKED_TEXT), 1, "revoked.txt")
    alice.chatList.open("bob")
    val id = fileItemId("revoked.txt")
    rightClick(Ui.Chat.Item(id))
    waitFor(Ui.Chat.ItemMenu(id, "select"))
    check(exists(Ui.Chat.ItemMenu(id, "download"))) { "the menu of the offered file has no Download" }
    click(Ui.Chat.ItemMenu(id, "select"))
    waitFor(Ui.Chat.Selection.part("title"))
    escape()
    poll(what = "selection mode to end") { !exists(Ui.Chat.Selection.part("title")) }
    bob sends "/fc 1"
    bob expects "cancelled sending file 1 (revoked.txt)"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice's copy of revoked.txt to be cancelled") { fileOf(id)?.fileStatus is CIFileStatus.RcvCancelled }
    rightClick(Ui.Chat.Item(id))
    waitFor(Ui.Chat.ItemMenu(id, "select"))
    assertFalse("the menu of the revoked file still offers Download", exists(Ui.Chat.ItemMenu(id, "download")))
  }

  private fun CliPeer.sendsFile(path: String, fileId: Int, name: String) {
    this sends "/f @alice $path"
    this expects "use /fc $fileId to cancel sending"
    eventually("completed uploading file $fileId ($name) for alice", CONNECT_TIMEOUT_MS)
  }

  private fun UiScope.itemId(what: String, match: (ChatItem) -> Boolean): Long {
    var id: Long? = null
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = what) {
      val found = chatModel.chatsContext.chatItems.value.lastOrNull(match)?.id
      id = found
      found != null && exists(Ui.Chat.Item(found))
    }
    return id!!
  }

  private fun UiScope.fileItemId(name: String): Long = itemId("the item of file `$name`") { it.file?.fileName == name }

  private fun fileOf(id: Long): CIFile? = chatModel.chatsContext.chatItems.value.firstOrNull { it.id == id }?.file

  private fun savedBytes(file: CIFile): ByteArray {
    val path = getLoadedFilePath(file) ?: throw AssertionError("${file.fileName} is not on disk (status ${file.fileStatus})")
    val args = file.fileSource?.cryptoArgs
    return if (args != null) readCryptoFile(path, args) else File(path).readBytes()
  }

  private companion object {
    const val NOTES = "Shopping list from bob: apples and pears"
    const val REVOKED_TEXT = "This file will be revoked"
    const val FIXTURE_JPG = "tests/fixtures/test.jpg"
    const val FIXTURE_JPG_SIZE = 139737
    val JPEG_MAGIC = listOf(0xFF.toByte(), 0xD8.toByte(), 0xFF.toByte())
  }
}
