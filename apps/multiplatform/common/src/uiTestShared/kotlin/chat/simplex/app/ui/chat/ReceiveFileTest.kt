package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.TestServers
import chat.simplex.app.ui.harness.PREVIEW_IMAGE_DATA
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowItem
import chat.simplex.app.ui.harness.attachFile
import chat.simplex.app.ui.harness.closeItemMenu
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.described
import chat.simplex.app.ui.harness.itemFileOf
import chat.simplex.app.ui.harness.itemMenu
import chat.simplex.app.ui.harness.plainBytesOf
import chat.simplex.common.model.CIFileStatus
import chat.simplex.common.model.MsgContent
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.MAX_IMAGE_SIZE_AUTO_RCV
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Test
import java.io.File

class ReceiveFileTest : UiTest() {
  @Test fun sndFile_revoke_peerSeesCancelled() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val source = File(exchangeDir(), REVOKED_NAME).apply { writeText(REVOKED) }
    val chat = alice.chatList.open("bob")
    waitFor(Ui.Chat.Message)
    attachFile(source)
    hideKeyboard()
    click(Ui.Chat.Send)
    bob.expectsMessage("alice> sends file $REVOKED_NAME (${REVOKED.length} bytes / ${REVOKED.length} bytes)", CONNECT_TIMEOUT_MS)
    bob expects "use /fr 1 [<dir>/ | <path>] to receive it"
    val id = chat.itemId("alice's $REVOKED_NAME") { it.chatDir.sent && it.file?.fileName == REVOKED_NAME }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "$REVOKED_NAME uploaded") { itemFileOf(id)?.fileStatus is CIFileStatus.SndComplete }
    rightClick(Ui.Chat.Item(id))
    poll(what = "`Revoke file` in the menu of the uploaded file") {
      exists(Ui.Chat.ItemMenu(id, "cancelFile")) && "Revoke file" in textsOf(Ui.Chat.ItemMenu(id, "cancelFile"))
    }
    click(Ui.Chat.ItemMenu(id, "cancelFile"))
    poll(what = "the `Revoke file?` alert with `Revoke`") {
      seesText("Revoke file?") && seesText("File will be deleted from servers.") && "Revoke" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    assertEquals("alice cancelled sending file 1 ($REVOKED_NAME)", bob.nextLine(CONNECT_TIMEOUT_MS))
    poll(what = "alice's $REVOKED_NAME cancelled") { itemFileOf(id)?.fileStatus is CIFileStatus.SndCancelled }
    rightClick(Ui.Chat.Item(id))
    waitFor(Ui.Chat.ItemMenu(id, "select"))
    assertFalse("the menu of the revoked file still offers to cancel it", exists(Ui.Chat.ItemMenu(id, "cancelFile")))
    closeItemMenu(id)
    val dir = File(bob.createFile(".fr-probe", "probe")).parent
    bob sends "/fr 1 $dir/"
    bob expects "file cancelled: $REVOKED_NAME"
  }

  @Test fun rcvImage_click_opensFullScreen() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "/img @alice $FIXTURE_JPG"
    bob expectsMessage "/f @alice $FIXTURE_JPG"
    bob expects "use /fc 1 to cancel sending"
    bob.eventually("completed uploading file 1 (test.jpg) for alice", CONNECT_TIMEOUT_MS)
    awaitRowItem(alice, "bob", "bob's image") { it.content.msgContent is MsgContent.MCImage }
    val chat = alice.chatList.open("bob")
    val id = chat.itemId("bob's image") { it.content.msgContent is MsgContent.MCImage }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the image received and shown in full") {
      itemFileOf(id)?.fileStatus is CIFileStatus.RcvComplete && exists(Ui.Chat.ItemFile(id))
    }
    check(!exists(Ui.Chat.FullScreenMedia) && !chatModel.fullscreenGalleryVisible.value) { "the full-screen viewer is open before the image is clicked" }
    hideKeyboard()
    click(Ui.Chat.ItemFile(id))
    poll(what = "the full-screen viewer showing the image") { exists(Ui.Chat.FullScreenMedia) && "Image" in descriptionsOf(Ui.Chat.FullScreenMedia) }
    click(Ui.Chat.FullScreenMedia)
    poll(what = "the full-screen viewer closed, back to the chat") { !exists(Ui.Chat.FullScreenMedia) && exists(Ui.Chat.ItemFile(id)) }
  }

  @Test fun rcvFile_acceptedBeforeUpload_showsWaitingAlert() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val path = bob.createFile(WAITING_NAME, WAITING)
    TestServers.stop("xftp1")
    bob sends "/f @alice $path"
    bob expects "use /fc 1 to cancel sending"
    awaitRowItem(alice, "bob", "bob's $WAITING_NAME") { it.file?.fileName == WAITING_NAME }
    val chat = alice.chatList.open("bob")
    val id = chat.itemId("bob's $WAITING_NAME") { it.file?.fileName == WAITING_NAME }
    check(itemFileOf(id)?.fileStatus is CIFileStatus.RcvInvitation) { "$WAITING_NAME before the click: ${itemFileOf(id)?.fileStatus}" }
    hideKeyboard()
    click(Ui.Chat.ItemFile(id))
    poll(what = "$WAITING_NAME accepted and waiting for bob's upload") { itemFileOf(id)?.fileStatus is CIFileStatus.RcvAccepted }
    check(!seesText(WAITING_TITLE)) { "the `$WAITING_TITLE` alert is shown before the accepted file is clicked again" }
    click(Ui.Chat.ItemFile(id))
    poll(what = "the `$WAITING_TITLE` alert") { seesText(WAITING_TITLE) && seesText(WAITING_INFO) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the `$WAITING_TITLE` alert to close") { !seesText(WAITING_TITLE) }
    check(itemFileOf(id)?.fileStatus is CIFileStatus.RcvAccepted) { "$WAITING_NAME left the waiting state with xftp1 down: ${itemFileOf(id)?.fileStatus}" }
    val saved = File(exchangeDir(), "saved-$WAITING_NAME").apply { delete() }
    TestServers.start("xftp1")
    bob.eventually("completed uploading file 1 ($WAITING_NAME) for alice", CONNECT_TIMEOUT_MS)
    saveFileAs(saved) {
      poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "$WAITING_NAME received once xftp1 is back") { itemFileOf(id)?.fileStatus is CIFileStatus.RcvComplete }
      if (appPlatform.isAndroid) click(described("Save"))
    }
    assertEquals(WAITING, plainBytesOf(itemFileOf(id)!!).toString(Charsets.UTF_8))
    poll(what = "the received $WAITING_NAME written to ${saved.name}") { saved.length() == WAITING.length.toLong() }
    assertEquals(WAITING, saved.readText())
  }

  @Test fun rcvImage_large_clickDownloads() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val aliceId = bob.contactId("alice")
    bob sends """/_send @$aliceId json [{"filePath": "$LARGE_PNG", "msgContent": {"type": "image", "text": "", "image": "$PREVIEW_IMAGE_DATA"}}]"""
    bob.eventually("completed uploading file 1 ($LARGE_PNG_NAME) for alice", CONNECT_TIMEOUT_MS)
    bob sends "/img @alice $FIXTURE_JPG"
    bob.eventually("completed uploading file 2 (test.jpg) for alice", CONNECT_TIMEOUT_MS)
    awaitRowItem(alice, "bob", "bob's test.jpg") { it.file?.fileName == "test.jpg" }
    val chat = alice.chatList.open("bob")
    val large = chat.itemId("bob's large image") { it.content.msgContent is MsgContent.MCImage && it.file?.fileName == LARGE_PNG_NAME }
    val small = chat.itemId("bob's small image") { it.content.msgContent is MsgContent.MCImage && it.file?.fileName == "test.jpg" }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the small image, sent after the large one, received without a click") { itemFileOf(small)?.fileStatus is CIFileStatus.RcvComplete }
    val offered = itemFileOf(large)!!
    check(offered.fileStatus is CIFileStatus.RcvInvitation && offered.fileSize > MAX_IMAGE_SIZE_AUTO_RCV) {
      "$LARGE_PNG_NAME (${offered.fileSize} bytes) was received without a click: ${offered.fileStatus}"
    }
    poll(what = "the preview of $LARGE_PNG_NAME with `Asked to receive the image`") {
      exists(Ui.Chat.ItemFile(large).part("preview")) && "Asked to receive the image" in descriptionsOf(Ui.Chat.Item(large))
    }
    hideKeyboard()
    click(Ui.Chat.ItemFile(large).part("preview"))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "$LARGE_PNG_NAME received and shown in full, without `Asked to receive the image`") {
      itemFileOf(large)?.fileStatus is CIFileStatus.RcvComplete && exists(Ui.Chat.ItemFile(large)) &&
        "Asked to receive the image" !in descriptionsOf(Ui.Chat.Item(large))
    }
    val received = plainBytesOf(itemFileOf(large)!!)
    assertEquals(offered.fileSize, received.size.toLong())
    check(received.take(PNG_MAGIC.size) == PNG_MAGIC) { "the received $LARGE_PNG_NAME is not the PNG bob sent: ${received.take(PNG_MAGIC.size)}" }
  }

  @KnownAppIssue("15.5 item 34", "the revealed deleted item offers Download")
  @Test fun rcvFile_senderDeletedItem_cannotReceive() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val path = bob.createFile(DELETED_NAME, DELETED)
    val aliceId = bob.contactId("alice")
    bob sends """/_send @$aliceId json [{"filePath": "$path", "msgContent": {"type": "text", "text": "$CAPTION"}}]"""
    bob.eventually("completed uploading file 1 ($DELETED_NAME) for alice", CONNECT_TIMEOUT_MS)
    awaitRowItem(alice, "bob", "bob's $DELETED_NAME") { it.file?.fileName == DELETED_NAME }
    val chat = alice.chatList.open("bob")
    val id = chat.itemId("`$CAPTION` with $DELETED_NAME") { it.content.text == CAPTION && it.file?.fileName == DELETED_NAME }
    rightClick(Ui.Chat.Item(id))
    waitFor(Ui.Chat.ItemMenu(id, "select"))
    check(exists(Ui.Chat.ItemMenu(id, "download"))) { "the menu of the offered file has no Download" }
    closeItemMenu(id)
    bob sends "\\@alice $CAPTION"
    bob.eventually("message marked deleted")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the item showing `marked deleted` in place of `$CAPTION`") {
      chat.item(id)?.meta?.itemDeleted != null && textsOf(Ui.Chat.Item(id)).let { t -> t.any { "marked deleted" in it } && t.none { CAPTION in it } }
    }
    itemMenu(id, "reveal")
    poll(what = "the revealed deleted item showing `$CAPTION`") { textsOf(Ui.Chat.Item(id)).any { CAPTION in it } }
    rightClick(Ui.Chat.Item(id))
    waitFor(Ui.Chat.ItemMenu(id, "hide"))
    assertFalse("the revealed deleted item offers Download", exists(Ui.Chat.ItemMenu(id, "download")))
    closeItemMenu(id)
  }

  private companion object {
    const val REVOKED_NAME = "revoke-me.txt"
    const val REVOKED = "This file will be revoked by alice"
    const val DELETED_NAME = "deleted.txt"
    const val DELETED = "This file goes with a deleted message"
    const val CAPTION = "hi, sending a file"
    const val WAITING_NAME = "waiting.txt"
    const val WAITING = "Uploaded after the file server is back"
    const val WAITING_TITLE = "Waiting for file"
    const val WAITING_INFO = "File will be received when your contact completes uploading it."
    const val FIXTURE_JPG = "tests/fixtures/test.jpg"
    const val LARGE_PNG = "website/src/img/simplex_mobile.png"
    const val LARGE_PNG_NAME = "simplex_mobile.png"
    val PNG_MAGIC = listOf(0x89.toByte(), 'P'.code.toByte(), 'N'.code.toByte(), 'G'.code.toByte())
  }
}
