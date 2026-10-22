package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowText
import chat.simplex.app.ui.harness.attachFile
import chat.simplex.app.ui.harness.attachImage
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.described
import chat.simplex.app.ui.harness.emptyGroup
import chat.simplex.app.ui.harness.groupInfoOf
import chat.simplex.app.ui.harness.jpegFile
import chat.simplex.app.ui.harness.openAttachmentSheet
import chat.simplex.app.ui.harness.receivesCompressedImage
import chat.simplex.app.ui.harness.receivesFile
import chat.simplex.common.model.GroupFeature
import chat.simplex.common.model.GroupFeatureEnabled
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.MAX_FILE_SIZE_XFTP
import org.junit.Test
import java.io.File
import java.io.RandomAccessFile

class SendAttachmentTest : UiTest() {
  @Test fun attachFile_withText_peerReceivesTextAndFile() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val source = File(exchangeDir(), NOTES_NAME).apply { writeText(NOTES) }
    alice.chatList.open("bob")
    waitFor(Ui.Chat.Message)
    check(!seesDescription("Close sheet") && !seesDescription("Cancel file preview")) { "the attachment sheet or a file preview is shown before Attach" }
    attachFile(source)
    click(described("Cancel file preview"))
    poll(what = "the preview of $NOTES_NAME removed") { !seesDescription("Cancel file preview") && !seesText(NOTES_NAME) }
    attachFile(source)
    typeInto(Ui.Chat.Message, CAPTION)
    hideKeyboard()
    click(Ui.Chat.Send)
    poll(what = "the preview and the text cleared after Send") { !seesDescription("Cancel file preview") && editableText(Ui.Chat.Message).isNullOrEmpty() }
    bob.expectsMessage("alice> $CAPTION", CONNECT_TIMEOUT_MS)
    bob expectsMessage "alice> sends file $NOTES_NAME (${NOTES.length} bytes / ${NOTES.length} bytes)"
    bob expects "use /fr 1 [<dir>/ | <path>] to receive it"
    bob.receivesFile(1, NOTES_NAME, "alice")
  }

  @Test fun attachFile_tooLarge_showsLargeFileAlert() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val huge = File(exchangeDir(), HUGE_NAME)
    RandomAccessFile(huge, "rw").use { it.setLength(MAX_FILE_SIZE_XFTP + 1) }
    try {
      val chat = alice.chatList.open("bob")
      waitFor(Ui.Chat.Message)
      check(!seesText("Large file!")) { "the `Large file!` alert is shown before the file is picked" }
      openAttachmentSheet()
      openFile(huge) { click(Ui.Chat.Attach("file")) }
      poll(what = "the `Large file!` alert with OK and no preview of $HUGE_NAME") {
        seesText("Large file!") && "OK" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) && !seesDescription("Cancel file preview")
      }
      click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
      poll(what = "the alert closed and still no preview of $HUGE_NAME") {
        !seesText("Large file!") && !seesDescription("Cancel file preview") && !seesText(HUGE_NAME)
      }
      chat.send("after")
      bob.expectsMessage("alice> after", CONNECT_TIMEOUT_MS)
    } finally {
      huge.delete()
    }
  }

  @Test fun attachImage_withQuote_peerSeesQuote() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val photo = jpegFile(PHOTO_NAME)
    bob sends "@alice $QUESTION"
    awaitRowText(alice, "bob", QUESTION)
    val chat = alice.chatList.open("bob")
    chat.menu(QUESTION, "reply")
    poll(what = "the reply bar and no image preview") { seesDescription("Context icon") && !seesDescription("Cancel image preview") }
    attachImage(photo)
    typeInto(Ui.Chat.Message, IMAGE_CAPTION)
    hideKeyboard()
    click(Ui.Chat.Send)
    poll(what = "the image preview, the reply bar and the caption cleared after Send") {
      !seesDescription("Cancel image preview") && !seesDescription("Context icon") && editableText(Ui.Chat.Message).isNullOrEmpty()
    }
    bob.expectsMessage("alice> > $QUESTION", CONNECT_TIMEOUT_MS)
    bob expects "      $IMAGE_CAPTION"
    val sent = bob.receivesCompressedImage("alice> ", 1)
    bob.receivesFile(1, sent, "alice")
  }

  @Test fun attach_groupFilesProhibited_buttonDisabled() = uiTest {
    val alice = app("alice")
    val chat = emptyGroup(alice, "team")
    val attach = described("Attach")
    chat.send("draft")
    poll(what = "the attach button enabled in team") { exists(attach) && isEnabled(attach) }
    hideKeyboard()
    chat.menu("draft", "edit")
    poll(what = "the edit bar with `draft` in the field and the attach button disabled") {
      editableText(Ui.Chat.Message) == "draft" && exists(described("Cancel")) && exists(attach) && !isEnabled(attach)
    }
    hideKeyboard()
    click(described("Cancel"))
    poll(what = "the edit bar closed and the attach button enabled again") {
      !exists(described("Cancel")) && editableText(Ui.Chat.Message).isNullOrEmpty() && exists(attach) && isEnabled(attach)
    }
    val info = chat.groupInfo()
    val prefs = info.preferences()
    prefs.toggle(GroupFeature.Files)
    prefs.save()
    poll(what = "files and media prohibited in team") { groupInfoOf("team").fullGroupPreferences.files.enable == GroupFeatureEnabled.OFF }
    prefs.backClosing()
    info.close()
    poll(what = "the attach button disabled in team with files prohibited") { exists(attach) && !isEnabled(attach) }
    chat.send("text still goes")
  }

  private companion object {
    const val NOTES_NAME = "attach-notes.txt"
    const val NOTES = "Notes attached through the file chooser"
    const val CAPTION = "hi, sending a file"
    const val HUGE_NAME = "attach-huge.bin"
    const val PHOTO_NAME = "attach-photo.jpg"
    const val QUESTION = "hi alice"
    const val IMAGE_CAPTION = "hey bob"
  }
}
