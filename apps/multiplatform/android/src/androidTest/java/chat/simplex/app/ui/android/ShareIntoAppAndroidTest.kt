package chat.simplex.app.ui.android

import android.net.Uri
import androidx.compose.ui.test.hasClickAction
import androidx.compose.ui.test.hasContentDescription
import androidx.compose.ui.test.hasTestTag
import chat.simplex.app.ui.back.backFromRoot
import chat.simplex.app.ui.back.relaunch
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.ChatScreen
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.described
import chat.simplex.app.ui.harness.jpegFile
import chat.simplex.app.ui.harness.receivesCompressedImage
import chat.simplex.app.ui.harness.receivesFile
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.GroupFeatureEnabled
import chat.simplex.common.model.GroupPreferences
import chat.simplex.common.model.GroupProfile
import chat.simplex.common.model.RoleGroupPreference
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.SharedContent
import chat.simplex.common.views.helpers.ModalManager
import kotlinx.coroutines.runBlocking
import org.junit.Test
import java.io.File

class ShareIntoAppAndroidTest : UiTest() {
  @Test fun shareIn_text_toContact() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val bobId = alice.chatList.chatId("bob")
    val chat = alice.chatList.open("bob")
    chat.info()
    waitFor(Ui.Info.Header)
    check(ModalManager.end.hasModalsOpen() && shareListTitle() == null) { "bob's contact info is not open over his chat before the share" }
    deliverToApp(shareTextIntent(TEXT))
    poll(what = "the share list titled `$SHARE_MESSAGE` in place of bob's chat and contact info") {
      shareListTitle() == SHARE_MESSAGE && chatModel.chatId.value == null && !ModalManager.end.hasModalsOpen() && !exists(Ui.Info.Header) && exists(Ui.Share.Row(bobId))
    }
    alice.chatList.shareTo("bob")
    poll(what = "bob's chat with the shared text in the message field") { chatModel.chatId.value == bobId && editableText(Ui.Chat.Message) == TEXT }
    chat.sendComposed()
    bob.expectsMessage("alice> $TEXT", CONNECT_TIMEOUT_MS)
  }

  @Test fun shareIn_file_toContact() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val bobId = alice.chatList.chatId("bob")
    val pdf = File(exchangeDir(), PDF_NAME).apply { writeText(PDF_CONTENT) }
    deliverToApp(shareStreamIntent("application/pdf", contentUri(pdf), CAPTION))
    awaitShareList(SHARE_FILE)
    alice.chatList.shareTo("bob")
    poll(what = "bob's chat with the preview of $PDF_NAME and the shared caption") {
      chatModel.chatId.value == bobId && seesDescription("Cancel file preview") && seesText(PDF_NAME) && editableText(Ui.Chat.Message) == CAPTION
    }
    ChatScreen(this).sendComposed()
    poll(what = "the file preview cleared after Send") { !seesDescription("Cancel file preview") }
    bob.expectsMessage("alice> $CAPTION", CONNECT_TIMEOUT_MS)
    bob expectsMessage "alice> sends file $PDF_NAME (${PDF_CONTENT.length} bytes / ${PDF_CONTENT.length} bytes)"
    bob expects "use /fr 1 [<dir>/ | <path>] to receive it"
    bob.receivesFile(1, PDF_NAME, "alice")
  }

  @Test fun shareIn_image_toContact() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val bobId = alice.chatList.chatId("bob")
    val (one, two, three) = listOf("share-1.jpg", "share-2.jpg", "share-3.jpg").map { contentUri(jpegFile(it)) }
    deliverToApp(shareStreamIntent("image/jpeg", one))
    awaitShareList(SHARE_MEDIA)
    alice.chatList.shareTo("bob")
    poll(what = "bob's chat with one image preview") { chatModel.chatId.value == bobId && seesDescription("Cancel image preview") && previewImages() == 1 }
    sendImages()
    bob.receivesCompressedImage("alice> ", 1)

    deliverToApp(shareStreamsIntent("image/*", listOf(two, three)))
    awaitShareList(SHARE_MEDIA)
    alice.chatList.shareTo("bob")
    poll(what = "bob's chat with two image previews") { chatModel.chatId.value == bobId && seesDescription("Cancel image preview") && previewImages() == 2 }
    sendImages()
    bob.receivesCompressedImage("alice> ", 2)
    bob.receivesCompressedImage("alice> ", 3)
  }

  @Test fun shareIn_fileSchemeUri_invalidPathAlert() = uiTest {
    app("alice").chatList.row(NOTES)
    val fileUri = Uri.parse("file:///sdcard/Download/shared.jpg")
    val intents = listOf(
      shareStreamIntent("image/jpeg", fileUri),
      shareStreamsIntent("image/*", listOf(contentUri(jpegFile("share-ok.jpg")), fileUri)),
      shareStreamIntent("text/plain", fileUri, TEXT),
      shareStreamIntent("application/pdf", fileUri),
    )
    for (intent in intents) {
      val what = "${intent.action} ${intent.type}"
      check(!AlertManager.shared.hasAlertsShown() && shareListTitle() == null) { "an alert or the share list is shown before $what" }
      deliverToApp(intent)
      poll(what = "the Invalid file path alert for $what") { seesText("Invalid file path") && seesText(INVALID_PATH) }
      check(shareListTitle() == null && chatModel.sharedContent.value == null) { "$what with a file: URI was shared: ${chatModel.sharedContent.value}" }
      click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
      poll(what = "the alert closed over the chat list after $what") { !AlertManager.shared.hasAlertsShown() && exists(Ui.ChatList.Search) }
    }
  }

  @KnownAppIssue("15.5 item 32", "a shared PDF and JPEG gave no feedback")
  @Test fun shareIn_sendMultipleNonMedia_giveFeedback() = uiTest {
    val alice = app("alice")
    val notesId = alice.chatList.chatId(NOTES)
    alice.chatList.open(NOTES)
    waitFor(Ui.Chat.Message)
    check(chatModel.chatId.value == notesId) { "Private notes did not open" }
    val pdf = contentUri(File(exchangeDir(), PDF_NAME).apply { writeText(PDF_CONTENT) })
    deliverToApp(shareStreamsIntent("*/*", listOf(pdf, contentUri(jpegFile("share-mixed.jpg")))))
    poll(what = "a shared PDF and JPEG gave no feedback: an alert or a share list") { AlertManager.shared.hasAlertsShown() || shareListTitle() != null }
    check(chatModel.sharedContent.value !is SharedContent.Media) { "a shared PDF and JPEG were taken as media: ${chatModel.sharedContent.value}" }
  }

  @Test fun shareIn_fileToGroupWithFilesOff_alert() = uiTest {
    app("alice").chatList.row(NOTES)
    newGroupViaApi(NO_FILES, GroupPreferences(files = RoleGroupPreference(GroupFeatureEnabled.OFF)))
    newGroupViaApi(NO_LINKS, GroupPreferences(simplexLinks = RoleGroupPreference(GroupFeatureEnabled.OFF)))
    val alice = restart()
    poll(what = "both groups loaded after the restart") { chatModel.chats.value.count { it.chatInfo is ChatInfo.Group } == 2 }
    val noFilesId = alice.chatList.chatId(NO_FILES)
    val noLinksId = alice.chatList.chatId(NO_LINKS)
    val linkText = "join me: ${invitationLinkViaApi()}"
    val pdf = File(exchangeDir(), PDF_NAME).apply { writeText(PDF_CONTENT) }

    deliverToApp(shareStreamIntent("application/pdf", contentUri(pdf)))
    awaitShareList(SHARE_FILE)
    alice.chatList.shareTo(NO_FILES)
    awaitProhibitedAlert()
    alice.chatList.shareTo(NO_LINKS)
    poll(what = "$NO_LINKS open with the preview of $PDF_NAME") { chatModel.chatId.value == noLinksId && seesDescription("Cancel file preview") && seesText(PDF_NAME) }

    deliverToApp(shareTextIntent(linkText))
    awaitShareList(SHARE_MESSAGE)
    alice.chatList.shareTo(NO_LINKS)
    awaitProhibitedAlert()
    alice.chatList.shareTo(NO_FILES)
    poll(what = "$NO_FILES open with the link in the message field") { chatModel.chatId.value == noFilesId && editableText(Ui.Chat.Message) == linkText }
  }

  @Test fun shareIn_systemBack_dropsContentAndFinishes() = uiTest {
    app("alice").chatList.row(NOTES)
    deliverToApp(shareTextIntent(TEXT))
    awaitShareList(SHARE_MESSAGE)
    backFromRoot()
    check(chatModel.sharedContent.value == null) { "Back left the shared content: ${chatModel.sharedContent.value}" }
    relaunch {
      poll(what = "the chat list, not the share list, after relaunching") { exists(Ui.ChatList.Search) && shareListTitle() == null }
    }
  }

  @Test fun shareIn_chatStopped_showsStoppedIcon() = uiTest {
    val alice = app("alice")
    val notesId = alice.chatList.chatId(NOTES)
    deliverToApp(shareTextIntent(TEXT))
    awaitShareList(SHARE_MESSAGE)
    check(!seesDescription(STOPPED) && rowTappable(notesId)) { "the share list of a running chat: stopped icon ${seesDescription(STOPPED)}, notes tappable ${rowTappable(notesId)}" }
    click(described("Back"))
    poll(what = "the chat list after the toolbar Back, with the shared text dropped") {
      shareListTitle() == null && exists(Ui.ChatList.Search) && chatModel.sharedContent.value == null
    }

    stopChatFromChatData(alice)
    deliverToApp(shareTextIntent(TEXT))
    awaitShareList(SHARE_MESSAGE)
    check(seesDescription(STOPPED) && !rowTappable(notesId)) { "the share list of a stopped chat: stopped icon ${seesDescription(STOPPED)}, notes tappable ${rowTappable(notesId)}" }
    click(described(STOPPED))
    poll(what = "the `$STOPPED` alert with how to start the chat") { AlertManager.shared.hasAlertsShown() && seesText(STOPPED) && seesText(START_HINT) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the alert closed over the share list") { !AlertManager.shared.hasAlertsShown() && shareListTitle() == SHARE_MESSAGE }
  }

  private fun UiScope.previewImages(): Int =
    rule.onAllNodes(hasContentDescription("preview image"), useUnmergedTree = true).fetchSemanticsNodes().size

  private fun UiScope.sendImages() {
    click(Ui.Chat.Send)
    poll(what = "the image previews cleared after Send") { !seesDescription("Cancel image preview") }
  }

  private fun UiScope.awaitProhibitedAlert() {
    poll(what = "the `Cannot send message` alert") { seesText("Cannot send message") && seesText("Selected chat preferences prohibit this message.") }
    check(chatModel.chatId.value == null) { "a prohibited chat opened: ${chatModel.chatId.value}" }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the alert closed over the share list") { !AlertManager.shared.hasAlertsShown() && shareListTitle() != null }
  }

  private fun UiScope.rowTappable(chatId: String): Boolean {
    waitFor(Ui.Share.Row(chatId))
    return rule.onAllNodes(hasTestTag(Ui.Share.Row(chatId).tag) and hasClickAction()).fetchSemanticsNodes().isNotEmpty()
  }

  private fun newGroupViaApi(name: String, prefs: GroupPreferences) {
    runBlocking { chatController.apiNewGroup(null, incognito = false, GroupProfile(name, "", null, groupPreferences = prefs)) } ?: error("apiNewGroup($name) failed")
  }

  private fun invitationLinkViaApi(): String =
    runBlocking { chatController.apiAddContact(null, incognito = false) }.first?.first?.simplexChatUri(short = false) ?: error("apiAddContact failed")

  private companion object {
    const val NOTES = "Private notes"
    const val TEXT = "hello from share"
    const val CAPTION = "the notes, shared from another app"
    const val PDF_NAME = "share-notes.pdf"
    const val PDF_CONTENT = "Notes shared into SimpleX from another app"
    const val NO_FILES = "nofiles"
    const val NO_LINKS = "nolinks"
    const val SHARE_MESSAGE = "Share message…"
    const val SHARE_MEDIA = "Share media…"
    const val SHARE_FILE = "Share file…"
    const val STOPPED = "Chat is stopped"
    const val START_HINT = "You can start chat via app Settings / Database or by restarting the app."
    const val INVALID_PATH = "You shared an invalid file path. Report the issue to the app developers."
  }
}
