package chat.simplex.app.ui.settings

import chat.simplex.app.ui.harness.App
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.CliPeer
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.groupId
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.itemStatus
import chat.simplex.app.ui.harness.linkGroupWithDirectMessages
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.app.ui.harness.switchAwayFromChat
import chat.simplex.app.ui.harness.visitNotesAndBack
import chat.simplex.app.ui.harness.yourPrivacy
import chat.simplex.common.model.CIFile
import chat.simplex.common.model.CIFileStatus
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.GroupMemberStatus
import chat.simplex.common.model.MsgContent
import chat.simplex.common.model.User
import chat.simplex.common.model.UserMsgReceiptSettings
import chat.simplex.common.platform.appFilesDir
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.platform.getAppFilePath
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle
import chat.simplex.common.views.helpers.AlertManager
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.File

private const val SHOW_PREVIEWS = "PrivacyShowChatPreviews"
private const val MESSAGE = "hello from bob"
private const val ACCEPT_IMAGES = "PrivacyAcceptImages"
private const val BLUR_RADIUS = "PrivacyMediaBlurRadius"
private const val RECEIPTS_SET = "PrivacyDeliveryReceiptsSet"
private const val ASKED_TO_RECEIVE = "Asked to receive the image"
private const val FIXTURE_JPG = "tests/fixtures/test.jpg"
private val BLUR_OPTIONS = listOf(0 to "Off", 12 to "Soft", 24 to "Medium", 48 to "Strong")
private const val SAVE_DRAFT = "PrivacySaveLastDraft"
private const val KEPT_DRAFT = "draft kept for bob"
private const val LOST_DRAFT = "draft not kept"
private const val ENCRYPT_FILES = "PrivacyEncryptLocalFiles"
private const val MEMBER_HI = "hi from the group"
private const val NOTES = "Private notes"
private const val RECEIPTS_TITLE = "Delivery receipts!"
private val JPEG_MAGIC = listOf(0xFF.toByte(), 0xD8.toByte(), 0xFF.toByte())
private val CONFIRM = Ui.Alert.Confirm(Ui.AlertSource.Shared)
private val DISMISS = Ui.Alert.Dismiss(Ui.AlertSource.Shared)

class PrivacyTest : UiTest() {
  @Test fun morePrivacy_showLastMessages() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice $MESSAGE"
    poll(what = "bob's message on the row") { MESSAGE in alice.chatList.rowTexts("bob") }
    check(storedSetting(SHOW_PREVIEWS) != "false") { "`Show last messages` is stored as off before it is turned off" }
    toggleShowLastMessages(alice)
    poll(what = "`Show last messages` stored as off") { storedSetting(SHOW_PREVIEWS) == "false" && !chatModel.showChatPreviews.value }
    closeMorePrivacy()
    poll(what = "bob's row without the message text") { alice.chatList.rowTexts("bob").let { "bob" in it && MESSAGE !in it } }
    val after = restart()
    check(storedSetting(SHOW_PREVIEWS) == "false") { "`Show last messages` is not stored as off after restart" }
    poll(what = "bob's row still without the message text after restart") { after.chatList.rowTexts("bob").let { "bob" in it && MESSAGE !in it } }
    toggleShowLastMessages(after)
    poll(what = "`Show last messages` stored as on") { storedSetting(SHOW_PREVIEWS) == "true" && chatModel.showChatPreviews.value }
    closeMorePrivacy()
    poll(what = "bob's message on the row again") { MESSAGE in after.chatList.rowTexts("bob") }
  }

  @Test fun privacy_autoAcceptImagesOff() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    check(storedSetting(ACCEPT_IMAGES) != "false") { "Auto-accept images is stored as off before it is turned off" }
    val privacy = yourPrivacy(alice)
    click(Ui.Privacy.AcceptImages)
    poll(what = "Auto-accept images stored as off") { storedSetting(ACCEPT_IMAGES) == "false" && !appPrefs.privacyAcceptImages.get() }
    privacy.close()
    bob.sendsImage(1)
    val chat = alice.chatList.open("bob")
    val first = chat.itemId("bob's first image") { it.content.msgContent is MsgContent.MCImage }
    poll(what = "the app to finish handling bob's first image") { UiIdle.inFlight().isEmpty() }
    check(fileOf(first)?.fileStatus is CIFileStatus.RcvInvitation && ASKED_TO_RECEIVE in descriptionsOf(Ui.Chat.Item(first))) {
      "the first image was not left to be received: ${fileOf(first)?.fileStatus}, ${descriptionsOf(Ui.Chat.Item(first))}"
    }
    check(appFilesDir.listFiles().isNullOrEmpty()) { "files were saved with Auto-accept images off: ${appFilesDir.listFiles()?.map { it.name }}" }
    backToChatList()
    val again = yourPrivacy(alice)
    click(Ui.Privacy.AcceptImages)
    poll(what = "Auto-accept images stored as on") { storedSetting(ACCEPT_IMAGES) == "true" && appPrefs.privacyAcceptImages.get() }
    again.close()
    bob.sendsImage(2)
    val chatAgain = alice.chatList.open("bob")
    val second = chatAgain.itemId("bob's second image") { it.content.msgContent is MsgContent.MCImage && it.id != first }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's second image to be received without a tap") { fileOf(second)?.fileStatus is CIFileStatus.RcvComplete }
    check(fileOf(first)?.fileStatus is CIFileStatus.RcvInvitation) { "turning Auto-accept images on received the earlier image: ${fileOf(first)?.fileStatus}" }
  }

  @Test fun privacy_blurMediaOptions() = uiTest {
    val alice = app("alice")
    val privacy = yourPrivacy(alice)
    check(textOf(Ui.Privacy.BlurMedia) == "Off" && storedSetting(BLUR_RADIUS) != "24") { "blur before the change: ${textOf(Ui.Privacy.BlurMedia)}, stored ${storedSetting(BLUR_RADIUS)}" }
    click(Ui.Privacy.BlurMedia)
    for ((radius, name) in BLUR_OPTIONS) {
      check(name in textsOf(Ui.Privacy.BlurMedia.option(radius))) { "blur option $radius is not `$name`: ${textsOf(Ui.Privacy.BlurMedia.option(radius))}" }
    }
    click(Ui.Privacy.BlurMedia.option(24))
    poll(what = "Medium blur shown and stored") { textOf(Ui.Privacy.BlurMedia) == "Medium" && storedSetting(BLUR_RADIUS) == "24" }
    privacy.close()
    val after = restart()
    yourPrivacy(after)
    assertEquals("Medium", textOf(Ui.Privacy.BlurMedia))
    assertEquals(24, appPrefs.privacyMediaBlurRadius.get())
  }

  @Test fun receipts_contactsAndGroupsToggles_peerReceipts() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    clearReceiptsChoiceViaApi()
    connect(bob)
    ownGroup(alice, "team", bob)
    backToChatList()
    val direct = "@${bob.contactId("alice")}"
    val team = "#${bob.groupId("team")}"
    bob sends "@alice d0"
    bob sends "#team g0"
    awaitReceipt(bob, direct, "d0")
    awaitReceipt(bob, team, "g0")
    check(storedSetting(RECEIPTS_SET) != "true") { "the receipts choice is stored before any toggle" }
    yourPrivacy(alice).more()
    click(Ui.Privacy.ReceiptsContacts)
    poll(what = "contact receipts off and the receipts choice stored") { !user().sendRcptsContacts && user().sendRcptsSmallGroups && storedSetting(RECEIPTS_SET) == "true" }
    check(!AlertManager.shared.hasAlertsShown()) { "turning contact receipts off without overrides asked for confirmation" }
    bob sends "@alice d1"
    bob sends "#team g1"
    awaitReceived(alice, "bob", "d1")
    awaitReceived(alice, "team", "g1")
    click(Ui.Privacy.ReceiptsGroups)
    poll(what = "group receipts off") { !user().sendRcptsSmallGroups }
    click(Ui.Privacy.ReceiptsContacts)
    poll(what = "contact receipts on again") { user().sendRcptsContacts }
    bob sends "@alice d2"
    bob sends "#team g2"
    awaitReceived(alice, "bob", "d2")
    awaitReceived(alice, "team", "g2")
    click(Ui.Privacy.ReceiptsGroups)
    poll(what = "group receipts on again") { user().sendRcptsSmallGroups }
    bob sends "#team g3"
    awaitReceipt(bob, direct, "d2")
    awaitReceipt(bob, team, "g3")
    assertEquals("d1 sent while contact receipts were off", "sndSent", bob.itemStatus(direct, "d1"))
    assertEquals("g1 sent while only contact receipts were off", "sndRcvd", bob.itemStatus(team, "g1"))
    assertEquals("g2 sent while group receipts were off", "sndSent", bob.itemStatus(team, "g2"))
  }

  @Test fun privacy_autoAcceptGroupInvitations() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "club", "alice")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's invitation to club pending for alice") { membershipStatus("club") == GroupMemberStatus.MemInvited }
    check(!user().autoAcceptGroupInvitations && !coreUser().autoAcceptGroupInvitations) { "group invitations are auto-accepted before the setting is turned on" }
    val privacy = yourPrivacy(alice)
    click(Ui.Privacy.AutoAcceptGroupInvitations)
    poll(what = "group invitations auto-accepted in the model and the core") { user().autoAcceptGroupInvitations && coreUser().autoAcceptGroupInvitations }
    privacy.close()
    hostGroup(bob, "team", "alice")
    bob.eventually("#team: alice joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice connected to team without accepting the invitation") { membershipStatus("team") == GroupMemberStatus.MemConnected }
    check(membershipStatus("club") == GroupMemberStatus.MemInvited) { "the invitation received before the setting was turned on was accepted: ${membershipStatus("club")}" }
  }

  @Test fun privacy_autoAcceptContactRequestsInGroups() = uiTest {
    val alice = app("alice"); val cath = cli("cath")
    linkGroupWithDirectMessages(alice, "team", cath).close()
    backToChatList()
    check(!user().autoAcceptMemberContacts && !coreUser().autoAcceptMemberContacts && directChat("cath") == null) { "member contacts are auto-accepted, or cath is a contact, before the setting is turned on" }
    val privacy = yourPrivacy(alice)
    click(Ui.Privacy.AutoAcceptMemberContacts)
    poll(what = "member contact requests auto-accepted in the model and the core") { user().autoAcceptMemberContacts && coreUser().autoAcceptMemberContacts }
    privacy.close()
    cath sends "@#team alice $MEMBER_HI"
    cath.eventuallyAll(
      lines = listOf(
        "member #team alice does not have direct connection, creating",
        "contact for member #team alice is created",
        "sent invitation to connect directly to member #team alice",
      ),
      messages = listOf("@alice $MEMBER_HI"),
      timeoutMs = CONNECT_TIMEOUT_MS,
    )
    cath.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice's direct chat with cath connected without accepting") { directChat("cath")?.chatInfo?.ready == true }
    alice.chatList.open("cath").itemId(MEMBER_HI)
    poll(what = "the message field without `Accept` or `Reject`") { exists(Ui.Chat.Message) && !exists(Ui.Chat.RequestAccept) && !exists(Ui.Chat.RequestReject) }
  }

  @Test fun morePrivacy_messageDraftDisabledClears() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val bobId = alice.chatList.chatId("bob")
    alice.chatList.open("bob")
    typeInto(Ui.Chat.Message, KEPT_DRAFT)
    visitNotesAndBack(alice, "bob")
    poll(what = "the draft restored with `Message draft` on") { editableText(Ui.Chat.Message) == KEPT_DRAFT }
    switchAwayFromChat(alice)
    poll(what = "the draft on bob's row") { alice.chatList.rowTexts("bob").any { it.endsWith(" $KEPT_DRAFT") } }
    check(storedSetting(SAVE_DRAFT) != "false") { "`Message draft` is stored as off before it is turned off" }
    val privacy = yourPrivacy(alice).more()
    click(Ui.Privacy.SaveDraft)
    poll(what = "`Message draft` stored as off") { storedSetting(SAVE_DRAFT) == "false" }
    privacy.closeMore()
    privacy.close()
    poll(what = "bob's row without the draft") { alice.chatList.rowTexts("bob").let { "bob" in it && it.none { t -> t.endsWith(" $KEPT_DRAFT") } } }
    alice.chatList.open("bob")
    poll(what = "bob's field empty after turning `Message draft` off") { chatModel.chatId.value == bobId && editableText(Ui.Chat.Message).isNullOrEmpty() }
    typeInto(Ui.Chat.Message, LOST_DRAFT)
    visitNotesAndBack(alice, "bob")
    poll(what = "bob's field empty after leaving the chat") { chatModel.chatId.value == bobId && editableText(Ui.Chat.Message).isNullOrEmpty() }
    switchAwayFromChat(alice)
    check(alice.chatList.rowTexts("bob").none { it.endsWith(" $LOST_DRAFT") }) { "a draft was saved with `Message draft` off: ${alice.chatList.rowTexts("bob")}" }
  }

  @Test fun morePrivacy_encryptLocalFilesOff() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    check(storedSetting(ENCRYPT_FILES) != "false") { "`Encrypt local files` is stored as off before it is turned off" }
    bob.sendsImage(1)
    val chat = alice.chatList.open("bob")
    val first = chat.itemId("bob's first image") { it.content.msgContent is MsgContent.MCImage }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's first image received") { fileOf(first)?.fileStatus is CIFileStatus.RcvComplete }
    fileOf(first)!!.let { check(it.fileSource?.cryptoArgs != null && !storedAsPlainJpeg(it)) { "the first image is not stored encrypted: ${it.fileSource}" } }
    backToChatList()
    val privacy = yourPrivacy(alice).more()
    click(Ui.Privacy.EncryptLocalFiles)
    poll(what = "`Encrypt local files` stored as off and applied") { storedSetting(ENCRYPT_FILES) == "false" && UiIdle.inFlight().isEmpty() }
    privacy.closeMore()
    privacy.close()
    bob.sendsImage(2)
    val again = alice.chatList.open("bob")
    val second = again.itemId("bob's second image") { it.content.msgContent is MsgContent.MCImage && it.id != first }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's second image received") { fileOf(second)?.fileStatus is CIFileStatus.RcvComplete }
    fileOf(second)!!.let { check(it.fileSource?.cryptoArgs == null && storedAsPlainJpeg(it)) { "the second image is not stored as the plain JPEG: ${it.fileSource}" } }
    fileOf(first)!!.let { check(it.fileSource?.cryptoArgs != null && !storedAsPlainJpeg(it)) { "turning encryption off changed the first image: ${it.fileSource}" } }
  }

  @Test fun deliveryReceiptsStartupView_enable() = uiTest {
    app("alice")
    turnOffContactReceiptsViaApi()
    clearReceiptsChoiceViaApi()
    restart()
    awaitReceiptsView()
    check(!coreUser().sendRcptsContacts && storedSetting(RECEIPTS_SET) == "false") { "contact receipts are on, or the choice is stored, before Enable" }
    click(Ui.DeliveryReceipts.Enable)
    poll(what = "contact receipts on in the core, the choice stored and the chat list shown") {
      coreUser().sendRcptsContacts && storedSetting(RECEIPTS_SET) == "true" && !exists(Ui.DeliveryReceipts.Enable) && exists(Ui.ChatList.Search)
    }
    val after = restart()
    poll(what = "the chat list after the restart") { after.chatList.shown(NOTES) }
    check(!exists(Ui.DeliveryReceipts.Enable) && !seesText(RECEIPTS_TITLE)) { "`$RECEIPTS_TITLE` shown again after Enable" }
  }

  @Test fun deliveryReceiptsStartupView_skip() = uiTest {
    app("alice")
    turnOffContactReceiptsViaApi()
    clearReceiptsChoiceViaApi()
    restart()
    awaitReceiptsView()
    click(Ui.DeliveryReceipts.Skip)
    awaitReceiptsDisabledAlert()
    click(CONFIRM)
    poll(what = "the chat list after OK") { !AlertManager.shared.hasAlertsShown() && !exists(Ui.DeliveryReceipts.Enable) && exists(Ui.ChatList.Search) }
    check(storedSetting(RECEIPTS_SET) == "false" && !coreUser().sendRcptsContacts) { "OK stored the choice or turned contact receipts on" }
    restart()
    awaitReceiptsView()
    click(Ui.DeliveryReceipts.Skip)
    awaitReceiptsDisabledAlert()
    click(DISMISS)
    poll(what = "the chat list and the choice stored after `Don't show again`") {
      !AlertManager.shared.hasAlertsShown() && exists(Ui.ChatList.Search) && storedSetting(RECEIPTS_SET) == "true"
    }
    check(!coreUser().sendRcptsContacts) { "`Don't show again` turned contact receipts on" }
    val after = restart()
    poll(what = "the chat list after the restart") { after.chatList.shown(NOTES) }
    check(!exists(Ui.DeliveryReceipts.Enable) && !seesText(RECEIPTS_TITLE)) { "`$RECEIPTS_TITLE` shown again after `Don't show again`" }
  }

  private fun CliPeer.sendsImage(fileId: Int) {
    this sends "/img @alice $FIXTURE_JPG"
    this expectsMessage "/f @alice $FIXTURE_JPG"
    this expects "use /fc $fileId to cancel sending"
    eventually("completed uploading file $fileId (test.jpg) for alice", CONNECT_TIMEOUT_MS)
  }

  private fun fileOf(id: Long): CIFile? = chatModel.chatsContext.chatItems.value.firstOrNull { it.id == id }?.file

  private fun user(): User = chatModel.currentUser.value!!

  private fun clearReceiptsChoiceViaApi() = appPrefs.privacyDeliveryReceiptsSet.set(false)

  private fun coreUser(): User = runBlocking { chatController.listUsers(null) }.first { it.user.activeUser }.user

  private fun turnOffContactReceiptsViaApi() {
    val u = user()
    runBlocking { chatController.apiSetUserContactReceipts(u, UserMsgReceiptSettings(enable = false, clearOverrides = false)) }
    chatModel.currentUser.value = u.copy(sendRcptsContacts = false)
  }

  private fun membershipStatus(group: String): GroupMemberStatus? =
    (chatModel.chats.value.firstOrNull { it.chatInfo.chatViewName == group }?.chatInfo as? ChatInfo.Group)?.groupInfo?.membership?.memberStatus

  private fun directChat(name: String) =
    chatModel.chats.value.firstOrNull { (it.chatInfo as? ChatInfo.Direct)?.contact?.displayName == name }

  private fun storedAsPlainJpeg(file: CIFile): Boolean {
    val bytes = File(getAppFilePath(file.fileSource!!.filePath)).readBytes()
    return bytes.size.toLong() == file.fileSize && bytes.take(JPEG_MAGIC.size) == JPEG_MAGIC
  }

  private fun UiScope.awaitReceiptsView() =
    poll(what = "the `$RECEIPTS_TITLE` screen with Enable and `Don't enable`") {
      seesText(RECEIPTS_TITLE) && exists(Ui.DeliveryReceipts.Enable) && exists(Ui.DeliveryReceipts.Skip) && !exists(Ui.ChatList.Search)
    }

  private fun UiScope.awaitReceiptsDisabledAlert() =
    poll(what = "the `Delivery receipts are disabled!` alert with OK and `Don't show again`") {
      seesText("Delivery receipts are disabled!") && seesText("You can enable them later via app Your privacy settings.") &&
        "OK" in textsOf(CONFIRM) && "Don't show again" in textsOf(DISMISS)
    }

  private fun UiScope.awaitReceipt(peer: CliPeer, chatRef: String, text: String) =
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "${peer.name} to get alice's receipt for `$text`") { peer.itemStatus(chatRef, text) == "sndRcvd" }

  private fun UiScope.awaitReceived(app: App, chat: String, text: String) =
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice to receive `$text` in $chat") { app.chatList.chat(chat).chatItems.any { it.content.text == text } }

  private fun UiScope.toggleShowLastMessages(app: App) {
    app.settings()
    click(Ui.Settings.Privacy)
    click(Ui.Privacy.More)
    click(Ui.Privacy.ShowLastMessages)
  }

  private fun UiScope.closeMorePrivacy() {
    escape()
    poll(what = "More privacy to close") { !exists(Ui.Privacy.ShowLastMessages) && exists(Ui.Privacy.More) }
    escape()
    poll(what = "Your privacy to close") { !exists(Ui.Privacy.More) && exists(Ui.Settings.Privacy) }
    escape()
    poll(what = "Your settings to close") { !exists(Ui.Settings.Privacy) && exists(Ui.ChatList.Search) }
  }
}
