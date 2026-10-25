package chat.simplex.app.ui.settings

import chat.simplex.app.ui.harness.App
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.DB_WORK_TIMEOUT_MS
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.PassphraseScreen
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowText
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.chatData
import chat.simplex.app.ui.harness.chatRunningInCore
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.onboarding
import chat.simplex.app.ui.harness.typeSecret
import chat.simplex.app.ui.harness.userIdOf
import chat.simplex.common.model.API
import chat.simplex.common.model.CC
import chat.simplex.common.model.CIFileStatus
import chat.simplex.common.model.CR
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatItemTTL
import chat.simplex.common.platform.appFilesDir
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.onboarding.OnboardingStage
import kotlinx.coroutines.runBlocking
import org.junit.Test
import java.io.File
import java.util.zip.ZipFile

class ChatDataTest : UiTest() {
  @Test fun chatData_ttlDecrease_alertConfirm() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice $KEPT"
    poll(what = "bob's first message on the row") { KEPT in alice.chatList.rowTexts("bob") }
    bob sends "@alice $EXPIRED"
    poll(what = "bob's second message on the row") { EXPIRED in alice.chatList.rowTexts("bob") }
    backdateItemViaSql(alice.chatList.chat("bob").chatItems.last { it.content.text == EXPIRED }.id)
    val data = chatData(alice)
    check(seesText("This setting applies to messages in your current chat profile alice.")) { "the Messages footer does not name alice" }
    check(data.ttlText() == "never" && coreTtl() == ChatItemTTL.None) { "TTL before the change: picker ${data.ttlText()}, core ${coreTtl()}" }
    data.selectTtl(ChatItemTTL.Day)
    awaitDeletionAlert()
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the picker back to never after Cancel") { !AlertManager.shared.hasAlertsShown() && data.ttlText() == "never" }
    check(coreTtl() == ChatItemTTL.None) { "Cancel applied the TTL: ${coreTtl()}" }
    data.selectTtl(ChatItemTTL.Day)
    awaitDeletionAlert()
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "1 day in the picker and in the core") { data.ttlText() == "1 day" && coreTtl() == ChatItemTTL.Day }
    data.selectTtl(ChatItemTTL.Week)
    poll(what = "1 week in the picker and in the core") { data.ttlText() == "1 week" && coreTtl() == ChatItemTTL.Week }
    check(!AlertManager.shared.hasAlertsShown()) { "a longer TTL asked for confirmation" }
    data.close()
    poll(what = "bob's row previewing the kept message") { alice.chatList.rowTexts("bob").let { KEPT in it && EXPIRED !in it } }
    val chat = alice.chatList.open("bob")
    poll(what = "bob's chat with the kept message and without the expired one") { chat.itemTexts().let { KEPT in it && EXPIRED !in it } }
  }

  @Test fun chatData_ttlPerProfile() = uiTest {
    val alice = app("alice")
    val aliceId = userIdOf("alice")
    var data = chatData(alice)
    check(footerNames("alice") && data.ttlText() == "never" && DELETE_ALL_FILES in textsOf(Ui.Database.DeleteFiles)) {
      "Chat data with one profile: TTL ${data.ttlText()}, files item ${textsOf(Ui.Database.DeleteFiles)}"
    }
    data.selectTtl(ChatItemTTL.Week)
    awaitDeletionAlert()
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "1 week for alice in the picker and in the core") { data.ttlText() == "1 week" && coreTtlOf(aliceId) == ChatItemTTL.Week }
    data.close()
    alice.userPicker().profiles().apply { add("work"); close() }
    val workId = userIdOf("work")
    data = chatData(alice)
    poll(what = "Chat data for work with its own TTL and the files item for all profiles") {
      footerNames("work") && data.ttlText() == "never" && DELETE_FILES_ALL_PROFILES in textsOf(Ui.Database.DeleteFiles)
    }
    check(coreTtlOf(workId) == ChatItemTTL.None) { "work's TTL in the core: ${coreTtlOf(workId)}" }
    data.selectTtl(ChatItemTTL.Day)
    awaitDeletionAlert()
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "1 day for work in the picker and in the core") { data.ttlText() == "1 day" && coreTtlOf(workId) == ChatItemTTL.Day }
    data.close()
    alice.userPicker().switchTo("alice")
    data = chatData(alice)
    poll(what = "Chat data for alice with her TTL, unchanged by work's") {
      footerNames("alice") && data.ttlText() == "1 week" && DELETE_FILES_ALL_PROFILES in textsOf(Ui.Database.DeleteFiles) && coreTtlOf(aliceId) == ChatItemTTL.Week
    }
  }

  @Test fun dbMgmt_stopAndStartChat() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val db = chatData(alice).management()
    check(seesText("Chat is running") && chatRunningInCore() && storedSetting("ChatStopped") != "true") { "the chat is not running before the stop" }
    click(Ui.Database.RunChat)
    awaitStopAlert()
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the chat still running after Cancel") { !AlertManager.shared.hasAlertsShown() && seesText("Chat is running") }
    check(chatRunningInCore() && storedSetting("ChatStopped") != "true") { "Cancel stopped the chat" }
    click(Ui.Database.RunChat)
    awaitStopAlert()
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the chat stopped on screen, on disk and in the core") {
      seesText("Chat is stopped") && seesText(MOST_RECENT_DB, substring = true) && storedSetting("ChatStopped") == "true" && !chatRunningInCore()
    }
    bob sends "@alice $WHILE_STOPPED"
    click(Ui.Database.RunChat)
    poll(what = "the chat running again on screen, on disk and in the core") {
      seesText("Chat is running") && !seesText(MOST_RECENT_DB, substring = true) && storedSetting("ChatStopped") == "false" && chatRunningInCore()
    }
    db.close()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the message bob sent while the chat was stopped") { WHILE_STOPPED in alice.chatList.rowTexts("bob") }
    alice.chatList.open("bob").send(BACK_ONLINE)
    bob.expectsMessage("alice> $BACK_ONLINE", CONNECT_TIMEOUT_MS)
  }

  @KnownAppIssue("15.5 item 27", "waiting for the Database encrypted! alert", intermittent = true, androidOnly = true)
  @Test fun dbMgmt_importArchive_restoresData() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice $BEFORE_EXPORT"
    poll(what = "bob's message before the export") { BEFORE_EXPORT in alice.chatList.rowTexts("bob") }
    writeNote(alice, NOTE_BEFORE_EXPORT)
    setLinkPreviewsPrefDirectly(false)
    check(storedSetting("PrivacyLinkPreviews") == "false") { "link previews are not stored as off before the export" }
    val zip = File(exchangeDir(), "export.zip")
    val db = chatData(alice).management()
    click(Ui.Database.Export)
    poll(what = "the Set passphrase to export alert") {
      seesText("Set passphrase to export") && seesText("Database is encrypted using a random passphrase. Please change it before exporting.")
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    waitFor(Ui.DbPassphrase.New)
    val pass = PassphraseScreen(this)
    pass.setFirst(PASSPHRASE)
    pass.close()
    check(!zip.exists() && seesText("Chat is running")) { "before the export: archive exists ${zip.exists()}, texts ${texts().take(30)}" }
    saveFileAs(zip) { click(Ui.Database.Export) }
    poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the archive with both databases at the chosen path") {
      zip.length() > 0 && ZipFile(zip).use { z -> z.entries().toList().map { it.name }.containsAll(listOf("simplex_v1_chat.db", "simplex_v1_agent.db")) }
    }
    poll(what = "the chat left stopped after the export") { seesText("Chat is stopped") }
    setLinkPreviewsPrefDirectly(true)
    check(storedSetting("PrivacyLinkPreviews") == "true") { "link previews are not stored as on after the export" }
    click(Ui.Database.RunChat)
    poll(what = "the chat running again") { seesText("Chat is running") && chatModel.chatRunning.value == true }
    db.close()
    writeNote(alice, NOTE_AFTER_EXPORT)
    chatData(alice).management()
    openFile(zip) { click(Ui.Database.Import) }
    poll(what = "the Import chat database? alert") { seesText("Import chat database?") && "Import" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the Chat database imported alert") { seesText("Chat database imported") && seesText("Restart the app to use imported chat database.") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the passphrase prompt with Save passphrase and open chat") {
      seesText("Encrypted database") && seesText("Database passphrase is required to open chat.") && "Save passphrase and open chat" in textsOf(Ui.DbError.Open)
    }
    check(storedSetting("EncryptedDBPassphrase") == null) { "the import left the old passphrase stored" }
    typeSecret(Ui.DbError.Passphrase, PASSPHRASE)
    click(Ui.DbError.Open)
    poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the Start chat? alert") { seesText("Start chat?") && seesText(START_CHAT_TEXT) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the chat started in the core with bob from the archive listed") { chatRunningInCore() && alice.chatList.shown("bob") }
    check(storedSetting("PrivacyLinkPreviews") == "false" && storedSetting("ShouldImportAppSettings") == "false" && storedSetting("EncryptedDBPassphrase") != null) {
      "after the import: PrivacyLinkPreviews=${storedSetting("PrivacyLinkPreviews")}, ShouldImportAppSettings=${storedSetting("ShouldImportAppSettings")}, passphrase stored ${storedSetting("EncryptedDBPassphrase") != null}"
    }
    val notes = alice.chatList.open("Private notes")
    poll(what = "the notes from the archive, without the note written after the export") {
      notes.itemTexts().let { NOTE_BEFORE_EXPORT in it && NOTE_AFTER_EXPORT !in it }
    }
    backToChatList()
    val chat = alice.chatList.open("bob")
    chat.itemId(BEFORE_EXPORT)
    chat.send(AFTER_IMPORT)
    bob.expectsMessage("alice> $AFTER_IMPORT", CONNECT_TIMEOUT_MS)
  }

  @KnownAppIssue("15.5 item 7", "waiting for the Error importing chat database alert")
  @Test fun dbMgmt_importInvalidArchive() = uiTest {
    val alice = app("alice")
    writeNote(alice, NOTE_BEFORE_IMPORT)
    val bad = File(exchangeDir(), "not-an-archive.zip").apply { writeText("not a zip archive") }
    val db = chatData(alice).management()
    openFile(bad) { click(Ui.Database.Import) }
    poll(what = "the Import chat database? alert") { seesText("Import chat database?") && "Import" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the Error importing chat database alert") { seesText("Error importing chat database") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "alice still the user, without the welcome screen") {
      !AlertManager.shared.hasAlertsShown() && chatModel.currentUser.value?.displayName == "alice" && !exists(Ui.Onboarding.GetStarted)
    }
    check(coreUserNames() == listOf("alice")) { "users after the failed import: ${coreUserNames()}" }
    db.close()
    poll(what = "alice's note still listed") { NOTE_BEFORE_IMPORT in alice.chatList.rowTexts("Private notes") }
  }

  @Test fun dbMgmt_deleteDatabase() = uiTest {
    val alice = app("alice")
    chatData(alice).management()
    click(Ui.Database.Delete)
    awaitDeleteAlert()
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the database screen after Cancel") { !AlertManager.shared.hasAlertsShown() && exists(Ui.Database.Delete) }
    check(coreUserNames() == listOf("alice")) { "users after Cancel: ${coreUserNames()}" }
    click(Ui.Database.Delete)
    awaitDeleteAlert()
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the Chat database deleted alert") { seesText("Chat database deleted") && seesText("Restart the app to create a new chat profile.") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the welcome screen without a restart") {
      exists(Ui.Onboarding.GetStarted) && onboarding().stage == OnboardingStage.Step1_SimpleXInfo && chatModel.currentUser.value == null
    }
    check(coreUserNames().isEmpty()) { "users after the deletion: ${coreUserNames()}" }
  }

  @Test fun chatData_deleteFilesAndMedia() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "/img @alice $FIXTURE_JPG"
    bob expectsMessage "/f @alice $FIXTURE_JPG"
    bob expects "use /fc 1 to cancel sending"
    bob.eventually("completed uploading file 1 (test.jpg) for alice", CONNECT_TIMEOUT_MS)
    bob sends "@alice $KEPT"
    awaitRowText(alice, "bob", KEPT)
    val chat = alice.chatList.open("bob")
    chat.itemId(KEPT)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's image to be received") { imageStatus() is CIFileStatus.RcvComplete }
    backToChatList()
    val data = chatData(alice)
    check(seesText("1 file(s) with total size of", substring = true) && !seesText("No received or sent files")) { "the files footer before the deletion: ${texts().take(30)}" }
    check(!appFilesDir.listFiles().isNullOrEmpty()) { "no received file on disk before the deletion" }
    click(Ui.Database.DeleteFiles)
    poll(what = "the Delete files and media? alert") {
      seesText("Delete files and media?") && seesText(DELETE_FILES_TEXT) && "Delete" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "no files on screen or on disk and the chat running again") {
      seesText("No received or sent files") && appFilesDir.listFiles().isNullOrEmpty() && chatModel.chatRunning.value == true && chatRunningInCore()
    }
    data.close()
    val after = alice.chatList.open("bob")
    after.itemId(KEPT)
    check(chatModel.chatsContext.chatItems.value.any { it.file?.fileName == "test.jpg" }) { "the image message was deleted with its file" }
    after.send(FILES_GONE)
    bob.expectsMessage("alice> $FILES_GONE", CONNECT_TIMEOUT_MS)
  }

  private fun UiScope.writeNote(app: App, text: String) {
    val notes = app.chatList.open("Private notes")
    notes.send(text)
    notes.itemId(text)
    backToChatList()
  }

  private fun UiScope.awaitDeletionAlert() =
    poll(what = "the Enable automatic message deletion? alert with Delete messages") {
      seesText("Enable automatic message deletion?") && "Delete messages" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }

  private fun UiScope.awaitStopAlert() =
    poll(what = "the Stop chat? alert with Stop") {
      seesText("Stop chat?") && seesText(STOP_CHAT_TEXT) && "Stop" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }

  private fun UiScope.awaitDeleteAlert() =
    poll(what = "the Delete chat profile? alert with Delete") {
      seesText("Delete chat profile?") && seesText(DELETE_PROFILE_TEXT) && "Delete" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }

  private fun backdateItemViaSql(itemId: Long) {
    val r = runBlocking { chatController.sendCmd(null, CC.Console("/sql chat UPDATE chat_items SET created_at = '2020-01-01 00:00:00' WHERE chat_item_id = $itemId")) }
    check(r !is API.Error) { "backdating chat item $itemId failed: ${r.responseType} ${r.details}" }
  }

  private fun setLinkPreviewsPrefDirectly(on: Boolean) = appPrefs.privacyLinkPreviews.set(on)

  private fun coreTtl(): ChatItemTTL = runBlocking { chatController.getChatItemTTL(null) }

  private fun coreTtlOf(userId: Long): ChatItemTTL {
    val r = runBlocking { chatController.sendCmd(null, CC.APIGetChatItemTTL(userId)) }
    val ttl = r.result as? CR.ChatItemTTL ?: error("/_ttl $userId failed: ${r.responseType} ${r.details}")
    return ttl.chatItemTTL?.let { ChatItemTTL.fromSeconds(it) } ?: ChatItemTTL.None
  }

  private fun UiScope.footerNames(name: String) = seesText("This setting applies to messages in your current chat profile $name.")

  private fun coreUserNames(): List<String> = runBlocking { chatController.listUsers(null) }.map { it.user.displayName }

  private fun imageStatus(): CIFileStatus? = chatModel.chatsContext.chatItems.value.lastOrNull { it.file?.fileName == "test.jpg" }?.file?.fileStatus

  private companion object {
    const val KEPT = "kept message"
    const val DELETE_ALL_FILES = "Delete all files"
    const val DELETE_FILES_ALL_PROFILES = "Delete files for all chat profiles"
    const val EXPIRED = "expired message"
    const val WHILE_STOPPED = "sent while stopped"
    const val BACK_ONLINE = "back online"
    const val BEFORE_EXPORT = "before export"
    const val AFTER_IMPORT = "after import"
    const val NOTE_BEFORE_EXPORT = "note written before the export"
    const val NOTE_AFTER_EXPORT = "note written after the export"
    const val NOTE_BEFORE_IMPORT = "note written before the import"
    const val FILES_GONE = "files gone"
    const val PASSPHRASE = "uitest-export-8Wd"
    const val FIXTURE_JPG = "tests/fixtures/test.jpg"
    const val MOST_RECENT_DB = "You must use the most recent version of your chat database on one device ONLY"
    const val STOP_CHAT_TEXT = "Stop chat to export, import or delete chat database. You will not be able to receive and send messages while the chat is stopped."
    const val START_CHAT_TEXT = "Chat is stopped. If you already used this database on another device, you should transfer it back before starting chat."
    const val DELETE_PROFILE_TEXT = "This action cannot be undone - your profile, contacts, messages and files will be irreversibly lost."
    const val DELETE_FILES_TEXT = "This action cannot be undone - all received and sent files and media will be deleted. Low resolution pictures will remain."
  }
}
