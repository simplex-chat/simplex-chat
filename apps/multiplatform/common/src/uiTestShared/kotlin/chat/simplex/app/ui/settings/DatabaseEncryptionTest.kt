package chat.simplex.app.ui.settings

import chat.simplex.app.ui.harness.App
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.DB_WORK_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.chatData
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.typeSecret
import chat.simplex.common.model.API
import chat.simplex.common.model.CC
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatDatabaseFileName
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.DBMigrationResult
import kotlinx.coroutines.runBlocking
import org.junit.Test

class DatabaseEncryptionTest : UiTest() {
  @Test fun dbEncrypt_changePassphrase() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice $BEFORE_CHANGE"
    poll(what = "bob's message on the row") { BEFORE_CHANGE in alice.chatList.rowTexts("bob") }
    val pass = chatData(alice).management().passphrase()
    check(!exists(Ui.DbPassphrase.Current) && storedSetting("InitialRandomDBPassphrase") == "true") { "a passphrase is set before the first one" }
    pass.setFirst(FIRST)
    val storedFirst = storedSetting("EncryptedDBPassphrase")
    check(storedFirst != null && storedSetting("InitialRandomDBPassphrase") == "false") { "after the first passphrase: stored ${storedFirst != null}, random ${storedSetting("InitialRandomDBPassphrase")}" }
    pass.change(current = "wrong-$FIRST", new = SECOND)
    poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the Wrong passphrase! alert") { seesText("Wrong passphrase!") && seesText("Please enter correct current passphrase.") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    check(storedSetting("EncryptedDBPassphrase") == storedFirst) { "a wrong current passphrase changed the stored passphrase" }
    pass.reopen()
    pass.change(current = FIRST, new = SECOND)
    poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the Database encrypted! alert") { seesText("Database encrypted!") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the chat running again with the new passphrase stored") {
      chatModel.chatRunning.value == true && storedSetting("EncryptedDBPassphrase").let { it != null && it != storedFirst }
    }
    val after = restart()
    poll(what = "the chat list without a passphrase prompt") { after.chatList.shown("bob") && !seesText("Encrypted database") && !seesText("Wrong database passphrase") }
    val chat = after.chatList.open("bob")
    chat.itemId(BEFORE_CHANGE)
    chat.send(AFTER_CHANGE)
    bob.expectsMessage("alice> $AFTER_CHANGE", CONNECT_TIMEOUT_MS)
  }

  @Test fun dbEncrypt_removeFromSettings() = uiTest {
    val alice = app("alice")
    writeNote(alice)
    chatData(alice).management().passphrase().setFirst(FIRST)
    check(seesText(onPlatform(STORED_DESKTOP, STORED_ANDROID), substring = true) && seesText(NO_RECOVERY, substring = true)) { "the stored-passphrase footer: ${texts().take(30)}" }
    val stored = storedSetting("EncryptedDBPassphrase")
    check(stored != null && storedSetting("StoreDBPassphrase") == "true") { "no passphrase stored before Remove" }
    click(Ui.DbPassphrase.Save)
    awaitRemoveAlert()
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the passphrase still stored after Cancel") { !AlertManager.shared.hasAlertsShown() && seesText(onPlatform(STORED_DESKTOP, STORED_ANDROID), substring = true) }
    check(storedSetting("EncryptedDBPassphrase") == stored && storedSetting("StoreDBPassphrase") == "true") { "Cancel removed the stored passphrase" }
    click(Ui.DbPassphrase.Save)
    awaitRemoveAlert()
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the passphrase removed on screen and on disk") {
      seesText(NOT_STORED) && seesText(NO_RECOVERY, substring = true) && storedSetting("EncryptedDBPassphrase") == null && storedSetting("StoreDBPassphrase") == "false"
    }
    var current = restart(ready = { passphrasePrompted() }, what = "the passphrase prompt")
    poll(what = "the Encrypted database prompt with Open chat") {
      seesText("Encrypted database") && seesText("Database passphrase is required to open chat.") && "Open chat" in textsOf(Ui.DbError.Open)
    }
    typeSecret(Ui.DbError.Passphrase, FIRST.dropLast(1))
    click(Ui.DbError.Open)
    poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the Wrong passphrase! alert") {
      seesText("Wrong passphrase!") && seesText("Enter correct passphrase.") && chatModel.chatDbStatus.value is DBMigrationResult.ErrorNotADatabase
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    typeSecret(Ui.DbError.Passphrase, FIRST.takeLast(1))
    click(Ui.DbError.Open)
    poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the chat list after the right passphrase") { chatModel.chatDbStatus.value == DBMigrationResult.OK && current.chatList.shown(NOTES) }
    check(storedSetting("EncryptedDBPassphrase") == null) { "Open chat stored the passphrase" }
    val notes = current.chatList.open(NOTES)
    notes.itemId(NOTE)
    backToChatList()
    chatData(current).management().passphrase()
    check(seesText(NOT_STORED)) { "the passphrase screen after Open chat: ${texts().take(30)}" }
    click(Ui.DbPassphrase.Save)
    poll(what = "storing turned on but nothing stored yet") {
      seesText(onPlatform(WILL_STORE_DESKTOP, WILL_STORE_ANDROID)) && storedSetting("StoreDBPassphrase") == "true" && storedSetting("EncryptedDBPassphrase") == null
    }
    check(!AlertManager.shared.hasAlertsShown()) { "turning storing on asked for confirmation" }
    current = restart(ready = { passphrasePrompted() }, what = "the passphrase prompt")
    poll(what = "the Encrypted database prompt with Save passphrase and open chat") {
      seesText("Encrypted database") && "Save passphrase and open chat" in textsOf(Ui.DbError.Open)
    }
    typeSecret(Ui.DbError.Passphrase, FIRST)
    click(Ui.DbError.Open)
    poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the chat list with the passphrase stored") {
      chatModel.chatDbStatus.value == DBMigrationResult.OK && current.chatList.shown(NOTES) && storedSetting("EncryptedDBPassphrase") != null
    }
    current = restart()
    poll(what = "the chat list without a passphrase prompt") { current.chatList.shown(NOTES) && !seesText("Encrypted database") }
  }

  @Test fun dbError_incompatibleDowngrade() = uiTest {
    app("alice")
    waitFor(Ui.ChatList.Search)
    check(chatModel.chatDbStatus.value == DBMigrationResult.OK && !seesText(INCOMPATIBLE)) { "database status before the fixture: ${chatModel.chatDbStatus.value}" }
    addFutureMigrationViaSql()
    restart(ready = { chatModel.chatDbStatus.value is DBMigrationResult.ErrorMigration && !chatModel.ctrlInitInProgress.value }, what = "the database migration error")
    poll(what = "Incompatible database version naming the file and the migration") {
      seesText(INCOMPATIBLE) && seesText("File: $chatDatabaseFileName") &&
        seesText("Error: database version is newer than the app, but no down migration for: $FUTURE_MIGRATION")
    }
    check(!exists(Ui.DbError.Passphrase) && !exists(Ui.ChatList.Search)) { "the incompatible database screen offers a passphrase field or the chat list" }
  }

  private fun UiScope.writeNote(app: App) {
    val notes = app.chatList.open(NOTES)
    notes.send(NOTE)
    notes.itemId(NOTE)
    backToChatList()
  }

  private fun UiScope.awaitRemoveAlert() =
    poll(what = "the Remove passphrase alert with Remove") {
      seesText(onPlatform("Remove passphrase from settings?", "Remove passphrase from Keystore?")) && "Remove" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }

  private fun passphrasePrompted(): Boolean =
    chatModel.chatDbStatus.value is DBMigrationResult.ErrorNotADatabase && !chatModel.ctrlInitInProgress.value

  private fun onPlatform(desktop: String, android: String): String = if (appPlatform.isDesktop) desktop else android

  private fun addFutureMigrationViaSql() {
    val r = runBlocking { chatController.sendCmd(null, CC.Console("/sql chat INSERT INTO migrations (name, ts, down) VALUES ('$FUTURE_MIGRATION', '2999-12-31 00:00:00', NULL)")) }
    check(r !is API.Error) { "adding the future migration failed: ${r.responseType} ${r.details}" }
  }

  private companion object {
    const val FIRST = "uitest-first-7Kq"
    const val SECOND = "uitest-second-3Zp"
    const val BEFORE_CHANGE = "before passphrase change"
    const val AFTER_CHANGE = "after passphrase change"
    const val NOTES = "Private notes"
    const val NOTE = "kept across restarts"
    const val FUTURE_MIGRATION = "29991231_uitest_newer"
    const val INCOMPATIBLE = "Incompatible database version"
    const val STORED_DESKTOP = "The passphrase is stored in settings as plaintext."
    const val STORED_ANDROID = "is used to securely store passphrase - it allows notification service to work."
    const val NO_RECOVERY = "you will NOT be able to recover or change passphrase if you lose it."
    const val NOT_STORED = "You have to enter passphrase every time the app starts - it is not stored on the device."
    const val WILL_STORE_DESKTOP = "The passphrase will be stored in settings as plaintext after you change it or restart the app."
    const val WILL_STORE_ANDROID = "Android Keystore will be used to securely store passphrase after you restart the app or change passphrase - it will allow receiving notifications."
  }
}
