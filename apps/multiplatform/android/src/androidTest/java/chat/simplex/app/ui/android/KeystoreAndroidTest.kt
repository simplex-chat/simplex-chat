package chat.simplex.app.ui.android

import androidx.compose.ui.state.ToggleableState
import chat.simplex.app.ui.harness.DB_WORK_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.chatData
import chat.simplex.app.ui.harness.onboarding
import chat.simplex.app.ui.harness.typeSecret
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.chatCloseStore
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.platform.initChatController
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.database.deleteChatDatabaseFilesAndState
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.DBMigrationResult
import chat.simplex.common.views.helpers.DatabaseUtils
import chat.simplex.common.views.onboarding.OnboardingStage
import kotlinx.coroutines.runBlocking
import org.junit.Test
import java.security.KeyStore

class KeystoreAndroidTest : UiTest() {
  @Test fun keystore_freshDb_randomPassphraseFooter() = uiTest {
    val alice = app(null)
    withBackgroundServiceAllowed {
      freshInstallDatabaseViaApi()
      val randomKey = DatabaseUtils.ksDatabasePassword.get()
      check(!randomKey.isNullOrEmpty() && chatModel.chatDbEncrypted.value == true && storedSetting("InitialRandomDBPassphrase") == "true") {
        "the fresh database: key in the Keystore ${!randomKey.isNullOrEmpty()}, encrypted ${chatModel.chatDbEncrypted.value}, random ${storedSetting("InitialRandomDBPassphrase")}"
      }
      val o = onboarding()
      o.createProfile("alice")
      check(o.stage == OnboardingStage.Step3_ChooseServerOperators) { "the step after Create profile is ${o.stage}, not Your network" }
      click(Ui.Onboarding.Continue)
      poll(what = "Network commitments with no alert over it") { o.stage == OnboardingStage.Step4_NetworkCommitments && exists(Ui.Onboarding.Accept) && !AlertManager.shared.hasAlertsShown() }
      click(Ui.Onboarding.Accept)
      o.waitForChatList()
      check(DatabaseUtils.ksDatabasePassword.get() == randomKey && storedSetting("StoreDBPassphrase") == "true") { "onboarding changed the random key or stopped storing it" }

      chatData(alice).management().passphrase()
      val stored = storedFooter()
      poll(what = "the random-passphrase footers") { seesText(stored) && seesText(RANDOM) }
      check(toggleState(Ui.DbPassphrase.Save) == ToggleableState.On && !isEnabled(Ui.DbPassphrase.Save)) {
        "Save passphrase in Keystore: ${toggleState(Ui.DbPassphrase.Save)}, enabled ${isEnabled(Ui.DbPassphrase.Save)}"
      }
      check(!exists(Ui.DbPassphrase.Current) && !seesText(NO_RECOVERY, substring = true)) { "the random passphrase screen asks for the current passphrase or shows the no-recovery note" }

      typeSecret(Ui.DbPassphrase.New, NEW)
      typeSecret(Ui.DbPassphrase.Confirm, NEW)
      hideKeyboard()
      click(Ui.DbPassphrase.Update)
      poll(what = "the Change database passphrase? alert naming the Keystore") {
        seesText("Change database passphrase?") && seesText(WILL_STORE, substring = true) && seesText(STORE_SECURELY, substring = true) &&
          "Update" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
      }
      click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
      poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the Database encrypted! alert") { seesText("Database encrypted!") }
      click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
      poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the chat running again and the Current passphrase field") {
        !AlertManager.shared.hasAlertsShown() && chatModel.chatRunning.value == true && exists(Ui.DbPassphrase.Current)
      }
      poll(what = "the stored-passphrase footers without the random one") { seesText(stored) && seesText(NO_RECOVERY, substring = true) && !seesText(RANDOM) }
      check(toggleState(Ui.DbPassphrase.Save) == ToggleableState.On && isEnabled(Ui.DbPassphrase.Save)) {
        "Save passphrase in Keystore after the change: ${toggleState(Ui.DbPassphrase.Save)}, enabled ${isEnabled(Ui.DbPassphrase.Save)}"
      }
      check(DatabaseUtils.ksDatabasePassword.get() == NEW && storedSetting("InitialRandomDBPassphrase") == "false") {
        "after the change: Keystore holds the new passphrase ${DatabaseUtils.ksDatabasePassword.get() == NEW}, random ${storedSetting("InitialRandomDBPassphrase")}"
      }
    }
  }

  @Test fun keystore_keyMissing_showsNotFoundAlert() = uiTest {
    val alice = app("alice")
    val notes = alice.chatList.open(NOTES)
    notes.send(NOTE)
    notes.itemId(NOTE)
    backToChatList()
    encryptDatabaseViaApi(NEW)
    check(DatabaseUtils.ksDatabasePassword.get() == NEW && storedSetting("StoreDBPassphrase") == "true") { "the passphrase is not stored in the Keystore before the key is lost" }
    deleteKeystoreKeyViaApi()
    check(storedSetting("EncryptedDBPassphrase") != null && !seesText(NOT_FOUND)) { "the encrypted passphrase is gone from the prefs, or the alert shows before the restart" }
    val after = restart(ready = { chatModel.chatDbStatus.value is DBMigrationResult.ErrorNotADatabase && !chatModel.ctrlInitInProgress.value }, what = "the database left locked without its key")
    poll(what = "the Wrong database passphrase alert saying the passphrase is not in the Keystore") {
      AlertManager.shared.hasAlertsShown() && seesText("Wrong database passphrase") && seesText(NOT_FOUND)
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the Encrypted database prompt with Save passphrase and open chat") {
      !AlertManager.shared.hasAlertsShown() && seesText("Encrypted database") && "Save passphrase and open chat" in textsOf(Ui.DbError.Open)
    }
    typeSecret(Ui.DbError.Passphrase, NEW)
    pressEnter(Ui.DbError.Passphrase)
    poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the chat list with alice's note after the keyboard's Done key") {
      chatModel.chatDbStatus.value == DBMigrationResult.OK && chatModel.chatRunning.value == true && NOTE in after.chatList.rowTexts(NOTES)
    }
    check(DatabaseUtils.ksDatabasePassword.get() == NEW) { "Save passphrase and open chat did not store the passphrase in the Keystore again" }
  }

  private fun freshInstallDatabaseViaApi() {
    val ctrl = chatController.getChatCtrl()
    if (ctrl != null && ctrl != -1L) chatCloseStore(ctrl).let { check(it.isEmpty()) { "chatCloseStore failed: $it" } }
    chatController.setChatCtrl(null)
    deleteChatDatabaseFilesAndState()
    appPrefs.initialRandomDBPassphrase.set(false)
    runBlocking { initChatController() }
    check(chatModel.chatDbStatus.value == DBMigrationResult.OK) { "fresh database: ${chatModel.chatDbStatus.value}" }
  }

  private fun encryptDatabaseViaApi(passphrase: String) {
    val current = DatabaseUtils.ksDatabasePassword.get() ?: ""
    val error = runBlocking {
      chatController.apiStopChat()
      chatModel.chatRunning.value = false
      chatController.apiStorageEncryption(currentKey = current, newKey = passphrase)
    }
    check(error == null) { "encrypting the database failed: $error" }
    DatabaseUtils.ksDatabasePassword.set(passphrase)
    appPrefs.initialRandomDBPassphrase.set(false)
    chatModel.chatDbEncrypted.value = true
  }

  private fun deleteKeystoreKeyViaApi() {
    val ks = KeyStore.getInstance("AndroidKeyStore").apply { load(null) }
    ks.deleteEntry(DB_KEY_ALIAS)
    check(!ks.containsAlias(DB_KEY_ALIAS)) { "the Keystore still holds $DB_KEY_ALIAS" }
  }

  private fun storedFooter(): String =
    "Android Keystore${DatabaseUtils.ksDatabasePassword.storage()?.let { " ($it)" } ?: ""} is used to securely store passphrase - it allows notification service to work."

  private companion object {
    const val NEW = "uitest-keystore-5Hq"
    const val NOTES = "Private notes"
    const val NOTE = "written before the key was lost"
    const val DB_KEY_ALIAS = "databasePassword"
    const val RANDOM = "Database is encrypted using a random passphrase, you can change it."
    const val NO_RECOVERY = "you will NOT be able to recover or change passphrase if you lose it."
    const val WILL_STORE = "Database encryption passphrase will be updated and stored in the Keystore."
    const val STORE_SECURELY = "Please store passphrase securely, you will NOT be able to change it if you lose it."
    const val NOT_FOUND = "Passphrase not found in Keystore, please enter it manually. This may have happened if you restored the app's data using a backup tool. If it's not the case, please contact developers."
  }
}
