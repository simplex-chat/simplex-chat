package chat.simplex.app.ui.harness

import chat.simplex.common.model.CC
import chat.simplex.common.model.CR
import chat.simplex.common.model.ChatItemTTL
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle
import chat.simplex.common.ui.UiKey
import chat.simplex.common.views.helpers.AlertManager
import kotlinx.coroutines.runBlocking

const val DB_WORK_TIMEOUT_MS = 120_000L

fun UiScope.typeSecret(key: UiKey, text: String) {
  val before = editableText(key).orEmpty().length
  enterText(key, text)
  poll(what = "field ${key.tag} to hold ${before + text.length} characters") { editableText(key)?.length == before + text.length }
}

fun chatRunningInCore(): Boolean = runBlocking { chatController.sendCmd(null, CC.CheckChatRunning()) }.result is CR.ChatRunning

fun UiScope.chatData(app: App): ChatDataScreen {
  app.settings()
  click(Ui.Settings.Database)
  waitFor(Ui.Database.Management)
  return ChatDataScreen(this)
}

class ChatDataScreen(private val s: UiScope) {
  fun ttlText(): String? = s.textOf(Ui.Database.Ttl)

  fun selectTtl(ttl: ChatItemTTL) {
    s.poll(what = "the previous message deletion change to finish before opening the picker") { UiIdle.inFlight().isEmpty() }
    s.click(Ui.Database.Ttl)
    s.click(Ui.Database.Ttl.option(ttl))
  }

  fun management(): DatabaseManagementScreen {
    s.click(Ui.Database.Management)
    s.waitFor(Ui.Database.Export)
    return DatabaseManagementScreen(s)
  }

  fun close() {
    s.escape()
    s.poll(what = "Chat data to close") { !s.exists(Ui.Database.Management) && s.exists(Ui.Settings.Database) }
    s.escape()
    s.poll(what = "Your settings to close") { !s.exists(Ui.Settings.Database) && s.exists(Ui.ChatList.Search) }
  }
}

class DatabaseManagementScreen(private val s: UiScope) {
  fun passphrase(): PassphraseScreen {
    s.click(Ui.Database.Passphrase)
    s.waitFor(Ui.DbPassphrase.New)
    return PassphraseScreen(s)
  }

  fun close() {
    s.escape()
    s.poll(what = "Database passphrase & export to close") { !s.exists(Ui.Database.Export) && s.exists(Ui.Database.Management) }
    ChatDataScreen(s).close()
  }
}

class PassphraseScreen(private val s: UiScope) {
  fun setFirst(passphrase: String) {
    s.typeSecret(Ui.DbPassphrase.New, passphrase)
    s.typeSecret(Ui.DbPassphrase.Confirm, passphrase)
    s.click(Ui.DbPassphrase.Update)
    s.waitFor(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    s.click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    s.poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the Database encrypted! alert") { s.seesText("Database encrypted!") }
    s.click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    s.poll(timeoutMs = DB_WORK_TIMEOUT_MS, what = "the chat running again and the Current passphrase field") {
      !AlertManager.shared.hasAlertsShown() && chatModel.chatRunning.value == true && s.exists(Ui.DbPassphrase.Current)
    }
  }

  fun change(current: String, new: String) {
    s.typeSecret(Ui.DbPassphrase.Current, current)
    s.typeSecret(Ui.DbPassphrase.New, new)
    s.typeSecret(Ui.DbPassphrase.Confirm, new)
    s.click(Ui.DbPassphrase.Update)
    s.poll(what = "the Change database passphrase? alert with Update") {
      s.seesText("Change database passphrase?") && "Update" in s.textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    s.click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  }

  fun reopen() {
    close()
    s.click(Ui.Database.Passphrase)
    s.waitFor(Ui.DbPassphrase.New)
    for (key in listOf(Ui.DbPassphrase.Current, Ui.DbPassphrase.New, Ui.DbPassphrase.Confirm)) {
      check(s.editableText(key).isNullOrEmpty()) { "${key.tag} still holds ${s.editableText(key)?.length} characters after reopening" }
    }
  }

  fun close() {
    s.hideKeyboard()
    s.escape()
    s.poll(what = "Database passphrase to close") { !s.exists(Ui.DbPassphrase.New) && s.exists(Ui.Database.Export) }
  }
}
