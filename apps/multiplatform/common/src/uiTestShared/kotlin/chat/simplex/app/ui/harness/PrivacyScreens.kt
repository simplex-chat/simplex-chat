package chat.simplex.app.ui.harness

import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.AlertManager
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.jsonArray
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive

fun UiScope.yourPrivacy(app: App): PrivacyScreen {
  app.settings()
  click(Ui.Settings.Privacy)
  waitFor(Ui.Privacy.More)
  return PrivacyScreen(this)
}

class PrivacyScreen(private val s: UiScope) {
  fun more(): PrivacyScreen {
    s.click(Ui.Privacy.More)
    s.waitFor(Ui.Privacy.ShowLastMessages)
    return this
  }

  fun lock(): LockScreen {
    s.click(Ui.Privacy.Lock)
    s.waitFor(Ui.Lock.Enable)
    return LockScreen(s)
  }

  fun closeMore() {
    s.escape()
    s.poll(what = "More privacy to close") { !s.exists(Ui.Privacy.ShowLastMessages) && s.exists(Ui.Privacy.More) }
  }

  fun close() {
    s.escape()
    s.poll(what = "Your privacy to close") { !s.exists(Ui.Privacy.More) && s.exists(Ui.Settings.Privacy) }
    s.escape()
    s.poll(what = "Your settings to close") { !s.exists(Ui.Settings.Privacy) && s.exists(Ui.ChatList.Search) }
  }
}

class LockScreen(private val s: UiScope) {
  val passcode = PasscodeScreen(s)

  fun enable(code: String) {
    s.click(Ui.Lock.Enable)
    passcode.await(NEW_PASSCODE)
    passcode.enter(code)
    passcode.submit()
    passcode.await(CONFIRM_PASSCODE)
    passcode.enter(code)
    passcode.submit()
    passcode.confirmAlert(PASSCODE_SET)
    s.poll(what = "Lock after and Change passcode on the lock screen") { s.exists(Ui.Lock.Delay) && s.exists(Ui.Lock.ChangePasscode) }
  }

  fun close() {
    s.escape()
    s.poll(what = "SimpleX Lock to close") { !s.exists(Ui.Lock.Enable) && s.exists(Ui.Privacy.Lock) }
    PrivacyScreen(s).close()
  }

  companion object {
    const val NEW_PASSCODE = "New Passcode"
    const val CONFIRM_PASSCODE = "Confirm passcode"
    const val ENTER_PASSCODE = "Enter Passcode"
    const val PASSCODE_SET = "Passcode set!"
  }
}

class PasscodeScreen(private val s: UiScope) {
  fun await(title: String) =
    s.poll(what = "the `$title` passcode screen") { s.seesText(title) && s.exists(Ui.Passcode.Submit) }

  fun enter(code: String) {
    for (c in code) s.click(Ui.Passcode.Digit(c.digitToInt()))
  }

  fun submitLabel(): String? = s.textsOf(Ui.Passcode.Submit).firstOrNull()

  fun submit() = s.click(Ui.Passcode.Submit)

  fun clear() = s.click(Ui.Passcode.Clear)

  fun backspace() = s.click(Ui.Passcode.Backspace)

  fun gone(): Boolean = !s.exists(Ui.Passcode.Submit)

  fun confirmAlert(title: String) {
    s.poll(what = "the `$title` alert") { s.seesText(title) && AlertManager.shared.hasAlertsShown() }
    s.click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    s.poll(what = "the `$title` alert to close") { !AlertManager.shared.hasAlertsShown() }
  }

  fun authFailed() {
    s.poll(what = "the passcode screen to close") { gone() }
    s.poll(what = "the Authentication failed alert") { s.seesText(AUTH_FAILED) && s.seesText(NOT_VERIFIED) }
    confirmAlert(AUTH_FAILED)
  }

  private companion object {
    const val AUTH_FAILED = "Authentication failed"
    const val NOT_VERIFIED = "You could not be verified; please try again."
  }
}

fun PeerLines.groupId(group: String): Long {
  sends("/i #$group")
  val id = eventuallyMatching(Regex("group ID: (\\d+)")).groupValues[1].toLong()
  eventuallyMatching(Regex("current members: \\d+"))
  return id
}

fun PeerLines.chatItems(chatRef: String, count: Int = 20): List<JsonObject> {
  sends("/_get chat $chatRef count=$count")
  val json = eventuallyMatching(CHAT_JSON).value
  return Json.parseToJsonElement(json).jsonObject.getValue("chatItems").jsonArray.map { it.jsonObject }
}

fun PeerLines.item(chatRef: String, text: String): JsonObject? =
  chatItems(chatRef).lastOrNull { it.getValue("meta").jsonObject["itemText"]?.jsonPrimitive?.content == text }

fun PeerLines.itemStatus(chatRef: String, text: String): String? =
  item(chatRef, text)?.let { it.getValue("meta").jsonObject["itemStatus"]?.jsonObject?.get("type")?.jsonPrimitive?.content }

fun PeerLines.msgContent(chatRef: String, text: String): JsonObject? =
  item(chatRef, text)?.get("content")?.jsonObject?.get("msgContent")?.jsonObject

private val CHAT_JSON = Regex("\\{.*\"chatItems\":.*")
