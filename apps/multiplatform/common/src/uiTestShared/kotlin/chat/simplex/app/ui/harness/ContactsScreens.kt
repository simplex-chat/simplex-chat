package chat.simplex.app.ui.harness

import chat.simplex.common.model.AddressSettings
import chat.simplex.common.model.ChatFeature
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.Contact
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.ModalManager

fun contactOf(name: String): Contact =
  (chatModel.chats.value.first { it.chatInfo.chatViewName == name }.chatInfo as ChatInfo.Direct).contact

fun savedAddressSettings(): AddressSettings? = chatModel.userAddress.value?.addressSettings

fun UiScope.shareAddress(): String {
  click(Ui.Address.Share)
  var link: String? = null
  poll(what = "the address shared") { link = sharedText()?.takeIf { it.isNotBlank() }; link != null }
  return link!!
}

class AddressSettingsScreen(private val s: UiScope) {
  init {
    s.poll(what = "the `Address settings` screen") { s.exists(Ui.Address.AutoAccept) && s.exists(Ui.Address.WelcomeMessage) }
    s.waitStable(Ui.Address.AutoAccept)
  }

  fun toggleAutoAccept() = s.click(Ui.Address.AutoAccept)

  fun autoAcceptEnabled(): Boolean = s.isEnabled(Ui.Address.AutoAccept)

  fun welcome(text: String) {
    s.typeInto(Ui.Address.WelcomeMessage, text)
    s.hideKeyboard()
  }

  fun replaceWelcome(text: String) {
    s.replaceText(Ui.Address.WelcomeMessage, text)
    s.hideKeyboard()
  }

  fun save() = s.click(Ui.Address.SaveSettings)

  fun close() {
    s.escape()
    s.poll(what = "address settings to close to the address screen") { !s.exists(Ui.Address.AutoAccept) && s.exists(Ui.Address.Business) }
    s.waitStable(Ui.Address.Business)
  }
}

fun UiScope.addressSettings(): AddressSettingsScreen {
  click(Ui.Address.Settings)
  return AddressSettingsScreen(this)
}

fun CliPeer.addressPlanOk(link: String): Boolean {
  this sends "/_connect plan 1 $link"
  val line = nextLine(CONNECT_TIMEOUT_MS)
  if ("error: connection authorization failed" in line) return false
  check(line == "contact address: ok to connect") { "$name: unexpected plan for the address: `$line`; next: ${pending()}" }
  this sends "/u"
  eventually("user profile: $name")
  this expects "use /p <name> [<bio>] to change it"
  return true
}

fun CliPeer.awaitAddressGone(link: String) {
  val deadline = System.currentTimeMillis() + CONNECT_TIMEOUT_MS
  while (addressPlanOk(link)) {
    check(System.currentTimeMillis() < deadline) { "$name: the address still accepts connections ${CONNECT_TIMEOUT_MS / 1000} s after it was deleted" }
  }
}

class ContactPrefsScreen(private val s: UiScope) {
  init {
    s.poll(what = "the `Contact preferences` screen") { s.exists(Ui.ContactPrefs.Timed) && s.exists(Ui.ContactPrefs.Allow(ChatFeature.FullDelete)) }
    s.waitStable(Ui.ContactPrefs.Allow(ChatFeature.FullDelete))
  }

  fun allow(f: ChatFeature, option: String) {
    s.click(Ui.ContactPrefs.Allow(f))
    s.click(Ui.ContactPrefs.Allow(f).option(option))
  }

  fun allowTexts(f: ChatFeature): List<String> = s.textsOf(Ui.ContactPrefs.Allow(f))

  fun toggleTimed() = s.click(Ui.ContactPrefs.Timed)

  fun ttlShown(): Boolean = s.exists(Ui.ContactPrefs.Ttl)

  fun ttlTexts(): List<String> = s.textsOf(Ui.ContactPrefs.Ttl)

  fun reset() = s.click(Ui.ContactPrefs.Reset)

  fun save() = s.click(Ui.ContactPrefs.Save)

  fun backExpectingAlert() {
    s.goBack()
    s.poll(what = "the `Save preferences?` alert") { s.seesText("Save preferences?") }
  }

  fun backClosing() {
    s.goBack()
    s.poll(what = "contact preferences to close without an alert") { !s.exists(Ui.ContactPrefs.Save) && !s.seesText("Save preferences?") }
  }
}

fun UiScope.contactPrefs(): ContactPrefsScreen {
  click(Ui.Info.Preferences)
  return ContactPrefsScreen(this)
}

fun UiScope.closeContactInfo() {
  escape()
  poll(what = "contact info to close") { !ModalManager.end.hasModalsOpen() }
}
