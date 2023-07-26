package chat.simplex.common.platform

import chat.simplex.common.model.*
import chat.simplex.common.views.call.RcvCallInvitation
import chat.simplex.common.views.helpers.withBGApi
import java.util.*

actual val appPlatform = AppPlatform.DESKTOP

@Suppress("ConstantLocale")
val defaultLocale: Locale = Locale.getDefault()

fun initApp() {
  ntfManager = object : NtfManager() { // LALAL
    override fun notifyContactConnected(user: User, contact: Contact) {}
    override fun notifyContactRequestReceived(user: User, cInfo: ChatInfo.ContactRequest) {}
    override fun notifyMessageReceived(user: User, cInfo: ChatInfo, cItem: ChatItem) {}
    override fun notifyCallInvitation(invitation: RcvCallInvitation) {}
    override fun hasNotificationsForChat(chatId: String): Boolean = false
    override fun cancelNotificationsForChat(chatId: String) {}
    override fun displayNotification(user: User, chatId: String, displayName: String, msgText: String, image: String?, actions: List<NotificationAction>) {}
    override fun createNtfChannelsMaybeShowAlert() {}
    override fun cancelCallNotification() {}
    override fun cancelAllNotifications() {}
  }
  applyAppLocale()
  withBGApi {
    initChatController()
    runMigrations()
  }
}

private fun applyAppLocale() {
  val lang = ChatController.appPrefs.appLanguage.get()
  if (lang == null || lang == Locale.getDefault().language) return
  Locale.setDefault(Locale.forLanguageTag(lang))
}
