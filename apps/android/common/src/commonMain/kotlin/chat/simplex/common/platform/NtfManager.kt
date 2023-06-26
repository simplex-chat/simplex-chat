package chat.simplex.common.platform

import chat.simplex.common.model.*
import chat.simplex.common.views.call.RcvCallInvitation

enum class NotificationAction {
  ACCEPT_CONTACT_REQUEST
}

lateinit var ntfManager: NtfManager

abstract class NtfManager {
  abstract fun notifyContactConnected(user: User, contact: Contact)
  abstract fun notifyContactRequestReceived(user: User, cInfo: ChatInfo.ContactRequest)
  abstract fun notifyMessageReceived(user: User, cInfo: ChatInfo, cItem: ChatItem)
  abstract fun notifyCallInvitation(invitation: RcvCallInvitation)
  abstract fun hasNotificationsForChat(chatId: String): Boolean
  abstract fun cancelNotificationsForChat(chatId: String)
  abstract fun displayNotification(user: User, chatId: String, displayName: String, msgText: String, image: String? = null, actions: List<NotificationAction> = emptyList())
  abstract fun createNtfChannelsMaybeShowAlert()
  abstract fun cancelCallNotification()
  abstract fun cancelAllNotifications()
}