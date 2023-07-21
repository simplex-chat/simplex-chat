package chat.simplex.common.platform

import chat.simplex.common.model.*
import chat.simplex.common.views.call.RcvCallInvitation
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR

enum class NotificationAction {
  ACCEPT_CONTACT_REQUEST
}

lateinit var ntfManager: NtfManager

abstract class NtfManager {
  fun notifyContactConnected(user: User, contact: Contact) = displayNotification(
    user = user,
    chatId = contact.id,
    displayName = contact.displayName,
    msgText = generalGetString(MR.strings.notification_contact_connected)
  )

  fun notifyContactRequestReceived(user: User, cInfo: ChatInfo.ContactRequest) = displayNotification(
    user = user,
    chatId = cInfo.id,
    displayName = cInfo.displayName,
    msgText = generalGetString(MR.strings.notification_new_contact_request),
    image = cInfo.image,
    listOf(NotificationAction.ACCEPT_CONTACT_REQUEST)
  )

  fun notifyMessageReceived(user: User, cInfo: ChatInfo, cItem: ChatItem) {
    if (!cInfo.ntfsEnabled) return
    displayNotification(user = user, chatId = cInfo.id, displayName = cInfo.displayName, msgText = hideSecrets(cItem))
  }

  private fun hideSecrets(cItem: ChatItem): String {
    val md = cItem.formattedText
    return if (md != null) {
      var res = ""
      for (ft in md) {
        res += if (ft.format is Format.Secret) "..." else ft.text
      }
      res
    } else {
      cItem.text
    }
  }


  abstract fun notifyCallInvitation(invitation: RcvCallInvitation)
  abstract fun hasNotificationsForChat(chatId: String): Boolean
  abstract fun cancelNotificationsForChat(chatId: String)
  abstract fun displayNotification(user: User, chatId: String, displayName: String, msgText: String, image: String? = null, actions: List<NotificationAction> = emptyList())
  abstract fun cancelCallNotification()
  abstract fun cancelAllNotifications()
  // Android only
  abstract fun androidCreateNtfChannelsMaybeShowAlert()
}
