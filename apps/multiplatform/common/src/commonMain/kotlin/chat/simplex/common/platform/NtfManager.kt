package chat.simplex.common.platform

import chat.simplex.common.model.*
import chat.simplex.common.views.call.RcvCallInvitation
import chat.simplex.common.views.chatlist.acceptContactRequest
import chat.simplex.common.views.chatlist.openChat
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingStage
import chat.simplex.res.MR
import kotlinx.coroutines.delay

enum class NotificationAction {
  ACCEPT_CONTACT_REQUEST,
  ACCEPT_CONTACT_REQUEST_INCOGNITO
}

lateinit var ntfManager: NtfManager

abstract class NtfManager {
  fun notifyContactConnected(user: UserLike, contact: Contact) = displayNotification(
    user = user,
    chatId = contact.id,
    displayName = contact.displayName,
    msgText = generalGetString(MR.strings.notification_contact_connected)
  )

  fun notifyContactRequestReceived(user: UserLike, cInfo: ChatInfo.ContactRequest) = displayNotification(
    user = user,
    chatId = cInfo.id,
    displayName = cInfo.displayName,
    msgText = generalGetString(MR.strings.notification_new_contact_request),
    image = cInfo.image,
    listOf(
      NotificationAction.ACCEPT_CONTACT_REQUEST to { acceptContactRequestAction(user.userId, incognito = false, cInfo.id) },
      NotificationAction.ACCEPT_CONTACT_REQUEST_INCOGNITO to { acceptContactRequestAction(user.userId, incognito = true, cInfo.id) }
    )
  )

  fun notifyMessageReceived(user: UserLike, cInfo: ChatInfo, cItem: ChatItem) {
    if (!cInfo.ntfsEnabled) return
    displayNotification(user = user, chatId = cInfo.id, displayName = cInfo.displayName, msgText = hideSecrets(cItem))
  }

  fun acceptContactRequestAction(userId: Long?, incognito: Boolean, chatId: ChatId) {
    val isCurrentUser = ChatModel.currentUser.value?.userId == userId
    val cInfo: ChatInfo.ContactRequest? = if (isCurrentUser) {
      (ChatModel.getChat(chatId)?.chatInfo as? ChatInfo.ContactRequest) ?: return
    } else {
      null
    }
    val apiId = chatId.replace("<@", "").toLongOrNull() ?: return
    // TODO include remote host in notification
    acceptContactRequest(null, incognito, apiId, cInfo, isCurrentUser, ChatModel)
    cancelNotificationsForChat(chatId)
  }

  fun openChatAction(userId: Long?, chatId: ChatId) {
    withLongRunningApi {
      awaitChatStartedIfNeeded(chatModel)
      if (userId != null && userId != chatModel.currentUser.value?.userId && chatModel.currentUser.value != null) {
        // TODO include remote host ID in desktop notifications?
        chatModel.controller.showProgressIfNeeded {
          chatModel.controller.changeActiveUser(null, userId, null)
        }
      }
      val cInfo = chatModel.getChat(chatId)?.chatInfo
      chatModel.clearOverlays.value = true
      if (cInfo != null && (cInfo is ChatInfo.Direct || cInfo is ChatInfo.Group)) openChat(null, cInfo, chatModel)
    }
  }

  fun showChatsAction(userId: Long?) {
    withLongRunningApi {
      awaitChatStartedIfNeeded(chatModel)
      if (userId != null && userId != chatModel.currentUser.value?.userId && chatModel.currentUser.value != null) {
        // TODO include remote host ID in desktop notifications?
        chatModel.controller.showProgressIfNeeded {
          chatModel.controller.changeActiveUser(null, userId, null)
        }
      }
      chatModel.chatId.value = null
      chatModel.clearOverlays.value = true
    }
  }

  fun acceptCallAction(chatId: ChatId) {
    chatModel.clearOverlays.value = true
    val invitation = chatModel.callInvitations[chatId]
    if (invitation == null) {
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.call_already_ended))
    } else {
      chatModel.callManager.acceptIncomingCall(invitation = invitation)
    }
  }

  abstract fun notifyCallInvitation(invitation: RcvCallInvitation): Boolean
  abstract fun hasNotificationsForChat(chatId: String): Boolean
  abstract fun cancelNotificationsForChat(chatId: String)
  abstract fun displayNotification(user: UserLike, chatId: String, displayName: String, msgText: String, image: String? = null, actions: List<Pair<NotificationAction, () -> Unit>> = emptyList())
  abstract fun cancelCallNotification()
  abstract fun cancelAllNotifications()
  abstract fun showMessage(title: String, text: String)
  // Android only
  abstract fun androidCreateNtfChannelsMaybeShowAlert()

  private suspend fun awaitChatStartedIfNeeded(chatModel: ChatModel, timeout: Long = 30_000) {
    // Still decrypting database
    if (chatModel.chatRunning.value == null) {
      val step = 50L
      for (i in 0..(timeout / step)) {
        if (chatModel.chatRunning.value == true || chatModel.controller.appPrefs.onboardingStage.get() == OnboardingStage.Step1_SimpleXInfo) {
          break
        }
        delay(step)
      }
    }
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
}
