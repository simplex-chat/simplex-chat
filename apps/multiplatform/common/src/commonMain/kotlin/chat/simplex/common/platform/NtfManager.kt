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
  ACCEPT_CALL,
  REJECT_CALL
}

enum class DesktopNotificationCategory { MESSAGE, CONTACT_REQUEST, INCOMING_CALL }

enum class NotificationPermissionState { NOT_DETERMINED, DENIED, AUTHORIZED, PROVISIONAL, UNKNOWN }

data class NotificationRoute(
  val userId: Long?,
  val remoteHostId: Long?,
  val chatId: ChatId,
  val messageId: Long? = null,
)

data class DesktopNotificationPayload(
  val identifier: String,
  val route: NotificationRoute,
  val title: String,
  val preview: String,
  val category: DesktopNotificationCategory,
  val playSound: Boolean,
  val imagePath: String? = null,
)

data class DesktopNotificationPreview(val title: String, val body: String)

fun desktopNotificationPreview(
  mode: NotificationPreviewMode,
  displayName: String,
  message: String,
  hiddenTitle: String,
  hiddenBody: String,
): DesktopNotificationPreview = DesktopNotificationPreview(
  title = if (mode == NotificationPreviewMode.HIDDEN) hiddenTitle else displayName,
  body = if (mode == NotificationPreviewMode.MESSAGE) message else hiddenBody,
)

fun desktopNotificationIdentifier(route: NotificationRoute, fallbackMessageId: Long): String {
  val safeChat = route.chatId.replace(Regex("[^A-Za-z0-9._-]"), "_")
  return "simplex.${route.userId ?: -1}.${route.remoteHostId ?: -1}.$safeChat.${route.messageId ?: fallbackMessageId}"
}

fun suppressDesktopNotification(
  windowFocused: Boolean,
  activeUserId: Long?,
  activeRemoteHostId: Long?,
  activeChatId: String?,
  route: NotificationRoute,
): Boolean = windowFocused && activeUserId == route.userId &&
  activeRemoteHostId == route.remoteHostId && activeChatId == route.chatId

class NotificationRouteQueue {
  private val routes = ArrayDeque<NotificationRoute>()

  fun enqueue(route: NotificationRoute) { routes.addLast(route) }

  fun consumeIfReady(ready: Boolean): List<NotificationRoute> =
    if (!ready) emptyList() else buildList { while (routes.isNotEmpty()) add(routes.removeFirst()) }
}

// Spec: spec/services/notifications.md#ntfManager
lateinit var ntfManager: NtfManager

abstract class NtfManager {
  fun notifyContactConnected(user: UserLike, contact: Contact) = displayNotification(
    user = user,
    chatId = contact.id,
    displayName = contact.displayName,
    msgText = generalGetString(MR.strings.notification_contact_connected),
    remoteHostId = chatModel.remoteHostId(),
  )

  fun notifyContactRequestReceived(user: UserLike, cInfo: ChatInfo.ContactRequest) = displayNotification(
    user = user,
    chatId = cInfo.id,
    displayName = cInfo.displayName,
    msgText = generalGetString(MR.strings.notification_new_contact_request),
    image = cInfo.image,
    actions = listOf(
      NotificationAction.ACCEPT_CONTACT_REQUEST to { acceptContactRequestAction(user.userId, incognito = false, cInfo.id) }
    ),
    remoteHostId = chatModel.remoteHostId(),
  )

  fun notifyMessageReceived(rhId: Long?, user: UserLike, cInfo: ChatInfo, cItem: ChatItem) {
    if (
      cItem.showNotification &&
      cInfo.ntfsEnabled(cItem) &&
      (
          allowedToShowNotification() ||
              chatModel.currentUser.value?.userId != user.userId ||
              chatModel.chatId.value != cInfo.id ||
              chatModel.remoteHostId() != rhId)
    ) {
      displayNotification(
        user = user,
        chatId = cInfo.id,
        displayName = cInfo.displayName,
        msgText = hideSecrets(cItem, cInfo.isChannel),
        remoteHostId = rhId,
        messageId = cItem.id,
      )
    }
  }

  fun acceptContactRequestAction(userId: Long?, incognito: Boolean, chatId: ChatId) =
    acceptContactRequestAction(userId, null, incognito, chatId)

  fun acceptContactRequestAction(userId: Long?, remoteHostId: Long?, incognito: Boolean, chatId: ChatId) {
    val apiId = chatId.replace("<@", "").toLongOrNull() ?: return
    withLongRunningApi {
      awaitChatStartedIfNeeded(chatModel)
      if (chatModel.remoteHostId() != remoteHostId) chatModel.controller.switchUIRemoteHost(remoteHostId)
      // switching to the user the request was sent to, so that accepted contact is shown
      if (userId != null && userId != chatModel.currentUser.value?.userId && chatModel.currentUser.value != null) {
        chatModel.controller.showProgressIfNeeded {
          chatModel.controller.changeActiveUser(remoteHostId, userId, null)
        }
        chatModel.clearOverlays.value = true
      }
      val isCurrentUser = chatModel.currentUser.value?.userId == userId
      acceptContactRequest(remoteHostId, incognito, apiId, isCurrentUser, chatModel)
      cancelNotificationsForChat(chatId)
    }
  }

  fun openChatAction(userId: Long?, chatId: ChatId) = openChatAction(userId, null, chatId, null)

  fun openChatAction(userId: Long?, remoteHostId: Long?, chatId: ChatId, messageId: Long?) {
    withLongRunningApi {
      awaitChatStartedIfNeeded(chatModel)
      if (chatModel.remoteHostId() != remoteHostId) {
        chatModel.controller.switchUIRemoteHost(remoteHostId)
      }
      if (userId != null && userId != chatModel.currentUser.value?.userId && chatModel.currentUser.value != null) {
        chatModel.controller.showProgressIfNeeded {
          chatModel.controller.changeActiveUser(remoteHostId, userId, null)
        }
      }
      val cInfo = chatModel.chats.value.firstOrNull { it.id == chatId && it.remoteHostId == remoteHostId }?.chatInfo
      chatModel.clearOverlays.value = true
      if (cInfo != null && (cInfo is ChatInfo.Direct || cInfo is ChatInfo.Group)) {
        openChat(secondaryChatsCtx = null, rhId = remoteHostId, cInfo.chatType, cInfo.apiId, messageId)
      }
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

  fun acceptCallAction(userId: Long?, remoteHostId: Long?, chatId: ChatId) {
    withLongRunningApi {
      awaitChatStartedIfNeeded(chatModel)
      if (chatModel.remoteHostId() != remoteHostId) chatModel.controller.switchUIRemoteHost(remoteHostId)
      if (userId != null && userId != chatModel.currentUser.value?.userId && chatModel.currentUser.value != null) {
        chatModel.controller.changeActiveUser(remoteHostId, userId, null)
      }
      acceptCallAction(chatId)
    }
  }

  abstract fun notifyCallInvitation(invitation: RcvCallInvitation): Boolean
  abstract fun hasNotificationsForChat(chatId: String): Boolean
  abstract fun cancelNotificationsForChat(chatId: String)
  abstract fun cancelNotificationsForUser(userId: Long)
  abstract fun displayNotification(
    user: UserLike,
    chatId: String,
    displayName: String,
    msgText: String,
    image: String? = null,
    actions: List<Pair<NotificationAction, () -> Unit>> = emptyList(),
    remoteHostId: Long? = null,
    messageId: Long? = null,
  )
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

  private fun hideSecrets(cItem: ChatItem, isChannel: Boolean = false): String {
    val md = cItem.formattedText
    return if (md != null) {
      var res = ""
      for (ft in md) {
        res += if (ft.format is Format.Secret) "..." else ft.text
      }
      res
    } else {
      val mc = cItem.content.msgContent
      if (mc is MsgContent.MCReport) {
        generalGetString(MR.strings.notification_group_report).format(cItem.text(isChannel).ifEmpty { mc.reason.text })
      } else {
        cItem.text(isChannel)
      }
    }
  }
}
