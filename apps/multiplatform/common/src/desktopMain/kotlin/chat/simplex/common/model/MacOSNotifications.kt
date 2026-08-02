package chat.simplex.common.model

import chat.simplex.common.showWindow
import chat.simplex.common.platform.*
import java.awt.EventQueue
import kotlinx.coroutines.*

private external fun macOSInitializeNotifications(): Boolean
private external fun macOSRequestNotificationPermission()
private external fun macOSNotificationPermissionState(): Int
private external fun macOSDeliverNotification(
  identifier: String,
  userId: Long,
  remoteHostId: Long,
  chatId: String,
  messageId: Long,
  title: String,
  body: String,
  category: String,
  playSound: Boolean,
  imagePath: String?,
): Boolean
private external fun macOSRemoveNotificationsForChat(userId: Long, remoteHostId: Long, chatId: String)
private external fun macOSRemoveNotificationsForUser(userId: Long)
private external fun macOSRemoveAllNotifications()
private external fun macOSOpenNotificationSettings()

private const val NO_ID = -1L

internal object MacOSNotifications {
  private data class PendingResponse(val route: NotificationRoute, val action: String)
  private val responseQueue = ArrayDeque<PendingResponse>()
  private val routingScope = CoroutineScope(SupervisorJob() + Dispatchers.Default)
  private var routingJob: Job? = null

  fun initialize(): Boolean = macOSInitializeNotifications()

  fun requestPermission() = macOSRequestNotificationPermission()

  fun permissionState(): NotificationPermissionState = when (macOSNotificationPermissionState()) {
    0 -> NotificationPermissionState.NOT_DETERMINED
    1 -> NotificationPermissionState.DENIED
    2 -> NotificationPermissionState.AUTHORIZED
    3 -> NotificationPermissionState.PROVISIONAL
    else -> NotificationPermissionState.UNKNOWN
  }

  fun deliver(payload: DesktopNotificationPayload): Boolean = macOSDeliverNotification(
    identifier = payload.identifier,
    userId = payload.route.userId ?: NO_ID,
    remoteHostId = payload.route.remoteHostId ?: NO_ID,
    chatId = payload.route.chatId,
    messageId = payload.route.messageId ?: NO_ID,
    title = payload.title,
    body = payload.preview,
    category = payload.category.name,
    playSound = payload.playSound,
    imagePath = payload.imagePath,
  )

  fun removeChat(userId: Long?, remoteHostId: Long?, chatId: String) =
    macOSRemoveNotificationsForChat(userId ?: NO_ID, remoteHostId ?: NO_ID, chatId)

  fun removeUser(userId: Long) = macOSRemoveNotificationsForUser(userId)

  fun removeAll() = macOSRemoveAllNotifications()

  fun openSettings() = macOSOpenNotificationSettings()

  fun enqueueResponse(route: NotificationRoute, action: String) {
    synchronized(responseQueue) { responseQueue.addLast(PendingResponse(route, action)) }
    if (routingJob?.isActive == true) return
    routingJob = routingScope.launch {
      while (true) {
        while (chatModel.chatRunning.value != true) delay(100)
        val response = synchronized(responseQueue) {
          if (responseQueue.isEmpty()) {
            routingJob = null
            null
          } else responseQueue.removeFirst()
        } ?: break
        dispatchResponse(response.route, response.action)
        // Profile and remote-host transitions are asynchronous; route responses serially.
        delay(500)
      }
    }
  }

  private fun dispatchResponse(route: NotificationRoute, action: String) {
    EventQueue.invokeLater { showWindow() }
    when (action) {
      NotificationAction.ACCEPT_CONTACT_REQUEST.name ->
        ntfManager.acceptContactRequestAction(route.userId, route.remoteHostId, incognito = false, route.chatId)
      NotificationAction.ACCEPT_CALL.name -> ntfManager.acceptCallAction(route.userId, route.remoteHostId, route.chatId)
      NotificationAction.REJECT_CALL.name -> chatModel.callInvitations[route.chatId]?.let { chatModel.callManager.endCall(invitation = it) }
      else -> ntfManager.openChatAction(route.userId, route.remoteHostId, route.chatId, route.messageId)
    }
  }
}

@Suppress("unused") // Called by UNUserNotificationCenterDelegate through JNI.
fun onMacOSNotificationResponse(userId: Long, remoteHostId: Long, chatId: String, messageId: Long, action: String) {
  val route = NotificationRoute(
    userId = userId.takeUnless { it == NO_ID },
    remoteHostId = remoteHostId.takeUnless { it == NO_ID },
    chatId = chatId,
    messageId = messageId.takeUnless { it == NO_ID },
  )
  MacOSNotifications.enqueueResponse(route, action)
}
