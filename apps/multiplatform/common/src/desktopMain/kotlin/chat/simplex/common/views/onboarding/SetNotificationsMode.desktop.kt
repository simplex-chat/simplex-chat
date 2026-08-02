package chat.simplex.common.views.onboarding

import androidx.compose.runtime.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.MacOSNotifications
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.AlertManager

@Composable
actual fun SetNotificationsModeAdditions() {
  if (!desktopPlatform.isMac()) return
  LaunchedEffect(Unit) {
    if (!appPrefs.desktopNotificationExplanationShown.get() &&
      MacOSNotifications.permissionState() == NotificationPermissionState.NOT_DETERMINED
    ) {
      appPrefs.desktopNotificationExplanationShown.set(true)
      AlertManager.shared.showAlertDialog(
        title = "Stay up to date",
        text = "SimpleX can use native Mac notifications for new messages and calls. Previews follow your privacy setting, and alerts are suppressed while you are looking at that exact chat.",
        confirmText = "Allow Notifications",
        onConfirm = { MacOSNotifications.requestPermission() },
        dismissText = "Not Now",
      )
    }
  }
}
