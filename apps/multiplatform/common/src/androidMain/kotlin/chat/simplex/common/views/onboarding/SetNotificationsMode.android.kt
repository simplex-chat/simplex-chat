package chat.simplex.common.views.onboarding

import android.Manifest
import android.os.Build
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import com.google.accompanist.permissions.*

@Composable
actual fun SetNotificationsModeAdditions() {
  if (Build.VERSION.SDK_INT >= 33) {
    val notificationsPermissionState = rememberPermissionState(Manifest.permission.POST_NOTIFICATIONS)
    LaunchedEffect(notificationsPermissionState.status == PermissionStatus.Granted) {
      val canAsk = appPrefs.canAskToEnableNotifications.get()
      if (notificationsPermissionState.status is PermissionStatus.Denied) {
        if (notificationsPermissionState.status.shouldShowRationale || !canAsk) {
          if (canAsk) {
            appPrefs.canAskToEnableNotifications.set(false)
          }
          Log.w(TAG, "Notifications are disabled and nobody will ask to enable them")
        } else {
          notificationsPermissionState.launchPermissionRequest()
        }
      } else {
        if (!canAsk) {
          // the user allowed notifications in system alert or manually in settings, allow to ask him next time if needed
          appPrefs.canAskToEnableNotifications.set(true)
        }
        ntfManager.androidCreateNtfChannelsMaybeShowAlert()
      }
    }
  } else {
    LaunchedEffect(Unit) {
      ntfManager.androidCreateNtfChannelsMaybeShowAlert()
    }
  }
}
