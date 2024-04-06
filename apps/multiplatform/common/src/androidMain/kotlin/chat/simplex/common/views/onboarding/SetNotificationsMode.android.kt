package chat.simplex.common.views.onboarding

import android.Manifest
import android.os.Build
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import chat.simplex.common.platform.ntfManager
import com.google.accompanist.permissions.PermissionStatus
import com.google.accompanist.permissions.rememberPermissionState

@Composable
actual fun SetNotificationsModeAdditions() {
  if (Build.VERSION.SDK_INT >= 33) {
    val notificationsPermissionState = rememberPermissionState(Manifest.permission.POST_NOTIFICATIONS)
    LaunchedEffect(notificationsPermissionState.status == PermissionStatus.Granted) {
      if (notificationsPermissionState.status == PermissionStatus.Granted) {
        ntfManager.androidCreateNtfChannelsMaybeShowAlert()
      } else {
        notificationsPermissionState.launchPermissionRequest()
      }
    }
  } else {
    LaunchedEffect(Unit) {
      ntfManager.androidCreateNtfChannelsMaybeShowAlert()
    }
  }
}
