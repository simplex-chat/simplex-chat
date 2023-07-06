package chat.simplex.app.views.onboarding

import android.Manifest
import android.os.Build
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import chat.simplex.app.SimplexApp
import com.google.accompanist.permissions.rememberPermissionState

@Composable
fun SetNotificationsModeAdditions() {
  if (Build.VERSION.SDK_INT >= 33) {
    val notificationsPermissionState = rememberPermissionState(Manifest.permission.POST_NOTIFICATIONS)
    LaunchedEffect(notificationsPermissionState.hasPermission) {
      if (notificationsPermissionState.hasPermission) {
        SimplexApp.context.chatModel.controller.ntfManager.createNtfChannelsMaybeShowAlert()
      } else {
        notificationsPermissionState.launchPermissionRequest()
      }
    }
  } else {
    LaunchedEffect(Unit) {
      SimplexApp.context.chatModel.controller.ntfManager.createNtfChannelsMaybeShowAlert()
    }
  }
}