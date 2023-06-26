package chat.simplex.common.views.chat

import android.Manifest
import androidx.compose.runtime.Composable
import com.google.accompanist.permissions.rememberMultiplePermissionsState

@Composable
actual fun allowedToRecordVoiceByPlatform(): Boolean {
  val permissionsState = rememberMultiplePermissionsState(listOf(Manifest.permission.RECORD_AUDIO))
  return permissionsState.allPermissionsGranted
}

@Composable
actual fun VoiceButtonWithoutPermissionByPlatform() {
  val permissionsState = rememberMultiplePermissionsState(listOf(Manifest.permission.RECORD_AUDIO))
  VoiceButtonWithoutPermission { permissionsState.launchMultiplePermissionRequest() }
}