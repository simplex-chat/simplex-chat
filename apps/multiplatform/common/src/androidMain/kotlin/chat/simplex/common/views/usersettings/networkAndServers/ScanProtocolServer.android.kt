package chat.simplex.common.views.usersettings.networkAndServers

import android.Manifest
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import chat.simplex.common.model.UserServer
import com.google.accompanist.permissions.rememberPermissionState

@Composable
actual fun ScanProtocolServer(rhId: Long?, onNext: (UserServer) -> Unit) {
  val cameraPermissionState = rememberPermissionState(permission = Manifest.permission.CAMERA)
  LaunchedEffect(Unit) {
    cameraPermissionState.launchPermissionRequest()
  }
  ScanProtocolServerLayout(rhId, onNext)
}
