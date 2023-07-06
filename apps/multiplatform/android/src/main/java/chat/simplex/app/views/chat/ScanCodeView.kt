package chat.simplex.app.views.chat

import android.Manifest
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import com.google.accompanist.permissions.rememberPermissionState

@Composable
fun ScanCodeView(verifyCode: (String?, cb: (Boolean) -> Unit) -> Unit, close: () -> Unit) {
  val cameraPermissionState = rememberPermissionState(permission = Manifest.permission.CAMERA)
  LaunchedEffect(Unit) {
    cameraPermissionState.launchPermissionRequest()
  }
  ScanCodeLayout(verifyCode, close)
}
