package chat.simplex.common.views.newchat

import android.Manifest
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import chat.simplex.common.model.ChatModel
import com.google.accompanist.permissions.rememberPermissionState

@Composable
actual fun ScanToConnectView(chatModel: ChatModel, close: () -> Unit) {
  val cameraPermissionState = rememberPermissionState(permission = Manifest.permission.CAMERA)
  LaunchedEffect(Unit) {
    cameraPermissionState.launchPermissionRequest()
  }
  ConnectContactLayout(
    chatModelIncognito = chatModel.incognito.value,
    close = close
  )
}