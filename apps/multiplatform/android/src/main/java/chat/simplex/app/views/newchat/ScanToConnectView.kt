package chat.simplex.app.views.newchat

import android.Manifest
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import chat.simplex.app.model.ChatModel
import com.google.accompanist.permissions.rememberPermissionState

@Composable
fun ScanToConnectView(chatModel: ChatModel, close: () -> Unit) {
  val cameraPermissionState = rememberPermissionState(permission = Manifest.permission.CAMERA)
  LaunchedEffect(Unit) {
    cameraPermissionState.launchPermissionRequest()
  }
  ConnectContactLayout(
    chatModelIncognito = chatModel.incognito.value,
    close
  )
}
