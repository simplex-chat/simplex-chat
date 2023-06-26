package chat.simplex.common.views.chat

import androidx.compose.runtime.Composable

@Composable
actual fun allowedToRecordVoiceByPlatform(): Boolean = true

@Composable
actual fun VoiceButtonWithoutPermissionByPlatform() {
  VoiceButtonWithoutPermission {}
}