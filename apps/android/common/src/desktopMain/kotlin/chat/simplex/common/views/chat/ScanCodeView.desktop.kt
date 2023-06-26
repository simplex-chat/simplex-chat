package chat.simplex.common.views.chat

import androidx.compose.runtime.Composable

@Composable
actual fun ScanCodeView(verifyCode: (String?, cb: (Boolean) -> Unit) -> Unit, close: () -> Unit) {
  ScanCodeLayout(verifyCode, close)
}