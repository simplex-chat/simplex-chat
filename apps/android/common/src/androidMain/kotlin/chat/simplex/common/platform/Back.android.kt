package chat.simplex.common.platform

import androidx.compose.runtime.*

@SuppressWarnings("MissingJvmstatic")
@Composable
actual fun BackHandler(enabled: Boolean, onBack: () -> Unit) {
  androidx.activity.compose.BackHandler(enabled, onBack)
}