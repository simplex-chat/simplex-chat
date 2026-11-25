package chat.simplex.common.platform

import androidx.compose.runtime.*
import chat.simplex.common.simplexWindowState

@SuppressWarnings("MissingJvmstatic")
@Composable
actual fun BackHandler(enabled: Boolean, onBack: () -> Unit) {
  DisposableEffect(enabled) {
    if (enabled) simplexWindowState.backstack.add(onBack)
    onDispose {
      simplexWindowState.backstack.remove(onBack)
    }
  }
}
