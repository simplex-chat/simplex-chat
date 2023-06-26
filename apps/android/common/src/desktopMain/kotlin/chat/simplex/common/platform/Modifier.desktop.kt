package chat.simplex.common.platform

import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier

actual fun Modifier.navigationBarsWithImePadding(): Modifier = this

@Composable
actual fun ProvideWindowInsets(
  consumeWindowInsets: Boolean,
  windowInsetsAnimationsEnabled: Boolean,
  content: @Composable () -> Unit
) {
  content()
}