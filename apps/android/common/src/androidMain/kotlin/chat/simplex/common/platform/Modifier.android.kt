package chat.simplex.common.platform

import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import com.google.accompanist.insets.navigationBarsWithImePadding

actual fun Modifier.navigationBarsWithImePadding(): Modifier = navigationBarsWithImePadding()

@Composable
actual fun ProvideWindowInsets(
  consumeWindowInsets: Boolean,
  windowInsetsAnimationsEnabled: Boolean,
  content: @Composable () -> Unit
) {
  com.google.accompanist.insets.ProvideWindowInsets(content = content)
}