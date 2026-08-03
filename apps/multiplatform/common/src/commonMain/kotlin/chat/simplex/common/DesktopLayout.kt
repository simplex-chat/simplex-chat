package chat.simplex.common

import androidx.compose.runtime.mutableFloatStateOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp

enum class DesktopChatDensity {
  Compact,
  Comfortable,
  Spacious;

  companion object { val default = Compact }
}

data class DesktopDensityTokens(
  val chatRowVerticalPadding: Dp,
  val sidebarAvatarSize: Dp,
  val sidebarPreviewMinHeight: Dp,
  val sidebarPreviewMaxLines: Int,
  val messageVerticalPadding: Dp,
  val groupedMessageGap: Dp,
  val conversationGap: Dp,
  val composerVerticalPadding: Dp,
)

fun DesktopChatDensity.tokens(): DesktopDensityTokens = when (this) {
  DesktopChatDensity.Compact -> DesktopDensityTokens(5.dp, 44.dp, 20.dp, 1, 4.dp, 2.dp, 8.dp, 6.dp)
  DesktopChatDensity.Comfortable -> DesktopDensityTokens(7.dp, 50.dp, 24.dp, 1, 7.dp, 4.dp, 12.dp, 9.dp)
  DesktopChatDensity.Spacious -> DesktopDensityTokens(10.dp, 58.dp, 36.dp, 2, 10.dp, 7.dp, 16.dp, 12.dp)
}

internal fun useOpaqueDesktopSidebar(
  vibrancyAvailable: Boolean,
  appDark: Boolean,
  systemDark: Boolean,
): Boolean = !vibrancyAvailable || appDark != systemDark

object DesktopLayoutState {
  const val DEFAULT_SIDEBAR_WIDTH = 320f
  const val MIN_SIDEBAR_WIDTH = 240f
  const val MAX_SIDEBAR_WIDTH = 480f

  val sidebarWidth = mutableFloatStateOf(DEFAULT_SIDEBAR_WIDTH)
  val sidebarCollapsed = mutableStateOf(false)

  fun setSidebarWidth(width: Float) {
    sidebarWidth.floatValue = width.coerceIn(MIN_SIDEBAR_WIDTH, MAX_SIDEBAR_WIDTH)
  }

  fun resetSidebarWidth() {
    sidebarWidth.floatValue = DEFAULT_SIDEBAR_WIDTH
  }

  fun toggleSidebar() {
    sidebarCollapsed.value = !sidebarCollapsed.value
  }
}
