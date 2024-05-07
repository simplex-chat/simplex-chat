package chat.simplex.common.ui.theme

import androidx.compose.material.LocalContentColor
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.ui.graphics.*
import chat.simplex.common.views.helpers.mixWith
import kotlin.math.min

val Purple200 = Color(0xFFBB86FC)
val Purple500 = Color(0xFF6200EE)
val Purple700 = Color(0xFF3700B3)
val Teal200 = Color(0xFF03DAC5)
val Gray = Color(0x22222222)
val Indigo = Color(0xFF9966FF)
val SimplexBlue = Color(0, 136, 255, 255)  // If this value changes also need to update #0088ff in string resource files
val SimplexGreen = Color(77, 218, 103, 255)
val SecretColor = Color(0x40808080)
val LightGray = Color(241, 242, 246, 255)
val DarkGray = Color(43, 44, 46, 255)
val HighOrLowlight = Color(139, 135, 134, 255)
val MessagePreviewDark = Color(179, 175, 174, 255)
val MessagePreviewLight = Color(49, 45, 44, 255)
val ToolbarLight = Color(220, 220, 220, 12)
val ToolbarDark = Color(80, 80, 80, 12)
val SettingsSecondaryLight = Color(200, 196, 195, 90)
val GroupDark = Color(80, 80, 80, 60)
val IncomingCallLight = Color(239, 237, 236, 255)
val WarningOrange = Color(255, 127, 0, 255)
val WarningYellow = Color(255, 192, 0, 255)
val FileLight = Color(183, 190, 199, 255)
val FileDark = Color(101, 101, 106, 255)
val SentMessageColor = Color(0x1E45B8FF)

val MenuTextColor: Color @Composable get () = if (isInDarkTheme()) LocalContentColor.current.copy(alpha = 0.8f) else Color.Black
val NoteFolderIconColor: Color @Composable get() = with(MaterialTheme.appColors.sentMessage) {
  // Default color looks too light and better to have it here a little bit brighter
  if (alpha == SentMessageColor.alpha) {
    copy(min(SentMessageColor.alpha + 0.1f, 1f))
  } else {
    // Color is non-standard and theme maker can choose color without alpha at all since the theme bound to dark/light variant,
    // and it shouldn't be universal
    this
  }
}

