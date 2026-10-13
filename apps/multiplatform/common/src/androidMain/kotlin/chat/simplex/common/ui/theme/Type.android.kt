package chat.simplex.common.ui.theme

import androidx.compose.ui.text.font.*
import chat.simplex.res.*

actual val Inter: FontFamily = FontFamily(
  Font(MR.fonts.inter_regular.fontResourceId),
  Font(MR.fonts.inter_italic.fontResourceId, style = FontStyle.Italic),
  Font(MR.fonts.inter_bold.fontResourceId, FontWeight.Bold),
  Font(MR.fonts.inter_semibold.fontResourceId, FontWeight.SemiBold),
  Font(MR.fonts.inter_medium.fontResourceId, FontWeight.Medium),
  Font(MR.fonts.inter_light.fontResourceId, FontWeight.Light)
)

actual val EmojiFont: FontFamily = FontFamily.Default
