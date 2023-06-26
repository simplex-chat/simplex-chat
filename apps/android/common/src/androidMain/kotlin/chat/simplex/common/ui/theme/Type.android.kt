package chat.simplex.common.ui.theme

import androidx.compose.ui.text.font.*
import com.icerockdev.library.MR

actual val Inter: FontFamily = FontFamily(
  Font(MR.fonts.Inter.regular.fontResourceId),
  Font(MR.fonts.Inter.italic.fontResourceId, style = FontStyle.Italic),
  Font(MR.fonts.Inter.bold.fontResourceId, FontWeight.Bold),
  Font(MR.fonts.Inter.semibold.fontResourceId, FontWeight.SemiBold),
  Font(MR.fonts.Inter.medium.fontResourceId, FontWeight.Medium),
  Font(MR.fonts.Inter.light.fontResourceId, FontWeight.Light)
)