package chat.simplex.common.ui.theme

import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.platform.Font
import com.icerockdev.library.MR

actual val Inter: FontFamily = FontFamily(
  Font(MR.fonts.Inter.regular.file),
  Font(MR.fonts.Inter.italic.file, style = FontStyle.Italic),
  Font(MR.fonts.Inter.bold.file, FontWeight.Bold),
  Font(MR.fonts.Inter.semibold.file, FontWeight.SemiBold),
  Font(MR.fonts.Inter.medium.file, FontWeight.Medium),
  Font(MR.fonts.Inter.light.file, FontWeight.Light)
)