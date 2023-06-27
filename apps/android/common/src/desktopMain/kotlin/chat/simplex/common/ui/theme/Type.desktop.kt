package chat.simplex.common.ui.theme

import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.platform.Font
import com.icerockdev.library.MR

actual val Inter: FontFamily = FontFamily(
  Font(R.font.Inter.regular.file),
  Font(R.font.Inter.italic.file, style = FontStyle.Italic),
  Font(R.font.Inter.bold.file, FontWeight.Bold),
  Font(R.font.Inter.semibold.file, FontWeight.SemiBold),
  Font(R.font.Inter.medium.file, FontWeight.Medium),
  Font(R.font.Inter.light.file, FontWeight.Light)
)