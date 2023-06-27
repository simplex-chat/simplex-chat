package chat.simplex.common.ui.theme

import androidx.compose.ui.text.font.*
import com.icerockdev.library.MR

actual val Inter: FontFamily = FontFamily(
  Font(R.font.Inter.regular.fontResourceId),
  Font(R.font.Inter.italic.fontResourceId, style = FontStyle.Italic),
  Font(R.font.Inter.bold.fontResourceId, FontWeight.Bold),
  Font(R.font.Inter.semibold.fontResourceId, FontWeight.SemiBold),
  Font(R.font.Inter.medium.fontResourceId, FontWeight.Medium),
  Font(R.font.Inter.light.fontResourceId, FontWeight.Light)
)