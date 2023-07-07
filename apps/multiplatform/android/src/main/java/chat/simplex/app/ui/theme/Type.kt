package chat.simplex.app.ui.theme

import androidx.compose.ui.text.font.*
import chat.simplex.res.MR

// https://github.com/rsms/inter
val Inter: FontFamily = FontFamily(
  Font(MR.fonts.Inter.regular.fontResourceId),
  Font(MR.fonts.Inter.italic.fontResourceId, style = FontStyle.Italic),
  Font(MR.fonts.Inter.bold.fontResourceId, FontWeight.Bold),
  Font(MR.fonts.Inter.semibold.fontResourceId, FontWeight.SemiBold),
  Font(MR.fonts.Inter.medium.fontResourceId, FontWeight.Medium),
  Font(MR.fonts.Inter.light.fontResourceId, FontWeight.Light)
)
