package chat.simplex.common.ui.theme

import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.platform.Font
import chat.simplex.common.platform.desktopPlatform
import chat.simplex.res.MR

actual val Inter: FontFamily = FontFamily(
  Font(MR.fonts.Inter.regular.file),
  Font(MR.fonts.Inter.italic.file, style = FontStyle.Italic),
  Font(MR.fonts.Inter.bold.file, FontWeight.Bold),
  Font(MR.fonts.Inter.semibold.file, FontWeight.SemiBold),
  Font(MR.fonts.Inter.medium.file, FontWeight.Medium),
  Font(MR.fonts.Inter.light.file, FontWeight.Light)
)

actual val EmojiFont: FontFamily = if (desktopPlatform.isMac()) {
  FontFamily.Default
} else {
  FontFamily(
    Font(MR.fonts.NotoColorEmoji.regular.file),
    Font(MR.fonts.NotoColorEmoji.regular.file, style = FontStyle.Italic),
    Font(MR.fonts.NotoColorEmoji.regular.file, FontWeight.Bold),
    Font(MR.fonts.NotoColorEmoji.regular.file, FontWeight.SemiBold),
    Font(MR.fonts.NotoColorEmoji.regular.file, FontWeight.Medium),
    Font(MR.fonts.NotoColorEmoji.regular.file, FontWeight.Light)
  )
}
