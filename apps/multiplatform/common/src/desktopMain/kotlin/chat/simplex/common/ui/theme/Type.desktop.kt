package chat.simplex.common.ui.theme

import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.platform.Font
import chat.simplex.common.platform.desktopPlatform
import chat.simplex.res.*

actual val Inter: FontFamily = FontFamily(
  Font(MR.fonts.inter_regular.file),
  Font(MR.fonts.inter_italic.file, style = FontStyle.Italic),
  Font(MR.fonts.inter_bold.file, FontWeight.Bold),
  Font(MR.fonts.inter_semibold.file, FontWeight.SemiBold),
  Font(MR.fonts.inter_medium.file, FontWeight.Medium),
  Font(MR.fonts.inter_light.file, FontWeight.Light)
)

actual val EmojiFont: FontFamily = if (desktopPlatform.isMac()) {
  FontFamily.Default
} else {
  FontFamily(
    Font(MRdesktopMain.fonts.notocoloremoji_regular.file),
    Font(MRdesktopMain.fonts.notocoloremoji_regular.file, style = FontStyle.Italic),
    Font(MRdesktopMain.fonts.notocoloremoji_regular.file, FontWeight.Bold),
    Font(MRdesktopMain.fonts.notocoloremoji_regular.file, FontWeight.SemiBold),
    Font(MRdesktopMain.fonts.notocoloremoji_regular.file, FontWeight.Medium),
    Font(MRdesktopMain.fonts.notocoloremoji_regular.file, FontWeight.Light)
  )
}
