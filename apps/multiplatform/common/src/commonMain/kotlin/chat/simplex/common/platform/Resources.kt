package chat.simplex.common.platform

import androidx.compose.runtime.Composable
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.text.font.Font
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.Dp
import com.russhwolf.settings.Settings
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.StringResource

@Composable
expect fun font(name: String, res: String, weight: FontWeight = FontWeight.Normal, style: FontStyle = FontStyle.Normal): Font

expect fun StringResource.localized(): String

// Non-@Composable implementation
expect fun isInNightMode(): Boolean

expect val settings: Settings
expect val settingsThemes: Settings

enum class WindowOrientation {
  UNDEFINED, PORTRAIT, LANDSCAPE
}

expect fun windowOrientation(): WindowOrientation

@Composable
expect fun windowWidth(): Dp

expect fun desktopExpandWindowToWidth(width: Dp)

expect fun isRtl(text: CharSequence): Boolean

expect fun ImageResource.toComposeImageBitmap(): ImageBitmap?
