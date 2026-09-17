package chat.simplex.common.views.helpers

import dev.icerock.moko.resources.ImageResource

// Android uses the phone-sized wallpaper image (downscaled at draw time with FilterQuality.High).
actual fun PresetWallpaper.platformRes(): ImageResource = res
