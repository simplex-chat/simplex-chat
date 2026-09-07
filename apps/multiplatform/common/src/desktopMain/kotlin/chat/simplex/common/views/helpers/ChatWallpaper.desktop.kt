package chat.simplex.common.views.helpers

import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource

// Desktop uses smaller, desktop-tuned images: FilterQuality.Low there pixelates downscaling, so the
// source must already be near display size. These live in desktopMain/resources, out of the Android APK.
actual fun PresetWallpaper.platformRes(): ImageResource = when (this) {
  PresetWallpaper.CATS -> MR.images.wallpaper_cats_desktop
  PresetWallpaper.FLOWERS -> MR.images.wallpaper_flowers_desktop
  PresetWallpaper.HEARTS -> MR.images.wallpaper_hearts_desktop
  PresetWallpaper.KIDS -> MR.images.wallpaper_kids_desktop
  PresetWallpaper.SCHOOL -> MR.images.wallpaper_school_desktop
  PresetWallpaper.TRAVEL -> MR.images.wallpaper_travel_desktop
}
