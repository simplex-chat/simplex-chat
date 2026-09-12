package chat.simplex.common.platform

import android.os.Build
import android.test.InstrumentationTestCase
import coil.ImageLoader
import coil.request.*
import java.util.Base64
import kotlinx.coroutines.runBlocking

class LegacyImageLoaderTest : InstrumentationTestCase() {
  override fun setUp() {
    super.setUp()
    androidAppContext = instrumentation.targetContext
  }

  fun testApi26And27LegacyLoadersDoNotUseMovie() {
    if (Build.VERSION.SDK_INT !in 26..27) return
    val gif = Base64.getDecoder().decode("R0lGODlhAQABAIAAAAAAAP///ywAAAAAAQABAAACAUwAOw==")
    listOf("CIImageView_androidKt", "ImageFullScreenView_androidKt").forEach {
      val field = Class.forName("chat.simplex.common.views.chat.item.$it").getDeclaredField("imageLoader")
      val loader = field.apply { isAccessible = true }.get(null) as ImageLoader
      val result = runBlocking { loader.execute(ImageRequest.Builder(androidAppContext).data(gif).size(1000, 1000).build()) }
      assertTrue(result is SuccessResult)
      assertFalse((result as SuccessResult).drawable.javaClass.name.contains("Movie"))
    }
  }
}
