package chat.simplex.app

import android.graphics.Bitmap
import android.util.Base64
import androidx.test.ext.junit.runners.AndroidJUnit4
import chat.simplex.common.platform.base64ToBitmap
import org.junit.Assert.assertSame
import org.junit.Test
import org.junit.runner.RunWith
import java.io.ByteArrayOutputStream

@RunWith(AndroidJUnit4::class)
class Base64ToBitmapTest {
  @Test
  fun decodedImageIsReused() {
    val out = ByteArrayOutputStream()
    Bitmap.createBitmap(64, 64, Bitmap.Config.ARGB_8888).compress(Bitmap.CompressFormat.JPEG, 85, out)
    val image = "data:image/jpg;base64," + Base64.encodeToString(out.toByteArray(), Base64.NO_WRAP)
    assertSame(base64ToBitmap(image), base64ToBitmap(String(image.toCharArray())))
  }
}
