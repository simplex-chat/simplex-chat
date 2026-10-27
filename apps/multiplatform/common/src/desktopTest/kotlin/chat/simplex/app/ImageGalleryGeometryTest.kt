package chat.simplex.app

import androidx.compose.ui.geometry.*
import chat.simplex.common.views.chat.item.ImageGallerySourceBounds
import chat.simplex.common.views.chat.item.imageGalleryGeometry
import kotlin.test.*

class ImageGalleryGeometryTest {
  private val viewport = Rect(100f, 50f, 1180f, 1970f)

  @Test
  fun landscapeImageFitsWithoutStretchingAndAccountsForWindowOffset() {
    val source = ImageGallerySourceBounds(Rect(200f, 300f, 500f, 500f), Rect(200f, 300f, 500f, 500f), 17f, false)
    val start = assertNotNull(imageGalleryGeometry(source, viewport, Size(1200f, 800f), 0f))
    assertEquals(Rect(100f, 250f, 400f, 450f), start.image)
    val end = assertNotNull(imageGalleryGeometry(source, viewport, Size(1200f, 800f), 1f))
    assertEquals(Rect(0f, 600f, 1080f, 1320f), end.image)
    assertEquals(end.image, end.clip)
    assertEquals(0f, end.cornerRadius)
    val middle = assertNotNull(imageGalleryGeometry(source, viewport, Size(1200f, 800f), 0.5f))
    assertEquals(1.5f, middle.image.width / middle.image.height, 0.0001f)
  }

  @Test
  fun tallThumbnailExpandsItsCropAndKeepsViewportClippingSeparate() {
    val bounds = Rect(200f, 300f, 500f, 999f) // Image viewport capped at 2.33.
    val visible = Rect(200f, 400f, 500f, 999f) // Top clipped by chat toolbar.
    val source = ImageGallerySourceBounds(bounds, visible, 17f, false)
    val start = assertNotNull(imageGalleryGeometry(source, viewport, Size(100f, 600f), 0f))
    assertEquals(1800f, start.image.height)
    assertEquals(bounds.translate(-viewport.topLeft), start.clip)
    assertEquals(visible.translate(-viewport.topLeft), start.visible)
    val end = assertNotNull(imageGalleryGeometry(source, viewport, Size(100f, 600f), 1f))
    assertEquals(1920f, end.image.height)
    assertEquals(320f, end.image.width)
    assertEquals(end.image, end.visible)
  }

  @Test
  fun squareReplyThumbnailCropsHorizontallyAndReverseUsesSameEndpoints() {
    val bounds = Rect(200f, 300f, 300f, 400f)
    val source = ImageGallerySourceBounds(bounds, bounds, 4f, true)
    val start = assertNotNull(imageGalleryGeometry(source, viewport, Size(400f, 200f), 0f))
    assertEquals(200f, start.image.width)
    assertEquals(100f, start.image.height)
    assertEquals(4f, start.cornerRadius)
    val end = assertNotNull(imageGalleryGeometry(source, viewport, Size(400f, 200f), 1f))
    assertEquals(2f, end.image.width / end.image.height)
    assertEquals(start, imageGalleryGeometry(source, viewport, Size(400f, 200f), -1f))
    assertEquals(end, imageGalleryGeometry(source, viewport, Size(400f, 200f), 2f))
  }

  @Test
  fun invalidOrMissingGeometryFallsBackWithoutDrawing() {
    val bounds = Rect(200f, 300f, 300f, 400f)
    val source = ImageGallerySourceBounds(bounds, bounds, 4f, false)
    assertNull(imageGalleryGeometry(source, Rect.Zero, Size(400f, 200f), 0f))
    assertNull(imageGalleryGeometry(source, viewport, Size.Zero, 0f))
    assertNull(imageGalleryGeometry(source.copy(visible = Rect.Zero), viewport, Size(400f, 200f), 0f))
    assertNull(imageGalleryGeometry(source, viewport, Size(400f, 200f), Float.NaN))
  }
}