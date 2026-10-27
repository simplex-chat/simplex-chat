package chat.simplex.app

import androidx.compose.ui.geometry.Size
import chat.simplex.common.views.chat.item.chatImageSize
import kotlin.test.*

class ChatImageSizingTest {
  private val phone = Size(390f, 844f)

  private fun assertSize(width: Float, height: Float, actual: Size) {
    assertEquals(width, actual.width, 0.001f)
    assertEquals(height, actual.height, 0.001f)
  }

  @Test
  fun portraitStartsAtSeventyPercentOfTheShortSide() {
    assertSize(273f, 364f, chatImageSize(Size(300f, 400f), phone, 3f))
  }

  @Test
  fun tallPortraitShrinksBothDimensionsAtTheHeightCap() {
    assertSize(209.8125f, 373f, chatImageSize(Size(900f, 1600f), phone, 3f))
  }

  @Test
  fun landscapeAndSquareUseTheWiderPhoneBranch() {
    assertSize(326f, 217.33333f, chatImageSize(Size(1200f, 800f), phone, 3f))
    assertSize(326f, 326f, chatImageSize(Size(1000f, 1000f), phone, 3f))
  }

  @Test
  fun panoramaGetsAMinimumCropHeight() {
    assertSize(326f, 120f, chatImageSize(Size(3000f, 300f), phone, 3f))
  }

  @Test
  fun veryTallScreenshotReservesMetadataWithoutGrowingItsHeight() {
    assertSize(90f, 373f, chatImageSize(Size(100f, 1000f), phone, 3f, minWidth = 90f))
  }

  @Test
  fun windowOrientationUsesTheSameShortSide() {
    val source = Size(900f, 1600f)
    assertEquals(chatImageSize(source, phone, 3f), chatImageSize(source, Size(844f, 390f), 3f))
  }

  @Test
  fun portraitPixelCapIsConvertedByDensityAndDoesNotAffectWidePhonePhotos() {
    assertSize(180f, 320f, chatImageSize(Size(900f, 1600f), phone, 4f))
    assertSize(326f, 326f, chatImageSize(Size(1000f, 1000f), phone, 4f))
  }

  @Test
  fun tabletsReserveTheNavigationPaneBeforeComputingPhotoBounds() {
    assertSize(327f, 436f, chatImageSize(Size(300f, 400f), Size(800f, 1280f), 2f))
    assertSize(390f, 520f, chatImageSize(Size(300f, 400f), Size(600f, 960f), 2f))
  }

  @Test
  fun invalidDimensionsDoNotProduceNonFiniteLayoutSizes() {
    assertEquals(Size.Zero, chatImageSize(Size.Zero, phone, 3f))
    assertEquals(Size.Zero, chatImageSize(Size(300f, 400f), Size.Zero, 3f))
    assertEquals(Size.Zero, chatImageSize(Size(Float.NaN, 400f), phone, 3f))
    assertEquals(Size.Zero, chatImageSize(Size(300f, 400f), phone, 0f))
  }
}