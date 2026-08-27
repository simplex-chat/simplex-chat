package chat.simplex.app

import chat.simplex.common.platform.MAX_SLOW_FRAME_DEBT
import chat.simplex.common.platform.frameWait
import chat.simplex.common.platform.fileSizeWithinBounds
import chat.simplex.common.platform.frameCountWithinBounds
import chat.simplex.common.platform.frameDuration
import chat.simplex.common.platform.looksAnimatable
import chat.simplex.common.platform.priorFrame
import chat.simplex.common.platform.rasterWithinBounds
import chat.simplex.common.platform.rebuiltFramesWithinBounds
import chat.simplex.common.platform.slowFrameDebt
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

// The bounds an animated image must satisfy, checked as arithmetic: skiko's native library is not on the
// test runtime classpath, and these numbers are the part that has to be right about someone else's file.
class AnimatedImageBoundsTest {
  private val BYTES_PER_PIXEL = 4 // what a GIF or WebP decodes to

  @Test
  fun testOrdinaryAnimationIsWithinBounds() {
    assertTrue(rasterWithinBounds(64, 64, BYTES_PER_PIXEL))
    assertTrue(rasterWithinBounds(1244, 554, BYTES_PER_PIXEL))
  }

  @Test
  fun testHugeDeclaredDimensionsAreRejected() {
    // A 17GB raster, declared by a GIF of 35 bytes
    assertFalse(rasterWithinBounds(65535, 65535, BYTES_PER_PIXEL))
  }

  @Test
  fun testDimensionsOverRasterBudgetAreRejected() {
    // Plausible-looking, but one raster of this size is ~64MB and a chat shows several at once
    assertFalse(rasterWithinBounds(4000, 4000, BYTES_PER_PIXEL))
  }

  @Test
  fun testAspectRatioIsBoundedOnEachSideSeparately() {
    // Only 2.1MP, so the raster bound alone would animate this with a 65535-pixel scanline
    assertFalse(rasterWithinBounds(65535, 32, BYTES_PER_PIXEL))
    assertFalse(rasterWithinBounds(32, 65535, BYTES_PER_PIXEL))
    assertTrue(rasterWithinBounds(3000, 500, BYTES_PER_PIXEL))
    assertTrue(rasterWithinBounds(4096, 900, BYTES_PER_PIXEL))
  }

  @Test
  fun testBudgetBoundariesAreExact() {
    assertTrue(rasterWithinBounds(1920, 1920, BYTES_PER_PIXEL))
    assertFalse(rasterWithinBounds(1921, 1920, BYTES_PER_PIXEL))
    assertFalse(rasterWithinBounds(4097, 100, BYTES_PER_PIXEL))
  }

  @Test
  fun testEmptyDimensionsAreRejected() {
    assertFalse(rasterWithinBounds(0, 64, BYTES_PER_PIXEL))
    assertFalse(rasterWithinBounds(64, 0, BYTES_PER_PIXEL))
    assertFalse(rasterWithinBounds(-1, 64, BYTES_PER_PIXEL))
  }

  @Test
  fun testWiderColorTypesCountAgainstTheSameBudget() {
    // The file chooses its color type, so the 1920x1920 that fits at four bytes is twice the raster at eight
    assertFalse(rasterWithinBounds(1920, 1920, 8))
    assertTrue(rasterWithinBounds(1357, 1357, 8))
    // A color type claiming no bytes per pixel would otherwise make any raster look free
    assertFalse(rasterWithinBounds(4096, 4096, 0))
  }

  @Test
  fun testAnimatableContainersAreRecognized() {
    assertTrue(looksAnimatable("GIF89a...".toByteArray()))
    assertTrue(looksAnimatable("GIF87a...".toByteArray()))
    assertTrue(looksAnimatable("RIFF????WEBPVP8X".toByteArray()))
  }

  @Test
  fun testPhotosNeverReachTheAnimationDecoder() {
    assertFalse(looksAnimatable(bytes(0x89, 'P'.code, 'N'.code, 'G'.code, 0x0D, 0x0A, 0x1A, 0x0A)))
    assertFalse(looksAnimatable(bytes(0xFF, 0xD8, 0xFF, 0xE0, 0x00, 0x10, 0x4A, 0x46)))
    // A RIFF container that is not WebP, a wave file say
    assertFalse(looksAnimatable("RIFF????WAVEfmt ".toByteArray()))
  }

  @Test
  fun testShortDataIsRejectedWithoutReadingPastTheEnd() {
    assertFalse(looksAnimatable(ByteArray(0)))
    assertFalse(looksAnimatable("GIF".toByteArray()))
    // Long enough for the RIFF tag, too short for the format that follows it
    assertFalse(looksAnimatable("RIFF".toByteArray()))
    assertFalse(looksAnimatable("RIFF1234WEB".toByteArray()))
  }

  @Test
  fun testPriorFrameIsReusedOnlyWhenTheBitmapHoldsIt() {
    assertEquals(4, priorFrame(5, 4))
    // An older required frame is no longer in the bitmap, which is also how a predecessor disposed to what
    // came before it is skipped, as Skia never requires one
    assertEquals(-1, priorFrame(5, 2))
    assertEquals(-1, priorFrame(5, -1))
    // For the first frame, -1 is both its required frame and no prior frame
    assertEquals(-1, priorFrame(0, -1))
  }

  @Test
  fun testOnlyFilesSmallEnoughToScanAreWithinBounds() {
    assertTrue(fileSizeWithinBounds(0))
    // The largest animation in this repository
    assertTrue(fileSizeWithinBounds(6_013_354))
    assertTrue(fileSizeWithinBounds(32 * 1024 * 1024))
    assertFalse(fileSizeWithinBounds(32 * 1024 * 1024 + 1))
  }

  @Test
  fun testOnlyAnimationsWorthHoldingFramesForAreWithinBounds() {
    assertFalse(frameCountWithinBounds(0))
    assertFalse(frameCountWithinBounds(1))
    assertFalse(frameCountWithinBounds(-1))
    assertTrue(frameCountWithinBounds(2))
    // The longest animation in this repository, and the bound itself
    assertTrue(frameCountWithinBounds(1041))
    assertTrue(frameCountWithinBounds(10_000))
    assertFalse(frameCountWithinBounds(10_001))
  }

  @Test
  fun testAnimationsThatRebuildNothingAreWithinBounds() {
    // What a real animation looks like: each frame continues the one before it, so nothing is rebuilt
    assertTrue(rebuiltFramesWithinBounds(IntArray(5000) { it - 1 }))
    assertTrue(rebuiltFramesWithinBounds(intArrayOf(-1, -1, -1)))
  }

  @Test
  fun testShortRebuiltChainsAreWithinBounds() {
    // A GIF disposing to what came before it: frame 2 continues frame 0, rebuilding two frames
    assertTrue(rebuiltFramesWithinBounds(intArrayOf(-1, 0, 0, 2, 2, 4)))
  }

  @Test
  fun testLongRebuiltChainsAreRejected() {
    // Alternating disposal makes every other frame rebuild the chain before it, which Skia recurses through:
    // 8000 frames of that overflows the native stack and kills the app
    val alternating = IntArray(8000) { if (it % 2 == 0) it - 2 else it - 1 }
    assertFalse(rebuiltFramesWithinBounds(alternating))
    // The bound is on what a rebuild costs, not on how long the animation is
    assertTrue(rebuiltFramesWithinBounds(IntArray(8000) { it - 1 }))
  }

  @Test
  fun testRebuiltChainBoundIsExact() {
    fun chainOf(length: Int) = IntArray(length + 2) { if (it == length + 1) it - 2 else it - 1 }
    assertTrue(rebuiltFramesWithinBounds(chainOf(64)))
    assertFalse(rebuiltFramesWithinBounds(chainOf(65)))
  }

  @Test
  fun testFramesContinuingSomethingImpossibleStartTheirOwnChain() {
    // A file is not trusted to say a frame continues itself, a later frame, or one that is not there
    assertTrue(rebuiltFramesWithinBounds(IntArray(5000) { it }))
    assertTrue(rebuiltFramesWithinBounds(IntArray(5000) { it + 1 }))
    assertTrue(rebuiltFramesWithinBounds(IntArray(5000) { 9999 }))
  }

  @Test
  fun testAFrameCheaperThanItsDelayWaitsAsItAlwaysDid() {
    assertEquals(70, frameWait(70, 0))
    assertEquals(70, frameWait(70, 2))
    assertEquals(70, frameWait(70, 70))
  }

  @Test
  fun testAFrameDearerThanItsDelayIsWaitedOut() {
    assertEquals(85, frameWait(20, 85))
    assertEquals(500, frameWait(20, 500))
    assertEquals(1000, frameWait(20, 1000))
  }

  @Test
  fun testAStallIsNotWaitedOut() {
    // Wall time counts a machine that suspended mid-decode, which the frame never spent
    assertEquals(1000, frameWait(20, 30_000))
    assertEquals(1000, frameWait(20, 8L * 60 * 60 * 1000))
    assertEquals(5000, frameWait(5000, 30_000))
  }

  @Test
  fun testTwoExpensiveFramesInARowStopTheAnimation() {
    var debt = slowFrameDebt(0, tooSlow = true)
    assertTrue(debt < MAX_SLOW_FRAME_DEBT)
    debt = slowFrameDebt(debt, tooSlow = true)
    assertTrue(debt >= MAX_SLOW_FRAME_DEBT)
  }

  @Test
  fun testAFrameThatOnlyOverranIsPaidOff() {
    // One expensive frame among cheap ones is a busy machine, not an expensive animation
    var debt = slowFrameDebt(0, tooSlow = true)
    repeat(4) { debt = slowFrameDebt(debt, tooSlow = false) }
    assertEquals(0, debt)
  }

  @Test
  fun testAlternatingExpensiveFramesStillStopTheAnimation() {
    // Frames that alternate are never expensive twice in a row, which is what a count that resets would miss
    var debt = 0
    var frames = 0
    while (debt < MAX_SLOW_FRAME_DEBT && frames < 100) {
      debt = slowFrameDebt(debt, tooSlow = frames % 2 == 0)
      frames++
    }
    assertEquals(5, frames)
  }

  @Test
  fun testCheapFramesEarnNoCreditAgainstLaterExpensiveOnes() {
    var debt = 0
    repeat(1000) { debt = slowFrameDebt(debt, tooSlow = false) }
    assertEquals(0, debt)
    debt = slowFrameDebt(debt, tooSlow = true)
    debt = slowFrameDebt(debt, tooSlow = true)
    assertTrue(debt >= MAX_SLOW_FRAME_DEBT)
  }

  @Test
  fun testFrameDurationSubstitutesTheDefaultForFramesInAHurry() {
    // Skia reports a GIF delay in milliseconds, so "no delay" and "one centisecond" arrive as 0 and 10
    assertEquals(100, frameDuration(0))
    assertEquals(100, frameDuration(10))
    // Not expected from Skia, but read from the file
    assertEquals(100, frameDuration(-1))
  }

  @Test
  fun testFrameDurationKeepsAuthoredDelays() {
    assertEquals(70, frameDuration(70))
    assertEquals(600, frameDuration(600))
    assertEquals(Int.MAX_VALUE.toLong(), frameDuration(Int.MAX_VALUE))
  }

  @Test
  fun testFrameDurationRaisesDelaysBelowTheFloor() {
    assertEquals(20, frameDuration(11))
    assertEquals(20, frameDuration(19))
    assertEquals(20, frameDuration(20))
    assertEquals(21, frameDuration(21))
  }

  private fun bytes(vararg values: Int): ByteArray = values.map { it.toByte() }.toByteArray()
}
