package chat.simplex.common.platform

import android.content.*
import android.database.*
import android.graphics.Bitmap
import android.net.Uri
import android.os.*
import android.provider.OpenableColumns
import android.test.InstrumentationTestCase
import chat.simplex.common.views.helpers.*
import java.io.*
import java.net.URI
import kotlinx.coroutines.runBlocking
import kotlin.random.Random

class ImageStreamDecoderTest : InstrumentationTestCase() {
  private val rejectedWidth = 16385

  override fun setUp() {
    super.setUp()
    androidAppContext = instrumentation.targetContext
    TestImageProvider.file = null
  }

  fun testBoundsInspectionIsLimitedAndFailureDoesNotCloseTheSource() {
    val stream = TrackingStream()
    try { loadImageBitmap(stream); fail("invalid input was decoded") } catch (_: IOException) {}
    assertTrue(stream.bytesRead <= MAX_IMAGE_HEADER_BYTES)
    assertFalse(stream.closed)
  }

  fun testSuccessfulDecodeContinuesPastTheHeaderWindowAndLeavesTheSourceOpen() {
    val random = Random(7)
    val pixels = IntArray(1024 * 1024) { random.nextInt() }
    val encoded = encodePng(1024, 1024, pixels)
    assertTrue(encoded.size > MAX_IMAGE_HEADER_BYTES)
    val stream = TrackingStream(ByteArrayInputStream(encoded))
    assertEquals(1024, loadImageBitmap(stream).width)
    assertTrue(stream.bytesRead > MAX_IMAGE_HEADER_BYTES)
    assertFalse(stream.closed)
  }

  fun testImmutableBytesAndPublicStreamUseTheirDistinctFixedTargets() {
    val encoded = encodePng(2400, 2400)
    assertEquals(1200, decodeImageBitmap(ByteArrayInputStream(encoded), MAX_THUMBNAIL_DIMENSION).width)
    assertEquals(2400, loadImageBitmap(ByteArrayInputStream(encoded)).width)
  }

  fun testApi28UriBitmapAndDrawableRevalidateChangedProviderContent() {
    if (Build.VERSION.SDK_INT < 28) return
    TestImageProvider.file = File(androidAppContext.cacheDir, "uri-policy.png").apply { writeBytes(encodePng(5000, 100)) }
    val uri = URI(TestImageProvider.URI.toString())
    assertEquals(5000, getBitmapFromUri(uri, false)?.width)
    assertNotNull(getDrawableFromUri(uri, false))
    TestImageProvider.file!!.writeBytes(encodePng(rejectedWidth, 1))
    assertNull(getBitmapFromUri(uri, false))
    assertNull(getDrawableFromUri(uri, false))
  }

  fun testApi26And27UriUsesPrivateFile() {
    if (Build.VERSION.SDK_INT !in 26..27) return
    appFilesDir.mkdirs()
    File(appFilesDir, TestImageProvider.DISPLAY_NAME).writeBytes(encodePng(5000, 100))
    TestImageProvider.file = File(androidAppContext.cacheDir, "provider-invalid.bin").apply { writeText("not decoded") }
    assertEquals(5000, getBitmapFromUri(URI(TestImageProvider.URI.toString()), false)?.width)

  }

  fun testWallpaperAndDirectImageLinkUseBoundedStreamDecoder() {
    val suffix = System.nanoTime()
    val normal = "wallpaper-normal-$suffix.png"
    File(getWallpaperFilePath(normal)).writeBytes(encodePng(5000, 100))
    assertEquals(5000, WallpaperType.Image(normal, null, WallpaperScaleType.FIT).image?.width)
    val rejected = "wallpaper-rejected-$suffix.png"
    File(getWallpaperFilePath(rejected)).writeBytes(encodePng(rejectedWidth, 1))
    assertNull(WallpaperType.Image(rejected, null, WallpaperScaleType.FIT).image)
    val link = File(androidAppContext.cacheDir, "link-rejected-$suffix.png").apply { writeBytes(encodePng(rejectedWidth, 1)) }
    assertNull(runBlocking { getLinkPreview(link.toURI().toString()) })
  }

  private fun encodePng(width: Int, height: Int, pixels: IntArray? = null): ByteArray {
    val bitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888)
    if (pixels != null) bitmap.setPixels(pixels, 0, width, 0, 0, width, height)
    return ByteArrayOutputStream().use { bitmap.compress(Bitmap.CompressFormat.PNG, 100, it); bitmap.recycle(); it.toByteArray() }
  }

  private class TrackingStream(private val delegate: InputStream? = null) : InputStream() {
    var bytesRead = 0
    var closed = false
    override fun read(): Int = (delegate?.read() ?: 0).also { if (it >= 0) bytesRead++ }
    override fun read(buffer: ByteArray, offset: Int, length: Int): Int =
      (delegate?.read(buffer, offset, length) ?: length.also { buffer.fill(0, offset, offset + it) }).also { if (it > 0) bytesRead += it }
    override fun close() { closed = true }
  }
}

class TestImageProvider : ContentProvider() {
  override fun onCreate() = true
  override fun query(uri: Uri, projection: Array<out String>?, selection: String?, selectionArgs: Array<out String>?, sortOrder: String?) =
    MatrixCursor(arrayOf(OpenableColumns.DISPLAY_NAME)).apply { addRow(arrayOf(DISPLAY_NAME)) }
  override fun getType(uri: Uri) = "image/png"
  override fun openFile(uri: Uri, mode: String) = ParcelFileDescriptor.open(checkNotNull(file), ParcelFileDescriptor.MODE_READ_ONLY)
  override fun insert(uri: Uri, values: ContentValues?): Uri? = null
  override fun delete(uri: Uri, selection: String?, selectionArgs: Array<out String>?) = 0
  override fun update(uri: Uri, values: ContentValues?, selection: String?, selectionArgs: Array<out String>?) = 0

  companion object {
    const val AUTHORITY = "chat.simplex.common.test.imageprovider"
    const val DISPLAY_NAME = "provider-image.png"
    val URI: Uri = Uri.parse("content://$AUTHORITY/image")
    var file: File? = null
  }
}
