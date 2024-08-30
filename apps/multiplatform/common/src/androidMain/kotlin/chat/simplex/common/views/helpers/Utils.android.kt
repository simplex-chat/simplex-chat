package chat.simplex.common.views.helpers

import android.content.ClipboardManager
import android.content.Context
import android.content.res.Resources
import android.graphics.*
import android.graphics.Typeface
import android.graphics.drawable.Drawable
import android.media.MediaMetadataRetriever
import android.os.*
import android.provider.OpenableColumns
import android.text.Spanned
import android.text.SpannedString
import android.text.style.*
import android.util.Base64
import android.view.WindowManager
import androidx.compose.runtime.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.style.BaselineShift
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.unit.*
import androidx.core.content.FileProvider
import androidx.core.text.HtmlCompat
import chat.simplex.common.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource
import java.io.*
import java.net.URI

fun Spanned.toHtmlWithoutParagraphs(): String {
  return HtmlCompat.toHtml(this, HtmlCompat.TO_HTML_PARAGRAPH_LINES_CONSECUTIVE)
    .substringAfter("<p dir=\"ltr\">").substringBeforeLast("</p>")
}

fun Resources.getText(id: StringResource, vararg args: Any): CharSequence {
  val escapedArgs = args.map {
    if (it is Spanned) it.toHtmlWithoutParagraphs() else it
  }.toTypedArray()
  val resource = SpannedString(getText(id))
  val htmlResource = resource.toHtmlWithoutParagraphs()
  val formattedHtml = String.format(htmlResource, *escapedArgs)
  return HtmlCompat.fromHtml(formattedHtml, HtmlCompat.FROM_HTML_MODE_LEGACY)
}

fun keepScreenOn(on: Boolean) {
  val window = mainActivity.get()?.window ?: return
  Handler(Looper.getMainLooper()).post {
    if (on) {
      window.addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON)
    } else {
      window.clearFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON)
    }
  }
}

@Composable
actual fun SetupClipboardListener() {
  DisposableEffect(Unit) {
    val service = androidAppContext.getSystemService(Context.CLIPBOARD_SERVICE) as ClipboardManager
    val listener = { chatModel.clipboardHasText.value = service.hasPrimaryClip() }
    chatModel.clipboardHasText.value = service.hasPrimaryClip()
    service.addPrimaryClipChangedListener(listener)
    onDispose {
      service.removePrimaryClipChangedListener(listener)
    }
  }
}

actual fun escapedHtmlToAnnotatedString(text: String, density: Density): AnnotatedString {
  return spannableStringToAnnotatedString(HtmlCompat.fromHtml(text, HtmlCompat.FROM_HTML_MODE_LEGACY), density)
}

private fun spannableStringToAnnotatedString(
  text: CharSequence,
  density: Density,
): AnnotatedString {
  return if (text is Spanned) {
    with(density) {
      buildAnnotatedString {
        append((text.toString()))
        text.getSpans(0, text.length, Any::class.java).forEach {
          val start = text.getSpanStart(it)
          val end = text.getSpanEnd(it)
          when (it) {
            is StyleSpan -> when (it.style) {
              Typeface.NORMAL -> addStyle(
                SpanStyle(
                  fontWeight = FontWeight.Normal,
                  fontStyle = FontStyle.Normal,
                ),
                start,
                end
              )
              Typeface.BOLD -> addStyle(
                SpanStyle(
                  fontWeight = FontWeight.Bold,
                  fontStyle = FontStyle.Normal
                ),
                start,
                end
              )
              Typeface.ITALIC -> addStyle(
                SpanStyle(
                  fontWeight = FontWeight.Normal,
                  fontStyle = FontStyle.Italic
                ),
                start,
                end
              )
              Typeface.BOLD_ITALIC -> addStyle(
                SpanStyle(
                  fontWeight = FontWeight.Bold,
                  fontStyle = FontStyle.Italic
                ),
                start,
                end
              )
            }
            is TypefaceSpan -> addStyle(
              SpanStyle(
                fontFamily = when (it.family) {
                  FontFamily.SansSerif.name -> FontFamily.SansSerif
                  FontFamily.Serif.name -> FontFamily.Serif
                  FontFamily.Monospace.name -> FontFamily.Monospace
                  FontFamily.Cursive.name -> FontFamily.Cursive
                  else -> FontFamily.Default
                }
              ),
              start,
              end
            )
            is AbsoluteSizeSpan -> addStyle(
              SpanStyle(fontSize = if (it.dip) it.size.dp.toSp() else it.size.toSp()),
              start,
              end
            )
            is RelativeSizeSpan -> addStyle(
              SpanStyle(fontSize = it.sizeChange.em),
              start,
              end
            )
            is StrikethroughSpan -> addStyle(
              SpanStyle(textDecoration = TextDecoration.LineThrough),
              start,
              end
            )
            is UnderlineSpan -> addStyle(
              SpanStyle(textDecoration = TextDecoration.Underline),
              start,
              end
            )
            is SuperscriptSpan -> addStyle(
              SpanStyle(baselineShift = BaselineShift.Superscript),
              start,
              end
            )
            is SubscriptSpan -> addStyle(
              SpanStyle(baselineShift = BaselineShift.Subscript),
              start,
              end
            )
            is ForegroundColorSpan -> addStyle(
              SpanStyle(color = Color(it.foregroundColor)),
              start,
              end
            )
            else -> addStyle(SpanStyle(color = Color.White), start, end)
          }
        }
      }
    }
  } else {
    AnnotatedString(text.toString())
  }
}

actual fun getAppFileUri(fileName: String): URI =
  FileProvider.getUriForFile(androidAppContext, "$APPLICATION_ID.provider", if (File(fileName).isAbsolute) File(fileName) else File(getAppFilePath(fileName))).toURI()

// https://developer.android.com/training/data-storage/shared/documents-files#bitmap
actual suspend fun getLoadedImage(file: CIFile?): Pair<ImageBitmap, ByteArray>? {
  val filePath = getLoadedFilePath(file)
  return if (filePath != null && file != null) {
    try {
      val data = if (file.fileSource?.cryptoArgs != null) {
        try {
          readCryptoFile(getAppFilePath(file.fileSource.filePath), file.fileSource.cryptoArgs)
        } catch (e: Exception) {
          Log.e(TAG, "Unable to read crypto file: " + e.stackTraceToString())
          return null
        }
      } else {
        File(getAppFilePath(file.fileName)).readBytes()
      }
      decodeSampledBitmapFromByteArray(data, 1000, 1000).asImageBitmap() to data
    } catch (e: Exception) {
      Log.e(TAG, e.stackTraceToString())
      null
    }
  } else {
    null
  }
}

// https://developer.android.com/topic/performance/graphics/load-bitmap#load-bitmap
private fun decodeSampledBitmapFromByteArray(data: ByteArray, reqWidth: Int, reqHeight: Int): Bitmap {
  // First decode with inJustDecodeBounds=true to check dimensions
  return BitmapFactory.Options().run {
    inJustDecodeBounds = true
    BitmapFactory.decodeByteArray(data, 0, data.size)
    // Calculate inSampleSize
    inSampleSize = calculateInSampleSize(this, reqWidth, reqHeight)
    // Decode bitmap with inSampleSize set
    inJustDecodeBounds = false

    BitmapFactory.decodeByteArray(data, 0, data.size)
  }
}

private fun calculateInSampleSize(options: BitmapFactory.Options, reqWidth: Int, reqHeight: Int): Int {
  // Raw height and width of image
  val (height: Int, width: Int) = options.run { outHeight to outWidth }
  var inSampleSize = 1

  if (height > reqHeight || width > reqWidth) {
    val halfHeight: Int = height / 2
    val halfWidth: Int = width / 2
    // Calculate the largest inSampleSize value that is a power of 2 and keeps both
    // height and width larger than the requested height and width.
    while (halfHeight / inSampleSize >= reqHeight && halfWidth / inSampleSize >= reqWidth) {
      inSampleSize *= 2
    }
  }

  return inSampleSize
}

actual fun getFileName(uri: URI): String? {
  return try {
    androidAppContext.contentResolver.query(uri.toUri(), null, null, null, null)?.use { cursor ->
      val nameIndex = cursor.getColumnIndex(OpenableColumns.DISPLAY_NAME)
      cursor.moveToFirst()
      // Can make an exception
      cursor.getString(nameIndex)
    }
  } catch (e: Exception) {
    null
  }
}

actual fun getAppFilePath(uri: URI): String? {
  return androidAppContext.contentResolver.query(uri.toUri(), null, null, null, null)?.use { cursor ->
    val nameIndex = cursor.getColumnIndex(OpenableColumns.DISPLAY_NAME)
    cursor.moveToFirst()
    getAppFilePath(cursor.getString(nameIndex))
  }
}

actual fun getFileSize(uri: URI): Long? {
  return androidAppContext.contentResolver.query(uri.toUri(), null, null, null, null)?.use { cursor ->
    val sizeIndex = cursor.getColumnIndex(OpenableColumns.SIZE)
    cursor.moveToFirst()
    cursor.getLong(sizeIndex)
  }
}

actual fun getBitmapFromUri(uri: URI, withAlertOnException: Boolean): ImageBitmap? {
  return if (Build.VERSION.SDK_INT >= 28) {
    try {
      val source = ImageDecoder.createSource(androidAppContext.contentResolver, uri.toUri())
      ImageDecoder.decodeBitmap(source)
    } catch (e: Exception) {
      Log.e(TAG, "Unable to decode the image: ${e.stackTraceToString()}")
      if (withAlertOnException) showImageDecodingException()

      null
    }
  } else {
    BitmapFactory.decodeFile(getAppFilePath(uri))
  }?.asImageBitmap()
}

actual fun getBitmapFromByteArray(data: ByteArray, withAlertOnException: Boolean): ImageBitmap? {
  return if (Build.VERSION.SDK_INT >= 31) {
    try {
      val source = ImageDecoder.createSource(data)
      ImageDecoder.decodeBitmap(source)
    } catch (e: android.graphics.ImageDecoder.DecodeException) {
      Log.e(TAG, "Unable to decode the image: ${e.stackTraceToString()}")
      if (withAlertOnException) showImageDecodingException()

      null
    }
  } else {
    BitmapFactory.decodeByteArray(data, 0, data.size)
  }?.asImageBitmap()
}

actual fun getDrawableFromUri(uri: URI, withAlertOnException: Boolean): Any? {
  return if (Build.VERSION.SDK_INT >= 28) {
    try {
      val source = ImageDecoder.createSource(androidAppContext.contentResolver, uri.toUri())
      ImageDecoder.decodeDrawable(source)
    } catch (e: Exception) {
      Log.e(TAG, "Error while decoding drawable: ${e.stackTraceToString()}")
      if (withAlertOnException) showImageDecodingException()

      null
    }
  } else {
    Drawable.createFromPath(getAppFilePath(uri))
  }
}

actual suspend fun saveTempImageUncompressed(image: ImageBitmap, asPng: Boolean): File? {
  return try {
    val ext = if (asPng) "png" else "jpg"
    tmpDir.mkdir()
    return File(tmpDir.absolutePath + File.separator + generateNewFileName("IMG", ext, tmpDir)).apply {
      outputStream().use { out ->
        image.asAndroidBitmap().compress(if (asPng) Bitmap.CompressFormat.PNG else Bitmap.CompressFormat.JPEG, 85, out)
        out.flush()
      }
      deleteOnExit()
      ChatModel.filesToDelete.add(this)
    }
  } catch (e: Exception) {
    Log.e(TAG, "Utils.android saveTempImageUncompressed error: ${e.message}")
    null
  }
}

actual suspend fun getBitmapFromVideo(uri: URI, timestamp: Long?, random: Boolean, withAlertOnException: Boolean): VideoPlayerInterface.PreviewAndDuration =
  try {
    val mmr = MediaMetadataRetriever()
    mmr.setDataSource(androidAppContext, uri.toUri())
    val durationMs = mmr.extractMetadata(MediaMetadataRetriever.METADATA_KEY_DURATION)?.toLong()
    val image = when {
      timestamp != null -> mmr.getFrameAtTime(timestamp * 1000, MediaMetadataRetriever.OPTION_CLOSEST)
      random -> mmr.frameAtTime
      else -> mmr.getFrameAtTime(0)
    }
    mmr.release()
    VideoPlayerInterface.PreviewAndDuration(image?.asImageBitmap(), durationMs, timestamp ?: 0)
  } catch (e: Exception) {
    Log.e(TAG, "Utils.android getBitmapFromVideo error: ${e.message}")
    if (withAlertOnException) showVideoDecodingException()

    VideoPlayerInterface.PreviewAndDuration(null, 0, 0)
  }

actual fun ByteArray.toBase64StringForPassphrase(): String = Base64.encodeToString(this, Base64.DEFAULT)

actual fun String.toByteArrayFromBase64ForPassphrase(): ByteArray = Base64.decode(this, Base64.DEFAULT)
