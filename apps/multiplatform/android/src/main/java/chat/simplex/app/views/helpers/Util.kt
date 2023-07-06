package chat.simplex.app.views.helpers

import android.app.Application
//import android.app.LocaleManager
import android.content.res.Resources
import android.graphics.*
import android.graphics.Typeface
import android.graphics.drawable.Drawable
import android.media.MediaMetadataRetriever
import android.net.Uri
import android.os.*
import android.provider.OpenableColumns
import android.text.Spanned
import android.text.SpannedString
import android.text.style.*
import android.util.Log
import androidx.compose.runtime.*
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.*
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.style.BaselineShift
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.unit.*
import androidx.core.content.FileProvider
import androidx.core.text.HtmlCompat
import chat.simplex.app.*
import chat.simplex.app.model.*
import chat.simplex.app.platform.getLoadedFilePath
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource
import java.io.*

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

fun escapedHtmlToAnnotatedString(text: String, density: Density): AnnotatedString {
  return spannableStringToAnnotatedString(HtmlCompat.fromHtml(text, HtmlCompat.FROM_HTML_MODE_LEGACY), density)
}

@Composable
fun annotatedStringResource(id: StringResource): AnnotatedString {
  val density = LocalDensity.current
  return remember(id) {
    val text = id.getString(SimplexApp.context)
    escapedHtmlToAnnotatedString(text, density)
  }
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

// https://developer.android.com/training/data-storage/shared/documents-files#bitmap
fun getLoadedImage(file: CIFile?): Bitmap? {
  val filePath = getLoadedFilePath(file)
  return if (filePath != null) {
    try {
      val uri = FileProvider.getUriForFile(SimplexApp.context, "${BuildConfig.APPLICATION_ID}.provider", File(filePath))
      val parcelFileDescriptor = SimplexApp.context.contentResolver.openFileDescriptor(uri, "r")
      val fileDescriptor = parcelFileDescriptor?.fileDescriptor
      val image = decodeSampledBitmapFromFileDescriptor(fileDescriptor, 1000, 1000)
      parcelFileDescriptor?.close()
      image
    } catch (e: Exception) {
      null
    }
  } else {
    null
  }
}

// https://developer.android.com/topic/performance/graphics/load-bitmap#load-bitmap
private fun decodeSampledBitmapFromFileDescriptor(fileDescriptor: FileDescriptor?, reqWidth: Int, reqHeight: Int): Bitmap {
  // First decode with inJustDecodeBounds=true to check dimensions
  return BitmapFactory.Options().run {
    inJustDecodeBounds = true
    BitmapFactory.decodeFileDescriptor(fileDescriptor, null, this)
    // Calculate inSampleSize
    inSampleSize = calculateInSampleSize(this, reqWidth, reqHeight)
    // Decode bitmap with inSampleSize set
    inJustDecodeBounds = false

    BitmapFactory.decodeFileDescriptor(fileDescriptor, null, this)
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

fun getFileName(uri: Uri): String? {
  return SimplexApp.context.contentResolver.query(uri, null, null, null, null)?.use { cursor ->
    val nameIndex = cursor.getColumnIndex(OpenableColumns.DISPLAY_NAME)
    cursor.moveToFirst()
    cursor.getString(nameIndex)
  }
}

fun getAppFilePath(uri: Uri): String? {
  return SimplexApp.context.contentResolver.query(uri, null, null, null, null)?.use { cursor ->
    val nameIndex = cursor.getColumnIndex(OpenableColumns.DISPLAY_NAME)
    cursor.moveToFirst()
    chat.simplex.app.platform.getAppFilePath(cursor.getString(nameIndex))
  }
}

fun getFileSize(uri: Uri): Long? {
  return SimplexApp.context.contentResolver.query(uri, null, null, null, null)?.use { cursor ->
    val sizeIndex = cursor.getColumnIndex(OpenableColumns.SIZE)
    cursor.moveToFirst()
    cursor.getLong(sizeIndex)
  }
}

fun getBitmapFromUri(uri: Uri, withAlertOnException: Boolean = true): Bitmap? {
  return if (Build.VERSION.SDK_INT >= 28) {
    val source = ImageDecoder.createSource(SimplexApp.context.contentResolver, uri)
    try {
      ImageDecoder.decodeBitmap(source)
    } catch (e: android.graphics.ImageDecoder.DecodeException) {
      Log.e(TAG, "Unable to decode the image: ${e.stackTraceToString()}")
      if (withAlertOnException) {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.image_decoding_exception_title),
          text = generalGetString(MR.strings.image_decoding_exception_desc)
        )
      }
      null
    }
  } else {
    BitmapFactory.decodeFile(getAppFilePath(uri))
  }
}

fun getDrawableFromUri(uri: Uri, withAlertOnException: Boolean = true): Drawable? {
  return if (Build.VERSION.SDK_INT >= 28) {
    val source = ImageDecoder.createSource(SimplexApp.context.contentResolver, uri)
    try {
      ImageDecoder.decodeDrawable(source)
    } catch (e: android.graphics.ImageDecoder.DecodeException) {
      if (withAlertOnException) {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.image_decoding_exception_title),
          text = generalGetString(MR.strings.image_decoding_exception_desc)
        )
      }
      Log.e(TAG, "Error while decoding drawable: ${e.stackTraceToString()}")
      null
    }
  } else {
    Drawable.createFromPath(getAppFilePath(uri))
  }
}

fun saveTempImageUncompressed(image: Bitmap, asPng: Boolean): File? {
  return try {
    val ext = if (asPng) "png" else "jpg"
    val tmpDir = SimplexApp.context.getDir("temp", Application.MODE_PRIVATE)
    return File(tmpDir.absolutePath + File.separator + generateNewFileName("IMG", ext)).apply {
      outputStream().use { out ->
        image.compress(if (asPng) Bitmap.CompressFormat.PNG else Bitmap.CompressFormat.JPEG, 85, out)
        out.flush()
      }
      deleteOnExit()
      SimplexApp.context.chatModel.filesToDelete.add(this)
    }
  } catch (e: Exception) {
    Log.e(TAG, "Util.kt saveTempImageUncompressed error: ${e.message}")
    null
  }
}

fun getBitmapFromVideo(uri: Uri, timestamp: Long? = null, random: Boolean = true): VideoPlayer.PreviewAndDuration {
  val mmr = MediaMetadataRetriever()
  mmr.setDataSource(SimplexApp.context, uri)
  val durationMs = mmr.extractMetadata(MediaMetadataRetriever.METADATA_KEY_DURATION)?.toLong()
  val image = when {
    timestamp != null -> mmr.getFrameAtTime(timestamp * 1000, MediaMetadataRetriever.OPTION_CLOSEST)
    random -> mmr.frameAtTime
    else -> mmr.getFrameAtTime(0)
  }
  mmr.release()
  return VideoPlayer.PreviewAndDuration(image, durationMs, timestamp ?: 0)
}
