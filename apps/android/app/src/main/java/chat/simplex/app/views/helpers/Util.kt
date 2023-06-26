package chat.simplex.common.views.helpers

import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.Saver
import androidx.compose.ui.graphics.*
import androidx.compose.ui.platform.*
import androidx.compose.ui.text.*
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.ThemeOverrides
import com.charleskorn.kaml.decodeFromStream
import com.icerockdev.library.MR
import dev.icerock.moko.resources.StringResource
import kotlinx.coroutines.*
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.encodeToString
import java.io.*
import java.net.URI
import java.nio.file.Files
import java.text.SimpleDateFormat
import java.util.*
import kotlin.math.*

fun withApi(action: suspend CoroutineScope.() -> Unit): Job = withScope(GlobalScope, action)

fun withScope(scope: CoroutineScope, action: suspend CoroutineScope.() -> Unit): Job =
  scope.launch { withContext(Dispatchers.Main, action) }

fun withBGApi(action: suspend CoroutineScope.() -> Unit): Job =
  CoroutineScope(Dispatchers.Default).launch(block = action)

enum class KeyboardState {
  Opened, Closed
}

// Resource to annotated string from
// https://stackoverflow.com/questions/68549248/android-jetpack-compose-how-to-show-styled-text-from-string-resources
fun generalGetString(id: StringResource): String {
  // prefer stringResource in Composable items to retain preview abilities
  return id.localized()
}

@Composable
expect fun annotatedStringResource(id: StringResource): AnnotatedString

// maximum image file size to be auto-accepted
const val MAX_IMAGE_SIZE: Long = 261_120 // 255KB
const val MAX_IMAGE_SIZE_AUTO_RCV: Long = MAX_IMAGE_SIZE * 2
const val MAX_VOICE_SIZE_AUTO_RCV: Long = MAX_IMAGE_SIZE * 2
const val MAX_VIDEO_SIZE_AUTO_RCV: Long = 1_047_552 // 1023KB

const val MAX_VOICE_MILLIS_FOR_SENDING: Int = 300_000

const val MAX_FILE_SIZE_SMP: Long = 8000000

const val MAX_FILE_SIZE_XFTP: Long = 1_073_741_824 // 1GB

fun getAppFileUri(fileName: String): URI {
  return URI("file:" + getAppFilesDirectory() + File.separator + fileName)
}

// https://developer.android.com/training/data-storage/shared/documents-files#bitmap
expect fun getLoadedImage(file: CIFile?): ImageBitmap?

expect fun getFileName(uri: URI): String?

expect fun getAppFilePath(uri: URI): String?

expect fun getFileSize(uri: URI): Long?

expect fun getBitmapFromUri(uri: URI, withAlertOnException: Boolean = true): ImageBitmap?

expect fun getDrawableFromUri(uri: URI, withAlertOnException: Boolean = true): Any?

fun getThemeFromUri(uri: URI, withAlertOnException: Boolean = true): ThemeOverrides? {
  uri.inputStream().use {
    runCatching {
      return yaml.decodeFromStream<ThemeOverrides>(it!!)
    }.onFailure {
      if (withAlertOnException) {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.import_theme_error),
          text = generalGetString(MR.strings.import_theme_error_desc),
        )
      }
    }
  }
  return null
}

fun saveImage(uri: URI): String? {
  val bitmap = getBitmapFromUri(uri) ?: return null
  return saveImage(bitmap)
}

fun saveImage(image: ImageBitmap): String? {
  return try {
    val ext = if (image.hasAlpha) "png" else "jpg"
    val dataResized = resizeImageToDataSize(image, ext == "png", maxDataSize = MAX_IMAGE_SIZE)
    val fileToSave = generateNewFileName("IMG", ext)
    val file = File(getAppFilePath(fileToSave))
    val output = FileOutputStream(file)
    dataResized.writeTo(output)
    output.flush()
    output.close()
    fileToSave
  } catch (e: Exception) {
    Log.e(TAG, "Util.kt saveImage error: ${e.message}")
    null
  }
}

fun saveAnimImage(uri: URI): String? {
  return try {
    val filename = getFileName(uri)?.lowercase()
    var ext = when {
      // remove everything but extension
      filename?.contains(".") == true -> filename.replaceBeforeLast('.', "").replace(".", "")
      else -> "gif"
    }
    // Just in case the image has a strange extension
    if (ext.length < 3 || ext.length > 4) ext = "gif"
    val fileToSave = generateNewFileName("IMG", ext)
    val file = File(getAppFilePath(fileToSave))
    val output = FileOutputStream(file)
    uri.inputStream().use { input ->
      output.use { output ->
        input?.copyTo(output)
      }
    }
    fileToSave
  } catch (e: Exception) {
    Log.e(TAG, "Util.kt saveAnimImage error: ${e.message}")
    null
  }
}

expect suspend fun saveTempImageUncompressed(image: ImageBitmap, asPng: Boolean): File?

fun saveFileFromUri(uri: URI): String? {
  return try {
    val inputStream = uri.inputStream()
    val fileToSave = getFileName(uri)
    if (inputStream != null && fileToSave != null) {
      val destFileName = uniqueCombine(fileToSave)
      val destFile = File(getAppFilePath(destFileName))
      Files.copy(inputStream, destFile.toPath())
      destFileName
    } else {
      Log.e(TAG, "Util.kt saveFileFromUri null inputStream")
      null
    }
  } catch (e: Exception) {
    Log.e(TAG, "Util.kt saveFileFromUri error: ${e.message}")
    null
  }
}

fun generateNewFileName(prefix: String, ext: String): String {
  val sdf = SimpleDateFormat("yyyyMMdd_HHmmss", Locale.US)
  sdf.timeZone = TimeZone.getTimeZone("GMT")
  val timestamp = sdf.format(Date())
  return uniqueCombine("${prefix}_$timestamp.$ext")
}

fun uniqueCombine(fileName: String): String {
  val orig = File(fileName)
  val name = orig.nameWithoutExtension
  val ext = orig.extension
  fun tryCombine(n: Int): String {
    val suffix = if (n == 0) "" else "_$n"
    val f = "$name$suffix.$ext"
    return if (File(getAppFilePath(f)).exists()) tryCombine(n + 1) else f
  }
  return tryCombine(0)
}

fun formatBytes(bytes: Long): String {
  if (bytes == 0.toLong()) {
    return "0 bytes"
  }
  val bytesDouble = bytes.toDouble()
  val k = 1024.toDouble()
  val units = arrayOf("bytes", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB")
  val i = floor(log2(bytesDouble) / log2(k))
  val size = bytesDouble / k.pow(i)
  val unit = units[i.toInt()]

  return if (i <= 1) {
    String.format("%.0f %s", size, unit)
  } else {
    String.format("%.2f %s", size, unit)
  }
}

fun removeFile(fileName: String): Boolean {
  val file = File(getAppFilePath(fileName))
  val fileDeleted = file.delete()
  if (!fileDeleted) {
    Log.e(TAG, "Util.kt removeFile error")
  }
  return fileDeleted
}

fun deleteAppFiles() {
  val dir = File(getAppFilesDirectory())
  try {
    dir.list()?.forEach {
      removeFile(it)
    }
  } catch (e: java.lang.Exception) {
    Log.e(TAG, "Util deleteAppFiles error: ${e.stackTraceToString()}")
  }
}

fun directoryFileCountAndSize(dir: String): Pair<Int, Long> { // count, size in bytes
  var fileCount = 0
  var bytes = 0L
  try {
    File(dir).listFiles()?.forEach {
      fileCount++
      bytes += it.length()
    }
  } catch (e: Exception) {
    Log.e(TAG, "Util directoryFileCountAndSize error: ${e.stackTraceToString()}")
  }
  return fileCount to bytes
}

fun getMaxFileSize(fileProtocol: FileProtocol): Long {
  return when (fileProtocol) {
    FileProtocol.XFTP -> MAX_FILE_SIZE_XFTP
    FileProtocol.SMP -> MAX_FILE_SIZE_SMP
  }
}

expect fun getBitmapFromVideo(uri: URI, timestamp: Long? = null, random: Boolean = true): VideoPlayerInterface.PreviewAndDuration

fun Color.darker(factor: Float = 0.1f): Color =
  Color(max(red * (1 - factor), 0f), max(green * (1 - factor), 0f), max(blue * (1 - factor), 0f), alpha)

fun Color.lighter(factor: Float = 0.1f): Color =
  Color(min(red * (1 + factor), 1f), min(green * (1 + factor), 1f), min(blue * (1 + factor), 1f), alpha)

fun Color.mixWith(color: Color, alpha: Float): Color = blendARGB(color, this, alpha)

fun blendARGB(
  color1: Color, color2: Color,
  ratio: Float
): Color {
  val inverseRatio = 1 - ratio
  val a: Float = color1.alpha * inverseRatio + color2.alpha * ratio
  val r: Float = color1.red * inverseRatio + color2.red * ratio
  val g: Float = color1.green * inverseRatio + color2.green * ratio
  val b: Float = color1.blue * inverseRatio + color2.blue * ratio
  return Color(r, g, b, a)
}

fun ByteArray.toBase64String(): String = Base64.getEncoder().encodeToString(this)

fun String.toByteArrayFromBase64(): ByteArray = Base64.getDecoder().decode(this)

val LongRange.Companion.saver
  get() = Saver<MutableState<LongRange>, Pair<Long, Long>>(
    save = { it.value.first to it.value.last },
    restore = { mutableStateOf(it.first..it.second) }
    )

/* Make sure that T class has @Serializable annotation */
inline fun <reified T> serializableSaver(): Saver<T, *> = Saver(
    save = { json.encodeToString(it) },
    restore = { json.decodeFromString(it) }
  )

fun UriHandler.openUriCatching(uri: String) {
  try {
    openUri(uri)
  } catch (e: Exception/*ActivityNotFoundException*/) {
    Log.e(TAG, e.stackTraceToString())
  }
}

fun IntSize.Companion.Saver(): Saver<IntSize, *> = Saver(
  save = { it.width to it.height },
  restore = { IntSize(it.first, it.second) }
)

@Composable
fun DisposableEffectOnGone(always: () -> Unit = {}, whenDispose: () -> Unit = {}, whenGone: () -> Unit) {
  DisposableEffect(Unit) {
    always()
    val orientation = screenOrientation()
    onDispose {
      whenDispose()
      if (orientation == screenOrientation()) {
        whenGone()
      }
    }
  }
}

@Composable
fun DisposableEffectOnRotate(always: () -> Unit = {}, whenDispose: () -> Unit = {}, whenRotate: () -> Unit) {
  DisposableEffect(Unit) {
    always()
    val orientation = screenOrientation()
    onDispose {
      whenDispose()
      if (orientation != screenOrientation()) {
        whenRotate()
      }
    }
  }
}
