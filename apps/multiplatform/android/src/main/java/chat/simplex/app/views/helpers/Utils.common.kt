package chat.simplex.app.views.helpers

import android.app.Activity
import android.content.ActivityNotFoundException
import android.graphics.Bitmap
import android.net.Uri
import android.util.Base64
import android.util.Log
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.Saver
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.toArgb
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.unit.IntSize
import androidx.core.graphics.ColorUtils
import chat.simplex.app.*
import chat.simplex.app.model.*
import chat.simplex.app.platform.getAppFilesDirectory
import chat.simplex.app.platform.resizeImageToDataSize
import chat.simplex.app.ui.theme.ThemeOverrides
import chat.simplex.res.MR
import com.charleskorn.kaml.decodeFromStream
import dev.icerock.moko.resources.StringResource
import kotlinx.coroutines.*
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.encodeToString
import org.apache.commons.io.IOUtils
import java.io.*
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

fun generalGetString(id: StringResource): String {
  // prefer stringResource in Composable items to retain preview abilities
  return id.getString(SimplexApp.context)
}

// maximum image file size to be auto-accepted
const val MAX_IMAGE_SIZE: Long = 261_120 // 255KB
const val MAX_IMAGE_SIZE_AUTO_RCV: Long = MAX_IMAGE_SIZE * 2
const val MAX_VOICE_SIZE_AUTO_RCV: Long = MAX_IMAGE_SIZE * 2
const val MAX_VIDEO_SIZE_AUTO_RCV: Long = 1_047_552 // 1023KB

const val MAX_VOICE_MILLIS_FOR_SENDING: Int = 300_000

const val MAX_FILE_SIZE_SMP: Long = 8000000

const val MAX_FILE_SIZE_XFTP: Long = 1_073_741_824 // 1GB

fun getAppFileUri(fileName: String): Uri {
  return Uri.parse("${getAppFilesDirectory()}/$fileName")
}

fun getThemeFromUri(uri: Uri, withAlertOnException: Boolean = true): ThemeOverrides? {
  SimplexApp.context.contentResolver.openInputStream(uri).use {
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

fun saveImage(uri: Uri): String? {
  val bitmap = getBitmapFromUri(uri) ?: return null
  return saveImage(bitmap)
}

fun saveImage(image: Bitmap): String? {
  return try {
    val ext = if (image.hasAlpha()) "png" else "jpg"
    val dataResized = resizeImageToDataSize(image, ext == "png", maxDataSize = MAX_IMAGE_SIZE)
    val fileToSave = generateNewFileName("IMG", ext)
    val file = File(chat.simplex.app.platform.getAppFilePath(fileToSave))
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

fun saveAnimImage(uri: Uri): String? {
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
    val file = File(chat.simplex.app.platform.getAppFilePath(fileToSave))
    val output = FileOutputStream(file)
    SimplexApp.context.contentResolver.openInputStream(uri)!!.use { input ->
      output.use { output ->
        input.copyTo(output)
      }
    }
    fileToSave
  } catch (e: Exception) {
    Log.e(TAG, "Util.kt saveAnimImage error: ${e.message}")
    null
  }
}


fun saveFileFromUri(uri: Uri): String? {
  return try {
    val inputStream = SimplexApp.context.contentResolver.openInputStream(uri)
    val fileToSave = getFileName(uri)
    if (inputStream != null && fileToSave != null) {
      val destFileName = uniqueCombine(fileToSave)
      val destFile = File(chat.simplex.app.platform.getAppFilePath(destFileName))
      IOUtils.copy(inputStream, FileOutputStream(destFile))
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
    return if (File(chat.simplex.app.platform.getAppFilePath(f)).exists()) tryCombine(n + 1) else f
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
  val file = File(chat.simplex.app.platform.getAppFilePath(fileName))
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
  } catch (e: java.lang.Exception) {
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

fun Color.darker(factor: Float = 0.1f): Color =
  Color(max(red * (1 - factor), 0f), max(green * (1 - factor), 0f), max(blue * (1 - factor), 0f), alpha)

fun Color.lighter(factor: Float = 0.1f): Color =
  Color(min(red * (1 + factor), 1f), min(green * (1 + factor), 1f), min(blue * (1 + factor), 1f), alpha)

fun Color.mixWith(color: Color, alpha: Float): Color =
  Color(ColorUtils.blendARGB(color.toArgb(), toArgb(), alpha))

fun ByteArray.toBase64String() = Base64.encodeToString(this, Base64.DEFAULT)

fun String.toByteArrayFromBase64() = Base64.decode(this, Base64.DEFAULT)

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
  } catch (e: ActivityNotFoundException) {
    Log.e(TAG, e.stackTraceToString())
  }
}

fun IntSize.Companion.Saver(): Saver<IntSize, *> = Saver(
  save = { it.width to it.height },
  restore = { IntSize(it.first, it.second) }
)

@Composable
fun DisposableEffectOnGone(always: () -> Unit = {}, whenDispose: () -> Unit = {}, whenGone: () -> Unit) {
  val context = LocalContext.current
  DisposableEffect(Unit) {
    always()
    val activity = context as? Activity ?: return@DisposableEffect onDispose {}
    val orientation = activity.resources.configuration.orientation
    onDispose {
      whenDispose()
      if (orientation == activity.resources.configuration.orientation) {
        whenGone()
      }
    }
  }
}

@Composable
fun DisposableEffectOnRotate(always: () -> Unit = {}, whenDispose: () -> Unit = {}, whenRotate: () -> Unit) {
  val context = LocalContext.current
  DisposableEffect(Unit) {
    always()
    val activity = context as? Activity ?: return@DisposableEffect onDispose {}
    val orientation = activity.resources.configuration.orientation
    onDispose {
      whenDispose()
      if (orientation != activity.resources.configuration.orientation) {
        whenRotate()
      }
    }
  }
}