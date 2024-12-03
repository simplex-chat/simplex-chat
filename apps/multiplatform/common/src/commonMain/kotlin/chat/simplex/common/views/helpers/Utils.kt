package chat.simplex.common.views.helpers

import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.Saver
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.graphics.*
import androidx.compose.ui.platform.*
import androidx.compose.ui.text.*
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.ThemeOverrides
import chat.simplex.common.views.chatlist.connectIfOpenedViaUri
import chat.simplex.res.MR
import com.charleskorn.kaml.decodeFromStream
import dev.icerock.moko.resources.StringResource
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*
import kotlinx.serialization.encodeToString
import java.io.*
import java.net.URI
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.text.SimpleDateFormat
import java.util.*
import java.util.concurrent.Executors
import kotlin.math.*

private val singleThreadDispatcher = Executors.newSingleThreadExecutor().asCoroutineDispatcher()

fun withApi(action: suspend CoroutineScope.() -> Unit): Job =
  Exception().let {
    CoroutineScope(Dispatchers.Main).launch(block = { wrapWithLogging(action, it) })
  }

fun withBGApi(action: suspend CoroutineScope.() -> Unit): Job =
  Exception().let {
    CoroutineScope(singleThreadDispatcher).launch(block = { wrapWithLogging(action, it) })
  }

fun withLongRunningApi(slow: Long = Long.MAX_VALUE, action: suspend CoroutineScope.() -> Unit): Job =
  Exception().let {
    CoroutineScope(Dispatchers.Default).launch(block = { wrapWithLogging(action, it, slow = slow) })
  }

private suspend fun wrapWithLogging(action: suspend CoroutineScope.() -> Unit, exception: java.lang.Exception, slow: Long = 20_000) = coroutineScope {
  val start = System.currentTimeMillis()
  action()
  val end = System.currentTimeMillis()
  if (end - start > slow) {
    Log.e(TAG, "Possible problem with execution of the thread, took ${(end - start) / 1000}s:\n${exception.stackTraceToString()}")
    if (appPreferences.developerTools.get() && appPreferences.showSlowApiCalls.get()) {
      AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.possible_slow_function_title),
        text = generalGetString(MR.strings.possible_slow_function_desc).format((end - start) / 1000, exception.stackTraceToString()),
        shareText = true
      )
    }
  }
}

@OptIn(InternalCoroutinesApi::class)
suspend fun interruptIfCancelled() = coroutineScope {
  if (!isActive) {
    Log.d(TAG, "Coroutine was cancelled and interrupted: ${Exception().stackTraceToString()}")
    throw coroutineContext.job.getCancellationException()
  }
}

/**
 * This coroutine helper makes possible to cancel coroutine scope when a user goes back but not when the user rotates a screen
 * */
@Composable
fun ModalData.CancellableOnGoneJob(key: String = rememberSaveable { UUID.randomUUID().toString() }): MutableState<Job> {
  val job = remember { stateGetOrPut<Job>(key) { Job() } }
  DisposableEffectOnGone {
    job.value.cancel()
  }
  return job
}

enum class KeyboardState {
  Opened, Closed
}

// Resource to annotated string from
// https://stackoverflow.com/questions/68549248/android-jetpack-compose-how-to-show-styled-text-from-string-resources
fun generalGetString(id: StringResource): String {
  // prefer stringResource in Composable items to retain preview abilities
  return id.localized()
}

expect fun escapedHtmlToAnnotatedString(text: String, density: Density): AnnotatedString

@Composable
fun annotatedStringResource(id: StringResource): AnnotatedString {
  val density = LocalDensity.current
  return remember(id) {
    escapedHtmlToAnnotatedString(id.localized(), density)
  }
}

@Composable
fun annotatedStringResource(id: StringResource, vararg args: Any?): AnnotatedString {
  val density = LocalDensity.current
  return remember(id, args) {
    escapedHtmlToAnnotatedString(id.localized().format(args = args), density)
  }
}

@Composable
expect fun SetupClipboardListener()

// maximum image file size to be auto-accepted
const val MAX_IMAGE_SIZE: Long = 261_120 // 255KB
const val MAX_IMAGE_SIZE_AUTO_RCV: Long = MAX_IMAGE_SIZE * 2
const val MAX_VOICE_SIZE_AUTO_RCV: Long = MAX_IMAGE_SIZE * 2
const val MAX_VIDEO_SIZE_AUTO_RCV: Long = 1_047_552 // 1023KB

const val MAX_VOICE_MILLIS_FOR_SENDING: Int = 300_000

const val MAX_FILE_SIZE_SMP: Long = 8000000

const val MAX_FILE_SIZE_XFTP: Long = 1_073_741_824 // 1GB

const val MAX_FILE_SIZE_LOCAL: Long = Long.MAX_VALUE

expect fun getAppFileUri(fileName: String): URI

// https://developer.android.com/training/data-storage/shared/documents-files#bitmap
expect suspend fun getLoadedImage(file: CIFile?): Pair<ImageBitmap, ByteArray>?

expect fun getFileName(uri: URI): String?

expect fun getAppFilePath(uri: URI): String?

expect fun getFileSize(uri: URI): Long?

expect fun getBitmapFromUri(uri: URI, withAlertOnException: Boolean = true): ImageBitmap?

expect fun getBitmapFromByteArray(data: ByteArray, withAlertOnException: Boolean): ImageBitmap?

expect fun getDrawableFromUri(uri: URI, withAlertOnException: Boolean = true): Any?

fun getThemeFromUri(uri: URI, withAlertOnException: Boolean = true): ThemeOverrides? {
  uri.inputStream().use {
    runCatching {
      return yaml.decodeFromStream<ThemeOverrides>(it!!)
    }.onFailure {
      Log.e(TAG, "Error while decoding theme: ${it.stackTraceToString()}")
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

fun saveImage(uri: URI): CryptoFile? {
  val bitmap = getBitmapFromUri(uri) ?: return null
  return saveImage(bitmap)
}

fun saveImage(image: ImageBitmap): CryptoFile? {
  return try {
    val encrypted = chatController.appPrefs.privacyEncryptLocalFiles.get()
    val ext = if (image.hasAlpha()) "png" else "jpg"
    val dataResized = resizeImageToDataSize(image, ext == "png", maxDataSize = MAX_IMAGE_SIZE)
    val destFileName = generateNewFileName("IMG", ext, File(getAppFilePath("")))
    val destFile = File(getAppFilePath(destFileName))
    if (encrypted) {
      try {
        val args = writeCryptoFile(destFile.absolutePath, dataResized.toByteArray())
        CryptoFile(destFileName, args)
      } catch (e: Exception) {
        Log.e(TAG, "Unable to write crypto file: " + e.stackTraceToString())
        AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.error), text = e.stackTraceToString())
        null
      }
    } else {
      val output = FileOutputStream(destFile)
      dataResized.writeTo(output)
      output.flush()
      output.close()
      CryptoFile.plain(destFileName)
    }
  } catch (e: Exception) {
    Log.e(TAG, "Util.kt saveImage error: ${e.stackTraceToString()}")
    null
  }
}

fun desktopSaveImageInTmp(uri: URI): CryptoFile? {
  val image = getBitmapFromUri(uri) ?: return null
  return try {
    val ext = if (image.hasAlpha()) "png" else "jpg"
    val dataResized = resizeImageToDataSize(image, ext == "png", maxDataSize = MAX_IMAGE_SIZE)
    val destFileName = generateNewFileName("IMG", ext, tmpDir)
    val destFile = File(tmpDir, destFileName)
    val output = FileOutputStream(destFile)
    dataResized.writeTo(output)
    output.flush()
    output.close()
    CryptoFile.plain(destFile.absolutePath)
  } catch (e: Exception) {
    Log.e(TAG, "Util.kt desktopSaveImageInTmp error: ${e.stackTraceToString()}")
    null
  }
}

fun saveAnimImage(uri: URI): CryptoFile? {
  return try {
    val encrypted = chatController.appPrefs.privacyEncryptLocalFiles.get()
    val filename = getFileName(uri)?.lowercase()
    var ext = when {
      // remove everything but extension
      filename?.contains(".") == true -> filename.replaceBeforeLast('.', "").replace(".", "")
      else -> "gif"
    }
    // Just in case the image has a strange extension
    if (ext.length < 3 || ext.length > 4) ext = "gif"
    val destFileName = generateNewFileName("IMG", ext, File(getAppFilePath("")))
    val destFile = File(getAppFilePath(destFileName))
    if (encrypted) {
      try {
        val args = writeCryptoFile(destFile.absolutePath, uri.inputStream()?.readBytes() ?: return null)
        CryptoFile(destFileName, args)
      } catch (e: Exception) {
        Log.e(TAG, "Unable to read crypto file: " + e.stackTraceToString())
        AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.error), text = e.stackTraceToString())
        null
      }
    } else {
      Files.copy(uri.inputStream(), destFile.toPath())
      CryptoFile.plain(destFileName)
    }
  } catch (e: Exception) {
    Log.e(TAG, "Util.kt saveAnimImage error: ${e.message}")
    null
  }
}

expect suspend fun saveTempImageUncompressed(image: ImageBitmap, asPng: Boolean): File?

fun saveFileFromUri(uri: URI, withAlertOnException: Boolean = true): CryptoFile? {
  return try {
    val encrypted = chatController.appPrefs.privacyEncryptLocalFiles.get()
    val inputStream = uri.inputStream()
    val fileToSave = getFileName(uri)
    return if (inputStream != null && fileToSave != null) {
      val destFileName = uniqueCombine(fileToSave, File(getAppFilePath("")))
      val destFile = File(getAppFilePath(destFileName))
      if (encrypted) {
        createTmpFileAndDelete { tmpFile ->
          Files.copy(inputStream, tmpFile.toPath())
          try {
            val args = encryptCryptoFile(tmpFile.absolutePath, destFile.absolutePath)
            CryptoFile(destFileName, args)
          } catch (e: Exception) {
            Log.e(TAG, "Unable to encrypt plain file: " + e.stackTraceToString())
            AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.error), text = e.stackTraceToString())
            null
          }
        }
      } else {
        Files.copy(inputStream, destFile.toPath())
        CryptoFile.plain(destFileName)
      }
    } else {
      Log.e(TAG, "Util.kt saveFileFromUri null inputStream")
      if (withAlertOnException) showWrongUriAlert()

      null
    }
  } catch (e: Exception) {
    Log.e(TAG, "Util.kt saveFileFromUri error: ${e.stackTraceToString()}")
    if (withAlertOnException) showWrongUriAlert()

    null
  }
}

fun saveWallpaperFile(uri: URI): String? {
  val destFileName = generateNewFileName("wallpaper", "jpg", File(getWallpaperFilePath("")))
  val destFile = File(getWallpaperFilePath(destFileName))
  try {
    val inputStream = uri.inputStream()
    Files.copy(inputStream!!, destFile.toPath(), StandardCopyOption.REPLACE_EXISTING)
  } catch (e: Exception) {
    Log.e(TAG, "Error saving wallpaper file: ${e.stackTraceToString()}")
    AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error), e.stackTraceToString())
    return null
  }
  return destFile.name
}

fun saveWallpaperFile(image: ImageBitmap): String {
  val destFileName = generateNewFileName("wallpaper", "jpg", File(getWallpaperFilePath("")))
  val destFile = File(getWallpaperFilePath(destFileName))
  val dataResized = resizeImageToDataSize(image, false, maxDataSize = 5_000_000)
  val output = FileOutputStream(destFile)
  dataResized.use {
    it.writeTo(output)
  }
  return destFile.name
}

fun removeWallpaperFile(fileName: String? = null) {
  File(getWallpaperFilePath("_")).parentFile.listFiles()?.forEach {
    if (it.name == fileName) it.delete()
  }
  WallpaperType.cachedImages.remove(fileName)
}

fun <T> createTmpFileAndDelete(onCreated: (File) -> T): T {
  val tmpFile = File(tmpDir, UUID.randomUUID().toString())
  tmpFile.deleteOnExit()
  ChatModel.filesToDelete.add(tmpFile)
  try {
    return onCreated(tmpFile)
  } finally {
    tmpFile.delete()
  }
}

fun generateNewFileName(prefix: String, ext: String, dir: File): String {
  val sdf = SimpleDateFormat("yyyyMMdd_HHmmss", Locale.US)
  sdf.timeZone = TimeZone.getTimeZone("GMT")
  val timestamp = sdf.format(Date())
  return uniqueCombine("${prefix}_$timestamp.$ext", dir)
}

fun uniqueCombine(fileName: String, dir: File): String {
  val orig = File(fileName)
  val name = orig.nameWithoutExtension
  val ext = orig.extension
  fun tryCombine(n: Int): String {
    val suffix = if (n == 0) "" else "_$n"
    val f = "$name$suffix.$ext"
    return if (File(dir, f).exists()) tryCombine(n + 1) else f
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
  val dir = appFilesDir
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
    FileProtocol.LOCAL -> MAX_FILE_SIZE_LOCAL
  }
}

expect suspend fun getBitmapFromVideo(uri: URI, timestamp: Long? = null, random: Boolean = true, withAlertOnException: Boolean = true): VideoPlayerInterface.PreviewAndDuration

fun showWrongUriAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.non_content_uri_alert_title),
    text = generalGetString(MR.strings.non_content_uri_alert_text)
  )
}

fun showImageDecodingException() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.image_decoding_exception_title),
    text = generalGetString(MR.strings.image_decoding_exception_desc)
  )
}

fun showVideoDecodingException() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.image_decoding_exception_title),
    text = generalGetString(MR.strings.video_decoding_exception_desc)
  )
}

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

fun InputStream.toByteArray(): ByteArray =
  ByteArrayOutputStream().use { output ->
    val b = ByteArray(4096)
    var n = read(b)
    while (n != -1) {
      output.write(b, 0, n);
      n = read(b)
    }
    return output.toByteArray()
  }

expect fun ByteArray.toBase64StringForPassphrase(): String

// Android's default implementation that was used before multiplatform, adds non-needed characters at the end of string
// which can be bypassed by:
// fun String.toByteArrayFromBase64(): ByteArray = Base64.getMimeDecoder().decode(this.trimEnd { it == '\n' || it == ' ' })
expect fun String.toByteArrayFromBase64ForPassphrase(): ByteArray

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

fun UriHandler.openVerifiedSimplexUri(uri: String) {
  connectIfOpenedViaUri(chatModel.remoteHostId(), uri, ChatModel)
}

fun uriCreateOrNull(uri: String) = try { URI.create(uri) } catch (e: Exception) { null }

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

private var lastExecutedComposables = HashSet<Any>()
private val failedComposables = HashSet<Any>()

@Composable
fun tryOrShowError(key: Any = Exception().stackTraceToString().lines()[2], error: @Composable () -> Unit = {}, content: @Composable () -> Unit) {
  if (!failedComposables.contains(key)) {
    lastExecutedComposables.add(key)
    content()
    lastExecutedComposables.remove(key)
  } else {
    error()
  }
}

fun includeMoreFailedComposables() {
  lastExecutedComposables.forEach {
    failedComposables.add(it)
    Log.i(TAG, "Added composable key as failed: $it")
  }
  lastExecutedComposables.clear()
}

val fontSizeMultiplier: Float
  @Composable get() = remember { appPrefs.fontScale.state }.value

val fontSizeSqrtMultiplier: Float
  @Composable get() = sqrt(remember { appPrefs.fontScale.state }.value)

val desktopDensityScaleMultiplier: Float
  @Composable get() = if (appPlatform.isDesktop) remember { appPrefs.densityScale.state }.value else 1f

@Composable
fun TextUnit.toDp(): Dp {
  check(type == TextUnitType.Sp) { "Only Sp can convert to Px" }
  return Dp(value * LocalDensity.current.fontScale)
}

fun <T> Flow<T>.throttleLatest(delayMillis: Long): Flow<T> = this
  .conflate()
  .transform {
    emit(it)
    delay(delayMillis)
  }

@Composable
fun DisposableEffectOnGone(always: () -> Unit = {}, whenDispose: () -> Unit = {}, whenGone: () -> Unit) {
  DisposableEffect(Unit) {
    always()
    val orientation = windowOrientation()
    onDispose {
      whenDispose()
      withApi {
        // It needs some delay before check orientation again because it can still be not updated to actual value
        delay(300)
        if (orientation == windowOrientation()) {
          whenGone()
        }
      }
    }
  }
}

@Composable
fun DisposableEffectOnRotate(always: () -> Unit = {}, whenDispose: () -> Unit = {}, whenRotate: () -> Unit) {
  DisposableEffect(Unit) {
    always()
    val orientation = windowOrientation()
    onDispose {
      whenDispose()
      if (orientation != windowOrientation()) {
        whenRotate()
      }
    }
  }
}

/**
 * Runs the [block] only after initial value of the [key1] changes, not after initial launch
 * */
@Composable
@NonRestartableComposable
fun <T> KeyChangeEffect(
  key1: T?,
  block: suspend CoroutineScope.(prevKey: T?) -> Unit
) {
  var prevKey by remember { mutableStateOf(key1) }
  var anyChange by remember { mutableStateOf(false) }
  LaunchedEffect(key1) {
    if (anyChange || key1 != prevKey) {
      val prev = prevKey
      prevKey = key1
      anyChange = true
      // Call it as the last statement because the coroutine can be cancelled earlier
      block(prev)
    }
  }
}

/**
 * Runs the [block] only after initial value of the [key1] or [key2] changes, not after initial launch
 * */
@Composable
@NonRestartableComposable
fun KeyChangeEffect(
  key1: Any?,
  key2: Any?,
  block: suspend CoroutineScope.() -> Unit
) {
  val initialKey = remember { key1 }
  val initialKey2 = remember { key2 }
  var anyChange by remember { mutableStateOf(false) }
  LaunchedEffect(key1, key2) {
    if (anyChange || key1 != initialKey || key2 != initialKey2) {
      anyChange = true
      block()
    }
  }
}

/**
 * Runs the [block] only after initial value of the [key1], or [key2], or [key3] changes, not after initial launch
 * */
@Composable
@NonRestartableComposable
fun KeyChangeEffect(
  key1: Any?,
  key2: Any?,
  key3: Any?,
  block: suspend CoroutineScope.() -> Unit
) {
  val initialKey = remember { key1 }
  val initialKey2 = remember { key2 }
  val initialKey3 = remember { key3 }
  var anyChange by remember { mutableStateOf(false) }
  LaunchedEffect(key1, key2, key3) {
    if (anyChange || key1 != initialKey || key2 != initialKey2 || key3 != initialKey3) {
      anyChange = true
      block()
    }
  }
}
