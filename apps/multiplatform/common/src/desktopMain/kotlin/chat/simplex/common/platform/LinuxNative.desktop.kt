package chat.simplex.common.platform

import java.io.File
import java.nio.charset.Charset

private const val LINUX_LIBRARY = "libsimplex_linux.so"
private const val COMPOSE_RESOURCES_DIR = "compose.application.resources.dir"
private const val NUCLEUS_LIBRARY_PATH = "nucleus.native.libraryPath"
private const val NUCLEUS_DESKTOP_ENTRY_ID = "nucleus.app.linux.desktop.entry.id"
private const val NUCLEUS_STARTUP_WM_CLASS = "nucleus.app.linux.startup.wm.class"
private const val FILE_NAME_ENCODING = "sun.jnu.encoding"

enum class FileChooserAction(val gtkAction: Int) {
  Open(0),
  Save(1),
  SelectFolder(2),
}

internal object LinuxNative {
  val isLoaded: Boolean by lazy {
    val dir = System.getProperty(COMPOSE_RESOURCES_DIR) ?: return@lazy false
    try {
      System.load(File(dir, LINUX_LIBRARY).absolutePath)
      true
    } catch (e: UnsatisfiedLinkError) {
      Log.e(TAG, "Linux native library is unavailable: ${e.message}")
      false
    }
  }

  fun interface FileChooserCallback {
    fun onResult(paths: Array<ByteArray>?)
  }

  @JvmStatic
  external fun nativeChooseFiles(
    parent: Long,
    title: String,
    action: Int,
    multiple: Boolean,
    currentName: String?,
    filterName: String?,
    patterns: Array<String>?,
    callback: FileChooserCallback,
  )

  @JvmStatic
  external fun nativeConfirm(title: String, message: String): Boolean

  @JvmStatic
  external fun nativeGtkThemeName(): String?

  @JvmStatic
  external fun nativeGlRenderer(): String?

  @JvmStatic
  external fun nativeSecretLookup(alias: String): String?

  @JvmStatic
  external fun nativeSecretStore(alias: String, label: String, value: String)

  @JvmStatic
  external fun nativeSecretClear(alias: String)
}

fun prepareLinuxRuntime() {
  System.getProperty(COMPOSE_RESOURCES_DIR)?.let { System.setProperty(NUCLEUS_LIBRARY_PATH, it) }
  linuxDesktopEntryId?.let {
    System.setProperty(NUCLEUS_DESKTOP_ENTRY_ID, it)
    System.setProperty(NUCLEUS_STARTUP_WM_CLASS, it)
  }
}

internal fun fileChooserPatterns(extensions: List<String>): Array<String> =
  extensions.flatMap { listOf("*.${it.lowercase()}", "*.${it.uppercase()}") }.toTypedArray()

internal fun fileFromNativePath(path: ByteArray): File =
  File(String(path, Charset.forName(System.getProperty(FILE_NAME_ENCODING) ?: Charsets.UTF_8.name())))

fun linuxConfirm(title: String, message: String): Boolean =
  LinuxNative.isLoaded && LinuxNative.nativeConfirm(title, message)
