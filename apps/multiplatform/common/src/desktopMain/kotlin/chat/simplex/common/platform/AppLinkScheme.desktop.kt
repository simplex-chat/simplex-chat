package chat.simplex.common.platform

import chat.simplex.common.views.chatlist.appLinkScheme
import com.sun.jna.platform.win32.Advapi32Util
import com.sun.jna.platform.win32.Win32Exception
import com.sun.jna.platform.win32.WinReg.HKEY_CURRENT_USER
import java.io.File
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.PosixFileAttributes
import java.nio.file.attribute.PosixFilePermission
import java.util.concurrent.TimeUnit
import kotlin.concurrent.thread

internal const val SCHEME_MIME_TYPE = "x-scheme-handler/$appLinkScheme"
private const val WINDOWS_SCHEME_KEY = "Software\\Classes\\$appLinkScheme"
private const val WINDOWS_COMMAND_KEY = "$WINDOWS_SCHEME_KEY\\shell\\open\\command"
private const val WINDOWS_ICON_KEY = "$WINDOWS_SCHEME_KEY\\DefaultIcon"
private const val WINDOWS_FIRST_ICON_INDEX = 0
// the Win32 name of a key's default value
private const val REGISTRY_DEFAULT_VALUE = ""
internal const val APPIMAGE_ENTRY_NAME = "chat.simplex.app-links.desktop"
private const val APP_DISPLAY_NAME = "SimpleX Chat"
private const val XDG_MIME = "xdg-mime"
// jpackage names the deb's desktop entry <package>-<app>.desktop
private const val DEB_ENTRY_NAME = "simplex-simplex.desktop"
private const val PROCESS_TIMEOUT_SECONDS = 5L
private const val ROOT_USER = "root"
// the characters the desktop entry spec requires an Exec argument to quote
private const val EXEC_RESERVED_CHARACTERS = " \t\n\"'\\><~|&;$*?#()`"

internal sealed interface DesktopInstallation {
  data object MacBundle : DesktopInstallation
  data class WindowsExe(val path: String) : DesktopInstallation
  data object Flatpak : DesktopInstallation
  data class AppImage(val path: String) : DesktopInstallation
  data object LinuxPackage : DesktopInstallation
  data object Unpackaged : DesktopInstallation
}

@Volatile
private var registered = false

actual fun appLinkSchemeRegistered(): Boolean = registered

fun registerAppLinkScheme() {
  thread(name = "simplex-app-link-scheme", isDaemon = true) {
    registered = registerForThisInstallation()
  }
}

// appPath is jpackage.app-path, set by jpackage's launcher, so only a packaged app has one.
internal fun desktopInstallation(platform: DesktopPlatform, appPath: String?, env: Map<String, String>): DesktopInstallation {
  val appImage = env["APPIMAGE"]
  val appDir = env["APPDIR"]
  return when {
    appPath == null -> DesktopInstallation.Unpackaged
    platform.isMac() -> DesktopInstallation.MacBundle
    platform.isWindows() -> DesktopInstallation.WindowsExe(appPath)
    env["container"] == "flatpak" -> DesktopInstallation.Flatpak
    // The AppImage runtime exports APPIMAGE to every child process, so it describes this app only
    // when the launcher runs from the AppImage's own mount.
    appImage != null && appDir != null && Path.of(appPath).startsWith(realPathOrSelf(appDir)) -> DesktopInstallation.AppImage(appImage)
    else -> DesktopInstallation.LinuxPackage
  }
}

// jpackage.app-path is resolved through /proc/self/exe, while APPDIR keeps any symlink in TMPDIR.
private fun realPathOrSelf(path: String): Path =
  try {
    Path.of(path).toRealPath()
  } catch (_: IOException) {
    Path.of(path)
  }

private fun registerForThisInstallation(): Boolean =
  when (val installation = desktopInstallation(desktopPlatform, System.getProperty("jpackage.app-path"), System.getenv())) {
    // The bundle's Info.plist and the Flatpak's exported desktop entry register the scheme.
    DesktopInstallation.MacBundle, DesktopInstallation.Flatpak -> true
    is DesktopInstallation.WindowsExe -> registerWindowsScheme(installation.path, ::registryDefault, ::writeRegistryValue)
    is DesktopInstallation.AppImage -> registerAppImageScheme(installation.path, File(unixDataHome, "applications"), ::runProcess)
    DesktopInstallation.LinuxPackage -> linuxPackageRegistered(::runProcess)
    DesktopInstallation.Unpackaged -> false
  }

internal fun windowsOpenCommand(appPath: String): String = "\"$appPath\" \"%1\""

internal data class RegistryValue(val key: String, val name: String, val value: String)

// The command goes last, so finding it means every value before it was written.
internal fun windowsSchemeValues(appPath: String): List<RegistryValue> = listOf(
  RegistryValue(key = WINDOWS_SCHEME_KEY, name = REGISTRY_DEFAULT_VALUE, value = "URL:$APP_DISPLAY_NAME"),
  // without this value Windows does not treat the key as a URL scheme
  RegistryValue(key = WINDOWS_SCHEME_KEY, name = "URL Protocol", value = ""),
  RegistryValue(key = WINDOWS_ICON_KEY, name = REGISTRY_DEFAULT_VALUE, value = "\"$appPath\",$WINDOWS_FIRST_ICON_INDEX"),
  RegistryValue(key = WINDOWS_COMMAND_KEY, name = REGISTRY_DEFAULT_VALUE, value = windowsOpenCommand(appPath)),
)

internal fun registerWindowsScheme(appPath: String, readDefault: (key: String) -> String?, write: (RegistryValue) -> Unit): Boolean {
  val command = windowsOpenCommand(appPath)
  return try {
    if (readDefault(WINDOWS_COMMAND_KEY) != command) windowsSchemeValues(appPath).forEach(write)
    readDefault(WINDOWS_COMMAND_KEY) == command
  } catch (e: Win32Exception) {
    Log.w(TAG, "app link scheme: cannot register in the registry: ${e.message}")
    false
  }
}

private fun writeRegistryValue(value: RegistryValue) {
  Advapi32Util.registryCreateKey(HKEY_CURRENT_USER, value.key)
  Advapi32Util.registrySetStringValue(HKEY_CURRENT_USER, value.key, value.name, value.value)
}

private fun registryDefault(key: String): String? =
  if (Advapi32Util.registryValueExists(HKEY_CURRENT_USER, key, REGISTRY_DEFAULT_VALUE)) {
    Advapi32Util.registryGetStringValue(HKEY_CURRENT_USER, key, REGISTRY_DEFAULT_VALUE)
  } else {
    null
  }

// Per the desktop entry spec, quoting escapes " ` $ \ and the string escape then doubles every backslash.
// xdg-open 1.1.3 cannot run a quoted program, so quotes are added only when required.
internal fun desktopExecArgument(arg: String): String {
  if (arg.none { it in EXEC_RESERVED_CHARACTERS }) return arg
  val quoted = arg.replace("\\", "\\\\").replace("\"", "\\\"").replace("`", "\\`").replace("$", "\\$")
  return "\"" + quoted.replace("\\", "\\\\") + "\""
}

internal fun appImageDesktopEntry(appImagePath: String): String? {
  // A line break would end the Exec value and start a key of the path's choosing.
  // GLib looks the program up before it expands %%, so no escaping lets it find a path containing %.
  if (appImagePath.any { it == '\n' || it == '\r' || it == '%' }) return null
  return """
    |[Desktop Entry]
    |Type=Application
    |Name=$APP_DISPLAY_NAME
    |NoDisplay=true
    |Exec=${desktopExecArgument(appImagePath)} %u
    |MimeType=$SCHEME_MIME_TYPE;
    |""".trimMargin()
}

// The entry runs this file for every link, so neither it nor any directory above it may be replaceable by another user,
// including the user's own directory inside /tmp, which can be cleaned away and recreated by anyone.
internal fun onlyOwnerCanReplace(path: Path, user: String): Boolean =
  try {
    generateSequence(path) { it.parent }.all { onlyOwnerCanWrite(Files.readAttributes(it, PosixFileAttributes::class.java), user) }
  } catch (_: IOException) {
    false
  }

// A group named after the user is a user private group, which umask 002 systems make group-writable.
internal fun onlyOwnerCanWrite(attributes: PosixFileAttributes, user: String): Boolean {
  val permissions = attributes.permissions()
  return attributes.owner().name in setOf(user, ROOT_USER) &&
    PosixFilePermission.OTHERS_WRITE !in permissions &&
    (PosixFilePermission.GROUP_WRITE !in permissions || attributes.group().name == user)
}

internal fun registerAppImageScheme(appImagePath: String, applicationsDir: File, run: (List<String>) -> String?): Boolean {
  if (!onlyOwnerCanReplace(Path.of(appImagePath), System.getProperty("user.name"))) {
    Log.w(TAG, "app link scheme: not registered, as other users may replace $appImagePath")
    return false
  }
  val content = appImageDesktopEntry(appImagePath) ?: return false
  val entry = File(applicationsDir, APPIMAGE_ENTRY_NAME)
  try {
    if (!entry.isFile || entry.readText() != content) {
      applicationsDir.mkdirs()
      entry.writeText(content)
      run(listOf("update-desktop-database", applicationsDir.path))
    }
  } catch (e: IOException) {
    Log.w(TAG, "app link scheme: cannot write desktop entry: ${e.message}")
    return false
  }
  if (xdgDefaultSchemeHandler(run) == APPIMAGE_ENTRY_NAME) return true
  run(listOf(XDG_MIME, "default", APPIMAGE_ENTRY_NAME, SCHEME_MIME_TYPE))
  return xdgDefaultSchemeHandler(run) == APPIMAGE_ENTRY_NAME
}

private fun xdgDefaultSchemeHandler(run: (List<String>) -> String?): String? =
  run(listOf(XDG_MIME, "query", "default", SCHEME_MIME_TYPE))?.trim()

internal fun linuxPackageRegistered(run: (List<String>) -> String?): Boolean =
  xdgDefaultSchemeHandler(run) == DEB_ENTRY_NAME

internal fun runProcess(command: List<String>): String? {
  val process = try {
    ProcessBuilder(command).redirectError(ProcessBuilder.Redirect.DISCARD).start()
  } catch (e: IOException) {
    Log.w(TAG, "app link scheme: cannot run ${command.first()}: ${e.message}")
    return null
  }
  return try {
    process.outputStream.close()
    if (!process.waitFor(PROCESS_TIMEOUT_SECONDS, TimeUnit.SECONDS)) {
      process.destroyForcibly()
      Log.w(TAG, "app link scheme: ${command.first()} timed out")
      null
    } else if (process.exitValue() == 0) {
      process.inputStream.bufferedReader().readText()
    } else {
      null
    }
  } catch (e: IOException) {
    Log.w(TAG, "app link scheme: ${command.first()} failed: ${e.message}")
    null
  } catch (_: InterruptedException) {
    process.destroyForcibly()
    Thread.currentThread().interrupt()
    null
  } finally {
    process.inputStream.close()
  }
}
