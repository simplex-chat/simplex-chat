package chat.simplex.common.platform

import chat.simplex.common.views.chatlist.appLinkScheme
import com.sun.jna.platform.win32.Advapi32Util
import com.sun.jna.platform.win32.Win32Exception
import com.sun.jna.platform.win32.WinReg.HKEY_CURRENT_USER
import java.io.File
import java.io.IOException
import java.util.concurrent.TimeUnit
import kotlin.concurrent.thread

private const val SCHEME_MIME_TYPE = "x-scheme-handler/$appLinkScheme"
private const val WINDOWS_SCHEME_KEY = "Software\\Classes\\$appLinkScheme"
private const val WINDOWS_COMMAND_KEY = "$WINDOWS_SCHEME_KEY\\shell\\open\\command"
private const val WINDOWS_ICON_KEY = "$WINDOWS_SCHEME_KEY\\DefaultIcon"
private const val APPIMAGE_ENTRY_NAME = "chat.simplex.app-links.desktop"
private const val PROCESS_TIMEOUT_SECONDS = 5L

@Volatile
private var registered = false

actual fun appLinkSchemeRegistered(): Boolean = registered

// Registration spawns processes and touches the registry, so it runs off the startup path.
fun registerAppLinkScheme() {
  thread(name = "simplex-app-link-scheme", isDaemon = true) {
    registered = registerForThisInstallation()
  }
}

// Only a packaged app has a stable path to register; jpackage's launcher sets jpackage.app-path.
private fun registerForThisInstallation(): Boolean {
  val appPath = System.getProperty("jpackage.app-path")
  val appImagePath = System.getenv("APPIMAGE")
  return when {
    desktopPlatform.isMac() -> appPath != null
    desktopPlatform.isWindows() -> appPath != null && registerWindowsScheme(appPath)
    System.getenv("FLATPAK_ID") != null -> true
    appImagePath != null -> registerAppImageScheme(appImagePath)
    else -> appPath != null && xdgDefaultSchemeHandler() != null
  }
}

fun windowsOpenCommand(appPath: String): String = "\"$appPath\" \"%1\""

// HKCU needs no elevation and shadows HKLM; rewritten when an upgrade or another copy moved the app.
private fun registerWindowsScheme(appPath: String): Boolean {
  val command = windowsOpenCommand(appPath)
  return try {
    if (registryDefault(WINDOWS_COMMAND_KEY) != command) {
      Advapi32Util.registryCreateKey(HKEY_CURRENT_USER, WINDOWS_COMMAND_KEY)
      Advapi32Util.registryCreateKey(HKEY_CURRENT_USER, WINDOWS_ICON_KEY)
      Advapi32Util.registrySetStringValue(HKEY_CURRENT_USER, WINDOWS_SCHEME_KEY, "", "URL:SimpleX Chat")
      // without this value Windows does not treat the key as a URL scheme
      Advapi32Util.registrySetStringValue(HKEY_CURRENT_USER, WINDOWS_SCHEME_KEY, "URL Protocol", "")
      Advapi32Util.registrySetStringValue(HKEY_CURRENT_USER, WINDOWS_ICON_KEY, "", "\"$appPath\",0")
      Advapi32Util.registrySetStringValue(HKEY_CURRENT_USER, WINDOWS_COMMAND_KEY, "", command)
    }
    registryDefault(WINDOWS_COMMAND_KEY) == command
  } catch (e: Win32Exception) {
    Log.w(TAG, "app link scheme: cannot register in the registry: ${e.message}")
    false
  }
}

private fun registryDefault(key: String): String? =
  if (Advapi32Util.registryKeyExists(HKEY_CURRENT_USER, key) && Advapi32Util.registryValueExists(HKEY_CURRENT_USER, key, "")) {
    Advapi32Util.registryGetStringValue(HKEY_CURRENT_USER, key, "")
  } else {
    null
  }

// Desktop entry spec: quoting escapes " ` $ \, then the string escape doubles every backslash again,
// and % is doubled so it is not read as a field code.
fun desktopExecArgument(arg: String): String {
  val quoted = arg.replace("\\", "\\\\").replace("\"", "\\\"").replace("`", "\\`").replace("$", "\\$")
  return "\"" + quoted.replace("\\", "\\\\").replace("%", "%%") + "\""
}

fun appImageDesktopEntry(appImagePath: String): String =
  """
  |[Desktop Entry]
  |Type=Application
  |Name=SimpleX Chat
  |NoDisplay=true
  |Exec=${desktopExecArgument(appImagePath)} %u
  |MimeType=$SCHEME_MIME_TYPE;
  |""".trimMargin()

// An AppImage is never installed, so it registers itself; the entry is rewritten when the file moved.
private fun registerAppImageScheme(appImagePath: String): Boolean {
  // a line break would end the Exec value and start a key of the path's choosing
  if (appImagePath.any { it == '\n' || it == '\r' }) return false
  val applicationsDir = File(unixDataHome, "applications")
  val entry = File(applicationsDir, APPIMAGE_ENTRY_NAME)
  val content = appImageDesktopEntry(appImagePath)
  try {
    if (!entry.isFile || entry.readText() != content) {
      applicationsDir.mkdirs()
      entry.writeText(content)
      runProcess("update-desktop-database", applicationsDir.path)
    }
  } catch (e: IOException) {
    Log.w(TAG, "app link scheme: cannot write desktop entry: ${e.message}")
    return false
  }
  if (xdgDefaultSchemeHandler() != APPIMAGE_ENTRY_NAME) {
    runProcess("xdg-mime", "default", APPIMAGE_ENTRY_NAME, SCHEME_MIME_TYPE)
  }
  return xdgDefaultSchemeHandler() == APPIMAGE_ENTRY_NAME
}

private fun xdgDefaultSchemeHandler(): String? =
  runProcess("xdg-mime", "query", "default", SCHEME_MIME_TYPE)?.trim()?.ifEmpty { null }

// No shell: every argument reaches the program as is. Returns stdout of a successful run.
private fun runProcess(vararg command: String): String? {
  val process = try {
    ProcessBuilder(*command).redirectError(ProcessBuilder.Redirect.DISCARD).start()
  } catch (e: IOException) {
    Log.w(TAG, "app link scheme: cannot run ${command.first()}: ${e.message}")
    return null
  }
  return try {
    val finished = process.waitFor(PROCESS_TIMEOUT_SECONDS, TimeUnit.SECONDS)
    if (!finished) process.destroyForcibly()
    if (finished && process.exitValue() == 0) process.inputStream.bufferedReader().readText() else null
  } catch (e: IOException) {
    Log.w(TAG, "app link scheme: cannot read ${command.first()} output: ${e.message}")
    null
  } catch (e: InterruptedException) {
    process.destroyForcibly()
    Thread.currentThread().interrupt()
    null
  } finally {
    process.inputStream.close()
  }
}
