package chat.simplex.common.platform

import chat.simplex.common.views.chatlist.appLinkScheme
import chat.simplex.common.views.chatlist.connectionLinkScheme
import com.sun.jna.NativeLibrary
import com.sun.jna.WString
import com.sun.jna.platform.win32.Advapi32Util
import com.sun.jna.platform.win32.Win32Exception
import com.sun.jna.platform.win32.WinReg.HKEY
import com.sun.jna.platform.win32.WinReg.HKEY_CURRENT_USER
import com.sun.jna.platform.win32.WinReg.HKEY_LOCAL_MACHINE
import com.sun.jna.ptr.IntByReference
import java.io.File
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.PosixFileAttributes
import java.nio.file.attribute.PosixFilePermission
import java.util.concurrent.TimeUnit
import kotlin.concurrent.thread

internal const val APP_LINK_MIME_TYPE = "x-scheme-handler/$appLinkScheme"
internal const val CONNECTION_LINK_MIME_TYPE = "x-scheme-handler/$connectionLinkScheme"
private const val WINDOWS_CLASSES_KEY = "Software\\Classes"
private const val WINDOWS_PROG_ID_PREFIX = "SimpleX."
private const val WINDOWS_CAPABILITIES_KEY = "Software\\chat.simplex.app\\Capabilities"
private const val WINDOWS_REGISTERED_APPLICATIONS_KEY = "Software\\RegisteredApplications"
private const val APP_DESCRIPTION = "Private and secure open-source messenger - no user IDs (not even random numbers)"
private const val WINDOWS_FIRST_ICON_INDEX = 0
// values from shlwapi.h and shlobj_core.h
private const val ASSOCF_IS_PROTOCOL = 0x1000
private const val ASSOCSTR_EXECUTABLE = 2
private const val S_OK = 0
private const val S_FALSE = 1
private const val SHCNE_ASSOCCHANGED = 0x08000000
private const val SHCNF_IDLIST = 0
// the Win32 name of a key's default value
private const val REGISTRY_DEFAULT_VALUE = ""
internal const val APPIMAGE_ENTRY_NAME = "chat.simplex.app-links.desktop"
private const val APPLICATIONS_DIR = "applications"
// the XDG Base Directory default when XDG_DATA_DIRS is unset
private const val DEFAULT_XDG_DATA_DIRS = "/usr/local/share:/usr/share"
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

// Only simplexchat: decides how the badge page ends, so simplex: is registered without being checked here.
@Volatile
private var registered = false

actual fun appLinkSchemeRegistered(): Boolean = registered

fun registerLinkSchemes() {
  thread(name = "simplex-link-schemes", isDaemon = true) {
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
    // The bundle's Info.plist and the Flatpak's exported desktop entry register the schemes.
    DesktopInstallation.MacBundle, DesktopInstallation.Flatpak -> true
    is DesktopInstallation.WindowsExe -> registerWindowsSchemes(installation.path, SystemWindowsShell)
    is DesktopInstallation.AppImage ->
      registerAppImageScheme(installation.path, File(unixDataHome, APPLICATIONS_DIR), systemApplicationsDirs(), ::runProcess)
    DesktopInstallation.LinuxPackage -> linuxPackageRegistered(::runProcess)
    DesktopInstallation.Unpackaged -> false
  }

internal fun windowsOpenCommand(appPath: String): String = "\"$appPath\" \"%1\""

internal fun windowsCommandProgram(command: String): String =
  if (command.startsWith("\"")) command.substring(1).substringBefore('"') else command.substringBefore(' ')

internal data class RegistryValue(val key: String, val name: String, val value: String)

private fun windowsSchemeKey(scheme: String): String = "$WINDOWS_CLASSES_KEY\\$scheme"

private fun windowsCommandKey(scheme: String): String = "${windowsSchemeKey(scheme)}\\shell\\open\\command"

private fun windowsProgId(scheme: String): String = "$WINDOWS_PROG_ID_PREFIX$scheme"

private fun windowsProgIdKey(scheme: String): String = "$WINDOWS_CLASSES_KEY\\${windowsProgId(scheme)}"

private fun windowsProgIdCommandKey(scheme: String): String = "${windowsProgIdKey(scheme)}\\shell\\open\\command"

private fun windowsIcon(appPath: String): String = "\"$appPath\",$WINDOWS_FIRST_ICON_INDEX"

// The scheme key declares the URL scheme, but its command opens links only while no app registered through
// Capabilities claims the scheme: Windows settings offer only the ProgIds that Capabilities name.
internal fun windowsSchemeValues(scheme: String, appPath: String): List<RegistryValue> {
  val schemeKey = windowsSchemeKey(scheme)
  return listOf(
    RegistryValue(key = schemeKey, name = REGISTRY_DEFAULT_VALUE, value = "URL:$APP_DISPLAY_NAME"),
    // without this value Windows does not treat the key as a URL scheme
    RegistryValue(key = schemeKey, name = "URL Protocol", value = ""),
    RegistryValue(key = "$schemeKey\\DefaultIcon", name = REGISTRY_DEFAULT_VALUE, value = windowsIcon(appPath)),
    RegistryValue(key = windowsCommandKey(scheme), name = REGISTRY_DEFAULT_VALUE, value = windowsOpenCommand(appPath)),
    RegistryValue(key = "$WINDOWS_CAPABILITIES_KEY\\URLAssociations", name = scheme, value = windowsProgId(scheme)),
  )
}

// The ProgId is what links resolve to once the user picks SimpleX in settings, so it is repaired even while
// another program holds the scheme key, but another installed copy's ProgId is kept like any other handler.
internal fun windowsProgIdValues(scheme: String, appPath: String): List<RegistryValue> {
  val progIdKey = windowsProgIdKey(scheme)
  return listOf(
    RegistryValue(key = progIdKey, name = REGISTRY_DEFAULT_VALUE, value = "URL:$APP_DISPLAY_NAME"),
    RegistryValue(key = "$progIdKey\\DefaultIcon", name = REGISTRY_DEFAULT_VALUE, value = windowsIcon(appPath)),
    RegistryValue(key = windowsProgIdCommandKey(scheme), name = REGISTRY_DEFAULT_VALUE, value = windowsOpenCommand(appPath)),
  )
}

// The application is listed last, so Windows never lists it before its capabilities are complete.
internal val windowsApplicationValues: List<RegistryValue> = listOf(
  RegistryValue(key = WINDOWS_CAPABILITIES_KEY, name = "ApplicationName", value = APP_DISPLAY_NAME),
  RegistryValue(key = WINDOWS_CAPABILITIES_KEY, name = "ApplicationDescription", value = APP_DESCRIPTION),
  RegistryValue(key = WINDOWS_REGISTERED_APPLICATIONS_KEY, name = APP_DISPLAY_NAME, value = WINDOWS_CAPABILITIES_KEY),
)

internal interface WindowsShell {
  fun readUser(key: String, name: String): String?
  fun readMachine(key: String, name: String): String?
  fun writeUser(value: RegistryValue)
  // the program the shell starts for a link, after any choice made in Windows settings
  fun linkHandler(scheme: String): String?
  fun notifyAssociationsChanged()
}

internal fun registerWindowsSchemes(appPath: String, shell: WindowsShell): Boolean {
  var changed = false
  val tracked = object : WindowsShell by shell {
    override fun writeUser(value: RegistryValue) {
      shell.writeUser(value)
      changed = true
    }
  }
  val associated = listOf(appLinkScheme, connectionLinkScheme).filter { registerWindowsScheme(it, appPath, tracked) }
  if (associated.isNotEmpty()) writeMissing(windowsApplicationValues, tracked, "the application")
  // The notification also clears Explorer's icon cache, so it is sent only after a change.
  if (changed) shell.notifyAssociationsChanged()
  return shell.linkHandler(appLinkScheme)?.let { isThisProgram(it, appPath) } == true
}

private fun registerWindowsScheme(scheme: String, appPath: String, shell: WindowsShell): Boolean =
  !heldByAnotherProgram(windowsProgIdCommandKey(scheme), appPath, shell) &&
    writeMissing(windowsProgIdValues(scheme, appPath), shell, "the $scheme: ProgId") &&
    !heldByAnotherProgram(windowsCommandKey(scheme), appPath, shell) &&
    writeMissing(windowsSchemeValues(scheme, appPath), shell, "the $scheme: scheme")

private fun isThisProgram(program: String, appPath: String): Boolean =
  File(program).absolutePath.equals(File(appPath).absolutePath, ignoreCase = true)

// A per-user key hides the machine-wide one, so the machine-wide command counts only without it.
private fun heldByAnotherProgram(commandKey: String, appPath: String, shell: WindowsShell): Boolean =
  try {
    val current = shell.readUser(commandKey, REGISTRY_DEFAULT_VALUE) ?: shell.readMachine(commandKey, REGISTRY_DEFAULT_VALUE)
    val program = current?.let(::windowsCommandProgram)
    program != null && !isThisProgram(program, appPath) && File(program).isFile
  } catch (e: Win32Exception) {
    Log.w(TAG, "link scheme: cannot read the handler under $commandKey: ${e.message}")
    true
  }

private fun writeMissing(values: List<RegistryValue>, shell: WindowsShell, what: String): Boolean {
  fun stored() = values.all { shell.readUser(it.key, it.name) == it.value }
  return try {
    if (!stored()) values.forEach(shell::writeUser)
    stored()
  } catch (e: Win32Exception) {
    Log.w(TAG, "link scheme: cannot register $what in the registry: ${e.message}")
    false
  }
}

private object SystemWindowsShell : WindowsShell {
  override fun readUser(key: String, name: String): String? = registryString(HKEY_CURRENT_USER, key, name)

  override fun readMachine(key: String, name: String): String? = registryString(HKEY_LOCAL_MACHINE, key, name)

  override fun writeUser(value: RegistryValue) {
    Advapi32Util.registryCreateKey(HKEY_CURRENT_USER, value.key)
    Advapi32Util.registrySetStringValue(HKEY_CURRENT_USER, value.key, value.name, value.value)
  }

  // jna-platform 5.14 binds neither AssocQueryStringW nor SHChangeNotify.
  override fun linkHandler(scheme: String): String? =
    try {
      val assocQueryString = NativeLibrary.getInstance("shlwapi").getFunction("AssocQueryStringW")
      val length = IntByReference(0)
      // with no buffer the call reports the length it needs
      val sized = assocQueryString.invokeInt(arrayOf(ASSOCF_IS_PROTOCOL, ASSOCSTR_EXECUTABLE, WString(scheme), null, null, length))
      if (sized != S_FALSE) {
        null
      } else {
        val out = CharArray(length.value)
        val result = assocQueryString.invokeInt(arrayOf(ASSOCF_IS_PROTOCOL, ASSOCSTR_EXECUTABLE, WString(scheme), null, out, length))
        if (result == S_OK) String(out).substringBefore('\u0000') else null
      }
    } catch (e: UnsatisfiedLinkError) {
      Log.w(TAG, "link scheme: cannot query the $scheme: handler: ${e.message}")
      null
    }

  override fun notifyAssociationsChanged() {
    try {
      NativeLibrary.getInstance("shell32").getFunction("SHChangeNotify").invokeVoid(arrayOf(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, null, null))
    } catch (e: UnsatisfiedLinkError) {
      Log.w(TAG, "link scheme: cannot announce changed associations: ${e.message}")
    }
  }
}

// registryGetStringValue throws a RuntimeException for a value of another type, which another program could write.
private fun registryString(hive: HKEY, key: String, name: String): String? =
  if (Advapi32Util.registryValueExists(hive, key, name)) {
    Advapi32Util.registryGetValue(hive, key, name) as? String
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
    |MimeType=$APP_LINK_MIME_TYPE;$CONNECTION_LINK_MIME_TYPE;
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

internal fun registerAppImageScheme(appImagePath: String, applicationsDir: File, systemApplicationsDirs: List<File>, run: (List<String>) -> String?): Boolean {
  if (!onlyOwnerCanReplace(Path.of(appImagePath), System.getProperty("user.name"))) {
    Log.w(TAG, "link scheme: not registered, as other users may replace $appImagePath")
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
    Log.w(TAG, "link scheme: cannot write desktop entry: ${e.message}")
    return false
  }
  val entryDirs = listOf(applicationsDir) + systemApplicationsDirs
  val registered = makeAppImageEntryDefault(APP_LINK_MIME_TYPE, entryDirs, run)
  makeAppImageEntryDefault(CONNECTION_LINK_MIME_TYPE, entryDirs, run)
  return registered
}

private fun makeAppImageEntryDefault(mimeType: String, entryDirs: List<File>, run: (List<String>) -> String?): Boolean {
  val current = xdgDefaultSchemeHandler(mimeType, run)
  if (current == APPIMAGE_ENTRY_NAME) return true
  // another installed handler keeps the default; only a missing one is replaced
  if (current != null && entryDirs.any { File(it, current).isFile }) return false
  run(listOf(XDG_MIME, "default", APPIMAGE_ENTRY_NAME, mimeType))
  return xdgDefaultSchemeHandler(mimeType, run) == APPIMAGE_ENTRY_NAME
}

private fun xdgDefaultSchemeHandler(mimeType: String, run: (List<String>) -> String?): String? =
  run(listOf(XDG_MIME, "query", "default", mimeType))?.trim()?.takeIf { it.isNotEmpty() }

private fun systemApplicationsDirs(): List<File> =
  (System.getenv("XDG_DATA_DIRS")?.takeIf { it.isNotEmpty() } ?: DEFAULT_XDG_DATA_DIRS).split(File.pathSeparator).map { File(it, APPLICATIONS_DIR) }

internal fun linuxPackageRegistered(run: (List<String>) -> String?): Boolean =
  xdgDefaultSchemeHandler(APP_LINK_MIME_TYPE, run) == DEB_ENTRY_NAME

internal fun runProcess(command: List<String>): String? {
  val process = try {
    ProcessBuilder(command).redirectError(ProcessBuilder.Redirect.DISCARD).start()
  } catch (e: IOException) {
    Log.w(TAG, "link scheme: cannot run ${command.first()}: ${e.message}")
    return null
  }
  return try {
    process.outputStream.close()
    if (!process.waitFor(PROCESS_TIMEOUT_SECONDS, TimeUnit.SECONDS)) {
      process.destroyForcibly()
      Log.w(TAG, "link scheme: ${command.first()} timed out")
      null
    } else if (process.exitValue() == 0) {
      process.inputStream.bufferedReader().readText()
    } else {
      null
    }
  } catch (e: IOException) {
    Log.w(TAG, "link scheme: ${command.first()} failed: ${e.message}")
    null
  } catch (_: InterruptedException) {
    process.destroyForcibly()
    Thread.currentThread().interrupt()
    null
  } finally {
    process.inputStream.close()
  }
}
