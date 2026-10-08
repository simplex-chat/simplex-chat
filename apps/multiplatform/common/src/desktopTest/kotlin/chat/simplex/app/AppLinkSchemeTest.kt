package chat.simplex.app

import chat.simplex.common.platform.APPIMAGE_ENTRY_NAME
import chat.simplex.common.platform.DesktopInstallation
import chat.simplex.common.platform.DesktopPlatform
import chat.simplex.common.platform.RegistryValue
import chat.simplex.common.platform.APP_LINK_MIME_TYPE
import chat.simplex.common.platform.CONNECTION_LINK_MIME_TYPE
import chat.simplex.common.platform.appImageDesktopEntry
import chat.simplex.common.platform.desktopExecArgument
import chat.simplex.common.platform.desktopInstallation
import chat.simplex.common.platform.linuxPackageRegistered
import chat.simplex.common.platform.onlyOwnerCanReplace
import chat.simplex.common.platform.onlyOwnerCanWrite
import chat.simplex.common.platform.registerAppImageScheme
import chat.simplex.common.platform.registerWindowsSchemes
import chat.simplex.common.platform.runProcess
import chat.simplex.common.platform.windowsCommandProgram
import chat.simplex.common.platform.windowsApplicationValues
import chat.simplex.common.platform.windowsOpenCommand
import chat.simplex.common.platform.WindowsShell
import chat.simplex.common.platform.windowsProgIdValues
import chat.simplex.common.platform.windowsSchemeValues
import com.sun.jna.platform.win32.Win32Exception
import com.sun.jna.platform.win32.WinError
import com.sun.jna.platform.win32.WinNT
import org.junit.Assume.assumeNoException
import org.junit.Assume.assumeTrue
import java.io.File
import java.lang.reflect.Proxy
import java.nio.file.FileSystemException
import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.GroupPrincipal
import java.nio.file.attribute.PosixFileAttributes
import java.nio.file.attribute.PosixFilePermission
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.attribute.UserPrincipal
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

private const val OWNER_WRITABLE = "rwxr-xr-x"
private const val GROUP_WRITABLE = "rwxrwxr-x"
private const val OTHERS_WRITABLE = "rwxr-xrwx"
private const val WORLD_WRITABLE = "rwxrwxrwx"
private const val APPIMAGE_FILE_NAME = "SimpleX.AppImage"
private const val XDG_MIME = "xdg-mime"

class AppLinkSchemeTest {
  private val windowsExe = "C:\\Program Files\\SimpleX\\SimpleX.exe"
  private val appLinkCommandKey = "Software\\Classes\\simplexchat\\shell\\open\\command"
  private val connectionCommandKey = "Software\\Classes\\simplex\\shell\\open\\command"
  private val appLinkCommand = appLinkCommandKey to ""
  private val connectionCommand = connectionCommandKey to ""
  private val olderWindowsCommand = "\"C:\\Old\\SimpleX.exe\" \"%1\""
  private val appImagePath = "/home/user/$APPIMAGE_FILE_NAME"

  @Test
  fun windowsCommandQuotesTheAppAndTheLink() {
    assertEquals(
      "\"C:\\Program Files\\SimpleX\\SimpleX.exe\" \"%1\"",
      windowsOpenCommand(windowsExe)
    )
  }

  @Test
  fun windowsSchemeIsAUrlProtocolNamingItsProgId() {
    assertEquals(
      listOf(
        RegistryValue(key = "Software\\Classes\\simplexchat", name = "", value = "URL:SimpleX Chat"),
        RegistryValue(key = "Software\\Classes\\simplexchat", name = "URL Protocol", value = ""),
        RegistryValue(key = "Software\\Classes\\simplexchat\\DefaultIcon", name = "", value = "\"$windowsExe\",0"),
        RegistryValue(key = appLinkCommandKey, name = "", value = "\"$windowsExe\" \"%1\""),
        RegistryValue(key = "Software\\chat.simplex.app\\Capabilities\\URLAssociations", name = "simplexchat", value = "SimpleX.simplexchat"),
      ),
      windowsSchemeValues("simplexchat", windowsExe),
      "app link scheme"
    )
    assertEquals(
      listOf(
        RegistryValue(key = "Software\\Classes\\simplex", name = "", value = "URL:SimpleX Chat"),
        RegistryValue(key = "Software\\Classes\\simplex", name = "URL Protocol", value = ""),
        RegistryValue(key = "Software\\Classes\\simplex\\DefaultIcon", name = "", value = "\"$windowsExe\",0"),
        RegistryValue(key = connectionCommandKey, name = "", value = "\"$windowsExe\" \"%1\""),
        RegistryValue(key = "Software\\chat.simplex.app\\Capabilities\\URLAssociations", name = "simplex", value = "SimpleX.simplex"),
      ),
      windowsSchemeValues("simplex", windowsExe),
      "connection link scheme"
    )
  }

  @Test
  fun windowsProgIdOpensTheExe() {
    assertEquals(
      listOf(
        RegistryValue(key = "Software\\Classes\\SimpleX.simplexchat", name = "", value = "URL:SimpleX Chat"),
        RegistryValue(key = "Software\\Classes\\SimpleX.simplexchat\\DefaultIcon", name = "", value = "\"$windowsExe\",0"),
        RegistryValue(key = "Software\\Classes\\SimpleX.simplexchat\\shell\\open\\command", name = "", value = "\"$windowsExe\" \"%1\""),
      ),
      windowsProgIdValues("simplexchat", windowsExe)
    )
  }

  @Test
  fun windowsApplicationIsListedInDefaultAppsAfterItsCapabilities() {
    assertEquals(
      listOf(
        RegistryValue(key = "Software\\chat.simplex.app\\Capabilities", name = "ApplicationName", value = "SimpleX Chat"),
        RegistryValue(
          key = "Software\\chat.simplex.app\\Capabilities",
          name = "ApplicationDescription",
          value = "Private and secure open-source messenger - no user IDs (not even random numbers)"
        ),
        RegistryValue(key = "Software\\RegisteredApplications", name = "SimpleX Chat", value = "Software\\chat.simplex.app\\Capabilities"),
      ),
      windowsApplicationValues
    )
  }

  @Test
  fun windowsRegistrationWritesEveryValueWhenTheCommandsPointAtAMissingProgram() {
    val shell = FakeWindowsShell(user = mutableMapOf(appLinkCommand to olderWindowsCommand, connectionCommand to olderWindowsCommand))
    registerWindowsSchemes(windowsExe, shell)
    assertEquals(fullRegistration(), shell.writes)
    assertTrue(shell.notified, "Windows is told that associations changed")
  }

  @Test
  fun windowsCommandProgramIsTheQuotedOrFirstArgument() {
    assertEquals("C:\\Program Files\\Other\\Other.exe", windowsCommandProgram("\"C:\\Program Files\\Other\\Other.exe\" \"%1\""), "quoted")
    assertEquals("C:\\Other\\Other.exe", windowsCommandProgram("C:\\Other\\Other.exe %1"), "unquoted")
    assertEquals("C:\\Other\\Other.exe", windowsCommandProgram("C:\\Other\\Other.exe"), "no arguments")
  }

  @Test
  fun windowsRegistrationLeavesAFullRegistrationAlone() {
    val shell = FakeWindowsShell(user = registered())
    registerWindowsSchemes(windowsExe, shell)
    assertEquals(listOf(), shell.writes, "nothing is written again")
    assertFalse(shell.notified, "an unchanged registration does not clear Explorer's caches")
  }

  @Test
  fun windowsRegistrationRepairsAValueMissingBesideAMatchingCommand() {
    val shell = FakeWindowsShell(user = registered().apply { remove("Software\\Classes\\simplexchat" to "URL Protocol") })
    registerWindowsSchemes(windowsExe, shell)
    assertEquals(windowsSchemeValues("simplexchat", windowsExe), shell.writes, "only the incomplete scheme is rewritten")
  }

  @Test
  fun windowsRegistrationAnnouncesARepairedApplicationListing() {
    val shell = FakeWindowsShell(user = registered().apply { remove("Software\\RegisteredApplications" to "SimpleX Chat") })
    registerWindowsSchemes(windowsExe, shell)
    assertEquals(windowsApplicationValues, shell.writes, "only the application listing is rewritten")
    assertTrue(shell.notified, "Windows is told that associations changed")
  }

  @Test
  fun windowsRegistrationKeepsAnotherInstalledHandler() = withTempDir { dir ->
    val otherCommand = "\"${Files.createFile(dir.resolve("Other.exe"))}\" \"%1\""
    val shell = FakeWindowsShell(user = mutableMapOf(appLinkCommand to otherCommand, connectionCommand to otherCommand))
    registerWindowsSchemes(windowsExe, shell)
    assertEquals(
      windowsProgIdValues("simplexchat", windowsExe) + windowsProgIdValues("simplex", windowsExe),
      shell.writes,
      "only this app's own ProgIds are written; no scheme is taken and no application is listed without one"
    )
  }

  @Test
  fun windowsRegistrationKeepsAMachineWideHandler() = withTempDir { dir ->
    val otherCommand = "\"${Files.createFile(dir.resolve("Other.exe"))}\" \"%1\""
    val shell = FakeWindowsShell(machine = mutableMapOf(appLinkCommand to otherCommand))
    registerWindowsSchemes(windowsExe, shell)
    assertTrue(shell.writes.none { it.key == appLinkCommandKey }, "a per-user key would hide the machine-wide handler")
    assertTrue(windowsSchemeValues("simplex", windowsExe).all { it in shell.writes }, "the free scheme is taken")
  }

  @Test
  fun windowsRegistrationRepairsItsProgIdWhileAnotherProgramHoldsTheScheme() = withTempDir { dir ->
    val otherCommand = "\"${Files.createFile(dir.resolve("Other.exe"))}\" \"%1\""
    val staleProgId = windowsProgIdValues("simplexchat", "C:\\Old\\SimpleX.exe").associate { (it.key to it.name) to it.value }
    val shell = FakeWindowsShell(user = (staleProgId + (appLinkCommand to otherCommand)).toMutableMap())
    registerWindowsSchemes(windowsExe, shell)
    assertEquals(windowsProgIdValues("simplexchat", windowsExe), shell.writes.take(3), "the ProgId points at this exe again")
  }

  @Test
  fun windowsRegistrationReportsTheProgramTheShellStarts() {
    assertFalse(registerWindowsSchemes(windowsExe, FakeWindowsShell(user = registered(), handler = "C:\\Other\\Other.exe")), "another app picked in settings")
    assertTrue(registerWindowsSchemes(windowsExe, FakeWindowsShell(user = registered(), handler = windowsExe.uppercase())), "paths differ only in case")
    assertFalse(registerWindowsSchemes(windowsExe, FakeWindowsShell(user = registered(), handler = null)), "the shell has no answer")
  }

  @Test
  fun windowsRegistrationContinuesPastARegistryError() {
    val shell = FakeWindowsShell(failingWrites = setOf("Software\\Classes\\simplexchat"))
    registerWindowsSchemes(windowsExe, shell)
    assertTrue(windowsSchemeValues("simplexchat", windowsExe).none { it in shell.writes }, "the app link scheme could not be written")
    assertTrue(windowsSchemeValues("simplex", windowsExe).all { it in shell.writes }, "the connection link scheme is still written")
    assertTrue(windowsApplicationValues.all { it in shell.writes }, "the application is listed for the scheme that is ours")
  }

  @Test
  fun windowsRegistrationTakesNoSchemeWhoseHandlerCannotBeRead() {
    val shell = FakeWindowsShell(failingReads = setOf(appLinkCommandKey, connectionCommandKey))
    registerWindowsSchemes(windowsExe, shell)
    assertEquals(
      windowsProgIdValues("simplexchat", windowsExe) + windowsProgIdValues("simplex", windowsExe),
      shell.writes,
      "an unreadable handler is left alone, and no application is listed without a scheme"
    )
  }

  @Test
  fun windowsRegistrationTakesNoSchemeWhoseMachineWideHandlerCannotBeRead() {
    val shell = FakeWindowsShell(failingMachineReads = setOf(appLinkCommandKey))
    registerWindowsSchemes(windowsExe, shell)
    assertTrue(windowsSchemeValues("simplexchat", windowsExe).none { it in shell.writes }, "the app link scheme is left alone")
    assertTrue(windowsSchemeValues("simplex", windowsExe).all { it in shell.writes }, "the readable scheme is taken")
  }

  @Test
  fun windowsRegistrationTakesNoSchemeWhoseProgIdCannotBeWritten() {
    val shell = FakeWindowsShell(failingWrites = setOf("Software\\Classes\\SimpleX.simplexchat"))
    registerWindowsSchemes(windowsExe, shell)
    assertTrue(windowsSchemeValues("simplexchat", windowsExe).none { it in shell.writes }, "URLAssociations must not name a missing ProgId")
  }

  @Test
  fun windowsRegistrationKeepsAnotherInstalledCopysProgId() = withTempDir { dir ->
    val otherCopy = Files.createFile(dir.resolve("SimpleX.exe")).toString()
    val otherProgId = windowsProgIdValues("simplexchat", otherCopy).associate { (it.key to it.name) to it.value }
    val shell = FakeWindowsShell(user = otherProgId.toMutableMap())
    registerWindowsSchemes(windowsExe, shell)
    assertTrue(shell.writes.none { it.key.contains("simplexchat") }, "links picked for the other copy keep reaching it: neither its ProgId nor the scheme is taken")
  }

  @Test
  fun windowsRegistrationTreatsItsOwnProgramAsItsOwnHandler() = withTempDir { dir ->
    val exe = Files.createFile(dir.resolve("SimpleX.exe")).toString()
    val otherExe = Files.createFile(dir.resolve("Other.exe")).toString()
    val shell = FakeWindowsShell(
      user = mutableMapOf(appLinkCommand to "\"$exe\" %1"),
      machine = mutableMapOf(appLinkCommand to "\"$otherExe\" \"%1\""),
    )
    registerWindowsSchemes(exe, shell)
    assertTrue(windowsSchemeValues("simplexchat", exe).all { it in shell.writes }, "a command for this exe is updated, and the hidden machine-wide one is ignored")
  }

  private fun fullRegistration(): List<RegistryValue> =
    windowsProgIdValues("simplexchat", windowsExe) + windowsSchemeValues("simplexchat", windowsExe) +
      windowsProgIdValues("simplex", windowsExe) + windowsSchemeValues("simplex", windowsExe) + windowsApplicationValues

  private fun registered(): MutableMap<Pair<String, String>, String> =
    fullRegistration().associate { (it.key to it.name) to it.value }.toMutableMap()

  private inner class FakeWindowsShell(
    private val user: MutableMap<Pair<String, String>, String> = mutableMapOf(),
    private val machine: Map<Pair<String, String>, String> = mapOf(),
    private val handler: String? = windowsExe,
    private val failingReads: Set<String> = setOf(),
    private val failingMachineReads: Set<String> = setOf(),
    private val failingWrites: Set<String> = setOf(),
  ) : WindowsShell {
    val writes = mutableListOf<RegistryValue>()
    var notified = false

    override fun readUser(key: String, name: String): String? {
      if (key in failingReads) throw AccessDenied()
      return user[key to name]
    }

    override fun readMachine(key: String, name: String): String? {
      if (key in failingMachineReads) throw AccessDenied()
      return machine[key to name]
    }

    override fun writeUser(value: RegistryValue) {
      if (value.key in failingWrites) throw AccessDenied()
      writes += value
      user[value.key to value.name] = value.value
    }

    override fun linkHandler(scheme: String): String? = handler

    override fun notifyAssociationsChanged() {
      notified = true
    }
  }

  // Win32Exception's public constructors format the message through kernel32, which only Windows has.
  private class AccessDenied : Win32Exception(WinError.ERROR_ACCESS_DENIED, WinNT.HRESULT(WinError.ERROR_ACCESS_DENIED), "access denied")

  @Test
  fun execArgumentOfAPlainPathIsLeftBare() {
    assertEquals("/home/user/Downloads/simplex-desktop-x86_64.AppImage", desktopExecArgument("/home/user/Downloads/simplex-desktop-x86_64.AppImage"))
  }

  @Test
  fun execArgumentIsQuotedForEveryReservedCharacter() {
    for (c in " \t\n\"'\\><~|&;$*?#()`") {
      val arg = desktopExecArgument("/a${c}b")
      assertTrue(arg.startsWith("\"") && arg.endsWith("\""), "reserved character ${c.code} must be quoted, got $arg")
    }
  }

  @Test
  fun execArgumentKeepsSpacesInsideTheQuotes() {
    assertEquals("\"/home/user/My Apps/simplex.AppImage\"", desktopExecArgument("/home/user/My Apps/simplex.AppImage"))
  }

  @Test
  fun execArgumentEscapesReservedCharacters() {
    assertEquals("\"/a\\\\\"b\"", desktopExecArgument("/a\"b"), "double quote")
    assertEquals("\"/a\\\\`b\"", desktopExecArgument("/a`b"), "backtick")
    assertEquals("\"/a\\\\\$b\"", desktopExecArgument("/a\$b"), "dollar")
  }

  @Test
  fun execArgumentTurnsABackslashIntoFour() {
    assertEquals("\"/a\\\\\\\\b\"", desktopExecArgument("/a\\b"))
  }

  @Test
  fun appImageEntryHandlesTheSchemeAndStaysOutOfMenus() {
    assertEquals(
      """
      |[Desktop Entry]
      |Type=Application
      |Name=SimpleX Chat
      |NoDisplay=true
      |Exec="/home/user/My Apps/simplex.AppImage" %u
      |MimeType=x-scheme-handler/simplexchat;x-scheme-handler/simplex;
      |""".trimMargin(),
      appImageDesktopEntry("/home/user/My Apps/simplex.AppImage")
    )
  }

  @Test
  fun appImageEntryIsRefusedForAPathItCannotRun() {
    assertNull(appImageDesktopEntry("/a\nExec=evil"), "line feed")
    assertNull(appImageDesktopEntry("/a\rb"), "carriage return")
    assertNull(appImageDesktopEntry("/a%1.AppImage"), "percent")
  }

  private val linux = DesktopPlatform.LINUX_X86_64
  private val linuxAppPath = "/opt/simplex/bin/simplex"

  @Test
  fun unpackagedRunHasNoInstallation() {
    assertEquals(DesktopInstallation.Unpackaged, desktopInstallation(linux, null, mapOf()), "linux")
    assertEquals(DesktopInstallation.Unpackaged, desktopInstallation(DesktopPlatform.MAC_AARCH64, null, mapOf()), "macOS")
    assertEquals(DesktopInstallation.Unpackaged, desktopInstallation(DesktopPlatform.WINDOWS_X86_64, null, mapOf()), "windows")
  }

  @Test
  fun packagedMacAndWindowsAreRecognised() {
    assertEquals(DesktopInstallation.MacBundle, desktopInstallation(DesktopPlatform.MAC_AARCH64, "/Applications/SimpleX.app/Contents/MacOS/SimpleX", mapOf()))
    assertEquals(DesktopInstallation.WindowsExe(windowsExe), desktopInstallation(DesktopPlatform.WINDOWS_X86_64, windowsExe, mapOf()))
  }

  @Test
  fun flatpakWinsWhenTheAppImageCheckWouldAlsoMatch() {
    val env = mapOf("container" to "flatpak", "APPIMAGE" to "/home/user/a.AppImage", "APPDIR" to "/tmp/.mount_a")
    assertEquals(DesktopInstallation.Flatpak, desktopInstallation(linux, "/tmp/.mount_a/usr/bin/simplex", env))
  }

  @Test
  fun appImageIsRecognisedWhenTheLauncherRunsFromItsMount() {
    val env = mapOf("APPIMAGE" to appImagePath, "APPDIR" to "/tmp/.mount_Simple")
    assertEquals(
      DesktopInstallation.AppImage(appImagePath),
      desktopInstallation(linux, "/tmp/.mount_Simple/usr/bin/simplex", env)
    )
  }

  @Test
  fun appImageIsRecognisedWhenItsMountIsReachedThroughASymlink() = withTempDir { tmp ->
    val mount = Files.createDirectories(tmp.resolve("real/.mount_Simple/usr/bin"))
    val linkedTmp = try {
      Files.createSymbolicLink(tmp.resolve("linked"), tmp.resolve("real"))
    } catch (e: FileSystemException) {
      assumeNoException("symbolic links cannot be created here", e)
      return@withTempDir
    }
    val env = mapOf("APPIMAGE" to appImagePath, "APPDIR" to linkedTmp.resolve(".mount_Simple").toString())
    assertEquals(
      DesktopInstallation.AppImage(appImagePath),
      desktopInstallation(linux, mount.toRealPath().resolve("simplex").toString(), env)
    )
  }

  @Test
  fun inheritedAppImageVariablesDoNotMakeTheDebAnAppImage() {
    val inherited = mapOf("APPIMAGE" to "/home/user/Terminal.AppImage", "APPDIR" to "/tmp/.mount_Termin")
    assertEquals(DesktopInstallation.LinuxPackage, desktopInstallation(linux, linuxAppPath, inherited), "another AppImage's mount")
    assertEquals(DesktopInstallation.LinuxPackage, desktopInstallation(linux, linuxAppPath, mapOf("APPIMAGE" to "/home/user/a.AppImage")), "no APPDIR")
    val prefix = mapOf("APPIMAGE" to "/home/user/a.AppImage", "APPDIR" to "/tmp/.mount_ab")
    assertEquals(DesktopInstallation.LinuxPackage, desktopInstallation(linux, "/tmp/.mount_abc/usr/bin/simplex", prefix), "mount path that only shares a prefix")
  }

  @Test
  fun fileOnlyItsOwnerCanWriteIsAccepted() {
    assertTrue(onlyOwnerCanWrite(attributes(owner = "alice", group = "staff", OWNER_WRITABLE), user = "alice"))
  }

  @Test
  fun rootOwnedFileIsAccepted() {
    assertTrue(onlyOwnerCanWrite(attributes(owner = "root", group = "root", OWNER_WRITABLE), user = "alice"))
  }

  @Test
  fun fileOfAnotherOwnerIsRefused() {
    assertFalse(onlyOwnerCanWrite(attributes(owner = "bob", group = "staff", OWNER_WRITABLE), user = "alice"))
  }

  @Test
  fun worldWritableFileIsRefused() {
    assertFalse(onlyOwnerCanWrite(attributes(owner = "alice", group = "alice", OTHERS_WRITABLE), user = "alice"))
  }

  @Test
  fun groupWritableFileIsRefusedForASharedGroup() {
    assertFalse(onlyOwnerCanWrite(attributes(owner = "alice", group = "staff", GROUP_WRITABLE), user = "alice"))
  }

  @Test
  fun groupWritableFileIsAcceptedForTheUsersOwnGroup() {
    assertTrue(onlyOwnerCanWrite(attributes(owner = "alice", group = "alice", GROUP_WRITABLE), user = "alice"), "owned by the user")
    assertTrue(onlyOwnerCanWrite(attributes(owner = "root", group = "alice", GROUP_WRITABLE), user = "alice"), "owned by root")
    assertFalse(onlyOwnerCanWrite(attributes(owner = "root", group = "root", GROUP_WRITABLE), user = "alice"), "root's own group")
  }

  private fun attributes(owner: String, group: String, permissions: String): PosixFileAttributes =
    Proxy.newProxyInstance(javaClass.classLoader, arrayOf(PosixFileAttributes::class.java)) { _, method, _ ->
      when (method.name) {
        "owner" -> UserPrincipal { owner }
        "group" -> GroupPrincipal { group }
        "permissions" -> PosixFilePermissions.fromString(permissions)
        else -> throw UnsupportedOperationException(method.name)
      }
    } as PosixFileAttributes

  @Test
  fun appImageOnlyItsOwnerCanWriteIsAccepted() = withAppImage {
    assertTrue(onlyOwnerCanReplace(it, Files.getOwner(it).name))
  }

  @Test
  fun appImageInAWorldWritableDirectoryIsRefused() = withAppImage(directoryPermissions = WORLD_WRITABLE) {
    assertFalse(onlyOwnerCanReplace(it, Files.getOwner(it).name), "the directory must be checked as well as the file")
  }

  @Test
  fun appImageUnderAWorldWritableAncestorIsRefused() = withAppImage(ancestorPermissions = WORLD_WRITABLE) {
    assertFalse(onlyOwnerCanReplace(it, Files.getOwner(it).name), "every directory up to the root must be checked")
  }

  @Test
  fun worldWritableAppImageIsRefused() = withAppImage(filePermissions = OTHERS_WRITABLE) {
    assertFalse(onlyOwnerCanReplace(it, Files.getOwner(it).name))
  }

  @Test
  fun appImageOfAnotherOwnerIsRefused() = withAppImage {
    val owner = Files.getOwner(it).name
    assumeTrue("root is accepted as an owner", owner != "root")
    assertFalse(onlyOwnerCanReplace(it, "$owner-other"), "a file owned by $owner checked for $owner-other")
  }

  @Test
  fun missingAppImageIsRefused() {
    assumePosix()
    assertFalse(onlyOwnerCanReplace(Path.of("/nonexistent-simplex-dir/SimpleX.AppImage"), "root"))
  }

  // Built under the module's build directory, as the check walks up to the root and the system temp directory is open to everyone.
  private fun withAppImage(
    directoryPermissions: String = OWNER_WRITABLE,
    filePermissions: String = OWNER_WRITABLE,
    ancestorPermissions: String = OWNER_WRITABLE,
    fileName: String = APPIMAGE_FILE_NAME,
    check: (Path) -> Unit
  ) {
    assumePosix()
    val base = Path.of("build").toAbsolutePath()
    val user = System.getProperty("user.name")
    assumeTrue(
      "$base and every directory above it must be owned by $user or root and closed to others",
      Files.isDirectory(base) && generateSequence(base) { it.parent }.all {
        val attributes = Files.readAttributes(it, PosixFileAttributes::class.java)
        attributes.owner().name in setOf(user, "root") &&
          PosixFilePermission.OTHERS_WRITE !in attributes.permissions() &&
          (PosixFilePermission.GROUP_WRITE !in attributes.permissions() || attributes.group().name == user)
      }
    )
    withTempDir(base) { tmp ->
      val ancestor = Files.createDirectory(tmp.resolve("shared"))
      val dir = Files.createDirectory(ancestor.resolve("apps"))
      val appImage = Files.createFile(dir.resolve(fileName))
      Files.setPosixFilePermissions(appImage, PosixFilePermissions.fromString(filePermissions))
      Files.setPosixFilePermissions(dir, PosixFilePermissions.fromString(directoryPermissions))
      Files.setPosixFilePermissions(ancestor, PosixFilePermissions.fromString(ancestorPermissions))
      check(appImage)
    }
  }

  private fun assumePosix() =
    assumeTrue("needs POSIX permissions", "posix" in FileSystems.getDefault().supportedFileAttributeViews())

  @Test
  fun runProcessReturnsTheOutputOfASuccessfulRun() {
    assumeShell()
    assertEquals("x\n", runProcess(listOf("sh", "-c", "echo x")))
  }

  @Test
  fun runProcessKeepsErrorOutputOutOfTheResult() {
    assumeShell()
    // more than a pipe holds, so error output left in a pipe would block the process until the timeout
    assertEquals("x\n", runProcess(listOf("sh", "-c", "head -c 100000 /dev/zero >&2; echo x")))
  }

  @Test
  fun runProcessGivesTheProcessAnEndOfInput() {
    assumeShell()
    assertEquals("x\n", runProcess(listOf("sh", "-c", "cat >/dev/null; echo x")), "a process reading its input must not wait for the timeout")
  }

  @Test
  fun runProcessReturnsNothingForAFailedRun() {
    assumeShell()
    assertNull(runProcess(listOf("sh", "-c", "echo x; exit 3")))
  }

  private fun queryDefault(mimeType: String) = listOf(XDG_MIME, "query", "default", mimeType)
  private fun setDefault(mimeType: String) = listOf(XDG_MIME, "default", APPIMAGE_ENTRY_NAME, mimeType)
  private val queryBothDefaults = listOf(queryDefault(APP_LINK_MIME_TYPE), queryDefault(CONNECTION_LINK_MIME_TYPE))
  private val setBothDefaults = listOf(
    queryDefault(APP_LINK_MIME_TYPE), setDefault(APP_LINK_MIME_TYPE), queryDefault(APP_LINK_MIME_TYPE),
    queryDefault(CONNECTION_LINK_MIME_TYPE), setDefault(CONNECTION_LINK_MIME_TYPE), queryDefault(CONNECTION_LINK_MIME_TYPE),
  )

  @Test
  fun appImageRegistrationWritesTheEntryAndMakesItTheDefault() = withRegistration { appImage, applicationsDir ->
    val xdg = FakeXdgMime(default = null)
    assertTrue(registerAppImageScheme(appImage, applicationsDir, listOf(), xdg::run), "the entry became the default handler")
    assertEquals(appImageDesktopEntry(appImage), File(applicationsDir, APPIMAGE_ENTRY_NAME).readText(), "the missing directory is created")
    assertEquals(listOf(updateDatabase(applicationsDir)) + setBothDefaults, xdg.calls)
  }

  @Test
  fun appImageRegistrationLeavesAnUnchangedEntryThatIsAlreadyTheDefault() = withRegistration { appImage, applicationsDir ->
    applicationsDir.mkdirs()
    File(applicationsDir, APPIMAGE_ENTRY_NAME).writeText(appImageDesktopEntry(appImage)!!)
    val xdg = FakeXdgMime(default = APPIMAGE_ENTRY_NAME)
    assertTrue(registerAppImageScheme(appImage, applicationsDir, listOf(), xdg::run))
    assertEquals(queryBothDefaults, xdg.calls, "nothing is rewritten or set again")
  }

  @Test
  fun appImageRegistrationTakesTheDefaultFromAHandlerNoLongerInstalled() = withRegistration { appImage, applicationsDir ->
    applicationsDir.mkdirs()
    File(applicationsDir, APPIMAGE_ENTRY_NAME).writeText(appImageDesktopEntry(appImage)!!)
    val xdg = FakeXdgMime(default = "other.desktop")
    assertTrue(registerAppImageScheme(appImage, applicationsDir, listOf(), xdg::run), "the entry became the default handler again")
    assertEquals(setBothDefaults, xdg.calls, "the entry is not rewritten, only made the default")
  }

  @Test
  fun appImageRegistrationSetsOnlyTheDefaultThatPointsElsewhere() = withRegistration { appImage, applicationsDir ->
    applicationsDir.mkdirs()
    File(applicationsDir, APPIMAGE_ENTRY_NAME).writeText(appImageDesktopEntry(appImage)!!)
    val xdg = FakeXdgMime(default = APPIMAGE_ENTRY_NAME, connectionDefault = "other.desktop")
    assertTrue(registerAppImageScheme(appImage, applicationsDir, listOf(), xdg::run))
    assertEquals(
      listOf(queryDefault(APP_LINK_MIME_TYPE), queryDefault(CONNECTION_LINK_MIME_TYPE), setDefault(CONNECTION_LINK_MIME_TYPE), queryDefault(CONNECTION_LINK_MIME_TYPE)),
      xdg.calls
    )
  }

  @Test
  fun appImageRegistrationRewritesTheEntryOfAMovedAppImage() = withRegistration { appImage, applicationsDir ->
    applicationsDir.mkdirs()
    File(applicationsDir, APPIMAGE_ENTRY_NAME).writeText(appImageDesktopEntry("/old/SimpleX.AppImage")!!)
    val xdg = FakeXdgMime(default = APPIMAGE_ENTRY_NAME)
    assertTrue(registerAppImageScheme(appImage, applicationsDir, listOf(), xdg::run))
    assertEquals(appImageDesktopEntry(appImage), File(applicationsDir, APPIMAGE_ENTRY_NAME).readText())
    assertEquals(listOf(updateDatabase(applicationsDir)) + queryBothDefaults, xdg.calls)
  }

  @Test
  fun appImageRegistrationFailsWhenAnotherHandlerKeepsTheAppLinkDefault() = withRegistration { appImage, applicationsDir ->
    val xdg = FakeXdgMime(default = "other.desktop", keptMimeTypes = setOf(APP_LINK_MIME_TYPE))
    assertFalse(registerAppImageScheme(appImage, applicationsDir, listOf(), xdg::run), "the default handler did not change")
    assertEquals(listOf(updateDatabase(applicationsDir)) + setBothDefaults, xdg.calls, "the connection link default is still set")
  }

  @Test
  fun appImageRegistrationSucceedsWhenAnotherHandlerKeepsOnlyTheConnectionDefault() = withRegistration { appImage, applicationsDir ->
    val xdg = FakeXdgMime(default = "other.desktop", keptMimeTypes = setOf(CONNECTION_LINK_MIME_TYPE))
    assertTrue(registerAppImageScheme(appImage, applicationsDir, listOf(), xdg::run), "only simplexchat: decides the badge page")
    assertEquals(listOf(updateDatabase(applicationsDir)) + setBothDefaults, xdg.calls)
  }

  @Test
  fun appImageRegistrationKeepsAnotherInstalledHandler() = withRegistration { appImage, applicationsDir ->
    val systemDir = File(applicationsDir.parentFile, "system-applications").apply { mkdirs() }
    File(systemDir, "other.desktop").writeText("[Desktop Entry]\n")
    val xdg = FakeXdgMime(default = "other.desktop")
    assertFalse(registerAppImageScheme(appImage, applicationsDir, listOf(systemDir), xdg::run), "links stay with the installed handler")
    assertEquals(listOf(updateDatabase(applicationsDir)) + queryBothDefaults, xdg.calls, "the user's handler is not replaced")
  }

  @Test
  fun appImageRegistrationKeepsAnotherInstalledConnectionHandlerOnly() = withRegistration { appImage, applicationsDir ->
    val systemDir = File(applicationsDir.parentFile, "system-applications").apply { mkdirs() }
    File(systemDir, "other.desktop").writeText("[Desktop Entry]\n")
    val xdg = FakeXdgMime(default = null, connectionDefault = "other.desktop")
    assertTrue(registerAppImageScheme(appImage, applicationsDir, listOf(systemDir), xdg::run), "app links have no handler, so the entry takes them")
    assertEquals(
      listOf(
        updateDatabase(applicationsDir), queryDefault(APP_LINK_MIME_TYPE), setDefault(APP_LINK_MIME_TYPE), queryDefault(APP_LINK_MIME_TYPE),
        queryDefault(CONNECTION_LINK_MIME_TYPE),
      ),
      xdg.calls
    )
  }

  @Test
  fun appImageRegistrationIsRefusedForAPathTheEntryCannotRun() =
    withRegistration(fileName = "Simple%X.AppImage") { appImage, applicationsDir ->
      val xdg = FakeXdgMime(default = null)
      assertFalse(registerAppImageScheme(appImage, applicationsDir, listOf(), xdg::run), "a path with % cannot be registered")
      assertEquals(listOf(), xdg.calls, "nothing is run")
      assertFalse(applicationsDir.exists(), "no entry is written")
    }

  @Test
  fun linuxPackageCountsAsRegisteredOnlyWhenItsEntryIsTheDefault() {
    assertTrue(linuxPackageRegistered(FakeXdgMime(default = "simplex-simplex.desktop")::run), "the deb's own entry")
    assertFalse(linuxPackageRegistered(FakeXdgMime(default = "other.desktop")::run), "another handler")
    assertFalse(linuxPackageRegistered(FakeXdgMime(default = null)::run), "no handler")
    assertFalse(
      linuxPackageRegistered(FakeXdgMime(default = "other.desktop", connectionDefault = "simplex-simplex.desktop")::run),
      "only the connection link handler"
    )
  }

  private fun updateDatabase(applicationsDir: File) = listOf("update-desktop-database", applicationsDir.path)

  private class FakeXdgMime(default: String?, connectionDefault: String? = default, private val keptMimeTypes: Set<String> = setOf()) {
    private val defaults = mutableMapOf(APP_LINK_MIME_TYPE to default, CONNECTION_LINK_MIME_TYPE to connectionDefault)
    val calls = mutableListOf<List<String>>()

    fun run(command: List<String>): String? {
      calls += command
      if (command.take(3) == listOf(XDG_MIME, "query", "default")) return defaults[command[3]]?.let { "$it\n" }
      // xdg-mime sets the default only when given an entry and then a MIME type, the order it requires
      if (command.size == 4 && command[1] == "default" && command[2].endsWith(".desktop") && command[3] !in keptMimeTypes) {
        defaults[command[3]] = command[2]
      }
      return ""
    }
  }

  private fun withRegistration(fileName: String = APPIMAGE_FILE_NAME, test: (appImage: String, applicationsDir: File) -> Unit) =
    withAppImage(fileName = fileName) { appImage ->
      assumeTrue("the AppImage must be owned by user.name", Files.getOwner(appImage).name == System.getProperty("user.name"))
      test(appImage.toString(), appImage.parent.parent.parent.resolve("share/applications").toFile())
    }

  private fun assumeShell() =
    assumeTrue("needs sh", System.getenv("PATH")?.split(File.pathSeparator)?.any { Files.isExecutable(Path.of(it, "sh")) } == true)
}
