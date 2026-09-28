package chat.simplex.app

import chat.simplex.common.platform.APPIMAGE_ENTRY_NAME
import chat.simplex.common.platform.DesktopInstallation
import chat.simplex.common.platform.DesktopPlatform
import chat.simplex.common.platform.RegistryValue
import chat.simplex.common.platform.SCHEME_MIME_TYPE
import chat.simplex.common.platform.appImageDesktopEntry
import chat.simplex.common.platform.desktopExecArgument
import chat.simplex.common.platform.desktopInstallation
import chat.simplex.common.platform.linuxPackageRegistered
import chat.simplex.common.platform.onlyOwnerCanReplace
import chat.simplex.common.platform.onlyOwnerCanWrite
import chat.simplex.common.platform.registerAppImageScheme
import chat.simplex.common.platform.registerWindowsScheme
import chat.simplex.common.platform.runProcess
import chat.simplex.common.platform.windowsOpenCommand
import chat.simplex.common.platform.windowsSchemeValues
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
  private val windowsCommandKey = "Software\\Classes\\simplexchat\\shell\\open\\command"
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
  fun windowsSchemeIsAUrlProtocolThatOpensTheExe() {
    assertEquals(
      listOf(
        RegistryValue(key = "Software\\Classes\\simplexchat", name = "", value = "URL:SimpleX Chat"),
        RegistryValue(key = "Software\\Classes\\simplexchat", name = "URL Protocol", value = ""),
        RegistryValue(key = "Software\\Classes\\simplexchat\\DefaultIcon", name = "", value = "\"$windowsExe\",0"),
        RegistryValue(key = windowsCommandKey, name = "", value = "\"$windowsExe\" \"%1\""),
      ),
      windowsSchemeValues(windowsExe)
    )
  }

  @Test
  fun windowsRegistrationWritesEveryValueWhenTheCommandPointsElsewhere() {
    val registry = FakeRegistry(mutableMapOf(windowsCommandKey to olderWindowsCommand))
    assertTrue(registerWindowsScheme(windowsExe, registry::readDefault, registry::write), "the command now opens this exe")
    assertEquals(windowsSchemeValues(windowsExe), registry.writes)
  }

  @Test
  fun windowsRegistrationLeavesAMatchingCommandAlone() {
    val registry = FakeRegistry(mutableMapOf(windowsCommandKey to windowsOpenCommand(windowsExe)))
    assertTrue(registerWindowsScheme(windowsExe, registry::readDefault, registry::write))
    assertEquals(listOf(), registry.writes, "nothing is written again")
  }

  @Test
  fun windowsRegistrationFailsWhenTheCommandIsNotStored() {
    val registry = FakeRegistry(mutableMapOf(windowsCommandKey to olderWindowsCommand), keepsWrites = false)
    assertFalse(registerWindowsScheme(windowsExe, registry::readDefault, registry::write), "the older command stayed")
    assertEquals(windowsSchemeValues(windowsExe), registry.writes)
  }

  private class FakeRegistry(private val defaults: MutableMap<String, String>, private val keepsWrites: Boolean = true) {
    val writes = mutableListOf<RegistryValue>()

    fun readDefault(key: String): String? = defaults[key]

    fun write(value: RegistryValue) {
      writes += value
      if (keepsWrites && value.name == "") defaults[value.key] = value.value
    }
  }

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
      |MimeType=x-scheme-handler/simplexchat;
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

  private val queryDefault = listOf(XDG_MIME, "query", "default", SCHEME_MIME_TYPE)
  private val setDefault = listOf(XDG_MIME, "default", APPIMAGE_ENTRY_NAME, SCHEME_MIME_TYPE)

  @Test
  fun appImageRegistrationWritesTheEntryAndMakesItTheDefault() = withRegistration { appImage, applicationsDir ->
    val xdg = FakeXdgMime(default = null)
    assertTrue(registerAppImageScheme(appImage, applicationsDir, xdg::run), "the entry became the default handler")
    assertEquals(appImageDesktopEntry(appImage), File(applicationsDir, APPIMAGE_ENTRY_NAME).readText(), "the missing directory is created")
    assertEquals(listOf(updateDatabase(applicationsDir), queryDefault, setDefault, queryDefault), xdg.calls)
  }

  @Test
  fun appImageRegistrationLeavesAnUnchangedEntryThatIsAlreadyTheDefault() = withRegistration { appImage, applicationsDir ->
    applicationsDir.mkdirs()
    File(applicationsDir, APPIMAGE_ENTRY_NAME).writeText(appImageDesktopEntry(appImage)!!)
    val xdg = FakeXdgMime(default = APPIMAGE_ENTRY_NAME)
    assertTrue(registerAppImageScheme(appImage, applicationsDir, xdg::run))
    assertEquals(listOf(queryDefault), xdg.calls, "nothing is rewritten or set again")
  }

  @Test
  fun appImageRegistrationSetsTheDefaultAgainForAnUnchangedEntry() = withRegistration { appImage, applicationsDir ->
    applicationsDir.mkdirs()
    File(applicationsDir, APPIMAGE_ENTRY_NAME).writeText(appImageDesktopEntry(appImage)!!)
    val xdg = FakeXdgMime(default = "other.desktop")
    assertTrue(registerAppImageScheme(appImage, applicationsDir, xdg::run), "the entry became the default handler again")
    assertEquals(listOf(queryDefault, setDefault, queryDefault), xdg.calls, "the entry is not rewritten, only made the default")
  }

  @Test
  fun appImageRegistrationRewritesTheEntryOfAMovedAppImage() = withRegistration { appImage, applicationsDir ->
    applicationsDir.mkdirs()
    File(applicationsDir, APPIMAGE_ENTRY_NAME).writeText(appImageDesktopEntry("/old/SimpleX.AppImage")!!)
    val xdg = FakeXdgMime(default = APPIMAGE_ENTRY_NAME)
    assertTrue(registerAppImageScheme(appImage, applicationsDir, xdg::run))
    assertEquals(appImageDesktopEntry(appImage), File(applicationsDir, APPIMAGE_ENTRY_NAME).readText())
    assertEquals(listOf(updateDatabase(applicationsDir), queryDefault), xdg.calls)
  }

  @Test
  fun appImageRegistrationFailsWhenAnotherHandlerKeepsTheDefault() = withRegistration { appImage, applicationsDir ->
    val xdg = FakeXdgMime(default = "other.desktop", keepsDefault = true)
    assertFalse(registerAppImageScheme(appImage, applicationsDir, xdg::run), "the default handler did not change")
    assertEquals(listOf(updateDatabase(applicationsDir), queryDefault, setDefault, queryDefault), xdg.calls)
  }

  @Test
  fun appImageRegistrationIsRefusedForAPathTheEntryCannotRun() =
    withRegistration(fileName = "Simple%X.AppImage") { appImage, applicationsDir ->
      val xdg = FakeXdgMime(default = null)
      assertFalse(registerAppImageScheme(appImage, applicationsDir, xdg::run), "a path with % cannot be registered")
      assertEquals(listOf(), xdg.calls, "nothing is run")
      assertFalse(applicationsDir.exists(), "no entry is written")
    }

  @Test
  fun linuxPackageCountsAsRegisteredOnlyWhenItsEntryIsTheDefault() {
    assertTrue(linuxPackageRegistered(FakeXdgMime(default = "simplex-simplex.desktop")::run), "the deb's own entry")
    assertFalse(linuxPackageRegistered(FakeXdgMime(default = "other.desktop")::run), "another handler")
    assertFalse(linuxPackageRegistered(FakeXdgMime(default = null)::run), "no handler")
  }

  private fun updateDatabase(applicationsDir: File) = listOf("update-desktop-database", applicationsDir.path)

  private class FakeXdgMime(private var default: String?, private val keepsDefault: Boolean = false) {
    val calls = mutableListOf<List<String>>()

    fun run(command: List<String>): String? {
      calls += command
      if (command.take(2) == listOf(XDG_MIME, "query")) return default?.let { "$it\n" }
      // xdg-mime sets the default only when given an entry and then a MIME type, the order it requires
      if (!keepsDefault && command.size == 4 && command[1] == "default" && command[2].endsWith(".desktop")) default = command[2]
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
