package chat.simplex.common.platform

import chat.simplex.common.DesktopApp
import chat.simplex.common.model.*
import chat.simplex.common.views.call.RcvCallInvitation
import chat.simplex.common.views.helpers.withBGApi
import java.io.*
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.util.*

actual val appPlatform = AppPlatform.DESKTOP

@Suppress("ConstantLocale")
val defaultLocale: Locale = Locale.getDefault()

fun initApp() {
  ntfManager = object : NtfManager() { // LALAL
    override fun notifyContactConnected(user: User, contact: Contact) {}
    override fun notifyContactRequestReceived(user: User, cInfo: ChatInfo.ContactRequest) {}
    override fun notifyMessageReceived(user: User, cInfo: ChatInfo, cItem: ChatItem) {}
    override fun notifyCallInvitation(invitation: RcvCallInvitation) {}
    override fun hasNotificationsForChat(chatId: String): Boolean = false
    override fun cancelNotificationsForChat(chatId: String) {}
    override fun displayNotification(user: User, chatId: String, displayName: String, msgText: String, image: String?, actions: List<NotificationAction>) {}
    override fun createNtfChannelsMaybeShowAlert() {}
    override fun cancelCallNotification() {}
    override fun cancelAllNotifications() {}
  }
  chatInitializedAndStarted = {}
  applyAppLocale()
  withBGApi {
    initChatController()
    runMigrations()
  }
}

private fun applyAppLocale() {
  val lang = ChatController.appPrefs.appLanguage.get()
  if (lang == null || lang == Locale.getDefault().language) return
  Locale.setDefault(Locale.forLanguageTag(lang))
}

@Suppress("UnsafeDynamicallyLoadedCode")
actual fun initHaskell() {
  val libApp = "libapp-lib.${platform.libExtension}"
  val tmpDir = Files.createTempDirectory("simplex-native-libs").toFile()
  tmpDir.deleteOnExit()
  copyResources(platform.libPath, tmpDir.toPath())
  System.load(File(tmpDir, libApp).absolutePath)
  initHS()
}

private val home = System.getProperty("user.home")
val platform = detectPlatform()

enum class Platform(val libPath: String, val libExtension: String, val configPath: String) {
  LINUX_X86_64("/libs/linux-x86_64", "so", "$home/.config/simplex"),
  LINUX_AARCH64("/libs/aarch64", "so", "$home/.config/simplex"),
  WINDOWS_X86_64("/libs/windows-x86_64", "dll", System.getenv("AppData") + File.separator + "simplex"),
  MAC_X86_64("/libs/mac-x86_64", "dylib", "$home/.config/simplex"),
  MAC_AARCH64("/libs/mac-aarch64", "dylib", "$home/.config/simplex");
}

private fun detectPlatform(): Platform {
  val os = System.getProperty("os.name", "generic").lowercase(Locale.ENGLISH)
  val arch = System.getProperty("os.arch")
  return when {
    os == "linux" && (arch.contains("x86") || arch == "amd64") -> Platform.LINUX_X86_64
    os == "linux" && arch == "aarch64" -> Platform.LINUX_AARCH64
    os.contains("windows") && (arch.contains("x86") || arch == "amd64") -> Platform.WINDOWS_X86_64
    os.contains("mac") && arch.contains("x86") -> Platform.MAC_X86_64
    os.contains("mac") && arch.contains("aarch64") -> Platform.MAC_AARCH64
    else -> TODO("Currently, your processor's architecture ($arch) or os ($os) are unsupported. Please, contact us")
  }
}

private fun copyResources(from: String, to: Path) {
  val resource = DesktopApp::class.java.getResource("")!!.toURI()
  val fileSystem = FileSystems.newFileSystem(resource, emptyMap<String, String>())
  val resPath = fileSystem.getPath(from)
  Files.walkFileTree(resPath, object: SimpleFileVisitor<Path>() {
    override fun preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult {
      Files.createDirectories(to.resolve(resPath.relativize(dir).toString()))
      return FileVisitResult.CONTINUE
    }
    override fun visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult {
      Files.copy(file, to.resolve(resPath.relativize(file).toString()), StandardCopyOption.REPLACE_EXISTING)
      return FileVisitResult.CONTINUE
    }
  })
}
