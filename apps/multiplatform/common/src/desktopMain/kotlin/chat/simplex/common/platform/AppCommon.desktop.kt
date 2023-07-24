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
  val libApp = "libapp-lib.${desktopPlatform.libExtension}"
  val libsTmpDir = File(tmpDir.absolutePath + File.separator + "libs")
  copyResources(desktopPlatform.libPath, libsTmpDir.toPath())
  System.load(File(libsTmpDir, libApp).absolutePath)
  libsTmpDir.deleteRecursively()
  initHS()
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
