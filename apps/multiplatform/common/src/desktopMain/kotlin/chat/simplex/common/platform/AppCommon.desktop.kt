package chat.simplex.common.platform

import chat.simplex.common.model.*
import chat.simplex.common.views.call.RcvCallInvitation
import chat.simplex.common.views.helpers.*
import java.util.*
import chat.simplex.res.MR
import java.io.File

actual val appPlatform = AppPlatform.DESKTOP

actual val deviceName = generalGetString(MR.strings.desktop_device)

@Suppress("ConstantLocale")
val defaultLocale: Locale = Locale.getDefault()

fun initApp() {
  ntfManager = object : NtfManager() {
    override fun notifyCallInvitation(invitation: RcvCallInvitation): Boolean = chat.simplex.common.model.NtfManager.notifyCallInvitation(invitation)
    override fun hasNotificationsForChat(chatId: String): Boolean = chat.simplex.common.model.NtfManager.hasNotificationsForChat(chatId)
    override fun cancelNotificationsForChat(chatId: String) = chat.simplex.common.model.NtfManager.cancelNotificationsForChat(chatId)
    override fun cancelNotificationsForUser(userId: Long) = chat.simplex.common.model.NtfManager.cancelNotificationsForUser(userId)
    override fun displayNotification(user: UserLike, chatId: String, displayName: String, msgText: String, image: String?, actions: List<Pair<NotificationAction, () -> Unit>>) = chat.simplex.common.model.NtfManager.displayNotification(user, chatId, displayName, msgText, image, actions)
    override fun androidCreateNtfChannelsMaybeShowAlert() {}
    override fun cancelCallNotification() {}
    override fun cancelAllNotifications() = chat.simplex.common.model.NtfManager.cancelAllNotifications()
    override fun showMessage(title: String, text: String) = chat.simplex.common.model.NtfManager.showMessage(title, text)
  }
  applyAppLocale()
  if (DatabaseUtils.ksSelfDestructPassword.get() == null) {
    initChatControllerOnStart()
  }
  // LALAL
  //testCrypto()
}

//fun discoverVlcLibs(path: String) {
//  uk.co.caprica.vlcj.binding.LibC.INSTANCE.setenv("VLC_PLUGIN_PATH", path, 1)
//}

private fun applyAppLocale() {
  val lang = ChatController.appPrefs.appLanguage.get()
  if (lang == null || lang == Locale.getDefault().language) return
  Locale.setDefault(Locale.forLanguageTag(lang))
}

actual fun chooseGitHubReleaseAssets(release: GitHubRelease): List<GitHubAsset> {
  val process = Runtime.getRuntime().exec("which dpkg").onExit().join()
  val isDebianBased = process.exitValue() == 0
  // Show all available .deb packages and user will choose the one that works on his system
  val res = if (isDebianBased) {
    release.assets.filter { it.name.lowercase().endsWith(".deb") }
  } else {
    release.assets.filter { it.name == desktopPlatform.githubAssetName }
  }
  return res
}

actual fun installAppUpdate(file: File) {
  when {
    desktopPlatform.isLinux() -> {
      val process = Runtime.getRuntime().exec("xdg-open ${file.absolutePath}").onExit().join()
      val startedInstallation = process.exitValue() == 0 && process.children().count() > 0
      if (!startedInstallation) {
        Log.e(TAG, "Error starting installation: ${process.inputReader().use { it.readLines().joinToString("\n") }}${process.errorStream.use { String(it.readAllBytes()) }}")
        // Failed to start installation. show directory with the file for manual installation
        desktopOpenDir(file.parentFile)
      }
    }
    desktopPlatform.isWindows() -> {
      val process = Runtime.getRuntime().exec("msiexec /i ${file.absolutePath}"/* /qb */).onExit().join()
      val startedInstallation = process.exitValue() == 0
      if (!startedInstallation) {
        Log.e(TAG, "Error starting installation: ${process.inputReader().use { it.readLines().joinToString("\n") }}${process.errorStream.use { String(it.readAllBytes()) }}")
        // Failed to start installation. show directory with the file for manual installation
        desktopOpenDir(file.parentFile)
      }
    }
    desktopPlatform.isMac() -> {
      val process = Runtime.getRuntime().exec("hdiutil mount ${file.absolutePath}").onExit().join()
      val startedInstallation = process.exitValue() == 0
      if (!startedInstallation) {
        Log.e(TAG, "Error starting installation: ${process.inputReader().use { it.readLines().joinToString("\n") }}${process.errorStream.use { String(it.readAllBytes()) }}")
        // Failed to start installation. show directory with the file for manual installation
        desktopOpenDir(file.parentFile)
        return
      }
      var process2 = Runtime.getRuntime().exec("cp -R /Volumes/SimpleX/SimpleX.app /Applications").onExit().join()
      val copiedSuccessfully = process.exitValue() == 0
      if (!copiedSuccessfully) {
        Log.e(TAG, "Error copying the app: ${process.inputReader().use { it.readLines().joinToString("\n") }}${process.errorStream.use { String(it.readAllBytes()) }}")
        // Failed to start installation. show directory with the file for manual installation
        desktopOpenDir(file.parentFile)
      }
    }
  }
//  file.delete()
}