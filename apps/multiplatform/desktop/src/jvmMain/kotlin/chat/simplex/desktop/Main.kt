package chat.simplex.desktop

import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.AnimationVector1D
import androidx.compose.foundation.*
import androidx.compose.foundation.lazy.LazyListState
import androidx.compose.runtime.*
import androidx.compose.ui.ExperimentalComposeUiApi
import androidx.compose.ui.Modifier
import androidx.compose.ui.input.pointer.*
import chat.simplex.common.acquireSingleInstance
import chat.simplex.common.appLinkFromArgs
import chat.simplex.common.installOpenUriHandler
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.size
import chat.simplex.common.platform.*
import chat.simplex.common.platform.DesktopPlatform
import chat.simplex.common.showApp
import chat.simplex.common.singleInstanceLock
import chat.simplex.common.startShowFileWatcher
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingStage
import kotlinx.coroutines.*
import java.io.File

fun main(args: Array<String>) {
  try {
    val appLink = appLinkFromArgs(args)
    if (!acquireSingleInstance(appLink)) return
    appLink?.let { chatModel.appOpenUrl.value = null to it }
    // started after the launch link is stored, so a link it forwards later is not overwritten
    if (singleInstanceLock) startShowFileWatcher()
    // Clean shared temp dirs only in the owning instance (not in a Files.desktop val
    // initializer, which a transient second instance would also run). Early: before settings writes.
    preferencesTmpDir.deleteRecursively()
    // Disable hardware acceleration
    //System.setProperty("skiko.renderApi", "SOFTWARE")
    initHaskell()
    runMigrations()
    setupUpdateChecker()
    initApp()
    registerAppLinkScheme()
    tmpDir.deleteRecursively()
    tmpDir.mkdir()
    // Only the owning instance cleans tmpDir on exit (see preferencesTmpDir above).
    tmpDir.deleteOnExit()
    // installOpenUriHandler and showApp stay inside the try: whichever touches AWT first makes the process's
    // first AWT init, a known startup failure cause (#4146); later crashes go to showApp's WindowExceptionHandler.
    if (desktopPlatform.isMac()) installOpenUriHandler()
    return showApp()
  } catch (e: Throwable) {
    showStartupError(e) // the jpackage launcher otherwise hides the error behind "Failed to launch JVM" (#4146)
    throw e
  }
}

@OptIn(ExperimentalComposeUiApi::class)
@Suppress("UnsafeDynamicallyLoadedCode")
private fun initHaskell() {
  val resourcesDir = File(System.getProperty("compose.application.resources.dir"))
  val vlcDir = File(resourcesDir.absolutePath + File.separator + "vlc")
  if (desktopPlatform == DesktopPlatform.WINDOWS_X86_64) {
    windowsLoadRequiredLibs(resourcesDir, vlcDir)
  } else {
    System.load(File(resourcesDir, "libapp-lib.${desktopPlatform.libExtension}").absolutePath)
  }
  // No picture without preloading it, only sound. However, with libs from AppImage it works without preloading
  //val libXcb = "libvlc_xcb_events.so.0.0.0"
  //System.load(File(File(vlcDir, "vlc"), libXcb).absolutePath)
  System.setProperty("jna.library.path", vlcDir.absolutePath)
  //discoverVlcLibs(File(File(vlcDir, "vlc"), "plugins").absolutePath)
  initHS()

  platform = object: PlatformInterface {
    @Composable
    override fun desktopShowAppUpdateNotice() {
      fun showNoticeIfNeeded() {
        if (
          !chatModel.controller.appPrefs.appUpdateNoticeShown.get()
          && chatModel.controller.appPrefs.onboardingStage.get() == OnboardingStage.OnboardingComplete
          && chatModel.chats.size > 3
          && chatModel.activeCallInvitation.value == null
        ) {
          appPrefs.appUpdateNoticeShown.set(true)
          showAppUpdateNotice()
        }
      }
      // Will show notice if chats were loaded before that moment and number of chats > 3
      LaunchedEffect(Unit) {
        showNoticeIfNeeded()
      }
      // Will show notice if chats were loaded later (a lot of chats/slow query) and number of chats > 3
      KeyChangeEffect(chatModel.chats.size) { oldSize ->
        if (oldSize == 0) {
          showNoticeIfNeeded()
        }
      }
    }
  }
}

private fun windowsLoadRequiredLibs(libsTmpDir: File, vlcDir: File) {
  val mainLibs = arrayOf(
    "libcrypto-3-x64.dll",
    "libsimplex.dll",
    "libapp-lib.dll"
  )
  mainLibs.forEach {
    System.load(File(libsTmpDir, it).absolutePath)
  }
  val vlcLibs = arrayOf(
    "libvlccore.dll",
    "libvlc.dll",
    "axvlc.dll",
    "npvlc.dll"
  )
  vlcLibs.forEach {
    System.load(File(vlcDir, it).absolutePath)
  }
}
