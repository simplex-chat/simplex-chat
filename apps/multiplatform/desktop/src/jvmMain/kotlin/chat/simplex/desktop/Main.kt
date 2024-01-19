package chat.simplex.desktop

import chat.simplex.common.platform.*
import chat.simplex.common.showApp
import java.io.File

fun main() {
  initHaskell()
  initApp()
  tmpDir.deleteRecursively()
  tmpDir.mkdir()
  return showApp()
}

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
}

private fun windowsLoadRequiredLibs(libsTmpDir: File, vlcDir: File) {
  val mainLibs = arrayOf(
    "libcrypto-1_1-x64.dll",
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
