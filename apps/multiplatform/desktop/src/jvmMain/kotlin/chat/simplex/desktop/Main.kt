package chat.simplex.desktop

import chat.simplex.common.platform.*
import chat.simplex.common.showApp
import java.io.File
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.attribute.FileTime
import kotlin.io.path.setLastModifiedTime

fun main() {
  initHaskell()
  initApp()
  tmpDir.deleteRecursively()
  tmpDir.mkdir()
  return showApp()
}

@Suppress("UnsafeDynamicallyLoadedCode")
private fun initHaskell() {
  val libsTmpDir = File(tmpDir.absolutePath + File.separator + "libs")
  copyResources(desktopPlatform.libPath, libsTmpDir.toPath())
  vlcDir.deleteRecursively()
  Files.move(File(libsTmpDir, "vlc").toPath(), vlcDir.toPath(), StandardCopyOption.REPLACE_EXISTING)
  if (desktopPlatform == DesktopPlatform.WINDOWS_X86_64) {
    windowsLoadRequiredLibs(libsTmpDir)
  } else {
    System.load(File(libsTmpDir, "libapp-lib.${desktopPlatform.libExtension}").absolutePath)
  }
  // No picture without preloading it, only sound. However, with libs from AppImage it works without preloading
  //val libXcb = "libvlc_xcb_events.so.0.0.0"
  //System.load(File(File(vlcDir, "vlc"), libXcb).absolutePath)
  System.setProperty("jna.library.path", vlcDir.absolutePath)
  //discoverVlcLibs(File(File(vlcDir, "vlc"), "plugins").absolutePath)

  libsTmpDir.deleteRecursively()
  initHS()
}

private fun copyResources(from: String, to: Path) {
  val resource = Class.forName("chat.simplex.desktop.MainKt").getResource("")!!.toURI()
  val fileSystem = FileSystems.newFileSystem(resource, emptyMap<String, String>())
  val resPath = fileSystem.getPath(from)
  Files.walkFileTree(resPath, object: SimpleFileVisitor<Path>() {
    override fun preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult {
      Files.createDirectories(to.resolve(resPath.relativize(dir).toString()))
      return FileVisitResult.CONTINUE
    }
    override fun visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult {
      val dest = to.resolve(resPath.relativize(file).toString())
      Files.copy(file, dest, StandardCopyOption.REPLACE_EXISTING)
      // Setting the same time on file as the time set in script that generates VLC libs
      if (dest.toString().contains("." + desktopPlatform.libExtension)) {
        dest.setLastModifiedTime(FileTime.fromMillis(0))
      }
      return FileVisitResult.CONTINUE
    }
  })
}

private fun windowsLoadRequiredLibs(libsTmpDir: File) {
  val mainLibs = arrayOf(
    "libcrypto-3-x64.dll",
    "libffi-8.dll",
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
