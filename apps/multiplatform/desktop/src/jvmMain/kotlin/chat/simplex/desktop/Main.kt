package chat.simplex.desktop

import chat.simplex.common.platform.*
import chat.simplex.common.showApp
import java.io.File
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes

fun main() {
  initHaskell()
  initApp()
  tmpDir.deleteRecursively()
  tmpDir.mkdir()
  return showApp()
}

@Suppress("UnsafeDynamicallyLoadedCode")
private fun initHaskell() {
  val libApp = "libapp-lib.${desktopPlatform.libExtension}"
  val libsTmpDir = File(tmpDir.absolutePath + File.separator + "libs")
  copyResources(desktopPlatform.libPath, libsTmpDir.toPath())
  System.load(File(libsTmpDir, libApp).absolutePath)
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
      Files.copy(file, to.resolve(resPath.relativize(file).toString()), StandardCopyOption.REPLACE_EXISTING)
      return FileVisitResult.CONTINUE
    }
  })
}
