package chat.simplex.common.platform

import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.DefaultTheme
import java.io.File
import java.util.*

enum class AppPlatform {
  ANDROID, DESKTOP;

  val isAndroid: Boolean
    get() = this == ANDROID

  val isDesktop: Boolean
    get() = this == DESKTOP
}

expect val appPlatform: AppPlatform

val appVersionInfo: Pair<String, Int?> = if (appPlatform == AppPlatform.ANDROID)
  BuildConfigCommon.ANDROID_VERSION_NAME to BuildConfigCommon.ANDROID_VERSION_CODE
else
  BuildConfigCommon.DESKTOP_VERSION_NAME to null

class FifoQueue<E>(private var capacity: Int) : LinkedList<E>() {
  override fun add(element: E): Boolean {
    if(size > capacity) removeFirst()
    return super.add(element)
  }
}

// LALAL VERSION CODE
fun runMigrations() {
  val lastMigration = ChatController.appPrefs.lastMigratedVersionCode
  if (lastMigration.get() < BuildConfigCommon.ANDROID_VERSION_CODE) {
    while (true) {
      if (lastMigration.get() < 117) {
        if (ChatController.appPrefs.currentTheme.get() == DefaultTheme.DARK.name) {
          ChatController.appPrefs.currentTheme.set(DefaultTheme.SIMPLEX.name)
        }
        lastMigration.set(117)
      } else {
        lastMigration.set(BuildConfigCommon.ANDROID_VERSION_CODE)
        break
      }
    }
  }
}

fun testCrypto() {
  val f = File(tmpDir.absolutePath, "LALAL")
  f.createNewFile()
  val enc = File(tmpDir.absolutePath, "LALAL.enc")
  val encToWrite = File(tmpDir.absolutePath, "LALALwrite.enc")
  f.writer().use {
    it.write("LALALAL")
  }
  val args = encryptCryptoFile(f.absolutePath, enc.absolutePath)
  println("LALAL RES enc " + args + " was text: " + f.readText() + " now text: " + enc.readText())
  f.delete()
  decryptCryptoFile(enc.absolutePath, args, f.absolutePath)
  println("LALAL RES dec was text: " + enc.readText() + " now text: " + f.readText())
  val write = writeCryptoFile(encToWrite.absolutePath, "LOLOLO".toByteArray())
  println("LALAL RES write $write path: ${encToWrite.absolutePath} text: ${encToWrite.readText()}")
  val read = readCryptoFile(encToWrite.absolutePath, args)
  println("LALAL RES read ${String(read)} from ${enc.readText()}")
}
