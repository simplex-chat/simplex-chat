package chat.simplex.common.platform

import com.sun.jna.platform.unix.LibC
import java.io.File
import java.io.IOException

internal const val FONTCONFIG_FILE = "FONTCONFIG_FILE"
private const val FONTCONFIG_FILE_NAME = "fonts.conf"
private const val SYSTEM_FONTCONFIG_FILE = "/etc/fonts/fonts.conf"
private const val OVERWRITE_ENV = 1

internal val bitmapFontsRejectedConfig = """
  |<?xml version="1.0"?>
  |<!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
  |<fontconfig>
  |  <include ignore_missing="yes">$SYSTEM_FONTCONFIG_FILE</include>
  |  <selectfont>
  |    <rejectfont>
  |      <pattern><patelt name="fontformat"><string>PCF</string></patelt></pattern>
  |      <pattern><patelt name="fontformat"><string>BDF</string></patelt></pattern>
  |    </rejectfont>
  |  </selectfont>
  |</fontconfig>
  |""".trimMargin()

internal fun bitmapFontsRejectedFontconfig(env: Map<String, String>, configDir: File): File? {
  if (env.containsKey(FONTCONFIG_FILE)) return null
  val file = File(configDir, FONTCONFIG_FILE_NAME)
  return try {
    if (!file.isFile || file.readText() != bitmapFontsRejectedConfig) {
      configDir.mkdirs()
      file.writeText(bitmapFontsRejectedConfig)
    }
    file
  } catch (e: IOException) {
    Log.w(TAG, "fontconfig: cannot write $file: ${e.message}")
    null
  }
}

fun rejectBitmapFonts() {
  if (!desktopPlatform.isLinux()) return
  val file = bitmapFontsRejectedFontconfig(System.getenv(), File(desktopPlatform.configPath)) ?: return
  try {
    LibC.INSTANCE.setenv(FONTCONFIG_FILE, file.absolutePath, OVERWRITE_ENV)
  } catch (e: LinkageError) {
    Log.w(TAG, "fontconfig: cannot set $FONTCONFIG_FILE: ${e.message}")
  }
}
