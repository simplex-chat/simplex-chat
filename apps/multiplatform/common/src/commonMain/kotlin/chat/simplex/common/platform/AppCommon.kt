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

expect val deviceName: String

val appVersionInfo: Pair<String, Int?> = if (appPlatform == AppPlatform.ANDROID)
  BuildConfigCommon.ANDROID_VERSION_NAME to BuildConfigCommon.ANDROID_VERSION_CODE
else
  BuildConfigCommon.DESKTOP_VERSION_NAME to BuildConfigCommon.DESKTOP_VERSION_CODE

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
      } else if (lastMigration.get() < 201) {
        // Making theme keys lowercase as API expects
        ChatController.appPrefs.themeOverrides.set(ChatController.appPrefs.themeOverrides.get().map { it.key.lowercase() to it.value }.toMap())
        ChatController.appPrefs.currentTheme.set(ChatController.appPrefs.currentTheme.get()?.lowercase())
        ChatController.appPrefs.systemDarkTheme.set(ChatController.appPrefs.systemDarkTheme.get()?.lowercase())
        lastMigration.set(201)
      } else {
        lastMigration.set(BuildConfigCommon.ANDROID_VERSION_CODE)
        break
      }
    }
  }
}
