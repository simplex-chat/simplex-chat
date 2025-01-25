package chat.simplex.common.platform

import chat.simplex.common.model.ChatController.appPrefs

actual object Log {
  actual fun d(tag: String, text: String) { if (appPrefs.logLevel.get() <= LogLevel.DEBUG && appPrefs.developerTools.get()) println("D: $text") }
  actual fun e(tag: String, text: String) { if (appPrefs.logLevel.get() <= LogLevel.ERROR || !appPrefs.developerTools.get()) println("E: $text") }
  actual fun i(tag: String, text: String) { if (appPrefs.logLevel.get() <= LogLevel.INFO && appPrefs.developerTools.get()) println("I: $text") }
  actual fun w(tag: String, text: String) { if (appPrefs.logLevel.get() <= LogLevel.WARNING || !appPrefs.developerTools.get()) println("W: $text") }
}
