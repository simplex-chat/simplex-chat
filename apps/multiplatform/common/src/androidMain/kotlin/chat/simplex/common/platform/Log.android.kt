package chat.simplex.common.platform

import android.util.Log
import chat.simplex.common.model.ChatController.appPrefs

actual object Log {
  actual fun d(tag: String, text: String) { if (appPrefs.logLevel.get() <= LogLevel.DEBUG && appPrefs.developerTools.get()) Log.d(tag, text) }
  actual fun e(tag: String, text: String) { if (appPrefs.logLevel.get() <= LogLevel.ERROR || !appPrefs.developerTools.get()) Log.e(tag, text) }
  actual fun i(tag: String, text: String) { if (appPrefs.logLevel.get() <= LogLevel.INFO && appPrefs.developerTools.get()) Log.i(tag, text) }
  actual fun w(tag: String, text: String) { if (appPrefs.logLevel.get() <= LogLevel.WARNING || !appPrefs.developerTools.get()) Log.w(tag, text) }
}
