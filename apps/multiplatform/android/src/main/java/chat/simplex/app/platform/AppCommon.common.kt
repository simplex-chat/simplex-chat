package chat.simplex.app.platform

import chat.simplex.app.BuildConfig
import chat.simplex.app.model.ChatController
import chat.simplex.app.ui.theme.DefaultTheme
import java.util.*

class FifoQueue<E>(private var capacity: Int) : LinkedList<E>() {
  override fun add(element: E): Boolean {
    if(size > capacity) removeFirst()
    return super.add(element)
  }
}

fun runMigrations() {
  val lastMigration = ChatController.appPrefs.lastMigratedVersionCode
  if (lastMigration.get() < BuildConfig.VERSION_CODE) {
    while (true) {
      if (lastMigration.get() < 117) {
        if (ChatController.appPrefs.currentTheme.get() == DefaultTheme.DARK.name) {
          ChatController.appPrefs.currentTheme.set(DefaultTheme.SIMPLEX.name)
        }
        lastMigration.set(117)
      } else {
        lastMigration.set(BuildConfig.VERSION_CODE)
        break
      }
    }
  }
}
