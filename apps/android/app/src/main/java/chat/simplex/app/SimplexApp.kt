package chat.simplex.app

import android.app.Application

class SimplexApp: Application() {
    private lateinit var filesDir: String

    override fun onCreate() {
        super.onCreate()
        filesDir = applicationContext.filesDir.toString()
    }

    val messageRepository by lazy { MessageRepository(filesDir) }
}
