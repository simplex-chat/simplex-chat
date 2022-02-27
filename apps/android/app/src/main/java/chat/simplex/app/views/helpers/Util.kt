package chat.simplex.app.views.helpers

import android.util.Log
import androidx.compose.runtime.*
import chat.simplex.app.views.newchat.ModalView
import kotlinx.coroutines.*

fun withApi(action: suspend CoroutineScope.() -> Unit): Job =
  GlobalScope.launch { withContext(Dispatchers.Main, action) }
