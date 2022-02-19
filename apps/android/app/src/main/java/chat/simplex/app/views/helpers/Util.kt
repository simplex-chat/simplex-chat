package chat.simplex.app.views.helpers

import kotlinx.coroutines.*

@DelicateCoroutinesApi
fun withApi(action: suspend CoroutineScope.() -> Unit): Job =
  GlobalScope.launch {
    withContext(Dispatchers.Main) { action() }
  }

