package chat.simplex.app.platform

import android.app.UiModeManager
import android.content.Context
import chat.simplex.app.SimplexApp

// Non-@Composable implementation
fun isInNightMode() =
  (SimplexApp.context.getSystemService(Context.UI_MODE_SERVICE) as UiModeManager).nightMode == UiModeManager.MODE_NIGHT_YES
