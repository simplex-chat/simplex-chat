package chat.simplex.common.views.helpers

import chat.simplex.common.views.usersettings.LAMode

actual fun authenticate(
  promptTitle: String,
  promptSubtitle: String,
  selfDestruct: Boolean,
  usingLAMode: LAMode,
  oneTime: Boolean,
  completed: (LAResult) -> Unit
) {
  when (usingLAMode) {
    LAMode.PASSCODE -> authenticateWithPasscode(promptTitle, promptSubtitle, selfDestruct, oneTime, completed)
    else -> {}
  }
}
