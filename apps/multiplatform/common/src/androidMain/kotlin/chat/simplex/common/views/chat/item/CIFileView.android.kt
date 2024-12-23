package chat.simplex.common.views.chat.item

import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.*
import chat.simplex.common.views.helpers.DefaultDropdownMenu
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

private data class OpenDefaultApp(
  val name: String,
  val intent: String
)

@Composable
actual fun AndroidSaveOrOpenFileMenu(showMenu: MutableState<Boolean>, encrypted: Boolean, ext: String, openFile: () -> Unit, saveFile: () -> Unit) {
  val defaultApp = remember { getDefaultApp(ext) }
  DefaultDropdownMenu(showMenu) {
    if (defaultApp != null) {
      ItemAction(
        stringResource(MR.strings.open_in_app).format(defaultApp.name),
        painterResource(MR.images.ic_open_in_new),
        color = MaterialTheme.colors.primary,
        onClick = {
          openFile()
          showMenu.value = false
        }
      )
    }
    ItemAction(
      stringResource(MR.strings.save_verb),
      painterResource(if (encrypted) MR.images.ic_lock_open_right else MR.images.ic_download),
      color = MaterialTheme.colors.primary,
      onClick = {
        saveFile()
        showMenu.value = false
      }
    )
  }
}

private fun getDefaultApp(ext: String): OpenDefaultApp? {
  return OpenDefaultApp("LALAL", "")
}
