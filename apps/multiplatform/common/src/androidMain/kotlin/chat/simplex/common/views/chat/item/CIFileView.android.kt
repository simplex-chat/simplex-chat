package chat.simplex.common.views.chat.item

import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.*
import chat.simplex.common.model.CryptoFile
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.DefaultDropdownMenu
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import java.net.URI

@Composable
actual fun SaveOrOpenFileMenu(
  showMenu: MutableState<Boolean>,
  encrypted: Boolean,
  ext: String?,
  encryptedUri: URI,
  fileSource: CryptoFile,
  saveFile: () -> Unit
) {
  val defaultApp = remember(encryptedUri.toString()) { if (ext != null) queryDefaultAppForExtension(ext, encryptedUri) else null }
  DefaultDropdownMenu(showMenu) {
    if (defaultApp != null) {
      if (!defaultApp.isSystemChooser) {
        ItemAction(
          stringResource(MR.strings.open_with_app).format(defaultApp.name),
          defaultApp.icon,
          textColor = MaterialTheme.colors.primary,
          onClick = {
            openOrShareFile("", fileSource, justOpen = true, useChooser = false)
            showMenu.value = false
          }
        )
      } else {
        ItemAction(
          stringResource(MR.strings.open_with_app).format("…"),
          painterResource(MR.images.ic_open_in_new),
          color = MaterialTheme.colors.primary,
          onClick = {
            openOrShareFile("", fileSource, justOpen = true, useChooser = false)
            showMenu.value = false
          }
        )
      }
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
