package chat.simplex.common.views.chat.item

import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import chat.simplex.common.model.CryptoFile
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

}
