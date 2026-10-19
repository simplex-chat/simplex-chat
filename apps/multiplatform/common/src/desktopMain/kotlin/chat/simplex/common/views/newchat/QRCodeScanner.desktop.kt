package chat.simplex.common.views.newchat

import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.runtime.*

@Composable
actual fun QRCodeScanner(
  showQRCodeScanner: MutableState<Boolean>,
  padding: PaddingValues,
  onBarcode: suspend (String) -> Boolean
) {
  //LALAL
}
