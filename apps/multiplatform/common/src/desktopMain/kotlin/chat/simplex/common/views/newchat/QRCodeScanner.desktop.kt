package chat.simplex.common.views.newchat

import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.runtime.*
import androidx.compose.ui.graphics.Shape

@Composable
actual fun QRCodeScanner(
  showQRCodeScanner: MutableState<Boolean>,
  padding: PaddingValues,
  clipShape: Shape,
  onBarcode: suspend (String) -> Boolean
) {
  //LALAL
}
