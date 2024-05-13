package chat.simplex.common.views.newchat

import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.runtime.*
import androidx.compose.ui.unit.dp
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF

@Composable
expect fun QRCodeScanner(
  showQRCodeScanner: MutableState<Boolean> = remember { mutableStateOf(true) },
  padding: PaddingValues = PaddingValues(horizontal = DEFAULT_PADDING * 2f, vertical = DEFAULT_PADDING_HALF),
  onBarcode: (String) -> Unit
)
