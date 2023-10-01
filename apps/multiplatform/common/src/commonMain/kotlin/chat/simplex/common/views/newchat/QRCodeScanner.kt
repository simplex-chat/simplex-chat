package chat.simplex.common.views.newchat

import androidx.compose.runtime.*

@Composable
expect fun QRCodeScanner(onBarcode: (String) -> Unit)
