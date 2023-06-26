package chat.simplex.common.views.usersettings

import androidx.compose.foundation.layout.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import com.icerockdev.library.MR
import chat.simplex.common.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.common.model.ServerCfg
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.QRCodeScanner

@Composable
expect fun ScanProtocolServer(onNext: (ServerCfg) -> Unit)

@Composable
fun ScanProtocolServerLayout(onNext: (ServerCfg) -> Unit) {
  Column(
    Modifier
      .fillMaxSize()
      .padding(horizontal = DEFAULT_PADDING)
  ) {
    AppBarTitle(stringResource(MR.strings.smp_servers_scan_qr), false)
    Box(
      Modifier
        .fillMaxWidth()
        .aspectRatio(ratio = 1F)
        .padding(bottom = 12.dp)
    ) {
      QRCodeScanner { text ->
        val res = parseServerAddress(text)
        if (res != null) {
          onNext(ServerCfg(text, false, null, true))
        } else {
          AlertManager.shared.showAlertMsg(
            title = generalGetString(MR.strings.smp_servers_invalid_address),
            text = generalGetString(MR.strings.smp_servers_check_address)
          )
        }
      }
    }
  }
}
