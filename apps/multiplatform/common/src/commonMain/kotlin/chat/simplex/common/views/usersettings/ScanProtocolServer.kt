package chat.simplex.common.views.usersettings

import androidx.compose.foundation.layout.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.common.model.ServerCfg
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.QRCodeScanner
import chat.simplex.res.MR

@Composable
expect fun ScanProtocolServer(rhId: Long?, onNext: (ServerCfg) -> Unit)

@Composable
fun ScanProtocolServerLayout(rhId: Long?, onNext: (ServerCfg) -> Unit) {
  Column(
    Modifier
      .fillMaxSize()
  ) {
    AppBarTitle(stringResource(MR.strings.smp_servers_scan_qr))
    QRCodeScanner { text ->
      val res = parseServerAddress(text)
      if (res != null) {
        onNext(ServerCfg(remoteHostId = rhId, text, false, null, false))
      } else {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.smp_servers_invalid_address),
          text = generalGetString(MR.strings.smp_servers_check_address)
        )
      }
    }
  }
}
