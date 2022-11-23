package chat.simplex.app.views.usersettings

import androidx.compose.foundation.layout.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import chat.simplex.app.R
import chat.simplex.app.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.app.model.ServerCfg
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.QRCodeScanner

@Composable
fun ScanSMPServer(onNext: (ServerCfg) -> Unit) {
  ScanSMPServerLayout(onNext)
}

@Composable
private fun ScanSMPServerLayout(onNext: (ServerCfg) -> Unit) {
  Column(
    Modifier
      .fillMaxSize()
  ) {
    AppBarTitle(stringResource(R.string.smp_servers_scan_qr))
    QRCodeScanner { text ->
      val res = parseServerAddress(text)
      if (res != null) {
        onNext(ServerCfg(text, false, null, true))
      } else {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(R.string.smp_servers_invalid_address),
          text = generalGetString(R.string.smp_servers_check_address)
        )
      }
    }
  }
}
