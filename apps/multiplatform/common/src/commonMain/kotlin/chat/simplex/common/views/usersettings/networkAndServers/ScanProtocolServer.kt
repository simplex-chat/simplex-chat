package chat.simplex.common.views.usersettings.networkAndServers

import androidx.compose.runtime.Composable
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.UserServer
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.*
import chat.simplex.res.MR

@Composable
expect fun ScanProtocolServer(rhId: Long?, close: () -> Unit, onNext: (UserServer) -> Unit)

@Composable
fun ScanProtocolServerLayout(rhId: Long?, close: () -> Unit, onNext: (UserServer) -> Unit) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.smp_servers_scan_qr))
    QRCodeScanner { text ->
      handleScan(rhId, text, QRCodeType.ServerAddress::class, close) { qr ->
        onNext(UserServer(remoteHostId = rhId, null, qr.text, false, null, false, false))
        true
      }
    }
  }
}
