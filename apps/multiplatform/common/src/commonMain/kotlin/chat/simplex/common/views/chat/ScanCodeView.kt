package chat.simplex.common.views.chat

import SectionBottomSpacer
import androidx.compose.foundation.layout.*
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.QRCodeScanner
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun ScanCodeView(verifyCode: suspend (String?) -> Boolean, close: () -> Unit) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.scan_code))
    QRCodeScanner { text ->
      val success  = verifyCode(text)
      if (success) {
        close()
      } else {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.incorrect_code)
        )
      }
      success
    }
    Text(stringResource(MR.strings.scan_code_from_contacts_app), Modifier.padding(horizontal = DEFAULT_PADDING))
    SectionBottomSpacer()
  }
}
