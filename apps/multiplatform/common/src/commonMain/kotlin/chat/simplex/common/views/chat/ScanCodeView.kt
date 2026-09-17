package chat.simplex.common.views.chat

import SectionBottomSpacer
import androidx.compose.foundation.layout.*
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import chat.simplex.common.model.ScannedLinkType
import chat.simplex.common.model.checkLink
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.QRCodeScanner
import chat.simplex.common.views.newchat.showWrongQRCodeAlert
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun ScanCodeView(verifyCode: suspend (String?) -> Boolean, close: () -> Unit) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.scan_code))
    QRCodeScanner { text ->
      val trimmed = text.trim()
      val type = checkLink(trimmed)
      if (type != null && type != ScannedLinkType.VerificationCode) {
        // valid SimpleX code of another kind: tell the user what it is
        showWrongQRCodeAlert(type)
        false
      } else {
        // a security code (or unrecognised text): run the existing verify path
        val success = verifyCode(trimmed)
        if (success) {
          close()
        } else {
          AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.incorrect_code))
        }
        success
      }
    }
    Text(stringResource(MR.strings.scan_code_from_contacts_app), Modifier.padding(horizontal = DEFAULT_PADDING))
    SectionBottomSpacer()
  }
}
