package chat.simplex.app.views.chat.item

import androidx.compose.runtime.Composable
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.views.helpers.AlertManager
import chat.simplex.app.views.helpers.generalGetString
import com.icerockdev.library.MR

@Composable
fun CIRcvDecryptionError(msgDecryptError: MsgDecryptError, msgCount: UInt, ci: ChatItem, timedMessagesTTL: Int?, showMember: Boolean) {
  CIMsgError(ci, timedMessagesTTL, showMember) {
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.decryption_error),
      text = when (msgDecryptError) {
        MsgDecryptError.RatchetHeader -> String.format(generalGetString(MR.strings.alert_text_decryption_error_header), msgCount.toLong()) + "\n" +
            generalGetString(MR.strings.alert_text_fragment_encryption_out_of_sync_old_database) + "\n" +
            generalGetString(MR.strings.alert_text_fragment_permanent_error_reconnect)
        MsgDecryptError.TooManySkipped -> String.format(generalGetString(MR.strings.alert_text_decryption_error_too_many_skipped), msgCount.toLong()) + "\n" +
            generalGetString(MR.strings.alert_text_fragment_encryption_out_of_sync_old_database) + "\n" +
            generalGetString(MR.strings.alert_text_fragment_permanent_error_reconnect)
      }
    )
  }
}
