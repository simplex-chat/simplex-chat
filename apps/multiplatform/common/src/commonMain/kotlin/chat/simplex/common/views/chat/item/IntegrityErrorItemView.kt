package chat.simplex.common.views.chat.item

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatItem
import chat.simplex.common.model.MsgErrorType
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR

@Composable
fun IntegrityErrorItemView(msgError: MsgErrorType, ci: ChatItem, timedMessagesTTL: Int?) {
  CIMsgError(ci, timedMessagesTTL) {
    when (msgError) {
      is MsgErrorType.MsgSkipped ->
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.alert_title_skipped_messages),
          text = generalGetString(MR.strings.alert_text_skipped_messages_it_can_happen_when)
        )
      is MsgErrorType.MsgBadHash ->
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.alert_title_msg_bad_hash),
          text = generalGetString(MR.strings.alert_text_msg_bad_hash) + "\n" +
              generalGetString(MR.strings.alert_text_fragment_encryption_out_of_sync_old_database) + "\n" +
              generalGetString(MR.strings.alert_text_fragment_please_report_to_developers)
        )
      is MsgErrorType.MsgBadId, is MsgErrorType.MsgDuplicate ->
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.alert_title_msg_bad_id),
          text = generalGetString(MR.strings.alert_text_msg_bad_id) + "\n" +
              generalGetString(MR.strings.alert_text_fragment_please_report_to_developers)
        )
    }
  }
}

@Composable
fun CIMsgError(ci: ChatItem, timedMessagesTTL: Int?, onClick: () -> Unit) {
  val receivedColor = MaterialTheme.appColors.receivedMessage
  Surface(
    Modifier.clickable(onClick = onClick),
    shape = RoundedCornerShape(18.dp),
    color = receivedColor,
    contentColor = LocalContentColor.current
  ) {
    Row(
      Modifier.padding(horizontal = 12.dp, vertical = 6.dp),
      verticalAlignment = Alignment.Bottom
    ) {
      Text(
        buildAnnotatedString {
          withStyle(SpanStyle(fontStyle = FontStyle.Italic, color = Color.Red)) { append(ci.content.text) }
        },
        style = MaterialTheme.typography.body1.copy(lineHeight = 22.sp),
        modifier = Modifier.padding(end = 8.dp)
      )
      CIMetaView(ci, timedMessagesTTL)
    }
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)*/
@Composable
fun IntegrityErrorItemViewPreview() {
  SimpleXTheme {
    IntegrityErrorItemView(
      MsgErrorType.MsgBadHash(),
      ChatItem.getDeletedContentSampleData(),
      null
    )
  }
}
