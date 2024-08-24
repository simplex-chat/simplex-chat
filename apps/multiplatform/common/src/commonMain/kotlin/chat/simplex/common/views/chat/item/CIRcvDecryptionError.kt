package chat.simplex.common.views.chat.item

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.ui.theme.appColors
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun CIRcvDecryptionError(
  msgDecryptError: MsgDecryptError,
  msgCount: UInt,
  cInfo: ChatInfo,
  ci: ChatItem,
  updateContactStats: (Contact) -> Unit,
  updateMemberStats: (GroupInfo, GroupMember) -> Unit,
  syncContactConnection: (Contact) -> Unit,
  syncMemberConnection: (GroupInfo, GroupMember) -> Unit,
  findModelChat: (String) -> Chat?,
  findModelMember: (String) -> GroupMember?,
) {
  LaunchedEffect(Unit) {
    if (cInfo is ChatInfo.Direct) {
      updateContactStats(cInfo.contact)
    } else if (cInfo is ChatInfo.Group && ci.chatDir is CIDirection.GroupRcv) {
      updateMemberStats(cInfo.groupInfo, ci.chatDir.groupMember)
    }
  }

  @Composable
  fun BasicDecryptionErrorItem() {
    DecryptionErrorItem(
      ci,
      onClick = {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.decryption_error),
          text = alertMessage(msgDecryptError, msgCount)
        )
      }
    )
  }

  if (cInfo is ChatInfo.Direct) {
    val modelCInfo = findModelChat(cInfo.id)?.chatInfo
    if (modelCInfo is ChatInfo.Direct) {
      val modelContactStats = modelCInfo.contact.activeConn?.connectionStats
      if (modelContactStats != null) {
        if (modelContactStats.ratchetSyncAllowed) {
          DecryptionErrorItemFixButton(
            ci,
            onClick = {
              AlertManager.shared.showAlertDialog(
                title = generalGetString(MR.strings.fix_connection_question),
                text = alertMessage(msgDecryptError, msgCount),
                confirmText = generalGetString(MR.strings.fix_connection_confirm),
                onConfirm = { syncContactConnection(cInfo.contact) },
              )
            },
            syncSupported = true
          )
        } else if (!modelContactStats.ratchetSyncSupported) {
          DecryptionErrorItemFixButton(
            ci,
            onClick = {
              AlertManager.shared.showAlertMsg(
                title = generalGetString(MR.strings.fix_connection_not_supported_by_contact),
                text = alertMessage(msgDecryptError, msgCount)
              )
            },
            syncSupported = false
          )
        } else {
          BasicDecryptionErrorItem()
        }
      } else {
        BasicDecryptionErrorItem()
      }
    } else {
      BasicDecryptionErrorItem()
    }
  } else if (cInfo is ChatInfo.Group && ci.chatDir is CIDirection.GroupRcv) {
    val modelMember = findModelMember(ci.chatDir.groupMember.id)
    val modelMemberStats = modelMember?.activeConn?.connectionStats
    if (modelMemberStats != null) {
      if (modelMemberStats.ratchetSyncAllowed) {
        DecryptionErrorItemFixButton(
          ci,
          onClick = {
            AlertManager.shared.showAlertDialog(
              title = generalGetString(MR.strings.fix_connection_question),
              text = alertMessage(msgDecryptError, msgCount),
              confirmText = generalGetString(MR.strings.fix_connection_confirm),
              onConfirm = { syncMemberConnection(cInfo.groupInfo, modelMember) },
            )
          },
          syncSupported = true
        )
      } else if (!modelMemberStats.ratchetSyncSupported) {
        DecryptionErrorItemFixButton(
          ci,
          onClick = {
            AlertManager.shared.showAlertMsg(
              title = generalGetString(MR.strings.fix_connection_not_supported_by_group_member),
              text = alertMessage(msgDecryptError, msgCount)
            )
          },
          syncSupported = false
        )
      } else {
        BasicDecryptionErrorItem()
      }
    } else {
      BasicDecryptionErrorItem()
    }
  } else {
    BasicDecryptionErrorItem()
  }
}

@Composable
fun DecryptionErrorItemFixButton(
  ci: ChatItem,
  onClick: () -> Unit,
  syncSupported: Boolean
) {
  val receivedColor = MaterialTheme.appColors.receivedMessage
  Surface(
    Modifier.clickable(onClick = onClick),
    shape = RoundedCornerShape(18.dp),
    color = receivedColor,
    contentColor = LocalContentColor.current
  ) {
    Box(
      Modifier.padding(vertical = 6.dp, horizontal = 12.dp),
      contentAlignment = Alignment.BottomEnd,
    ) {
      Column(
        verticalArrangement = Arrangement.spacedBy(2.dp)
      ) {
        Text(
          buildAnnotatedString {
            withStyle(SpanStyle(fontStyle = FontStyle.Italic, color = Color.Red)) { append(ci.content.text) }
          },
          style = MaterialTheme.typography.body1.copy(lineHeight = 22.sp)
        )
        Row {
          Icon(
            painterResource(MR.images.ic_sync_problem),
            stringResource(MR.strings.fix_connection),
            tint = if (syncSupported) MaterialTheme.colors.primary else MaterialTheme.colors.secondary
          )
          Spacer(Modifier.padding(2.dp))
          val secondaryColor = MaterialTheme.colors.secondary
          Text(
            buildAnnotatedString {
              append(generalGetString(MR.strings.fix_connection))
              withStyle(reserveTimestampStyle) { append(reserveSpaceForMeta(ci.meta, null, encrypted = null, secondaryColor = secondaryColor)) }
              withStyle(reserveTimestampStyle) { append("    ") } // for icon
            },
            color = if (syncSupported) MaterialTheme.colors.primary else MaterialTheme.colors.secondary
          )
        }
      }
      CIMetaView(ci, timedMessagesTTL = null, showViaProxy = false)
    }
  }
}

@Composable
fun DecryptionErrorItem(
  ci: ChatItem,
  onClick: () -> Unit
) {
  val receivedColor = MaterialTheme.appColors.receivedMessage
  Surface(
    Modifier.clickable(onClick = onClick),
    shape = RoundedCornerShape(18.dp),
    color = receivedColor,
    contentColor = LocalContentColor.current
  ) {
    Box(
      Modifier.padding(vertical = 6.dp, horizontal = 12.dp),
      contentAlignment = Alignment.BottomEnd,
    ) {
      val secondaryColor = MaterialTheme.colors.secondary
      Text(
        buildAnnotatedString {
          withStyle(SpanStyle(fontStyle = FontStyle.Italic, color = Color.Red)) { append(ci.content.text) }
          withStyle(reserveTimestampStyle) { append(reserveSpaceForMeta(ci.meta, null, encrypted = null, secondaryColor = secondaryColor)) }
        },
        style = MaterialTheme.typography.body1.copy(lineHeight = 22.sp)
      )
      CIMetaView(ci, timedMessagesTTL = null, showViaProxy = false)
    }
  }
}

private fun alertMessage(msgDecryptError: MsgDecryptError, msgCount: UInt): String {
  return when (msgDecryptError) {
    MsgDecryptError.RatchetHeader -> String.format(generalGetString(MR.strings.alert_text_decryption_error_n_messages_failed_to_decrypt), msgCount.toLong()) + "\n" +
        generalGetString(MR.strings.alert_text_fragment_encryption_out_of_sync_old_database)

    MsgDecryptError.TooManySkipped -> String.format(generalGetString(MR.strings.alert_text_decryption_error_too_many_skipped), msgCount.toLong()) + "\n" +
        generalGetString(MR.strings.alert_text_fragment_encryption_out_of_sync_old_database)

    MsgDecryptError.RatchetEarlier -> String.format(generalGetString(MR.strings.alert_text_decryption_error_n_messages_failed_to_decrypt), msgCount.toLong()) + "\n" +
        generalGetString(MR.strings.alert_text_fragment_encryption_out_of_sync_old_database)

    MsgDecryptError.Other -> String.format(generalGetString(MR.strings.alert_text_decryption_error_n_messages_failed_to_decrypt), msgCount.toLong()) + "\n" +
        generalGetString(MR.strings.alert_text_fragment_encryption_out_of_sync_old_database)

    MsgDecryptError.RatchetSync -> generalGetString(MR.strings.alert_text_encryption_renegotiation_failed)
  }
}
