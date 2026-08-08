package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource

// Giving a name to a contact.
//
// Only contacts whose profile carries a receiving address can be chosen: the
// name is sent to a one-time address derived from it, which is what keeps the
// transfer from linking the recipient to the name for everyone watching. A
// contact on an older version, or one who has not turned receiving on, simply
// has nowhere for it to land.
@Composable
fun GiveNameView(rhId: Long?, fqdn: String, close: () -> Unit) {
  val busy = remember { mutableStateOf(false) }
  val label = fqdn.removeSuffix(".simplex")

  val candidates = chatModel.chats.value.mapNotNull { c ->
    val ci = c.chatInfo
    if (ci is ChatInfo.Direct) ci.contact else null
  }
  val canReceive = candidates.filter { it.profile.metaAddress != null }
  val cannotReceive = candidates.size - canReceive.size

  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.names_give_title))
    SectionTextFooter(stringResource(MR.strings.names_give_intro).format(fqdn))

    SectionView(stringResource(MR.strings.names_give_choose).uppercase()) {
      if (canReceive.isEmpty()) {
        SectionItemView { Text(stringResource(MR.strings.names_give_nobody), color = MaterialTheme.colors.secondary) }
      } else {
        canReceive.forEach { ct ->
          SectionItemView(click = {
            if (!busy.value) confirmGive(rhId, label, fqdn, ct) { close() }
          }, disabled = busy.value) {
            Text(ct.profile.profileViewName)
          }
        }
      }
    }
    SectionTextFooter(
      if (cannotReceive > 0) stringResource(MR.strings.names_give_some_cannot).format(cannotReceive)
      else stringResource(MR.strings.names_give_footer)
    )
    SectionBottomSpacer()
  }
}

private fun confirmGive(rhId: Long?, label: String, fqdn: String, ct: Contact, onDone: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.names_give_confirm_title),
    // Irreversible without their cooperation: after this they own it, not you.
    text = generalGetString(MR.strings.names_give_confirm_text).format(fqdn, ct.profile.profileViewName),
    confirmText = generalGetString(MR.strings.names_give_action),
    destructive = true,
    onConfirm = {
      withBGApi {
        if (chatModel.controller.apiNameGift(rhId, label, "@${ct.profile.displayName}")) {
          AlertManager.shared.showAlertMsg(
            title = generalGetString(MR.strings.names_give_sent_title),
            text = generalGetString(MR.strings.names_give_sent_text).format(fqdn, ct.profile.profileViewName),
          )
          onDone()
        }
      }
    }
  )
}
