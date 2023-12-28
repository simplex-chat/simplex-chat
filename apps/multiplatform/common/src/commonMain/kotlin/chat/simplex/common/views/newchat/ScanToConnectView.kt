package chat.simplex.common.views.newchat

import SectionBottomSpacer
import SectionItemView
import SectionTextFooter
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.buildAnnotatedString
import androidx.compose.ui.text.style.TextAlign
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.IncognitoView
import chat.simplex.res.MR
import java.net.URI

@Composable
expect fun ScanToConnectView(chatModel: ChatModel, rh: RemoteHostInfo?, close: () -> Unit)

enum class ConnectionLinkType {
  INVITATION, CONTACT, GROUP
}

suspend fun planAndConnect(
  chatModel: ChatModel,
  rhId: Long?,
  uri: URI,
  incognito: Boolean?,
  close: (() -> Unit)?
) {
  val connectionPlan = chatModel.controller.apiConnectPlan(rhId, uri.toString())
  if (connectionPlan != null) {
    when (connectionPlan) {
      is ConnectionPlan.InvitationLink -> when (connectionPlan.invitationLinkPlan) {
        InvitationLinkPlan.Ok -> {
          Log.d(TAG, "planAndConnect, .InvitationLink, .Ok, incognito=$incognito")
          if (incognito != null) {
            connectViaUri(chatModel, rhId, uri, incognito, connectionPlan, close)
          } else {
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, uri, connectionPlan, close,
              title = generalGetString(MR.strings.connect_via_invitation_link),
              text = AnnotatedString(generalGetString(MR.strings.profile_will_be_sent_to_contact_sending_link)),
              connectDestructive = false
            )
          }
        }
        InvitationLinkPlan.OwnLink -> {
          Log.d(TAG, "planAndConnect, .InvitationLink, .OwnLink, incognito=$incognito")
          if (incognito != null) {
            AlertManager.privacySensitive.showAlertDialog(
              title = generalGetString(MR.strings.connect_plan_connect_to_yourself),
              text = generalGetString(MR.strings.connect_plan_this_is_your_own_one_time_link),
              confirmText = if (incognito) generalGetString(MR.strings.connect_via_link_incognito) else generalGetString(MR.strings.connect_via_link_verb),
              onConfirm = { withApi { connectViaUri(chatModel, rhId, uri, incognito, connectionPlan, close) } },
              destructive = true,
              hostDevice = hostDevice(rhId),
            )
          } else {
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, uri, connectionPlan, close,
              title = generalGetString(MR.strings.connect_plan_connect_to_yourself),
              text = AnnotatedString(generalGetString(MR.strings.connect_plan_this_is_your_own_one_time_link)),
              connectDestructive = true
            )
          }
        }
        is InvitationLinkPlan.Connecting -> {
          Log.d(TAG, "planAndConnect, .InvitationLink, .Connecting, incognito=$incognito")
          val contact = connectionPlan.invitationLinkPlan.contact_
          if (contact != null) {
            openKnownContact(chatModel, rhId, close, contact)
            AlertManager.privacySensitive.showAlertMsg(
              generalGetString(MR.strings.contact_already_exists),
              String.format(generalGetString(MR.strings.connect_plan_you_are_already_connecting_to_vName), contact.displayName),
              hostDevice = hostDevice(rhId),
            )
          } else {
            AlertManager.privacySensitive.showAlertMsg(
              generalGetString(MR.strings.connect_plan_already_connecting),
              generalGetString(MR.strings.connect_plan_you_are_already_connecting_via_this_one_time_link),
              hostDevice = hostDevice(rhId),
            )
          }
        }
        is InvitationLinkPlan.Known -> {
          Log.d(TAG, "planAndConnect, .InvitationLink, .Known, incognito=$incognito")
          val contact = connectionPlan.invitationLinkPlan.contact
          openKnownContact(chatModel, rhId, close, contact)
          AlertManager.privacySensitive.showAlertMsg(
            generalGetString(MR.strings.contact_already_exists),
            String.format(generalGetString(MR.strings.you_are_already_connected_to_vName_via_this_link), contact.displayName),
            hostDevice = hostDevice(rhId),
          )
        }
      }
      is ConnectionPlan.ContactAddress -> when (connectionPlan.contactAddressPlan) {
        ContactAddressPlan.Ok -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .Ok, incognito=$incognito")
          if (incognito != null) {
            connectViaUri(chatModel, rhId, uri, incognito, connectionPlan, close)
          } else {
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, uri, connectionPlan, close,
              title = generalGetString(MR.strings.connect_via_contact_link),
              text = AnnotatedString(generalGetString(MR.strings.profile_will_be_sent_to_contact_sending_link)),
              connectDestructive = false
            )
          }
        }
        ContactAddressPlan.OwnLink -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .OwnLink, incognito=$incognito")
          if (incognito != null) {
            AlertManager.privacySensitive.showAlertDialog(
              title = generalGetString(MR.strings.connect_plan_connect_to_yourself),
              text = generalGetString(MR.strings.connect_plan_this_is_your_own_simplex_address),
              confirmText = if (incognito) generalGetString(MR.strings.connect_via_link_incognito) else generalGetString(MR.strings.connect_via_link_verb),
              onConfirm = { withApi { connectViaUri(chatModel, rhId, uri, incognito, connectionPlan, close) } },
              destructive = true,
              hostDevice = hostDevice(rhId),
            )
          } else {
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, uri, connectionPlan, close,
              title = generalGetString(MR.strings.connect_plan_connect_to_yourself),
              text = AnnotatedString(generalGetString(MR.strings.connect_plan_this_is_your_own_simplex_address)),
              connectDestructive = true
            )
          }
        }
        ContactAddressPlan.ConnectingConfirmReconnect -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .ConnectingConfirmReconnect, incognito=$incognito")
          if (incognito != null) {
            AlertManager.privacySensitive.showAlertDialog(
              title = generalGetString(MR.strings.connect_plan_repeat_connection_request),
              text = generalGetString(MR.strings.connect_plan_you_have_already_requested_connection_via_this_address),
              confirmText = if (incognito) generalGetString(MR.strings.connect_via_link_incognito) else generalGetString(MR.strings.connect_via_link_verb),
              onConfirm = { withApi { connectViaUri(chatModel, rhId, uri, incognito, connectionPlan, close) } },
              destructive = true,
              hostDevice = hostDevice(rhId),
            )
          } else {
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, uri, connectionPlan, close,
              title = generalGetString(MR.strings.connect_plan_repeat_connection_request),
              text = AnnotatedString(generalGetString(MR.strings.connect_plan_you_have_already_requested_connection_via_this_address)),
              connectDestructive = true
            )
          }
        }
        is ContactAddressPlan.ConnectingProhibit -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .ConnectingProhibit, incognito=$incognito")
          val contact = connectionPlan.contactAddressPlan.contact
          openKnownContact(chatModel, rhId, close, contact)
          AlertManager.privacySensitive.showAlertMsg(
            generalGetString(MR.strings.contact_already_exists),
            String.format(generalGetString(MR.strings.connect_plan_you_are_already_connecting_to_vName), contact.displayName),
            hostDevice = hostDevice(rhId),
          )
        }
        is ContactAddressPlan.Known -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .Known, incognito=$incognito")
          val contact = connectionPlan.contactAddressPlan.contact
          openKnownContact(chatModel, rhId, close, contact)
          AlertManager.privacySensitive.showAlertMsg(
            generalGetString(MR.strings.contact_already_exists),
            String.format(generalGetString(MR.strings.you_are_already_connected_to_vName_via_this_link), contact.displayName),
            hostDevice = hostDevice(rhId),
          )
        }
        is ContactAddressPlan.ContactViaAddress -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .ContactViaAddress, incognito=$incognito")
          val contact = connectionPlan.contactAddressPlan.contact
          if (incognito != null) {
            close?.invoke()
            connectContactViaAddress(chatModel, rhId, contact.contactId, incognito)
          } else {
            askCurrentOrIncognitoProfileConnectContactViaAddress(chatModel, rhId, contact, close, openChat = false)
          }
        }
      }
      is ConnectionPlan.GroupLink -> when (connectionPlan.groupLinkPlan) {
        GroupLinkPlan.Ok -> {
          Log.d(TAG, "planAndConnect, .GroupLink, .Ok, incognito=$incognito")
          if (incognito != null) {
            AlertManager.privacySensitive.showAlertDialog(
              title = generalGetString(MR.strings.connect_via_group_link),
              text = generalGetString(MR.strings.you_will_join_group),
              confirmText = if (incognito) generalGetString(MR.strings.join_group_incognito_button) else generalGetString(MR.strings.join_group_button),
              onConfirm = { withApi { connectViaUri(chatModel, rhId, uri, incognito, connectionPlan, close) } },
              hostDevice = hostDevice(rhId),
            )
          } else {
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, uri, connectionPlan, close,
              title = generalGetString(MR.strings.connect_via_group_link),
              text = AnnotatedString(generalGetString(MR.strings.you_will_join_group)),
              connectDestructive = false
            )
          }
        }
        is GroupLinkPlan.OwnLink -> {
          Log.d(TAG, "planAndConnect, .GroupLink, .OwnLink, incognito=$incognito")
          val groupInfo = connectionPlan.groupLinkPlan.groupInfo
          ownGroupLinkConfirmConnect(chatModel, rhId, uri, incognito, connectionPlan, groupInfo, close)
        }
        GroupLinkPlan.ConnectingConfirmReconnect -> {
          Log.d(TAG, "planAndConnect, .GroupLink, .ConnectingConfirmReconnect, incognito=$incognito")
          if (incognito != null) {
            AlertManager.privacySensitive.showAlertDialog(
              title = generalGetString(MR.strings.connect_plan_repeat_join_request),
              text = generalGetString(MR.strings.connect_plan_you_are_already_joining_the_group_via_this_link),
              confirmText = if (incognito) generalGetString(MR.strings.join_group_incognito_button) else generalGetString(MR.strings.join_group_button),
              onConfirm = { withApi { connectViaUri(chatModel, rhId, uri, incognito, connectionPlan, close) } },
              destructive = true,
              hostDevice = hostDevice(rhId),
            )
          } else {
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, uri, connectionPlan, close,
              title = generalGetString(MR.strings.connect_plan_repeat_join_request),
              text = AnnotatedString(generalGetString(MR.strings.connect_plan_you_are_already_joining_the_group_via_this_link)),
              connectDestructive = true
            )
          }
        }
        is GroupLinkPlan.ConnectingProhibit -> {
          Log.d(TAG, "planAndConnect, .GroupLink, .ConnectingProhibit, incognito=$incognito")
          val groupInfo = connectionPlan.groupLinkPlan.groupInfo_
          if (groupInfo != null) {
            AlertManager.privacySensitive.showAlertMsg(
              generalGetString(MR.strings.connect_plan_group_already_exists),
              String.format(generalGetString(MR.strings.connect_plan_you_are_already_joining_the_group_vName), groupInfo.displayName)
            )
          } else {
            AlertManager.privacySensitive.showAlertMsg(
              generalGetString(MR.strings.connect_plan_already_joining_the_group),
              generalGetString(MR.strings.connect_plan_you_are_already_joining_the_group_via_this_link),
              hostDevice = hostDevice(rhId),
            )
          }
        }
        is GroupLinkPlan.Known -> {
          Log.d(TAG, "planAndConnect, .GroupLink, .Known, incognito=$incognito")
          val groupInfo = connectionPlan.groupLinkPlan.groupInfo
          openKnownGroup(chatModel, rhId, close, groupInfo)
          AlertManager.privacySensitive.showAlertMsg(
            generalGetString(MR.strings.connect_plan_group_already_exists),
            String.format(generalGetString(MR.strings.connect_plan_you_are_already_in_group_vName), groupInfo.displayName),
            hostDevice = hostDevice(rhId),
          )
        }
      }
    }
  } else {
    Log.d(TAG, "planAndConnect, plan error")
    if (incognito != null) {
      connectViaUri(chatModel, rhId, uri, incognito, connectionPlan = null, close)
    } else {
      askCurrentOrIncognitoProfileAlert(
        chatModel, rhId, uri, connectionPlan = null, close,
        title = generalGetString(MR.strings.connect_plan_connect_via_link),
        connectDestructive = false
      )
    }
  }
}

suspend fun connectViaUri(
  chatModel: ChatModel,
  rhId: Long?,
  uri: URI,
  incognito: Boolean,
  connectionPlan: ConnectionPlan?,
  close: (() -> Unit)?
) {
  val pcc = chatModel.controller.apiConnect(rhId, incognito, uri.toString())
  val connLinkType = if (connectionPlan != null) planToConnectionLinkType(connectionPlan) else ConnectionLinkType.INVITATION
  if (pcc != null) {
    chatModel.updateContactConnection(rhId, pcc)
    close?.invoke()
    AlertManager.privacySensitive.showAlertMsg(
      title = generalGetString(MR.strings.connection_request_sent),
      text =
      when (connLinkType) {
        ConnectionLinkType.CONTACT -> generalGetString(MR.strings.you_will_be_connected_when_your_connection_request_is_accepted)
        ConnectionLinkType.INVITATION -> generalGetString(MR.strings.you_will_be_connected_when_your_contacts_device_is_online)
        ConnectionLinkType.GROUP -> generalGetString(MR.strings.you_will_be_connected_when_group_host_device_is_online)
      },
      hostDevice = hostDevice(rhId),
    )
  }
}

fun planToConnectionLinkType(connectionPlan: ConnectionPlan): ConnectionLinkType {
  return when(connectionPlan) {
    is ConnectionPlan.InvitationLink -> ConnectionLinkType.INVITATION
    is ConnectionPlan.ContactAddress -> ConnectionLinkType.CONTACT
    is ConnectionPlan.GroupLink -> ConnectionLinkType.GROUP
  }
}

fun askCurrentOrIncognitoProfileAlert(
  chatModel: ChatModel,
  rhId: Long?,
  uri: URI,
  connectionPlan: ConnectionPlan?,
  close: (() -> Unit)?,
  title: String,
  text: AnnotatedString? = null,
  connectDestructive: Boolean,
) {
  AlertManager.privacySensitive.showAlertDialogButtonsColumn(
    title = title,
    text = text,
    buttons = {
      Column {
        val connectColor = if (connectDestructive) MaterialTheme.colors.error else MaterialTheme.colors.primary
        SectionItemView({
          AlertManager.privacySensitive.hideAlert()
          withApi {
            connectViaUri(chatModel, rhId, uri, incognito = false, connectionPlan, close)
          }
        }) {
          Text(generalGetString(MR.strings.connect_use_current_profile), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = connectColor)
        }
        SectionItemView({
          AlertManager.privacySensitive.hideAlert()
          withApi {
            connectViaUri(chatModel, rhId, uri, incognito = true, connectionPlan, close)
          }
        }) {
          Text(generalGetString(MR.strings.connect_use_new_incognito_profile), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = connectColor)
        }
        SectionItemView({
          AlertManager.privacySensitive.hideAlert()
        }) {
          Text(stringResource(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
      }
    },
    hostDevice = hostDevice(rhId),
  )
}

fun openKnownContact(chatModel: ChatModel, rhId: Long?, close: (() -> Unit)?, contact: Contact) {
  withApi {
    val c = chatModel.getContactChat(contact.contactId)
    if (c != null) {
      close?.invoke()
      openDirectChat(rhId, contact.contactId, chatModel)
    }
  }
}

fun ownGroupLinkConfirmConnect(
  chatModel: ChatModel,
  rhId: Long?,
  uri: URI,
  incognito: Boolean?,
  connectionPlan: ConnectionPlan?,
  groupInfo: GroupInfo,
  close: (() -> Unit)?,
) {
  AlertManager.privacySensitive.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.connect_plan_join_your_group),
    text = AnnotatedString(String.format(generalGetString(MR.strings.connect_plan_this_is_your_link_for_group_vName), groupInfo.displayName)),
    buttons = {
      Column {
        // Open group
        SectionItemView({
          AlertManager.privacySensitive.hideAlert()
          openKnownGroup(chatModel, rhId, close, groupInfo)
        }) {
          Text(generalGetString(MR.strings.connect_plan_open_group), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        if (incognito != null) {
          // Join incognito / Join with current profile
          SectionItemView({
            AlertManager.privacySensitive.hideAlert()
            withApi {
              connectViaUri(chatModel, rhId, uri, incognito, connectionPlan, close)
            }
          }) {
            Text(
              if (incognito) generalGetString(MR.strings.join_group_incognito_button) else generalGetString(MR.strings.join_group_button),
              Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.error
            )
          }
        } else {
          // Use current profile
          SectionItemView({
            AlertManager.privacySensitive.hideAlert()
            withApi {
              connectViaUri(chatModel, rhId, uri, incognito = false, connectionPlan, close)
            }
          }) {
            Text(generalGetString(MR.strings.connect_use_current_profile), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.error)
          }
          // Use new incognito profile
          SectionItemView({
            AlertManager.privacySensitive.hideAlert()
            withApi {
              connectViaUri(chatModel, rhId, uri, incognito = true, connectionPlan, close)
            }
          }) {
            Text(generalGetString(MR.strings.connect_use_new_incognito_profile), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.error)
          }
        }
        // Cancel
        SectionItemView({
          AlertManager.privacySensitive.hideAlert()
        }) {
          Text(stringResource(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
      }
    },
    hostDevice = hostDevice(rhId),
  )
}

fun openKnownGroup(chatModel: ChatModel, rhId: Long?, close: (() -> Unit)?, groupInfo: GroupInfo) {
  withApi {
    val g = chatModel.getGroupChat(groupInfo.groupId)
    if (g != null) {
      close?.invoke()
      openGroupChat(rhId, groupInfo.groupId, chatModel)
    }
  }
}

@Composable
fun ConnectContactLayout(
  chatModel: ChatModel,
  rh: RemoteHostInfo?,
  incognitoPref: SharedPreference<Boolean>,
  close: () -> Unit
) {
  val incognito = remember { mutableStateOf(incognitoPref.get()) }

  @Composable
  fun QRCodeScanner(close: () -> Unit) {
    QRCodeScanner { connReqUri ->
      try {
        val uri = URI(connReqUri)
        withApi {
          planAndConnect(chatModel, rh?.remoteHostId, uri, incognito = incognito.value, close)
        }
      } catch (e: RuntimeException) {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.invalid_QR_code),
          text = generalGetString(MR.strings.this_QR_code_is_not_a_link)
        )
      }
    }
  }

  Column(
    Modifier.verticalScroll(rememberScrollState()).padding(horizontal = DEFAULT_PADDING),
    verticalArrangement = Arrangement.SpaceBetween
  ) {
    AppBarTitle(stringResource(MR.strings.scan_QR_code), hostDevice(rh?.remoteHostId), withPadding = false)
    Box(
      Modifier
        .fillMaxWidth()
        .aspectRatio(ratio = 1F)
        .padding(bottom = 12.dp)
    ) { QRCodeScanner(close) }

    IncognitoToggle(incognitoPref, incognito) { ModalManager.start.showModal { IncognitoView() } }

    SectionTextFooter(
      buildAnnotatedString {
        append(sharedProfileInfo(chatModel, incognito.value))
        append("\n\n")
        append(annotatedStringResource(MR.strings.if_you_cannot_meet_in_person_scan_QR_in_video_call_or_ask_for_invitation_link))
      }
    )

    SectionBottomSpacer()
  }
}

fun URI.getQueryParameter(param: String): String? {
  if (!query.contains("$param=")) return null
  return query.substringAfter("$param=").substringBefore("&")
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewConnectContactLayout() {
  SimpleXTheme {
    ConnectContactLayout(
      chatModel = ChatModel,
      rh = null,
      incognitoPref = SharedPreference({ false }, {}),
      close = {},
    )
  }
}
