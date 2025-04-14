package chat.simplex.common.views.newchat

import SectionItemView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.style.TextAlign
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlinx.coroutines.*

enum class ConnectionLinkType {
  INVITATION, CONTACT, GROUP
}

suspend fun planAndConnect(
  rhId: Long?,
  shortOrFullLink: String,
  incognito: Boolean?,
  close: (() -> Unit)?,
  cleanup: (() -> Unit)? = null,
  filterKnownContact: ((Contact) -> Unit)? = null,
  filterKnownGroup: ((GroupInfo) -> Unit)? = null,
): CompletableDeferred<Boolean> {
  val completable = CompletableDeferred<Boolean>()
  val close: (() -> Unit) = {
    close?.invoke()
    // if close was called, it means the connection was created
    completable.complete(true)
  }
  val cleanup: (() -> Unit) = {
    cleanup?.invoke()
    completable.complete(!completable.isActive)
  }
  val result = chatModel.controller.apiConnectPlan(rhId, shortOrFullLink)
  if (result != null) {
    val (connectionLink, connectionPlan) = result
    val link = strHasSingleSimplexLink(shortOrFullLink.trim())
    val linkText = if (link?.format is Format.SimplexLink)
      "<br><br><u>${link.simplexLinkText(link.format.linkType, link.format.smpHosts)}</u>"
    else
      ""
    when (connectionPlan) {
      is ConnectionPlan.InvitationLink -> when (connectionPlan.invitationLinkPlan) {
        InvitationLinkPlan.Ok -> {
          Log.d(TAG, "planAndConnect, .InvitationLink, .Ok, incognito=$incognito")
          if (incognito != null) {
            connectViaUri(chatModel, rhId, connectionLink, incognito, connectionPlan, close, cleanup)
          } else {
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, connectionLink, connectionPlan, close,
              title = generalGetString(MR.strings.connect_via_invitation_link),
              text = generalGetString(MR.strings.profile_will_be_sent_to_contact_sending_link) + linkText,
              connectDestructive = false,
              cleanup = cleanup,
            )
          }
        }
        InvitationLinkPlan.OwnLink -> {
          Log.d(TAG, "planAndConnect, .InvitationLink, .OwnLink, incognito=$incognito")
          if (incognito != null) {
            AlertManager.privacySensitive.showAlertDialog(
              title = generalGetString(MR.strings.connect_plan_connect_to_yourself),
              text = generalGetString(MR.strings.connect_plan_this_is_your_own_one_time_link) + linkText,
              confirmText = if (incognito) generalGetString(MR.strings.connect_via_link_incognito) else generalGetString(MR.strings.connect_via_link_verb),
              onConfirm = { withBGApi { connectViaUri(chatModel, rhId, connectionLink, incognito, connectionPlan, close, cleanup) } },
              onDismiss = cleanup,
              onDismissRequest = cleanup,
              destructive = true,
              hostDevice = hostDevice(rhId),
            )
          } else {
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, connectionLink, connectionPlan, close,
              title = generalGetString(MR.strings.connect_plan_connect_to_yourself),
              text = generalGetString(MR.strings.connect_plan_this_is_your_own_one_time_link) + linkText,
              connectDestructive = true,
              cleanup = cleanup,
            )
          }
        }
        is InvitationLinkPlan.Connecting -> {
          Log.d(TAG, "planAndConnect, .InvitationLink, .Connecting, incognito=$incognito")
          val contact = connectionPlan.invitationLinkPlan.contact_
          if (contact != null) {
            if (filterKnownContact != null) {
              filterKnownContact(contact)
            } else {
              openKnownContact(chatModel, rhId, close, contact)
              AlertManager.privacySensitive.showAlertMsg(
                generalGetString(MR.strings.contact_already_exists),
                String.format(generalGetString(MR.strings.connect_plan_you_are_already_connecting_to_vName), contact.displayName) + linkText,
                hostDevice = hostDevice(rhId),
              )
              cleanup()
            }
          } else {
            AlertManager.privacySensitive.showAlertMsg(
              generalGetString(MR.strings.connect_plan_already_connecting),
              generalGetString(MR.strings.connect_plan_you_are_already_connecting_via_this_one_time_link) + linkText,
              hostDevice = hostDevice(rhId),
            )
            cleanup()
          }
        }
        is InvitationLinkPlan.Known -> {
          Log.d(TAG, "planAndConnect, .InvitationLink, .Known, incognito=$incognito")
          val contact = connectionPlan.invitationLinkPlan.contact
          if (filterKnownContact != null) {
            filterKnownContact(contact)
          } else {
            openKnownContact(chatModel, rhId, close, contact)
            AlertManager.privacySensitive.showAlertMsg(
              generalGetString(MR.strings.contact_already_exists),
              String.format(generalGetString(MR.strings.you_are_already_connected_to_vName_via_this_link), contact.displayName) + linkText,
              hostDevice = hostDevice(rhId),
            )
            cleanup()
          }
        }
      }
      is ConnectionPlan.ContactAddress -> when (connectionPlan.contactAddressPlan) {
        ContactAddressPlan.Ok -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .Ok, incognito=$incognito")
          if (incognito != null) {
            connectViaUri(chatModel, rhId, connectionLink, incognito, connectionPlan, close, cleanup)
          } else {
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, connectionLink, connectionPlan, close,
              title = generalGetString(MR.strings.connect_via_contact_link),
              text = generalGetString(MR.strings.profile_will_be_sent_to_contact_sending_link) + linkText,
              connectDestructive = false,
              cleanup,
            )
          }
        }
        ContactAddressPlan.OwnLink -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .OwnLink, incognito=$incognito")
          if (incognito != null) {
            AlertManager.privacySensitive.showAlertDialog(
              title = generalGetString(MR.strings.connect_plan_connect_to_yourself),
              text = generalGetString(MR.strings.connect_plan_this_is_your_own_simplex_address) + linkText,
              confirmText = if (incognito) generalGetString(MR.strings.connect_via_link_incognito) else generalGetString(MR.strings.connect_via_link_verb),
              onConfirm = { withBGApi { connectViaUri(chatModel, rhId, connectionLink, incognito, connectionPlan, close, cleanup) } },
              destructive = true,
              onDismiss = cleanup,
              onDismissRequest = cleanup,
              hostDevice = hostDevice(rhId),
            )
          } else {
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, connectionLink, connectionPlan, close,
              title = generalGetString(MR.strings.connect_plan_connect_to_yourself),
              text = generalGetString(MR.strings.connect_plan_this_is_your_own_simplex_address) + linkText,
              connectDestructive = true,
              cleanup = cleanup,
            )
          }
        }
        ContactAddressPlan.ConnectingConfirmReconnect -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .ConnectingConfirmReconnect, incognito=$incognito")
          if (incognito != null) {
            AlertManager.privacySensitive.showAlertDialog(
              title = generalGetString(MR.strings.connect_plan_repeat_connection_request),
              text = generalGetString(MR.strings.connect_plan_you_have_already_requested_connection_via_this_address) + linkText,
              confirmText = if (incognito) generalGetString(MR.strings.connect_via_link_incognito) else generalGetString(MR.strings.connect_via_link_verb),
              onConfirm = { withBGApi { connectViaUri(chatModel, rhId, connectionLink, incognito, connectionPlan, close, cleanup) } },
              onDismiss = cleanup,
              onDismissRequest = cleanup,
              destructive = true,
              hostDevice = hostDevice(rhId),
            )
          } else {
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, connectionLink, connectionPlan, close,
              title = generalGetString(MR.strings.connect_plan_repeat_connection_request),
              text = generalGetString(MR.strings.connect_plan_you_have_already_requested_connection_via_this_address) + linkText,
              connectDestructive = true,
              cleanup = cleanup,
            )
          }
        }
        is ContactAddressPlan.ConnectingProhibit -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .ConnectingProhibit, incognito=$incognito")
          val contact = connectionPlan.contactAddressPlan.contact
          if (filterKnownContact != null) {
            filterKnownContact(contact)
          } else {
            openKnownContact(chatModel, rhId, close, contact)
            AlertManager.privacySensitive.showAlertMsg(
              generalGetString(MR.strings.contact_already_exists),
              String.format(generalGetString(MR.strings.connect_plan_you_are_already_connecting_to_vName), contact.displayName) + linkText,
              hostDevice = hostDevice(rhId),
            )
            cleanup()
          }
        }
        is ContactAddressPlan.Known -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .Known, incognito=$incognito")
          val contact = connectionPlan.contactAddressPlan.contact
          if (filterKnownContact != null) {
            filterKnownContact(contact)
          } else {
            openKnownContact(chatModel, rhId, close, contact)
            AlertManager.privacySensitive.showAlertMsg(
              generalGetString(MR.strings.contact_already_exists),
              String.format(generalGetString(MR.strings.you_are_already_connected_to_vName_via_this_link), contact.displayName) + linkText,
              hostDevice = hostDevice(rhId),
            )
            cleanup()
          }
        }
        is ContactAddressPlan.ContactViaAddress -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .ContactViaAddress, incognito=$incognito")
          val contact = connectionPlan.contactAddressPlan.contact
          if (incognito != null) {
            close()
            connectContactViaAddress(chatModel, rhId, contact.contactId, incognito)
          } else {
            askCurrentOrIncognitoProfileConnectContactViaAddress(chatModel, rhId, contact, close, openChat = false)
          }
          cleanup()
        }
      }
      is ConnectionPlan.GroupLink -> when (connectionPlan.groupLinkPlan) {
        GroupLinkPlan.Ok -> {
          Log.d(TAG, "planAndConnect, .GroupLink, .Ok, incognito=$incognito")
          if (incognito != null) {
            AlertManager.privacySensitive.showAlertDialog(
              title = generalGetString(MR.strings.connect_via_group_link),
              text = generalGetString(MR.strings.you_will_join_group) + linkText,
              confirmText = if (incognito) generalGetString(MR.strings.join_group_incognito_button) else generalGetString(MR.strings.join_group_button),
              onConfirm = { withBGApi { connectViaUri(chatModel, rhId, connectionLink, incognito, connectionPlan, close, cleanup) } },
              onDismiss = cleanup,
              onDismissRequest = cleanup,
              hostDevice = hostDevice(rhId),
            )
          } else {
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, connectionLink, connectionPlan, close,
              title = generalGetString(MR.strings.connect_via_group_link),
              text = generalGetString(MR.strings.you_will_join_group) + linkText,
              connectDestructive = false,
              cleanup = cleanup,
            )
          }
        }
        is GroupLinkPlan.OwnLink -> {
          Log.d(TAG, "planAndConnect, .GroupLink, .OwnLink, incognito=$incognito")
          val groupInfo = connectionPlan.groupLinkPlan.groupInfo
          if (filterKnownGroup != null) {
            filterKnownGroup(groupInfo)
          } else {
            ownGroupLinkConfirmConnect(chatModel, rhId, connectionLink, linkText, incognito, connectionPlan, groupInfo, close, cleanup)
          }
        }
        GroupLinkPlan.ConnectingConfirmReconnect -> {
          Log.d(TAG, "planAndConnect, .GroupLink, .ConnectingConfirmReconnect, incognito=$incognito")
          if (incognito != null) {
            AlertManager.privacySensitive.showAlertDialog(
              title = generalGetString(MR.strings.connect_plan_repeat_join_request),
              text = generalGetString(MR.strings.connect_plan_you_are_already_joining_the_group_via_this_link) + linkText,
              confirmText = if (incognito) generalGetString(MR.strings.join_group_incognito_button) else generalGetString(MR.strings.join_group_button),
              onConfirm = { withBGApi { connectViaUri(chatModel, rhId, connectionLink, incognito, connectionPlan, close, cleanup) } },
              onDismiss = cleanup,
              onDismissRequest = cleanup,
              destructive = true,
              hostDevice = hostDevice(rhId),
            )
          } else {
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, connectionLink, connectionPlan, close,
              title = generalGetString(MR.strings.connect_plan_repeat_join_request),
              text = generalGetString(MR.strings.connect_plan_you_are_already_joining_the_group_via_this_link) + linkText,
              connectDestructive = true,
              cleanup = cleanup,
            )
          }
        }
        is GroupLinkPlan.ConnectingProhibit -> {
          Log.d(TAG, "planAndConnect, .GroupLink, .ConnectingProhibit, incognito=$incognito")
          val groupInfo = connectionPlan.groupLinkPlan.groupInfo_
          if (groupInfo != null) {
            if (groupInfo.businessChat == null) {
              AlertManager.privacySensitive.showAlertMsg(
                generalGetString(MR.strings.connect_plan_group_already_exists),
                String.format(generalGetString(MR.strings.connect_plan_you_are_already_joining_the_group_vName), groupInfo.displayName) + linkText
              )
            } else {
              AlertManager.privacySensitive.showAlertMsg(
                generalGetString(MR.strings.connect_plan_chat_already_exists),
                String.format(generalGetString(MR.strings.connect_plan_you_are_already_connecting_to_vName), groupInfo.displayName) + linkText
              )
            }
          } else {
            AlertManager.privacySensitive.showAlertMsg(
              generalGetString(MR.strings.connect_plan_already_joining_the_group),
              generalGetString(MR.strings.connect_plan_you_are_already_joining_the_group_via_this_link) + linkText,
              hostDevice = hostDevice(rhId),
            )
          }
          cleanup()
        }
        is GroupLinkPlan.Known -> {
          Log.d(TAG, "planAndConnect, .GroupLink, .Known, incognito=$incognito")
          val groupInfo = connectionPlan.groupLinkPlan.groupInfo
          if (filterKnownGroup != null) {
            filterKnownGroup(groupInfo)
          } else {
            openKnownGroup(chatModel, rhId, close, groupInfo)
            if (groupInfo.businessChat == null) {
              AlertManager.privacySensitive.showAlertMsg(
                generalGetString(MR.strings.connect_plan_group_already_exists),
                String.format(generalGetString(MR.strings.connect_plan_you_are_already_in_group_vName), groupInfo.displayName) + linkText,
                hostDevice = hostDevice(rhId),
              )
            } else {
              AlertManager.privacySensitive.showAlertMsg(
                generalGetString(MR.strings.connect_plan_chat_already_exists),
                String.format(generalGetString(MR.strings.connect_plan_you_are_already_connected_with_vName), groupInfo.displayName) + linkText,
                hostDevice = hostDevice(rhId),
              )
            }
            cleanup()
          }
        }
      }
      is ConnectionPlan.Error -> {
        Log.d(TAG, "planAndConnect, error ${connectionPlan.chatError}")
        if (incognito != null) {
          connectViaUri(chatModel, rhId, connectionLink, incognito, connectionPlan = null, close, cleanup)
        } else {
          askCurrentOrIncognitoProfileAlert(
            chatModel, rhId, connectionLink, connectionPlan = null, close,
            title = generalGetString(MR.strings.connect_plan_connect_via_link),
            connectDestructive = false,
            cleanup = cleanup,
          )
        }
      }
    }
  }
  return completable
}

suspend fun connectViaUri(
  chatModel: ChatModel,
  rhId: Long?,
  connLink: CreatedConnLink,
  incognito: Boolean,
  connectionPlan: ConnectionPlan?,
  close: (() -> Unit)?,
  cleanup: (() -> Unit)?,
): Boolean {
  val pcc = chatModel.controller.apiConnect(rhId, incognito, connLink)
  val connLinkType = if (connectionPlan != null) planToConnectionLinkType(connectionPlan) ?: ConnectionLinkType.INVITATION else ConnectionLinkType.INVITATION
  if (pcc != null) {
    withContext(Dispatchers.Main) {
      chatModel.chatsContext.updateContactConnection(rhId, pcc)
    }
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
  cleanup?.invoke()
  return pcc != null
}

fun planToConnectionLinkType(connectionPlan: ConnectionPlan): ConnectionLinkType? {
  return when(connectionPlan) {
    is ConnectionPlan.InvitationLink -> ConnectionLinkType.INVITATION
    is ConnectionPlan.ContactAddress -> ConnectionLinkType.CONTACT
    is ConnectionPlan.GroupLink -> ConnectionLinkType.GROUP
    is ConnectionPlan.Error -> null
  }
}

fun askCurrentOrIncognitoProfileAlert(
  chatModel: ChatModel,
  rhId: Long?,
  connectionLink: CreatedConnLink,
  connectionPlan: ConnectionPlan?,
  close: (() -> Unit)?,
  title: String,
  text: String? = null,
  connectDestructive: Boolean,
  cleanup: (() -> Unit)?,
) {
  AlertManager.privacySensitive.showAlertDialogButtonsColumn(
    title = title,
    text = text,
    buttons = {
      Column {
        val connectColor = if (connectDestructive) MaterialTheme.colors.error else MaterialTheme.colors.primary
        SectionItemView({
          AlertManager.privacySensitive.hideAlert()
          withBGApi {
            connectViaUri(chatModel, rhId, connectionLink, incognito = false, connectionPlan, close, cleanup)
          }
        }) {
          Text(generalGetString(MR.strings.connect_use_current_profile), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = connectColor)
        }
        SectionItemView({
          AlertManager.privacySensitive.hideAlert()
          withBGApi {
            connectViaUri(chatModel, rhId, connectionLink, incognito = true, connectionPlan, close, cleanup)
          }
        }) {
          Text(generalGetString(MR.strings.connect_use_new_incognito_profile), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = connectColor)
        }
        SectionItemView({
          AlertManager.privacySensitive.hideAlert()
          cleanup?.invoke()
        }) {
          Text(stringResource(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
      }
    },
    onDismissRequest = cleanup,
    hostDevice = hostDevice(rhId),
  )
}

fun openKnownContact(chatModel: ChatModel, rhId: Long?, close: (() -> Unit)?, contact: Contact) {
  withBGApi {
    val c = chatModel.getContactChat(contact.contactId)
    if (c != null) {
      close?.invoke()
      openDirectChat(rhId, contact.contactId)
    }
  }
}

fun ownGroupLinkConfirmConnect(
  chatModel: ChatModel,
  rhId: Long?,
  connectionLink: CreatedConnLink,
  linkText: String,
  incognito: Boolean?,
  connectionPlan: ConnectionPlan?,
  groupInfo: GroupInfo,
  close: (() -> Unit)?,
  cleanup: (() -> Unit)?,
) {
  AlertManager.privacySensitive.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.connect_plan_join_your_group),
    text = String.format(generalGetString(MR.strings.connect_plan_this_is_your_link_for_group_vName), groupInfo.displayName) + linkText,
    buttons = {
      Column {
        // Open group
        SectionItemView({
          AlertManager.privacySensitive.hideAlert()
          openKnownGroup(chatModel, rhId, close, groupInfo)
          cleanup?.invoke()
        }) {
          Text(generalGetString(MR.strings.connect_plan_open_group), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        if (incognito != null) {
          // Join incognito / Join with current profile
          SectionItemView({
            AlertManager.privacySensitive.hideAlert()
            withBGApi {
              connectViaUri(chatModel, rhId, connectionLink, incognito, connectionPlan, close, cleanup)
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
            withBGApi {
              connectViaUri(chatModel, rhId, connectionLink, incognito = false, connectionPlan, close, cleanup)
            }
          }) {
            Text(generalGetString(MR.strings.connect_use_current_profile), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.error)
          }
          // Use new incognito profile
          SectionItemView({
            AlertManager.privacySensitive.hideAlert()
            withBGApi {
              connectViaUri(chatModel, rhId, connectionLink, incognito = true, connectionPlan, close, cleanup)
            }
          }) {
            Text(generalGetString(MR.strings.connect_use_new_incognito_profile), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.error)
          }
        }
        // Cancel
        SectionItemView({
          AlertManager.privacySensitive.hideAlert()
          cleanup?.invoke()
        }) {
          Text(stringResource(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
      }
    },
    onDismissRequest = cleanup,
    hostDevice = hostDevice(rhId),
  )
}

fun openKnownGroup(chatModel: ChatModel, rhId: Long?, close: (() -> Unit)?, groupInfo: GroupInfo) {
  withBGApi {
    val g = chatModel.getGroupChat(groupInfo.groupId)
    if (g != null) {
      close?.invoke()
      openGroupChat(rhId, groupInfo.groupId)
    }
  }
}
