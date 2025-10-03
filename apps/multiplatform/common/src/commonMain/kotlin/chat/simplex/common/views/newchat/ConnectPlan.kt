package chat.simplex.common.views.newchat

import SectionItemView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
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
  close: (() -> Unit)?,
  cleanup: (() -> Unit)? = null,
  filterKnownContact: ((Contact) -> Unit)? = null,
  filterKnownGroup: ((GroupInfo) -> Unit)? = null,
): CompletableDeferred<Boolean> {
  connectProgressManager.cancelConnectProgress()
  val inProgress = mutableStateOf(true)
  connectProgressManager.startConnectProgress(generalGetString(MR.strings.loading_profile)) {
    inProgress.value = false
    cleanup?.invoke()
  }
  return planAndConnectTask(rhId, shortOrFullLink, close, cleanup, filterKnownContact, filterKnownGroup, inProgress)
}

private suspend fun planAndConnectTask(
  rhId: Long?,
  shortOrFullLink: String,
  close: (() -> Unit)?,
  cleanup: (() -> Unit)? = null,
  filterKnownContact: ((Contact) -> Unit)? = null,
  filterKnownGroup: ((GroupInfo) -> Unit)? = null,
  inProgress: MutableState<Boolean>
): CompletableDeferred<Boolean> {
  Log.e(TAG, "##### planAndConnectTask")
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
  val result = chatModel.controller.apiConnectPlan(rhId, shortOrFullLink, inProgress = inProgress)
  Log.e(TAG, "##### planAndConnectTask after apiConnectPlan result != null = ${result != null}")
  connectProgressManager.stopConnectProgress()
  Log.e(TAG, "##### planAndConnectTask after stopConnectProgress inProgress.value = ${inProgress.value}")
  if (!inProgress.value) { return completable }
  Log.e(TAG, "##### planAndConnectTask after if (!inProgress.value) { return completable }")
  if (result != null) {
    val (connectionLink, connectionPlan) = result
    val link = strHasSingleSimplexLink(shortOrFullLink.trim())
    val linkText = if (link?.format is Format.SimplexLink)
      "<br><br><u>${link.format.simplexLinkText}</u>"
    else
      ""
    when (connectionPlan) {
      is ConnectionPlan.InvitationLink -> when (connectionPlan.invitationLinkPlan) {
        is InvitationLinkPlan.Ok ->
          if (connectionPlan.invitationLinkPlan.contactSLinkData_ != null) {
            Log.d(TAG, "planAndConnect, .InvitationLink, .Ok, short link data present")
            showPrepareContactAlert(
              rhId,
              connectionLink,
              connectionPlan.invitationLinkPlan.contactSLinkData_,
              close,
              cleanup
            )
          } else {
            Log.d(TAG, "planAndConnect, .InvitationLink, .Ok, no short link data")
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, connectionLink, connectionPlan, close,
              title = generalGetString(MR.strings.connect_via_invitation_link),
              text = generalGetString(MR.strings.profile_will_be_sent_to_contact_sending_link) + linkText,
              connectDestructive = false,
              cleanup = cleanup,
            )
          }
        InvitationLinkPlan.OwnLink -> {
          Log.d(TAG, "planAndConnect, .InvitationLink, .OwnLink")
          askCurrentOrIncognitoProfileAlert(
            chatModel, rhId, connectionLink, connectionPlan, close,
            title = generalGetString(MR.strings.connect_plan_connect_to_yourself),
            text = generalGetString(MR.strings.connect_plan_this_is_your_own_one_time_link) + linkText,
            connectDestructive = true,
            cleanup = cleanup,
          )
        }
        is InvitationLinkPlan.Connecting -> {
          Log.d(TAG, "planAndConnect, .InvitationLink, .Connecting")
          val contact = connectionPlan.invitationLinkPlan.contact_
          if (contact != null) {
            if (filterKnownContact != null) {
              filterKnownContact(contact)
            } else {
              showOpenKnownContactAlert(chatModel, rhId, close, contact)
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
          Log.d(TAG, "planAndConnect, .InvitationLink, .Known")
          val contact = connectionPlan.invitationLinkPlan.contact
          if (filterKnownContact != null) {
            filterKnownContact(contact)
          } else {
            showOpenKnownContactAlert(chatModel, rhId, close, contact)
            cleanup()
          }
        }
      }
      is ConnectionPlan.ContactAddress -> {
        Log.e(TAG, "##### planAndConnectTask is ConnectionPlan.ContactAddress")
        when (connectionPlan.contactAddressPlan) {
        is ContactAddressPlan.Ok ->
          if (connectionPlan.contactAddressPlan.contactSLinkData_ != null) {
            Log.e(TAG, "##### planAndConnect, .ContactAddress, .Ok, short link data present")
            showPrepareContactAlert(
              rhId,
              connectionLink,
              connectionPlan.contactAddressPlan.contactSLinkData_,
              close,
              cleanup
            )
          } else {
            Log.d(TAG, "planAndConnect, .ContactAddress, .Ok, no short link data")
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, connectionLink, connectionPlan, close,
              title = generalGetString(MR.strings.connect_via_contact_link),
              text = generalGetString(MR.strings.profile_will_be_sent_to_contact_sending_link) + linkText,
              connectDestructive = false,
              cleanup,
            )
          }
        ContactAddressPlan.OwnLink -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .OwnLink")
          askCurrentOrIncognitoProfileAlert(
            chatModel, rhId, connectionLink, connectionPlan, close,
            title = generalGetString(MR.strings.connect_plan_connect_to_yourself),
            text = generalGetString(MR.strings.connect_plan_this_is_your_own_simplex_address) + linkText,
            connectDestructive = true,
            cleanup = cleanup,
          )
        }
        ContactAddressPlan.ConnectingConfirmReconnect -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .ConnectingConfirmReconnect")
          askCurrentOrIncognitoProfileAlert(
            chatModel, rhId, connectionLink, connectionPlan, close,
            title = generalGetString(MR.strings.connect_plan_repeat_connection_request),
            text = generalGetString(MR.strings.connect_plan_you_have_already_requested_connection_via_this_address) + linkText,
            connectDestructive = true,
            cleanup = cleanup,
          )
        }
        is ContactAddressPlan.ConnectingProhibit -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .ConnectingProhibit")
          val contact = connectionPlan.contactAddressPlan.contact
          if (filterKnownContact != null) {
            filterKnownContact(contact)
          } else {
            showOpenKnownContactAlert(chatModel, rhId, close, contact)
            cleanup()
          }
        }
        is ContactAddressPlan.Known -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .Known")
          val contact = connectionPlan.contactAddressPlan.contact
          if (filterKnownContact != null) {
            filterKnownContact(contact)
          } else {
            showOpenKnownContactAlert(chatModel, rhId, close, contact)
            cleanup()
          }
        }
        is ContactAddressPlan.ContactViaAddress -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .ContactViaAddress")
          val contact = connectionPlan.contactAddressPlan.contact
          askCurrentOrIncognitoProfileConnectContactViaAddress(chatModel, rhId, contact, close, openChat = false)
          cleanup()
        }
      }}
      is ConnectionPlan.GroupLink -> when (connectionPlan.groupLinkPlan) {
        is GroupLinkPlan.Ok ->
          if (connectionPlan.groupLinkPlan.groupSLinkData_ != null) {
            Log.d(TAG, "planAndConnect, .GroupLink, .Ok, short link data present")
            showPrepareGroupAlert(
              rhId,
              connectionLink,
              connectionPlan.groupLinkPlan.groupSLinkData_,
              close,
              cleanup
            )
          } else {
            Log.d(TAG, "planAndConnect, .GroupLink, .Ok, no short link data")
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, connectionLink, connectionPlan, close,
              title = generalGetString(MR.strings.connect_via_group_link),
              text = generalGetString(MR.strings.you_will_join_group) + linkText,
              connectDestructive = false,
              cleanup = cleanup,
            )
          }
        is GroupLinkPlan.OwnLink -> {
          Log.d(TAG, "planAndConnect, .GroupLink, .OwnLink")
          val groupInfo = connectionPlan.groupLinkPlan.groupInfo
          if (filterKnownGroup != null) {
            filterKnownGroup(groupInfo)
          } else {
            ownGroupLinkConfirmConnect(chatModel, rhId, connectionLink, linkText, connectionPlan, groupInfo, close, cleanup)
          }
        }
        GroupLinkPlan.ConnectingConfirmReconnect -> {
          Log.d(TAG, "planAndConnect, .GroupLink, .ConnectingConfirmReconnect")
          askCurrentOrIncognitoProfileAlert(
            chatModel, rhId, connectionLink, connectionPlan, close,
            title = generalGetString(MR.strings.connect_plan_repeat_join_request),
            text = generalGetString(MR.strings.connect_plan_you_are_already_joining_the_group_via_this_link) + linkText,
            connectDestructive = true,
            cleanup = cleanup,
          )
        }
        is GroupLinkPlan.ConnectingProhibit -> {
          Log.d(TAG, "planAndConnect, .GroupLink, .ConnectingProhibit")
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
          Log.d(TAG, "planAndConnect, .GroupLink, .Known")
          val groupInfo = connectionPlan.groupLinkPlan.groupInfo
          if (filterKnownGroup != null) {
            filterKnownGroup(groupInfo)
          } else {
            showOpenKnownGroupAlert(chatModel, rhId, close, groupInfo)
            cleanup()
          }
        }
      }
      is ConnectionPlan.Error -> {
        Log.d(TAG, "planAndConnect, error ${connectionPlan.chatError}")
        askCurrentOrIncognitoProfileAlert(
          chatModel, rhId, connectionLink, connectionPlan = null, close,
          title = generalGetString(MR.strings.connect_plan_connect_via_link),
          connectDestructive = false,
          cleanup = cleanup,
        )
      }
    }
  } else {
    cleanup()
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

fun openChat_(chatModel: ChatModel, rhId: Long?, close: (() -> Unit)?, chat: Chat) {
  withBGApi {
    close?.invoke()
    openChat(secondaryChatsCtx = null, rhId, chat.chatInfo)
  }
}

val alertProfileImageSize = 138.dp

private fun showOpenKnownContactAlert(chatModel: ChatModel, rhId: Long?, close: (() -> Unit)?, contact: Contact) {
  AlertManager.privacySensitive.showOpenChatAlert(
    profileName = contact.profile.displayName,
    profileFullName = contact.profile.fullName,
    profileImage = {
      ProfileImage(
        size = alertProfileImageSize,
        image = contact.profile.image,
        icon = contact.chatIconName
      )
    },
    confirmText = generalGetString(if (contact.nextConnectPrepared) MR.strings.connect_plan_open_new_chat else MR.strings.connect_plan_open_chat),
    onConfirm = {
      openKnownContact(chatModel, rhId, close, contact)
    },
    onDismiss = null
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

private fun showOpenKnownGroupAlert(chatModel: ChatModel, rhId: Long?, close: (() -> Unit)?, groupInfo: GroupInfo) {
  AlertManager.privacySensitive.showOpenChatAlert(
    profileName = groupInfo.groupProfile.displayName,
    profileFullName = groupInfo.groupProfile.fullName,
    profileImage = {
      ProfileImage(
        size = alertProfileImageSize,
        image = groupInfo.groupProfile.image,
        icon = groupInfo.chatIconName
      )
    },
    confirmText = generalGetString(
      if (groupInfo.businessChat == null) {
        if (groupInfo.nextConnectPrepared) MR.strings.connect_plan_open_new_group else MR.strings.connect_plan_open_group
      } else {
        if (groupInfo.nextConnectPrepared) MR.strings.connect_plan_open_new_chat else MR.strings.connect_plan_open_chat
      }
    ),
    onConfirm = {
      openKnownGroup(chatModel, rhId, close, groupInfo)
    },
    onDismiss = null
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

fun showPrepareContactAlert(
  rhId: Long?,
  connectionLink: CreatedConnLink,
  contactShortLinkData: ContactShortLinkData,
  close: (() -> Unit)?,
  cleanup: (() -> Unit)?
) {
  AlertManager.privacySensitive.showOpenChatAlert(
    profileName = contactShortLinkData.profile.displayName,
    profileFullName = contactShortLinkData.profile.fullName,
    profileImage = {
      ProfileImage(
        size = alertProfileImageSize,
        image = contactShortLinkData.profile.image,
        icon =
          if (contactShortLinkData.business) MR.images.ic_work_filled_padded
          else if (contactShortLinkData.profile.peerType == ChatPeerType.Bot) MR.images.ic_cube
          else MR.images.ic_account_circle_filled
      )
    },
    confirmText = generalGetString(MR.strings.connect_plan_open_new_chat),
    onConfirm = {
      AlertManager.privacySensitive.hideAlert()
      withBGApi {
        val chat = chatModel.controller.apiPrepareContact(rhId, connectionLink, contactShortLinkData)
        if (chat != null) {
          withContext(Dispatchers.Main) {
            ChatController.chatModel.chatsContext.addChat(chat)
            openChat_(chatModel, rhId, close, chat)
          }
        }
        cleanup?.invoke()
      }
    },
    onDismiss = {
      cleanup?.invoke()
    }
  )
}

fun showPrepareGroupAlert(
  rhId: Long?,
  connectionLink: CreatedConnLink,
  groupShortLinkData: GroupShortLinkData,
  close: (() -> Unit)?,
  cleanup: (() -> Unit)?
) {
  AlertManager.privacySensitive.showOpenChatAlert(
    profileName = groupShortLinkData.groupProfile.displayName,
    profileFullName = groupShortLinkData.groupProfile.fullName,
    profileImage = { ProfileImage(size = alertProfileImageSize, image = groupShortLinkData.groupProfile.image, icon = MR.images.ic_supervised_user_circle_filled) },
    confirmText = generalGetString(MR.strings.connect_plan_open_new_group),
    onConfirm = {
      AlertManager.privacySensitive.hideAlert()
      withBGApi {
        val chat = chatModel.controller.apiPrepareGroup(rhId, connectionLink, groupShortLinkData)
        if (chat != null) {
          withContext(Dispatchers.Main) {
            ChatController.chatModel.chatsContext.addChat(chat)
            openChat_(chatModel, rhId, close, chat)
          }
        }
        cleanup?.invoke()
      }
    },
    onDismiss = {
      cleanup?.invoke()
    }
  )
}
