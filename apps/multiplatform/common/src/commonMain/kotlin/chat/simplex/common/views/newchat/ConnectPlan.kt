package chat.simplex.common.views.newchat

import SectionItemView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.chat.item.openBrowserAlert
import chat.simplex.common.views.chat.subscriberCountStr
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.simplexTeamUri
import chat.simplex.res.MR
import kotlinx.coroutines.*
import kotlinx.datetime.*
import java.time.format.DateTimeFormatter
import java.time.format.FormatStyle

enum class ConnectionLinkType {
  INVITATION, CONTACT, GROUP
}

suspend fun planAndConnect(
  rhId: Long?,
  shortOrFullLink: String,
  linkOwnerSig: LinkOwnerSig? = null,
  close: (() -> Unit)?,
  cleanup: (() -> Unit)? = null,
  filterChats: ((List<ChatInfo>) -> Boolean)? = null,
): CompletableDeferred<Boolean> {
  when (val target = strConnectTarget(shortOrFullLink.trim())) {
    is ConnectTarget.Link -> {
      if (target.linkType == SimplexLinkType.relay) {
        AlertManager.privacySensitive.showAlertMsg(
          generalGetString(MR.strings.relay_address_alert_title),
          generalGetString(MR.strings.relay_address_alert_message),
        )
        cleanup?.invoke()
        return CompletableDeferred(false)
      }
    }
    // A SimplexName falls through to apiConnectPlan, which resolves it on the
    // core (the /_connect plan command accepts a name target, not only a link).
    is ConnectTarget.Name, null -> {}
  }
  connectProgressManager.cancelConnectProgress()
  val inProgress = mutableStateOf(true)
  connectProgressManager.startConnectProgress(generalGetString(MR.strings.loading_profile)) {
    inProgress.value = false
    cleanup?.invoke()
  }
  return planAndConnectTask(rhId, shortOrFullLink, linkOwnerSig, close, cleanup, filterChats, inProgress)
}

private fun nameDate(t: Instant): String =
  t.toLocalDateTime(TimeZone.currentSystemDefault()).toJavaLocalDateTime().format(DateTimeFormatter.ofLocalizedDate(FormatStyle.LONG))

private fun namePrice(price: NamePrice): String {
  val dollars = "$" + (price.amount / 100).toString() + (if (price.amount % 100 == 0L) "" else ".%02d".format(price.amount % 100))
  return String.format(generalGetString(MR.strings.simplex_name_price_for_years), dollars, price.years)
}

private fun openNameHowTo(uriHandler: UriHandler) = openBrowserAlert("https://simplex.domains/#testing", uriHandler)

private fun showNameWarningAlert(
  rhId: Long?,
  domain: SimplexDomain,
  warning: NameWarning,
  openExistingChat: (() -> Unit)?,
  cleanup: (() -> Unit)?
) {
  val nameStr = domain.fullDomainName
  fun dismiss() {
    AlertManager.privacySensitive.hideAlert()
    cleanup?.invoke()
  }
  fun alert(title: String, text: String, action: Pair<String, (UriHandler) -> Unit>? = null) {
    AlertManager.privacySensitive.showAlertDialogButtonsColumn(
      title = title,
      text = text,
      buttons = {
        val uriHandler = LocalUriHandler.current
        Column {
          if (action != null) {
            SectionItemView({ dismiss(); action.second(uriHandler) }) {
              Text(action.first, Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
            }
          }
          if (openExistingChat != null) {
            SectionItemView({ dismiss(); openExistingChat() }) {
              Text(generalGetString(MR.strings.connect_plan_open_existing_chat), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
            }
          }
          SectionItemView(::dismiss) {
            Text(generalGetString(MR.strings.ok), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
          }
        }
      },
      onDismissRequest = cleanup,
      hostDevice = hostDevice(rhId),
    )
  }
  val register = generalGetString(MR.strings.simplex_name_register) to ::openNameHowTo
  when (warning) {
    is NameWarning.Expired -> alert(
      generalGetString(MR.strings.simplex_name_expired),
      if (warning.graceUntil != null) String.format(generalGetString(MR.strings.simplex_name_expired_desc), nameStr, nameDate(warning.expiredAt), nameDate(warning.graceUntil))
      else String.format(generalGetString(MR.strings.simplex_name_expired_no_date_desc), nameStr, nameDate(warning.expiredAt))
    )
    is NameWarning.OwnExpired -> alert(
      generalGetString(MR.strings.simplex_name_own_expired),
      if (warning.graceUntil != null) String.format(generalGetString(MR.strings.simplex_name_own_expired_desc), nameStr, nameDate(warning.expiredAt), nameDate(warning.graceUntil))
      else String.format(generalGetString(MR.strings.simplex_name_own_expired_no_date_desc), nameStr, nameDate(warning.expiredAt)),
      generalGetString(MR.strings.simplex_name_renew) to ::openNameHowTo
    )
    is NameWarning.Available -> alert(
      generalGetString(MR.strings.simplex_name_not_registered),
      String.format(generalGetString(MR.strings.simplex_name_available_desc), nameStr, namePrice(warning.price)),
      register
    )
    is NameWarning.NoLongerRegistered -> alert(
      generalGetString(MR.strings.simplex_name_no_longer_registered),
      String.format(generalGetString(MR.strings.simplex_name_available_desc), nameStr, namePrice(warning.price)),
      register
    )
    is NameWarning.OwnAvailable -> alert(
      generalGetString(MR.strings.simplex_name_own_expired),
      String.format(generalGetString(MR.strings.simplex_name_own_available_desc), nameStr, namePrice(warning.price)),
      generalGetString(MR.strings.simplex_name_re_register) to ::openNameHowTo
    )
    NameWarning.ReservedForCommunity -> alert(
      generalGetString(MR.strings.simplex_name_not_registered),
      String.format(generalGetString(MR.strings.simplex_name_reserved_community_desc), nameStr),
      generalGetString(MR.strings.simplex_name_connect_simplex_team) to { uh: UriHandler -> uh.openVerifiedSimplexUri(simplexTeamUri) }
    )
    NameWarning.NotRegistered -> alert(generalGetString(MR.strings.simplex_name_not_registered), generalGetString(MR.strings.simplex_name_not_found_desc))
    NameWarning.NoValidLink -> alert(
      generalGetString(MR.strings.simplex_name_no_valid_link),
      String.format(generalGetString(MR.strings.simplex_name_no_valid_link_desc), nameStr)
    )
  }
}

private suspend fun planAndConnectTask(
  rhId: Long?,
  shortOrFullLink: String,
  linkOwnerSig: LinkOwnerSig? = null,
  close: (() -> Unit)?,
  cleanup: (() -> Unit)? = null,
  filterChats: ((List<ChatInfo>) -> Boolean)? = null,
  inProgress: MutableState<Boolean>
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
  val result = chatModel.controller.apiConnectPlan(rhId, shortOrFullLink, linkOwnerSig = linkOwnerSig, inProgress = inProgress)
  connectProgressManager.stopConnectProgress()
  if (!inProgress.value) { return completable }
  if (result != null) {
    val (connectionLink, planSimplexName, otherSimplexName, connectionPlan, localChats) = result
    addMissingChats(rhId, localChats)
    val target = strConnectTarget(shortOrFullLink.trim())
    val linkText = if (target is ConnectTarget.Link) "<br><br><u>${target.linkText}</u>" else ""
    // the name can also resolve to the other kind; its type picks the verb, its short form the label and target
    val connectOtherLink = otherSimplexName?.shortStr
    val connectOtherButton = otherSimplexName?.let {
      val label = if (it.nameType == SimplexNameType.publicGroup) MR.strings.connect_plan_join_name else MR.strings.connect_plan_connect_to_name
      generalGetString(label).format(it.shortStr)
    }
    val (nameDomain, nameWarning) = when (connectionPlan) {
      is ConnectionPlan.NameNotConnectable -> connectionPlan.simplexDomain to connectionPlan.nameWarning
      is ConnectionPlan.ContactAddress -> planSimplexName?.nameDomain to connectionPlan.nameWarning_
      is ConnectionPlan.GroupLink -> planSimplexName?.nameDomain to connectionPlan.nameWarning_
      else -> null to null
    }
    if (nameWarning != null && nameDomain != null) {
      val openExisting: (() -> Unit)? = localChats.firstOrNull()?.takeIf { filterChats?.invoke(localChats) != true }?.let { chatInfo -> { openChat_(chatModel, rhId, close, Chat(remoteHostId = rhId, chatInfo = chatInfo, chatItems = emptyList())) } }
      showNameWarningAlert(rhId, nameDomain, nameWarning, openExisting, cleanup)
      return completable
    }
    if (connectionLink == null) {
      cleanup()
      return completable
    }
    when (connectionPlan) {
      is ConnectionPlan.InvitationLink -> when (connectionPlan.invitationLinkPlan) {
        is InvitationLinkPlan.Ok ->
          if (connectionPlan.invitationLinkPlan.contactSLinkData_ != null) {
            Log.d(TAG, "planAndConnect, .InvitationLink, .Ok, short link data present")
            showPrepareContactAlert(
              rhId,
              connectionLink,
              connectionPlan.invitationLinkPlan.contactSLinkData_,
              ownerVerification = connectionPlan.invitationLinkPlan.ownerVerification,
              close = close,
              cleanup = cleanup
            )
          } else {
            Log.d(TAG, "planAndConnect, .InvitationLink, .Ok, no short link data")
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, connectionLink, connectionPlan, close,
              title = generalGetString(MR.strings.connect_via_invitation_link),
              text = generalGetString(MR.strings.profile_will_be_sent_to_contact_sending_link) + linkText,
              connectDestructive = false,
              cleanup = cleanup,
              ownerVerification = connectionPlan.invitationLinkPlan.ownerVerification,
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
            if (filterChats?.invoke(localChats) != true) {
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
          if (filterChats?.invoke(localChats) != true) {
            showOpenKnownContactAlert(chatModel, rhId, close, contact)
            cleanup()
          }
        }
      }
      is ConnectionPlan.ContactAddress -> when (connectionPlan.contactAddressPlan) {
        is ContactAddressPlan.Ok ->
          if (connectionPlan.contactAddressPlan.contactSLinkData_ != null) {
            Log.d(TAG, "planAndConnect, .ContactAddress, .Ok, short link data present")
            showPrepareContactAlert(
              rhId,
              connectionLink,
              connectionPlan.contactAddressPlan.contactSLinkData_,
              ownerVerification = connectionPlan.contactAddressPlan.ownerVerification,
              planSimplexName = planSimplexName,
              connectOtherButton = connectOtherButton,
              connectOtherLink = connectOtherLink,
              addressChanged = connectionPlan.contactAddressPlan.addressChanged,
              openExistingChat = localChats.firstOrNull()?.takeIf { filterChats?.invoke(localChats) != true }?.let { chatInfo -> { openChat_(chatModel, rhId, close, Chat(remoteHostId = rhId, chatInfo = chatInfo, chatItems = emptyList())); cleanup() } },
              close,
              cleanup,
              filterChats
            )
          } else {
            Log.d(TAG, "planAndConnect, .ContactAddress, .Ok, no short link data")
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, connectionLink, connectionPlan, close,
              title = generalGetString(MR.strings.connect_via_contact_link),
              text = generalGetString(MR.strings.profile_will_be_sent_to_contact_sending_link) + linkText,
              connectDestructive = false,
              cleanup,
              ownerVerification = connectionPlan.contactAddressPlan.ownerVerification,
              connectOtherButton = connectOtherButton,
              connectOtherLink = connectOtherLink,
              filterChats = filterChats,
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
            connectOtherButton = connectOtherButton,
            connectOtherLink = connectOtherLink,
            filterChats = filterChats,
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
            connectOtherButton = connectOtherButton,
            connectOtherLink = connectOtherLink,
            filterChats = filterChats,
          )
        }
        is ContactAddressPlan.ConnectingProhibit -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .ConnectingProhibit")
          val contact = connectionPlan.contactAddressPlan.contact
          if (filterChats?.invoke(localChats) != true) {
            showOpenKnownContactAlert(chatModel, rhId, close, contact, planSimplexName = planSimplexName, connectOtherButton = connectOtherButton, connectOtherLink = connectOtherLink, filterChats = filterChats)
            cleanup()
          }
        }
        is ContactAddressPlan.Known -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .Known")
          val contact = connectionPlan.contactAddressPlan.contact
          if (filterChats?.invoke(localChats) == true) {
            if (otherSimplexName != null && connectOtherButton != null) showOtherNameAlert(rhId, otherSimplexName, connectOtherButton, close, cleanup, filterChats)
          } else {
            showOpenKnownContactAlert(chatModel, rhId, close, contact, planSimplexName = planSimplexName, connectOtherButton = connectOtherButton, connectOtherLink = connectOtherLink, filterChats = filterChats)
            cleanup()
          }
        }
        is ContactAddressPlan.ContactViaAddress -> {
          Log.d(TAG, "planAndConnect, .ContactAddress, .ContactViaAddress")
          val contact = connectionPlan.contactAddressPlan.contact
          // the contact is already prepared in the store, so open the existing chat instead of sending a new
          // connection request
          if (filterChats?.invoke(localChats) != true) {
            showOpenKnownContactAlert(chatModel, rhId, close, contact, planSimplexName = planSimplexName, connectOtherButton = connectOtherButton, connectOtherLink = connectOtherLink, filterChats = filterChats)
            cleanup()
          }
        }
      }
      is ConnectionPlan.GroupLink -> when (connectionPlan.groupLinkPlan) {
        is GroupLinkPlan.Ok ->
          if (connectionPlan.groupLinkPlan.groupSLinkData_ != null) {
            Log.d(TAG, "planAndConnect, .GroupLink, .Ok, short link data present")
            showPrepareGroupAlert(
              rhId,
              connectionLink,
              connectionPlan.groupLinkPlan.groupSLinkInfo_,
              connectionPlan.groupLinkPlan.groupSLinkData_,
              ownerVerification = connectionPlan.groupLinkPlan.ownerVerification,
              planSimplexName = planSimplexName,
              connectOtherButton = connectOtherButton,
              connectOtherLink = connectOtherLink,
              addressChanged = connectionPlan.groupLinkPlan.addressChanged,
              openExistingChat = localChats.firstOrNull()?.takeIf { filterChats?.invoke(localChats) != true }?.let { chatInfo -> { openChat_(chatModel, rhId, close, Chat(remoteHostId = rhId, chatInfo = chatInfo, chatItems = emptyList())); cleanup() } },
              close,
              cleanup,
              filterChats
            )
          } else {
            Log.d(TAG, "planAndConnect, .GroupLink, .Ok, no short link data")
            askCurrentOrIncognitoProfileAlert(
              chatModel, rhId, connectionLink, connectionPlan, close,
              title = generalGetString(MR.strings.connect_via_group_link),
              text = generalGetString(MR.strings.you_will_join_group) + linkText,
              connectDestructive = false,
              cleanup = cleanup,
              ownerVerification = connectionPlan.groupLinkPlan.ownerVerification,
              connectOtherButton = connectOtherButton,
              connectOtherLink = connectOtherLink,
              filterChats = filterChats,
            )
          }
        is GroupLinkPlan.OwnLink -> {
          Log.d(TAG, "planAndConnect, .GroupLink, .OwnLink")
          val groupInfo = connectionPlan.groupLinkPlan.groupInfo
          if (filterChats?.invoke(localChats) == true) {
            if (otherSimplexName != null && connectOtherButton != null) showOtherNameAlert(rhId, otherSimplexName, connectOtherButton, close, cleanup, filterChats)
          } else {
            ownGroupLinkConfirmConnect(chatModel, rhId, connectionLink, linkText, connectionPlan, groupInfo, close, cleanup, planSimplexName = planSimplexName, connectOtherButton = connectOtherButton, connectOtherLink = connectOtherLink, filterChats = filterChats)
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
            connectOtherButton = connectOtherButton,
            connectOtherLink = connectOtherLink,
            filterChats = filterChats,
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
          if (filterChats?.invoke(localChats) == true) {
            if (otherSimplexName != null && connectOtherButton != null) showOtherNameAlert(rhId, otherSimplexName, connectOtherButton, close, cleanup, filterChats)
          } else {
            showOpenKnownGroupAlert(chatModel, rhId, close, groupInfo, planSimplexName = planSimplexName, connectOtherButton = connectOtherButton, connectOtherLink = connectOtherLink, filterChats = filterChats)
            cleanup()
          }
        }
        is GroupLinkPlan.NoRelays -> {
          Log.d(TAG, "planAndConnect, .GroupLink, .NoRelays")
          val groupSLinkData = connectionPlan.groupLinkPlan.groupSLinkData_
          if (groupSLinkData != null) {
            AlertManager.privacySensitive.showOpenChatAlert(
              profileName = groupSLinkData.groupProfile.displayName,
              profileFullName = groupSLinkData.groupProfile.fullName,
              profileImage = {
                ProfileImage(
                  size = alertProfileImageSize,
                  image = groupSLinkData.groupProfile.image,
                  icon = MR.images.ic_bigtop_updates_circle_filled
                )
              },
              subtitle = generalGetString(MR.strings.channel_no_active_relays_try_later),
              confirmText = null,
              dismissText = generalGetString(MR.strings.ok),
              onDismiss = { cleanup() }
            )
          } else {
            AlertManager.privacySensitive.showAlertMsg(
              generalGetString(MR.strings.channel_temporarily_unavailable),
              generalGetString(MR.strings.channel_no_active_relays_try_later)
            )
            cleanup()
          }
        }
        is GroupLinkPlan.UpdateRequired -> {
          Log.d(TAG, "planAndConnect, .GroupLink, .UpdateRequired")
          val groupSLinkData = connectionPlan.groupLinkPlan.groupSLinkData_
          if (groupSLinkData != null) {
            AlertManager.privacySensitive.showOpenChatAlert(
              profileName = groupSLinkData.groupProfile.displayName,
              profileFullName = groupSLinkData.groupProfile.fullName,
              profileImage = {
                ProfileImage(
                  size = alertProfileImageSize,
                  image = groupSLinkData.groupProfile.image,
                  icon = MR.images.ic_supervised_user_circle_filled
                )
              },
              subtitle = generalGetString(MR.strings.group_link_requires_newer_version),
              confirmText = null,
              dismissText = generalGetString(MR.strings.ok),
              onDismiss = { cleanup() }
            )
          } else {
            AlertManager.privacySensitive.showAlertMsg(
              generalGetString(MR.strings.app_update_required),
              generalGetString(MR.strings.group_link_requires_newer_version)
            )
            cleanup()
          }
        }
      }
      is ConnectionPlan.NameNotConnectable -> {}
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
    is ConnectionPlan.NameNotConnectable -> null
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
  ownerVerification: OwnerVerification? = null,
  connectOtherButton: String? = null,
  connectOtherLink: String? = null,
  filterChats: ((List<ChatInfo>) -> Boolean)? = null,
) {
  val fullText = listOfNotNull(text, ownerVerificationMessage(ownerVerification)).joinToString("\n\n").ifEmpty { null }
  AlertManager.privacySensitive.showAlertDialogButtonsColumn(
    title = title,
    text = fullText,
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
        if (connectOtherButton != null && connectOtherLink != null) {
          SectionItemView({
            AlertManager.privacySensitive.hideAlert()
            withBGApi { planAndConnect(rhId, connectOtherLink, close = close, cleanup = cleanup, filterChats = filterChats) }
          }) {
            Text(connectOtherButton, Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
          }
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

suspend fun addMissingChats(rhId: Long?, chats: List<ChatInfo>) {
  chats.forEach {
    if (!chatModel.chatsContext.hasChat(rhId, it.id)) {
      chatModel.chatsContext.addChat(Chat(remoteHostId = rhId, chatInfo = it, chatItems = emptyList()))
    }
  }
}

fun openChat_(chatModel: ChatModel, rhId: Long?, close: (() -> Unit)?, chat: Chat) {
  withBGApi {
    close?.invoke()
    openChat(secondaryChatsCtx = null, rhId, chat.chatInfo)
  }
}

val alertProfileImageSize = 138.dp

// For alerts that show the name inline (not as a profile with an avatar): "Alice" -> "Alice (@alice.testing)".
private fun nameWithDomain(name: String, planSimplexName: SimplexNameInfo?): String =
  name + (planSimplexName?.let { " (${it.shortStr})" } ?: "")

private fun showOpenKnownContactAlert(chatModel: ChatModel, rhId: Long?, close: (() -> Unit)?, contact: Contact, planSimplexName: SimplexNameInfo? = null, connectOtherButton: String? = null, connectOtherLink: String? = null, filterChats: ((List<ChatInfo>) -> Boolean)? = null) {
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
    // the alert shows the badge inline, so it skips the long-expired (ExpiredOld) badge here too
    profileBadge = if (contact.active && contact.profile.localBadge?.status != BadgeStatus.ExpiredOld) contact.profile.localBadge else null,
    nameCaption = planSimplexName?.shortStr,
    confirmText = generalGetString(if (contact.nextConnectPrepared) MR.strings.connect_plan_open_new_chat else MR.strings.connect_plan_open_chat),
    onConfirm = {
      openKnownContact(chatModel, rhId, close, contact)
    },
    connectOtherButton = connectOtherButton,
    onConnectOther = connectOtherLink?.let { link -> { withBGApi { planAndConnect(rhId, link, close = close, filterChats = filterChats) } } },
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
  planSimplexName: SimplexNameInfo? = null,
  connectOtherButton: String? = null,
  connectOtherLink: String? = null,
  filterChats: ((List<ChatInfo>) -> Boolean)? = null,
) {
  if (groupInfo.useRelays) {
    AlertManager.privacySensitive.showAlertDialogButtonsColumn(
      title = generalGetString(MR.strings.connect_plan_this_is_your_link_for_channel),
      text = String.format(generalGetString(MR.strings.connect_plan_this_is_your_link_for_channel_vName), nameWithDomain(groupInfo.displayName, planSimplexName)),
      buttons = {
        Column {
          SectionItemView({
            AlertManager.privacySensitive.hideAlert()
            openKnownGroup(chatModel, rhId, close, groupInfo)
            cleanup?.invoke()
          }) {
            Text(generalGetString(MR.strings.connect_plan_open_channel), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
          }
          if (connectOtherButton != null && connectOtherLink != null) {
            SectionItemView({
              AlertManager.privacySensitive.hideAlert()
              withBGApi { planAndConnect(rhId, connectOtherLink, close = close, cleanup = cleanup, filterChats = filterChats) }
            }) {
              Text(connectOtherButton, Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
            }
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
  } else {
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
}

private fun showOpenKnownGroupAlert(chatModel: ChatModel, rhId: Long?, close: (() -> Unit)?, groupInfo: GroupInfo, planSimplexName: SimplexNameInfo? = null, connectOtherButton: String? = null, connectOtherLink: String? = null, filterChats: ((List<ChatInfo>) -> Boolean)? = null) {
  val subscriberCount = if (groupInfo.useRelays) groupInfo.groupSummary.publicMemberCount?.let { subscriberCountStr(it) } else null
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
    nameCaption = planSimplexName?.shortStr,
    subtitle = subscriberCount,
    information = if (groupInfo.nextConnectPrepared || groupInfo.businessChat != null) {
      null
    } else {
      val isChannel = groupInfo.useRelays
      generalGetString(when (groupInfo.membership.memberRole) {
        GroupMemberRole.Observer -> if (isChannel) MR.strings.connect_plan_you_are_subscriber else MR.strings.connect_plan_you_are_observer
        GroupMemberRole.Moderator -> MR.strings.connect_plan_you_are_moderator
        GroupMemberRole.Admin -> MR.strings.connect_plan_you_are_admin
        GroupMemberRole.Owner -> MR.strings.connect_plan_you_are_owner
        else -> if (isChannel) MR.strings.connect_plan_you_are_contributor else MR.strings.connect_plan_you_are_member
      })
    },
    secondaryInformation = true,
    confirmText = generalGetString(
      if (groupInfo.useRelays) {
        MR.strings.connect_plan_open_channel
      } else if (groupInfo.businessChat == null) {
        MR.strings.connect_plan_open_group
      } else {
        if (groupInfo.nextConnectPrepared) MR.strings.connect_plan_open_new_chat else MR.strings.connect_plan_open_chat
      }
    ),
    onConfirm = {
      openKnownGroup(chatModel, rhId, close, groupInfo)
    },
    connectOtherButton = connectOtherButton,
    onConnectOther = connectOtherLink?.let { link -> { withBGApi { planAndConnect(rhId, link, close = close, filterChats = filterChats) } } },
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
  ownerVerification: OwnerVerification? = null,
  planSimplexName: SimplexNameInfo? = null,
  connectOtherButton: String? = null,
  connectOtherLink: String? = null,
  addressChanged: Boolean = false,
  openExistingChat: (() -> Unit)? = null,
  close: (() -> Unit)?,
  cleanup: (() -> Unit)?,
  filterChats: ((List<ChatInfo>) -> Boolean)? = null
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
    profileBadge = if (contactShortLinkData.localBadge?.status == BadgeStatus.ExpiredOld) null else contactShortLinkData.localBadge,
    nameCaption = planSimplexName?.shortStr,
    subtitle = if (addressChanged && planSimplexName != null)
      String.format(generalGetString(MR.strings.simplex_name_address_changed), planSimplexName.nameDomain.fullDomainName)
    else null,
    information = ownerVerificationMessage(ownerVerification),
    confirmText = generalGetString(MR.strings.connect_plan_open_new_chat),
    onConfirm = {
      AlertManager.privacySensitive.hideAlert()
      ModalManager.closeAllModalsEverywhere()
      withBGApi {
        val chat = chatModel.controller.apiPrepareContact(rhId, connectionLink, contactShortLinkData, planSimplexName?.nameDomain)
        if (chat != null) {
          withContext(Dispatchers.Main) {
            ChatController.chatModel.chatsContext.addChat(chat)
            openChat_(chatModel, rhId, close, chat)
          }
        }
        cleanup?.invoke()
      }
    },
    connectOtherButton = connectOtherButton,
    onConnectOther = connectOtherLink?.let { link -> { withBGApi { planAndConnect(rhId, link, close = close, cleanup = cleanup, filterChats = filterChats) } } },
    dismissText = generalGetString(if (openExistingChat != null) MR.strings.connect_plan_open_existing_chat else MR.strings.cancel_verb),
    onDismissButton = openExistingChat,
    onDismiss = {
      cleanup?.invoke()
    }
  )
}

private fun showOtherNameAlert(rhId: Long?, otherSimplexName: SimplexNameInfo, connectOtherButton: String, close: (() -> Unit)?, cleanup: (() -> Unit)?, filterChats: ((List<ChatInfo>) -> Boolean)?) {
  AlertManager.privacySensitive.showAlertDialogButtonsColumn(
    title = String.format(
      generalGetString(if (otherSimplexName.nameType == SimplexNameType.publicGroup) MR.strings.simplex_name_also_leads_to_channel else MR.strings.simplex_name_also_leads_to_contact),
      otherSimplexName.nameDomain.fullDomainName,
      otherSimplexName.shortStr
    ),
    hostDevice = hostDevice(rhId),
    buttons = {
      Column {
        SectionItemView({
          AlertManager.privacySensitive.hideAlert()
          withBGApi { planAndConnect(rhId, otherSimplexName.shortStr, close = close, cleanup = cleanup, filterChats = filterChats) }
        }) {
          Text(connectOtherButton, Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        SectionItemView({ AlertManager.privacySensitive.hideAlert() }) {
          Text(generalGetString(MR.strings.ok), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
      }
    }
  )
}

fun showPrepareGroupAlert(
  rhId: Long?,
  connectionLink: CreatedConnLink,
  groupShortLinkInfo: GroupShortLinkInfo?,
  groupShortLinkData: GroupShortLinkData,
  ownerVerification: OwnerVerification? = null,
  planSimplexName: SimplexNameInfo? = null,
  connectOtherButton: String? = null,
  connectOtherLink: String? = null,
  addressChanged: Boolean = false,
  openExistingChat: (() -> Unit)? = null,
  close: (() -> Unit)?,
  cleanup: (() -> Unit)?,
  filterChats: ((List<ChatInfo>) -> Boolean)? = null
) {
  val isChannel = !(groupShortLinkInfo?.direct ?: true)
  val subscriberCount = if (isChannel) groupShortLinkData.publicGroupData?.publicMemberCount?.let { subscriberCountStr(it) } else null
  AlertManager.privacySensitive.showOpenChatAlert(
    profileName = groupShortLinkData.groupProfile.displayName,
    profileFullName = groupShortLinkData.groupProfile.fullName,
    profileImage = {
      ProfileImage(
        size = alertProfileImageSize,
        image = groupShortLinkData.groupProfile.image,
        icon = if (isChannel) MR.images.ic_bigtop_updates_circle_filled else MR.images.ic_supervised_user_circle_filled
      )
    },
    nameCaption = planSimplexName?.shortStr,
    subtitle = subscriberCount,
    information = listOfNotNull(
      if (addressChanged && planSimplexName != null) String.format(generalGetString(MR.strings.simplex_name_channel_changed), planSimplexName.nameDomain.fullDomainName) else null,
      ownerVerificationMessage(ownerVerification)
    ).joinToString("\n").ifEmpty { null },
    confirmText = generalGetString(
      if (isChannel) (if (addressChanged) MR.strings.connect_plan_open_new_channel else MR.strings.connect_plan_open_channel)
      else MR.strings.connect_plan_open_group
    ),
    onConfirm = {
      AlertManager.privacySensitive.hideAlert()
      withBGApi {
        val directLink = groupShortLinkInfo?.direct ?: true
        val chat = chatModel.controller.apiPrepareGroup(rhId, connectionLink, directLink = directLink, groupShortLinkData, planSimplexName?.nameDomain)
        if (chat != null) {
          withContext(Dispatchers.Main) {
            val relays = groupShortLinkInfo?.groupRelays
            if (!relays.isNullOrEmpty()) {
              val chatInfo = chat.chatInfo
              if (chatInfo is ChatInfo.Group) {
                chatModel.channelRelayHostnames[chatInfo.groupInfo.groupId] = relays
              }
            }
            ChatController.chatModel.chatsContext.addChat(chat)
            openChat_(chatModel, rhId, close, chat)
          }
        }
        cleanup?.invoke()
      }
    },
    connectOtherButton = connectOtherButton,
    onConnectOther = connectOtherLink?.let { link -> { withBGApi { planAndConnect(rhId, link, close = close, cleanup = cleanup, filterChats = filterChats) } } },
    dismissText = generalGetString(if (openExistingChat != null) MR.strings.connect_plan_open_existing_chat else MR.strings.cancel_verb),
    onDismissButton = openExistingChat,
    onDismiss = {
      cleanup?.invoke()
    }
  )
}

fun ownerVerificationMessage(ov: OwnerVerification?): String? = when (ov) {
  is OwnerVerification.Verified -> generalGetString(MR.strings.owner_verification_passed)
  is OwnerVerification.Failed -> String.format(generalGetString(MR.strings.owner_verification_failed), ov.reason)
  null -> null
}
