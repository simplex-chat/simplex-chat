package chat.simplex.common.views.chat.group

import SectionBottomSpacer
import SectionCustomFooter
import SectionDividerSpaced
import SectionItemView
import SectionSpacer
import SectionView
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.icerockdev.library.MR
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.ChatInfoToolbarTitle
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.InfoAboutIncognito
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.common.model.GroupInfo
import chat.simplex.common.platform.BackHandler

@Composable
fun AddGroupMembersView(groupInfo: GroupInfo, creatingGroup: Boolean = false, chatModel: ChatModel, close: () -> Unit) {
  val selectedContacts = remember { mutableStateListOf<Long>() }
  val selectedRole = remember { mutableStateOf(GroupMemberRole.Member) }
  var allowModifyMembers by remember { mutableStateOf(true) }
  BackHandler(onBack = close)
  AddGroupMembersLayout(
    chatModel.incognito.value,
    groupInfo = groupInfo,
    creatingGroup = creatingGroup,
    contactsToAdd = getContactsToAdd(chatModel),
    selectedContacts = selectedContacts,
    selectedRole = selectedRole,
    allowModifyMembers = allowModifyMembers,
    openPreferences = {
      ModalManager.shared.showCustomModal { close ->
        GroupPreferencesView(chatModel, groupInfo.id, close)
      }
    },
    inviteMembers = {
      allowModifyMembers = false
      withApi {
        for (contactId in selectedContacts) {
          val member = chatModel.controller.apiAddMember(groupInfo.groupId, contactId, selectedRole.value)
          if (member != null) {
            chatModel.upsertGroupMember(groupInfo, member)
          } else {
            break
          }
        }
        close.invoke()
      }
    },
    clearSelection = { selectedContacts.clear() },
    addContact = { contactId -> if (contactId !in selectedContacts) selectedContacts.add(contactId) },
    removeContact = { contactId -> selectedContacts.removeIf { it == contactId } },
    close = close,
  )
}

fun getContactsToAdd(chatModel: ChatModel): List<Contact> {
  val memberContactIds = chatModel.groupMembers
    .filter { it.memberCurrent }
    .mapNotNull { it.memberContactId }
  return chatModel.chats
    .asSequence()
    .map { it.chatInfo }
    .filterIsInstance<ChatInfo.Direct>()
    .map { it.contact }
    .filter { it.contactId !in memberContactIds }
    .sortedBy { it.displayName.lowercase() }
    .toList()
}

@Composable
fun AddGroupMembersLayout(
  chatModelIncognito: Boolean,
  groupInfo: GroupInfo,
  creatingGroup: Boolean,
  contactsToAdd: List<Contact>,
  selectedContacts: List<Long>,
  selectedRole: MutableState<GroupMemberRole>,
  allowModifyMembers: Boolean,
  openPreferences: () -> Unit,
  inviteMembers: () -> Unit,
  clearSelection: () -> Unit,
  addContact: (Long) -> Unit,
  removeContact: (Long) -> Unit,
  close: () -> Unit,
) {
  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(stringResource(MR.strings.button_add_members))
    InfoAboutIncognito(
      chatModelIncognito,
      false,
      generalGetString(MR.strings.group_unsupported_incognito_main_profile_sent),
      generalGetString(MR.strings.group_main_profile_sent),
      true
    )
    Spacer(Modifier.size(DEFAULT_PADDING))
    Row(
      Modifier.fillMaxWidth(),
      horizontalArrangement = Arrangement.Center
    ) {
      ChatInfoToolbarTitle(
        ChatInfo.Group(groupInfo),
        imageSize = 60.dp,
        iconColor = if (isInDarkTheme()) GroupDark else SettingsSecondaryLight
      )
    }
    SectionSpacer()

    if (contactsToAdd.isEmpty()) {
      Row(
        Modifier.fillMaxWidth(),
        horizontalArrangement = Arrangement.Center
      ) {
        Text(
          stringResource(MR.strings.no_contacts_to_add),
          Modifier.padding(),
          color = MaterialTheme.colors.secondary
        )
      }
    } else {
      SectionView {
        if (creatingGroup) {
          SectionItemView(openPreferences) {
            Text(stringResource(MR.strings.set_group_preferences))
          }
        }
        RoleSelectionRow(groupInfo, selectedRole, allowModifyMembers)
        if (creatingGroup && selectedContacts.isEmpty()) {
          SkipInvitingButton(close)
        } else {
          InviteMembersButton(inviteMembers, disabled = selectedContacts.isEmpty() || !allowModifyMembers)
        }
      }
      SectionCustomFooter {
        InviteSectionFooter(selectedContactsCount = selectedContacts.size, allowModifyMembers, clearSelection)
      }
      SectionDividerSpaced(maxTopPadding = true)

      SectionView(stringResource(MR.strings.select_contacts)) {
        ContactList(contacts = contactsToAdd, selectedContacts, groupInfo, allowModifyMembers, addContact, removeContact)
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun RoleSelectionRow(groupInfo: GroupInfo, selectedRole: MutableState<GroupMemberRole>, enabled: Boolean) {
  Row(
    Modifier.fillMaxWidth(),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.SpaceBetween
  ) {
    val values = GroupMemberRole.values().filter { it <= groupInfo.membership.memberRole }.map { it to it.text }
    ExposedDropDownSettingRow(
      generalGetString(MR.strings.new_member_role),
      values,
      selectedRole,
      icon = null,
      enabled = rememberUpdatedState(enabled)
    ) { selectedRole.value = it }
  }
}

@Composable
fun InviteMembersButton(onClick: () -> Unit, disabled: Boolean) {
  SettingsActionItem(
    painterResource(MR.images.ic_check),
    stringResource(MR.strings.invite_to_group_button),
    click = onClick,
    textColor = MaterialTheme.colors.primary,
    iconColor = MaterialTheme.colors.primary,
    disabled = disabled,
  )
}

@Composable
fun SkipInvitingButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_check),
    stringResource(MR.strings.skip_inviting_button),
    click = onClick,
    textColor = MaterialTheme.colors.primary,
    iconColor = MaterialTheme.colors.primary,
  )
}

@Composable
fun InviteSectionFooter(selectedContactsCount: Int, enabled: Boolean, clearSelection: () -> Unit) {
  Row(
    Modifier.fillMaxWidth(),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    if (selectedContactsCount >= 1) {
      Text(
        String.format(generalGetString(MR.strings.num_contacts_selected), selectedContactsCount),
        color = MaterialTheme.colors.secondary,
        fontSize = 12.sp
      )
      Box(
        Modifier.clickable { if (enabled) clearSelection() }
      ) {
        Text(
          stringResource(MR.strings.clear_contacts_selection_button),
          color = if (enabled) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
          fontSize = 12.sp
        )
      }
    } else {
      Text(
        stringResource(MR.strings.no_contacts_selected),
        color = MaterialTheme.colors.secondary,
        fontSize = 12.sp
      )
    }
  }
}

@Composable
fun ContactList(
  contacts: List<Contact>,
  selectedContacts: List<Long>,
  groupInfo: GroupInfo,
  enabled: Boolean,
  addContact: (Long) -> Unit,
  removeContact: (Long) -> Unit
) {
  Column {
    contacts.forEachIndexed { index, contact ->
      ContactCheckRow(
        contact, groupInfo, addContact, removeContact,
        checked = selectedContacts.contains(contact.apiId),
        enabled = enabled,
      )
    }
  }
}

@Composable
fun ContactCheckRow(
  contact: Contact,
  groupInfo: GroupInfo,
  addContact: (Long) -> Unit,
  removeContact: (Long) -> Unit,
  checked: Boolean,
  enabled: Boolean,
) {
  val prohibitedToInviteIncognito = !groupInfo.membership.memberIncognito && contact.contactConnIncognito
  val icon: Painter
  val iconColor: Color
  if (prohibitedToInviteIncognito) {
    icon = painterResource(MR.images.ic_theater_comedy_filled)
    iconColor = MaterialTheme.colors.secondary
  } else if (checked) {
    icon = painterResource(MR.images.ic_check_circle_filled)
    iconColor = if (enabled) MaterialTheme.colors.primary else MaterialTheme.colors.secondary
  } else {
    icon = painterResource(MR.images.ic_circle)
    iconColor = MaterialTheme.colors.secondary
  }
  SectionItemView(
    click = if (enabled) {
      {
        if (prohibitedToInviteIncognito) {
          showProhibitedToInviteIncognitoAlertDialog()
        } else if (!checked)
          addContact(contact.apiId)
        else
          removeContact(contact.apiId)
      }
    } else null
  ) {
    ProfileImage(size = 36.dp, contact.image)
    Spacer(Modifier.width(DEFAULT_SPACE_AFTER_ICON))
    Text(
      contact.chatViewName, maxLines = 1, overflow = TextOverflow.Ellipsis,
      color = if (prohibitedToInviteIncognito) MaterialTheme.colors.secondary else Color.Unspecified
    )
    Spacer(Modifier.fillMaxWidth().weight(1f))
    Icon(
      icon,
      contentDescription = stringResource(MR.strings.icon_descr_contact_checked),
      tint = iconColor
    )
  }
}

fun showProhibitedToInviteIncognitoAlertDialog() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.invite_prohibited),
    text = generalGetString(MR.strings.invite_prohibited_description),
    confirmText = generalGetString(MR.strings.ok),
  )
}

@Preview
@Composable
fun PreviewAddGroupMembersLayout() {
  SimpleXTheme {
    AddGroupMembersLayout(
      chatModelIncognito = false,
      groupInfo = GroupInfo.sampleData,
      creatingGroup = false,
      contactsToAdd = listOf(Contact.sampleData, Contact.sampleData, Contact.sampleData),
      selectedContacts = remember { mutableStateListOf() },
      selectedRole = remember { mutableStateOf(GroupMemberRole.Admin) },
      allowModifyMembers = true,
      openPreferences = {},
      inviteMembers = {},
      clearSelection = {},
      addContact = {},
      removeContact = {},
      close = {},
    )
  }
}
