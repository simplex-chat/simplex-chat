package chat.simplex.app.views.chat.group

import SectionCustomFooter
import SectionDivider
import SectionItemView
import SectionSpacer
import SectionView
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.CheckCircle
import androidx.compose.material.icons.filled.TheaterComedy
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.ChatInfoToolbarTitle
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.SettingsActionItem

@Composable
fun AddGroupMembersView(groupInfo: GroupInfo, creatingGroup: Boolean = false, chatModel: ChatModel, close: () -> Unit) {
  val selectedContacts = remember { mutableStateListOf<Long>() }
  val selectedRole = remember { mutableStateOf(GroupMemberRole.Member) }
  var allowModifyMembers by remember { mutableStateOf(true) }
  BackHandler(onBack = close)
  AddGroupMembersLayout(
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
    horizontalAlignment = Alignment.Start,
  ) {
    AppBarTitle(stringResource(R.string.button_add_members))
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
          stringResource(R.string.no_contacts_to_add),
          Modifier.padding(),
          color = HighOrLowlight
        )
      }
    } else {
      SectionView {
        if (creatingGroup) {
          SectionItemView(openPreferences) {
            Text(stringResource(R.string.set_group_preferences))
          }
          SectionDivider()
        }
        SectionItemView {
          RoleSelectionRow(groupInfo, selectedRole, allowModifyMembers)
        }
        SectionDivider()
        if (creatingGroup && selectedContacts.isEmpty()) {
          SkipInvitingButton(close)
        } else {
          InviteMembersButton(inviteMembers, disabled = selectedContacts.isEmpty() || !allowModifyMembers)
        }
      }
      SectionCustomFooter {
        InviteSectionFooter(selectedContactsCount = selectedContacts.size, allowModifyMembers, clearSelection)
      }
      SectionSpacer()

      SectionView(stringResource(R.string.select_contacts)) {
        ContactList(contacts = contactsToAdd, selectedContacts, groupInfo, allowModifyMembers, addContact, removeContact)
      }
      SectionSpacer()
    }
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
      generalGetString(R.string.new_member_role),
      values,
      selectedRole,
      icon = null,
      enabled = rememberUpdatedState(enabled),
      onSelected = { selectedRole.value = it }
    )
  }
}

@Composable
fun InviteMembersButton(onClick: () -> Unit, disabled: Boolean) {
  SettingsActionItem(
    Icons.Outlined.Check,
    stringResource(R.string.invite_to_group_button),
    click = onClick,
    textColor = MaterialTheme.colors.primary,
    iconColor = MaterialTheme.colors.primary,
    disabled = disabled,
  )
}

@Composable
fun SkipInvitingButton(onClick: () -> Unit) {
  SettingsActionItem(
    Icons.Outlined.Check,
    stringResource(R.string.skip_inviting_button),
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
        String.format(generalGetString(R.string.num_contacts_selected), selectedContactsCount),
        color = HighOrLowlight,
        fontSize = 12.sp
      )
      Box(
        Modifier.clickable { if (enabled) clearSelection() }
      ) {
        Text(
          stringResource(R.string.clear_contacts_selection_button),
          color = if (enabled) MaterialTheme.colors.primary else HighOrLowlight,
          fontSize = 12.sp
        )
      }
    } else {
      Text(
        stringResource(R.string.no_contacts_selected),
        color = HighOrLowlight,
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
      if (index < contacts.lastIndex) {
        SectionDivider()
      }
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
  val icon: ImageVector
  val iconColor: Color
  if (prohibitedToInviteIncognito) {
    icon = Icons.Filled.TheaterComedy
    iconColor = HighOrLowlight
  } else if (checked) {
    icon = Icons.Filled.CheckCircle
    iconColor = if (enabled) MaterialTheme.colors.primary else HighOrLowlight
  } else {
    icon = Icons.Outlined.Circle
    iconColor = HighOrLowlight
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
      color = if (prohibitedToInviteIncognito) HighOrLowlight else Color.Unspecified
    )
    Spacer(Modifier.fillMaxWidth().weight(1f))
    Icon(
      icon,
      contentDescription = stringResource(R.string.icon_descr_contact_checked),
      tint = iconColor
    )
  }
}

fun showProhibitedToInviteIncognitoAlertDialog() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(R.string.invite_prohibited),
    text = generalGetString(R.string.invite_prohibited_description),
    confirmText = generalGetString(R.string.ok),
  )
}

@Preview
@Composable
fun PreviewAddGroupMembersLayout() {
  SimpleXTheme {
    AddGroupMembersLayout(
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
