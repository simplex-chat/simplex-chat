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
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.runtime.snapshots.SnapshotStateList
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
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

@Composable
fun AddGroupMembersView(groupInfo: GroupInfo, chatModel: ChatModel, close: () -> Unit) {
  val selectedContacts = remember { mutableStateListOf<Long>() }
  val selectedRole = remember { mutableStateOf(GroupMemberRole.Admin) }

  BackHandler(onBack = close)
  AddGroupMembersLayout(
    groupInfo = groupInfo,
    contactsToAdd = getContactsToAdd(chatModel),
    selectedContacts = selectedContacts,
    selectedRole = selectedRole,
    inviteMembers = {
      withApi {
        selectedContacts.forEach {
          val member = chatModel.controller.apiAddMember(groupInfo.groupId, it, selectedRole.value)
          if (member != null) {
            chatModel.upsertGroupMember(groupInfo, member)
          }
        }
        close.invoke()
      }
    },
    clearSelection = { selectedContacts.clear() },
    addContact = { contactId -> if (contactId !in selectedContacts) selectedContacts.add(contactId) },
    removeContact = { contactId -> selectedContacts.removeIf { it == contactId } },
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
  contactsToAdd: List<Contact>,
  selectedContacts: SnapshotStateList<Long>,
  selectedRole: MutableState<GroupMemberRole>,
  inviteMembers: () -> Unit,
  clearSelection: () -> Unit,
  addContact: (Long) -> Unit,
  removeContact: (Long) -> Unit,
) {
  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState()),
    horizontalAlignment = Alignment.Start,
  ) {
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
        SectionItemView {
          RoleSelectionRow(groupInfo, selectedRole)
        }
        SectionDivider()
        SectionItemView {
          InviteMembersButton(inviteMembers, disabled = selectedContacts.isEmpty())
        }
      }
      SectionCustomFooter {
        InviteSectionFooter(selectedContactsCount = selectedContacts.count(), clearSelection)
      }
      SectionSpacer()

      SectionView {
        ContactList(contacts = contactsToAdd, selectedContacts, addContact, removeContact)
      }
      SectionSpacer()
    }
  }
}

@Composable
fun RoleSelectionRow(groupInfo: GroupInfo, selectedRole: MutableState<GroupMemberRole>) {
  Row(
    Modifier.fillMaxWidth(),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.SpaceBetween
  ) {
    Text(stringResource(R.string.new_member_role))
    RoleDropdownMenu(groupInfo, selectedRole)
  }
}

@Composable
fun RoleDropdownMenu(groupInfo: GroupInfo, selectedRole: MutableState<GroupMemberRole>) {
  val options = GroupMemberRole.values()
    .filter { it <= groupInfo.membership.memberRole }
  var expanded by remember { mutableStateOf(false) }

  ExposedDropdownMenuBox(
    expanded = expanded,
    onExpandedChange = {
      expanded = !expanded
    }
  ) {
    Row(
      Modifier.fillMaxWidth(0.7f),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.End
    ) {
      Text(
        selectedRole.value.text,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        color = HighOrLowlight
      )
      Spacer(Modifier.size(4.dp))
      Icon(
        if (!expanded) Icons.Outlined.ExpandMore else Icons.Outlined.ExpandLess,
        generalGetString(R.string.invite_to_group_button),
        modifier = Modifier.padding(start = 8.dp),
        tint = HighOrLowlight
      )
    }
    ExposedDropdownMenu(
      expanded = expanded,
      onDismissRequest = {
        expanded = false
      }
    ) {
      options.forEach { selectionOption ->
        DropdownMenuItem(
          onClick = {
            selectedRole.value = selectionOption
            expanded = false
          }
        ) {
          Text(
            selectionOption.text,
            maxLines = 1,
            overflow = TextOverflow.Ellipsis,
          )
        }
      }
    }
  }
}

@Composable
fun InviteMembersButton(inviteMembers: () -> Unit, disabled: Boolean) {
  val modifier = if (disabled) Modifier else Modifier.clickable { inviteMembers() }
  Row(
    modifier.fillMaxSize(),
    horizontalArrangement = Arrangement.End,
    verticalAlignment = Alignment.CenterVertically
  ) {
    val color = if (disabled) HighOrLowlight else MaterialTheme.colors.primary
    Text(stringResource(R.string.invite_to_group_button), color = color)
    Spacer(Modifier.size(8.dp))
    Icon(
      Icons.Outlined.Check,
      stringResource(R.string.invite_to_group_button),
      tint = color
    )
  }
}

@Composable
fun InviteSectionFooter(selectedContactsCount: Int, clearSelection: () -> Unit) {
  Row(
    Modifier.fillMaxWidth(),
    horizontalArrangement = if (selectedContactsCount >= 1) Arrangement.SpaceBetween else Arrangement.End,
    verticalAlignment = Alignment.CenterVertically
  ) {
    if (selectedContactsCount >= 1) {
      Box(
        Modifier.clickable { clearSelection() }
      ) {
        Text(
          stringResource(R.string.clear_contacts_selection_button),
          color = MaterialTheme.colors.primary,
          fontSize = 12.sp
        )
      }

      Text(
        String.format(generalGetString(R.string.num_contacts_selected), selectedContactsCount),
        color = HighOrLowlight,
        fontSize = 12.sp
      )
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
  selectedContacts: SnapshotStateList<Long>,
  addContact: (Long) -> Unit,
  removeContact: (Long) -> Unit
) {
  Column {
    contacts.forEachIndexed { index, contact ->
      SectionItemView {
        ContactCheckRow(
          contact, addContact, removeContact,
          checked = selectedContacts.contains(contact.apiId)
        )
      }
      if (index < contacts.lastIndex) {
        SectionDivider()
      }
    }
  }
}

@Composable
fun ContactCheckRow(
  contact: Contact,
  addContact: (Long) -> Unit,
  removeContact: (Long) -> Unit,
  checked: Boolean
) {
  Row(
    Modifier
      .fillMaxSize()
      .clickable { if (!checked) addContact(contact.apiId) else removeContact(contact.apiId) },
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    Row(
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(4.dp)
    ) {
      ProfileImage(size = 36.dp, contact.image)
      Text(contact.chatViewName, maxLines = 1, overflow = TextOverflow.Ellipsis)
    }
    Icon(
      if (checked) Icons.Filled.CheckCircle else Icons.Outlined.Circle,
      contentDescription = stringResource(R.string.icon_descr_contact_checked),
      tint = if (checked) MaterialTheme.colors.primary else HighOrLowlight
    )
  }
}

@Preview
@Composable
fun PreviewAddGroupMembersLayout() {
  SimpleXTheme {
    AddGroupMembersLayout(
      groupInfo = GroupInfo.sampleData,
      contactsToAdd = listOf(Contact.sampleData, Contact.sampleData, Contact.sampleData),
      selectedContacts = remember { mutableStateListOf() },
      selectedRole = remember { mutableStateOf(GroupMemberRole.Admin) },
      inviteMembers = {},
      clearSelection = {},
      addContact = {},
      removeContact = {}
    )
  }
}
