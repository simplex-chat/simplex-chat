package chat.simplex.app.views.chat.group

import SectionDivider
import SectionItemView
import SectionSpacer
import SectionView
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.CheckCircle
import androidx.compose.material.icons.outlined.Check
import androidx.compose.material.icons.outlined.Circle
import androidx.compose.runtime.*
import androidx.compose.runtime.snapshots.SnapshotStateList
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.ChatInfoToolbarTitle
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.*

@Composable
fun AddGroupMembersView(groupInfo: GroupInfo, chatModel: ChatModel, close: () -> Unit) {
  val selectedContacts = remember { mutableStateListOf<Long>() }
  BackHandler(onBack = close)
  AddGroupMembersLayout(
    groupInfo = groupInfo,
    contactsToAdd = getContactsToAdd(chatModel),
    selectedContacts = selectedContacts,
    inviteMembers = {
      withApi {
        selectedContacts.forEach {
          chatModel.controller.apiAddMember(groupInfo.groupId, it, GroupMemberRole.Admin)
        }
        close.invoke()
      }
    },
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
  inviteMembers: () -> Unit,
  addContact: (Long) -> Unit,
  removeContact: (Long) -> Unit,
) {
  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start,
  ) {
    Row(
      Modifier.fillMaxWidth(),
      horizontalArrangement = Arrangement.Center
    ) {
      ChatInfoToolbarTitle(ChatInfo.Group(groupInfo), imageSize = 60.dp, iconColor = HighOrLowlight) // TODO tertiary color
    }
    SectionSpacer()

    SectionView {
      SectionItemView() {
        InviteMembersButton(inviteMembers, disabled = selectedContacts.isEmpty())
      }
    }
    SectionSpacer()

    SectionView {
      ContactList(contacts = contactsToAdd, selectedContacts, addContact, removeContact)
    }
  }
}

@Composable
fun InviteMembersButton(inviteMembers: () -> Unit, disabled: Boolean) {
  Row(
    Modifier.fillMaxWidth(),
    horizontalArrangement = Arrangement.End
  ) {
    SimpleButtonFrame(inviteMembers, disabled) {
      val color = if (disabled) HighOrLowlight else MaterialTheme.colors.primary
      Text(generalGetString(R.string.invite_to_group_button), color = color)
      Icon(
        Icons.Outlined.Check,
        generalGetString(R.string.invite_to_group_button),
        tint = color,
        modifier = Modifier.padding(start = 8.dp)
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
  LazyColumn {
    itemsIndexed(contacts) { index, contact ->
      ContactCheckRow(
        contact, addContact, removeContact,
        checked = selectedContacts.contains(contact.apiId)
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
  addContact: (Long) -> Unit,
  removeContact: (Long) -> Unit,
  checked: Boolean
) {
  SectionItemView {
    Row(
      Modifier
        .fillMaxWidth()
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
}

@Preview
@Composable
fun PreviewAddGroupMembersLayout() {
  SimpleXTheme {
    AddGroupMembersLayout(
      groupInfo = GroupInfo.sampleData,
      contactsToAdd = listOf(Contact.sampleData, Contact.sampleData, Contact.sampleData),
      selectedContacts = remember { mutableStateListOf() },
      inviteMembers = {},
      addContact = {},
      removeContact = {}
    )
  }
}
