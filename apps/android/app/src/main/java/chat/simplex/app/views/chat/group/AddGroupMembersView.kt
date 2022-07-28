package chat.simplex.app.views.chat.group

import androidx.activity.compose.BackHandler
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
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

@Composable
fun AddGroupMembersView(groupInfo: GroupInfo, memberContactIds: List<Long>, chatModel: ChatModel, close: () -> Unit) {
  val selectedContacts = remember { mutableStateListOf<Long>() }
  BackHandler(onBack = close)
  AddGroupMembersLayout(
    groupInfo = groupInfo,
    contactsToAdd = getContactsToAdd(memberContactIds, chatModel),
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
    close = close
  )
}

fun getContactsToAdd(memberContactIds: List<Long>, chatModel: ChatModel): List<Contact> {
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
  close: () -> Unit
) {
  Column(
    Modifier
      .fillMaxSize()
      .background(MaterialTheme.colors.background)
      .padding(horizontal = 8.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    CloseSheetBar(close)
    ChatInfoToolbarTitle(ChatInfo.Group(groupInfo), imageSize = 60.dp)
    InviteMembersButton(inviteMembers, disabled = selectedContacts.isEmpty())
    ContactList(contacts = contactsToAdd, selectedContacts, addContact, removeContact)
  }
}

@Composable
fun InviteMembersButton(inviteMembers: () -> Unit, disabled: Boolean) {
  SimpleButtonFrame(inviteMembers, disabled) {
    val color = if (disabled) HighOrLowlight else MaterialTheme.colors.primary
    Text(generalGetString(R.string.invite_to_group_button), color = color)
    Icon(
      Icons.Outlined.Check,
      generalGetString(R.string.invite_to_group_button),
      tint = color,
      modifier = Modifier.padding(end = 8.dp)
    )
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
    items(contacts) { contact ->
      ContactCheckRow(
        contact, addContact, removeContact,
        checked = selectedContacts.contains(contact.apiId)
      )
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
      .clickable { if (!checked) addContact(contact.apiId) else removeContact(contact.apiId) }
      .fillMaxWidth(),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    Row {
      ProfileImage(size = 30.dp, contact.image)
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
      inviteMembers = {},
      addContact = {},
      removeContact = {},
      close = {}
    )
  }
}
