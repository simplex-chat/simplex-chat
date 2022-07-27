package chat.simplex.app.views.chat.group

import androidx.activity.compose.BackHandler
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*

@Composable
fun AddGroupMembersView(groupInfo: GroupInfo, memberContactIds: List<Long>, chatModel: ChatModel, close: () -> Unit) {
  BackHandler(onBack = close)
  AddGroupMembersLayout(
    groupInfo = groupInfo,
    contactsToAdd = getContactsToAdd(memberContactIds, chatModel),
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
fun AddGroupMembersLayout(groupInfo: GroupInfo, contactsToAdd: List<Contact>, close: () -> Unit) {
  Column(
    Modifier
      .fillMaxSize()
      .background(MaterialTheme.colors.background)
      .padding(horizontal = 8.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    CloseSheetBar(close)
    ContactList(contacts = contactsToAdd)
  }
}

@Composable
fun ContactList(contacts: List<Contact>) {
  LazyColumn {
    items(contacts) { contact ->
      ContactCheckRow(contact)
    }
  }
}

@Composable
fun ContactCheckRow(contact: Contact) {
  Row {
    Text(contact.profile.displayName)
  }
}

@Preview
@Composable
fun PreviewAddGroupMembersLayout() {
  SimpleXTheme {
    AddGroupMembersLayout(
      groupInfo = GroupInfo.sampleData,
      contactsToAdd = listOf(Contact.sampleData, Contact.sampleData, Contact.sampleData),
      close = {}
    )
  }
}
