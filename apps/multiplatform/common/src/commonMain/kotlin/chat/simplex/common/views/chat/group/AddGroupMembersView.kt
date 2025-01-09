package chat.simplex.common.views.chat.group

import SectionBottomSpacer
import SectionCustomFooter
import SectionDividerSpaced
import SectionItemView
import SectionItemViewWithoutMinPadding
import SectionSpacer
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.withChats
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.ChatInfoToolbarTitle
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.common.model.GroupInfo
import chat.simplex.common.platform.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource

@Composable
fun AddGroupMembersView(rhId: Long?, groupInfo: GroupInfo, creatingGroup: Boolean = false, chatModel: ChatModel, close: () -> Unit) {
  val selectedContacts = remember { mutableStateListOf<Long>() }
  val selectedRole = remember { mutableStateOf(GroupMemberRole.Member) }
  var allowModifyMembers by remember { mutableStateOf(true) }
  val searchText = rememberSaveable(stateSaver = TextFieldValue.Saver) { mutableStateOf(TextFieldValue("")) }
  BackHandler(onBack = close)
  AddGroupMembersLayout(
    groupInfo = groupInfo,
    creatingGroup = creatingGroup,
    contactsToAdd = getContactsToAdd(chatModel, searchText.value.text),
    selectedContacts = selectedContacts,
    selectedRole = selectedRole,
    allowModifyMembers = allowModifyMembers,
    searchText,
    openPreferences = {
      ModalManager.end.showCustomModal { close ->
        GroupPreferencesView(chatModel, rhId, groupInfo.id, close)
      }
    },
    inviteMembers = {
      allowModifyMembers = false
      withLongRunningApi(slow = 120_000) {
        for (contactId in selectedContacts) {
          val member = chatModel.controller.apiAddMember(rhId, groupInfo.groupId, contactId, selectedRole.value)
          if (member != null) {
            withChats {
              upsertGroupMember(rhId, groupInfo, member)
            }
          } else {
            break
          }
        }
        close.invoke()
      }
    },
    clearSelection = { selectedContacts.clear() },
    addContact = { contactId -> if (contactId !in selectedContacts) selectedContacts.add(contactId) },
    removeContact = { contactId -> selectedContacts.removeAll { it == contactId } },
    close = close,
  )
  KeyChangeEffect(chatModel.chatId.value) {
    close()
  }
}

fun getContactsToAdd(chatModel: ChatModel, search: String): List<Contact> {
  val s = search.trim().lowercase()
  val memberContactIds = chatModel.groupMembers.value
    .filter { it.memberCurrent }
    .mapNotNull { it.memberContactId }
  return chatModel.chats.value
    .asSequence()
    .map { it.chatInfo }
    .filterIsInstance<ChatInfo.Direct>()
    .map { it.contact }
    .filter { c -> c.sendMsgEnabled && !c.nextSendGrpInv && c.contactId !in memberContactIds && c.anyNameContains(s)
    }
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
  searchText: MutableState<TextFieldValue>,
  openPreferences: () -> Unit,
  inviteMembers: () -> Unit,
  clearSelection: () -> Unit,
  addContact: (Long) -> Unit,
  removeContact: (Long) -> Unit,
  close: () -> Unit,
) {
  @Composable fun profileText() {
    Row(
      Modifier
        .fillMaxWidth()
        .padding(vertical = 4.dp),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.Center
    ) {
      Icon(
        painterResource(MR.images.ic_info),
        null,
        tint = MaterialTheme.colors.secondary,
        modifier = Modifier.padding(end = 10.dp).size(20.dp)
      )
      val textId = if (groupInfo.businessChat == null) MR.strings.group_main_profile_sent else MR.strings.chat_main_profile_sent
      Text(generalGetString(textId), textAlign = TextAlign.Center, style = MaterialTheme.typography.body2)
    }
  }

  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.button_add_members))
    profileText()
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

    if (contactsToAdd.isEmpty() && searchText.value.text.isEmpty()) {
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
          val titleId = if (groupInfo.businessChat == null) MR.strings.invite_to_group_button else MR.strings.invite_to_chat_button
          InviteMembersButton(titleId, inviteMembers, disabled = selectedContacts.isEmpty() || !allowModifyMembers)
        }
      }
      SectionCustomFooter {
        InviteSectionFooter(selectedContactsCount = selectedContacts.size, allowModifyMembers, clearSelection)
      }
      SectionDividerSpaced(maxTopPadding = true)
      SectionView(stringResource(MR.strings.select_contacts).uppercase()) {
        SectionItemView(padding = PaddingValues(start = DEFAULT_PADDING, end = DEFAULT_PADDING_HALF)) {
          SearchRowView(searchText)
        }
        ContactList(contacts = contactsToAdd, selectedContacts, groupInfo, allowModifyMembers, addContact, removeContact)
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun SearchRowView(
  searchText: MutableState<TextFieldValue> = rememberSaveable(stateSaver = TextFieldValue.Saver) { mutableStateOf(TextFieldValue()) }
) {
  Box(Modifier.width(36.dp), contentAlignment = Alignment.Center) {
    Icon(painterResource(MR.images.ic_search), stringResource(MR.strings.search_verb), tint = MaterialTheme.colors.secondary)
  }
  Spacer(Modifier.width(DEFAULT_SPACE_AFTER_ICON))
  SearchTextField(Modifier.fillMaxWidth(), searchText = searchText, alwaysVisible = true) {
    searchText.value = searchText.value.copy(it)
  }
}

@Composable
private fun RoleSelectionRow(groupInfo: GroupInfo, selectedRole: MutableState<GroupMemberRole>, enabled: Boolean) {
  Row(
    Modifier.fillMaxWidth(),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.SpaceBetween
  ) {
    val values = GroupMemberRole.selectableRoles
      .filter { it <= groupInfo.membership.memberRole }
      .map { it to it.text }
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
fun InviteMembersButton(titleId: StringResource, onClick: () -> Unit, disabled: Boolean) {
  SettingsActionItem(
    painterResource(MR.images.ic_check),
    stringResource(titleId),
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
        lineHeight = 18.sp,
        fontSize = 14.sp
      )
      Box(
        Modifier.clickable { if (enabled) clearSelection() }
      ) {
        Text(
          stringResource(MR.strings.clear_contacts_selection_button),
          color = if (enabled) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
          lineHeight = 18.sp,
          fontSize = 14.sp,
        )
      }
    } else {
      Text(
        stringResource(MR.strings.no_contacts_selected),
        color = MaterialTheme.colors.secondary,
        lineHeight = 18.sp,
        fontSize = 14.sp,
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
  SectionItemViewWithoutMinPadding(
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
      contact.chatViewName,
      modifier = Modifier.weight(10f, fill = true),
      maxLines = 1,
      overflow = TextOverflow.Ellipsis,
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
      groupInfo = GroupInfo.sampleData,
      creatingGroup = false,
      contactsToAdd = listOf(Contact.sampleData, Contact.sampleData, Contact.sampleData),
      selectedContacts = remember { mutableStateListOf() },
      selectedRole = remember { mutableStateOf(GroupMemberRole.Admin) },
      allowModifyMembers = true,
      searchText = remember { mutableStateOf(TextFieldValue("")) },
      openPreferences = {},
      inviteMembers = {},
      clearSelection = {},
      addContact = {},
      removeContact = {},
      close = {},
    )
  }
}
