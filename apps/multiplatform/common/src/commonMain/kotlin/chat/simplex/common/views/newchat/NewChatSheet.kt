package chat.simplex.common.views.newchat

import SectionDividerSpaced
import SectionItemView
import SectionView
import TextIconSpaced
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.Painter
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatController
import chat.simplex.common.model.RemoteHostInfo
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.contacts.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR

@Composable
fun NewChatButton(icon: Painter, text: String, click: () -> Unit, textColor: Color = Color.Unspecified, iconColor: Color = MaterialTheme.colors.secondary, disabled: Boolean = false, extraPadding: Boolean = false) {
  SectionItemView(click, disabled = disabled) {
    Icon(icon, text, tint = if (disabled) MaterialTheme.colors.secondary else iconColor)
    TextIconSpaced(extraPadding)
    Text(text, color = if (disabled) MaterialTheme.colors.secondary else textColor)
  }
}

@Composable
fun NewChatSheet(rh: RemoteHostInfo?, close: () -> Unit) {
  Column(
    Modifier.fillMaxSize(),
  ) {
    Box(contentAlignment = Alignment.Center) {
      val bottomPadding = DEFAULT_PADDING
      AppBarTitle(
        stringResource(MR.strings.new_chat),
        hostDevice(rh?.remoteHostId),
        bottomPadding = bottomPadding
      )
    }

    val closeAll = { ModalManager.closeAllModalsEverywhere() }

    NewChatSheetLayout(
      addContact = {
        ModalManager.center.showModalCloseable { _ -> NewChatView(chatModel.currentRemoteHost.value, NewChatOption.INVITE, close = closeAll ) }
      },
      scanPaste = {
        ModalManager.center.showModalCloseable { _ -> NewChatView(chatModel.currentRemoteHost.value, NewChatOption.CONNECT, showQRCodeScanner = true, close = closeAll) }
      },
      createGroup = {
        ModalManager.center.showCustomModal { close -> AddGroupView(chatModel, chatModel.currentRemoteHost.value, close, closeAll) }
      },
      rh = rh,
      close = close
    )
  }
}

private var lazyListState = 0 to 0

@Composable
fun NewChatSheetLayout(
  rh: RemoteHostInfo?,
  addContact: () -> Unit,
  scanPaste: () -> Unit,
  createGroup: () -> Unit,
  close: () -> Unit,
) {
  val listState = rememberLazyListState(lazyListState.first, lazyListState.second)
  val searchText = rememberSaveable(stateSaver = TextFieldValue.Saver) { mutableStateOf(TextFieldValue("")) }
  val searchShowingSimplexLink = remember { mutableStateOf(false) }
  val searchChatFilteredBySimplexLink = remember { mutableStateOf<String?>(null) }
  val showUnreadAndFavorites = remember { ChatController.appPrefs.showUnreadAndFavorites.state }.value
  val baseContactTypes = listOf(ContactType.CARD, ContactType.RECENT, ContactType.REQUEST)
  val contactTypes by remember(baseContactTypes, searchText.value.text.isEmpty()) {
    derivedStateOf { getContactTypes(baseContactTypes, searchText.value.text.isEmpty()) }
  }
  val allChats by remember(chatModel.chats, contactTypes) {
    derivedStateOf { contactChats(chatModel.chats, contactTypes) }
  }
  val filteredContactChats = filteredContactChats(
    showUnreadAndFavorites = showUnreadAndFavorites,
    searchChatFilteredBySimplexLink = searchChatFilteredBySimplexLink,
    searchShowingSimplexLink = searchShowingSimplexLink,
    searchText = searchText.value.text,
    contactChats = allChats
  )

  LazyColumn(
    Modifier.fillMaxWidth(),
    listState
  ) {
    item {
      Divider()
      ContactsSearchBar(
        listState = listState,
        searchText = searchText,
        searchShowingSimplexLink = searchShowingSimplexLink,
        searchChatFilteredBySimplexLink = searchChatFilteredBySimplexLink,
        close = close
      )
      Divider()

      Spacer(Modifier.padding(bottom = DEFAULT_PADDING))

      if (searchText.value.text.isEmpty()) {
        SectionView {
          NewChatButton(
            icon = painterResource(MR.images.ic_add_link),
            text = stringResource(MR.strings.add_contact_tab),
            click = addContact,
            extraPadding = true
          )
          NewChatButton(
            icon = painterResource(MR.images.ic_qr_code),
            text = stringResource(MR.strings.scan_paste_link),
            click = scanPaste,
            extraPadding = true
          )
          NewChatButton(
            icon = painterResource(MR.images.ic_group),
            text = stringResource(MR.strings.create_group_button),
            click = createGroup,
            extraPadding = true
          )
        }
        SectionDividerSpaced(maxBottomPadding = false)

        val deletedContactTypes = listOf(ContactType.CHAT_DELETED)
        val deletedChats by remember(chatModel.chats, deletedContactTypes) {
          derivedStateOf { contactChats(chatModel.chats, deletedContactTypes) }
        }
        if (deletedChats.isNotEmpty()) {
          SectionView {
            SectionItemView(
              click = {
                ModalManager.center.showCustomModal { closeDeletedChats ->
                  ModalView(close = closeDeletedChats) {
                    DeletedContactsView(rh = rh, close = {
                      ModalManager.center.closeModals()
                    })
                  }
                }
              }
            ) {
              Icon(
                painterResource(MR.images.ic_folder_open),
                contentDescription = stringResource(MR.strings.deleted_chats),
                tint = MaterialTheme.colors.secondary,
              )
              TextIconSpaced(extraPadding = true)
              Text(text = stringResource(MR.strings.deleted_chats), color = MaterialTheme.colors.onBackground)
            }
          }
          SectionDividerSpaced()
        }
      }
    }

    item {
      if (filteredContactChats.isNotEmpty()) {
        Text(
          stringResource(MR.strings.contact_list_header_title).uppercase(), color = MaterialTheme.colors.secondary, style = MaterialTheme.typography.body2,
          modifier = Modifier.padding(start = DEFAULT_PADDING, bottom = 5.dp), fontSize = 12.sp
        )
      }
    }

    itemsIndexed(filteredContactChats) { index, chat ->
      val nextChatSelected = remember(chat.id, filteredContactChats) {
        derivedStateOf {
          chatModel.chatId.value != null && filteredContactChats.getOrNull(index + 1)?.id == chatModel.chatId.value
        }
      }
      ContactListNavLinkView(chat, nextChatSelected)
    }
  }

  if (filteredContactChats.isEmpty() && allChats.isNotEmpty()) {
    Column(Modifier.fillMaxSize().padding(DEFAULT_PADDING)) {
      Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
        Text(
          generalGetString(MR.strings.no_filtered_contacts),
          color = MaterialTheme.colors.secondary
        )
      }
    }
  }
}

@Composable
fun ActionButton(
  text: String?,
  comment: String?,
  icon: Painter,
  disabled: Boolean = false,
  click: () -> Unit = {}
) {
  Surface(shape = RoundedCornerShape(18.dp), color = Color.Transparent, contentColor = LocalContentColor.current) {
    Column(
      Modifier
        .clickable(onClick = click)
        .padding(8.dp),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      val tint = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
      Icon(
        icon, text,
        tint = tint,
        modifier = Modifier
          .size(40.dp)
          .padding(bottom = 8.dp)
      )
      if (text != null) {
        Text(
          text,
          textAlign = TextAlign.Center,
          fontWeight = FontWeight.Bold,
          color = tint,
          modifier = Modifier.padding(bottom = 4.dp)
        )
      }
      if (comment != null) {
        Text(
          comment,
          textAlign = TextAlign.Center,
          style = MaterialTheme.typography.body2
        )
      }
    }
  }
}

@Composable
fun ActionButton(
  modifier: Modifier,
  text: String?,
  comment: String?,
  icon: Painter,
  tint: Color = MaterialTheme.colors.primary,
  disabled: Boolean = false,
  click: () -> Unit = {}
) {
  Surface(modifier, shape = RoundedCornerShape(18.dp), contentColor = LocalContentColor.current) {
    Column(
      Modifier
        .fillMaxWidth()
        .clickable(onClick = click)
        .padding(8.dp),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      val tint = if (disabled) MaterialTheme.colors.secondary else tint
      Icon(
        icon, text,
        tint = tint,
        modifier = Modifier
          .size(40.dp)
          .padding(bottom = 8.dp)
      )
      if (text != null) {
        Text(
          text,
          textAlign = TextAlign.Center,
          fontWeight = FontWeight.Bold,
          color = tint,
          modifier = Modifier.padding(bottom = 4.dp)
        )
      }
      if (comment != null) {
        Text(
          comment,
          textAlign = TextAlign.Center,
          style = MaterialTheme.typography.body2
        )
      }
    }
  }
}

@Preview
@Composable
private fun PreviewNewChatSheet() {
  SimpleXTheme {
    NewChatSheetLayout(rh = null, scanPaste = {}, addContact = {}, createGroup = {}, close = {})
  }
}
