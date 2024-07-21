package chat.simplex.common.views.newchat

import SectionBottomSpacer
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.pager.HorizontalPager
import androidx.compose.foundation.pager.rememberPagerState
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.model.ChatModel.getChat
import chat.simplex.common.model.ChatModel.hasChat
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.MR
import kotlinx.coroutines.launch
import java.net.URI

enum class NewChatOption {
  INVITE, CONNECT
}

@Composable
fun ModalData.NewChatView(rh: RemoteHostInfo?, selection: NewChatOption, showQRCodeScanner: Boolean = false, close: () -> Unit) {
  val selection = remember { stateGetOrPut("selection") { selection } }
  val showQRCodeScanner = remember { stateGetOrPut("showQRCodeScanner") { showQRCodeScanner } }
  val contactConnection: MutableState<PendingContactConnection?> = rememberSaveable(stateSaver = serializableSaver()) { mutableStateOf(null) }
  val connReqInvitation by remember { derivedStateOf { chatModel.showingInvitation.value?.connReq ?: "" } }
  val creatingConnReq = rememberSaveable { mutableStateOf(false) }
  val pastedLink = rememberSaveable { mutableStateOf("") }
  LaunchedEffect(selection.value) {
    if (
      selection.value == NewChatOption.INVITE
      && connReqInvitation.isEmpty()
      && contactConnection.value == null
      && !creatingConnReq.value
    ) {
      createInvitation(rh?.remoteHostId, creatingConnReq, connReqInvitation, contactConnection)
    }
  }
  DisposableEffect(Unit) {
    onDispose {
      /** When [AddContactLearnMore] is open, we don't need to drop [ChatModel.showingInvitation].
       * Otherwise, it will be called here AFTER [AddContactLearnMore] is launched and will clear the value too soon.
       * It will be dropped automatically when connection established or when user goes away from this screen.
       * It applies only to Android because on Desktop center space will not be overlapped by [AddContactLearnMore]
       **/
      if (chatModel.showingInvitation.value != null && chatModel.newChatConnectionStage.value == NewChatConnectionStage.STARTED) {
        chatModel.newChatConnectionStage.value = NewChatConnectionStage.IDLE

        val conn = contactConnection.value
        if (chatModel.showingInvitation.value?.connChatUsed == false && conn != null) {
          AlertManager.shared.showAlertDialog(
            title = generalGetString(MR.strings.keep_unused_invitation_question),
            text = generalGetString(MR.strings.you_can_view_invitation_link_again),
            confirmText = generalGetString(MR.strings.delete_verb),
            dismissText = generalGetString(MR.strings.keep_invitation_link),
            destructive = true,
            onConfirm = {
              withBGApi {
                val chatInfo = ChatInfo.ContactConnection(conn)
                controller.deleteChat(Chat(remoteHostId = rh?.remoteHostId, chatInfo = chatInfo, chatItems = listOf()))
                if (chatModel.chatId.value == chatInfo.id) {
                  chatModel.chatId.value = null
                  ModalManager.end.closeModals()
                }
              }
            }
          )
        }
        chatModel.showingInvitation.value = null
      }
    }
  }
  val tabTitles = NewChatOption.values().map {
    when(it) {
      NewChatOption.INVITE ->
        stringResource(MR.strings.add_contact_tab)
      NewChatOption.CONNECT ->
        stringResource(MR.strings.connect_via_link)
    }
  }

  Column(
    Modifier.fillMaxSize(),
  ) {
    Box(contentAlignment = Alignment.Center) {
      val bottomPadding = DEFAULT_PADDING
      AppBarTitle(stringResource(MR.strings.new_chat), hostDevice(rh?.remoteHostId), bottomPadding = bottomPadding)
      Column(Modifier.align(Alignment.CenterEnd).padding(bottom = bottomPadding, end = DEFAULT_PADDING)) {
        AddContactLearnMoreButton()
      }
    }
    val scope = rememberCoroutineScope()
    val pagerState = rememberPagerState(
      initialPage = selection.value.ordinal,
      initialPageOffsetFraction = 0f
    ) { NewChatOption.values().size }
    KeyChangeEffect(pagerState.currentPage) {
      selection.value = NewChatOption.values()[pagerState.currentPage]
    }
    TabRow(
      selectedTabIndex = pagerState.currentPage,
      backgroundColor = Color.Transparent,
      contentColor = MaterialTheme.colors.primary,
    ) {
      tabTitles.forEachIndexed { index, it ->
        LeadingIconTab(
          selected = pagerState.currentPage == index,
          onClick = {
            scope.launch {
              pagerState.animateScrollToPage(index)
            }
          },
          text = { Text(it, fontSize = 13.sp) },
          icon = {
            Icon(
              if (NewChatOption.INVITE.ordinal == index) painterResource(MR.images.ic_repeat_one) else painterResource(MR.images.ic_qr_code),
              it
            )
          },
          selectedContentColor = MaterialTheme.colors.primary,
          unselectedContentColor = MaterialTheme.colors.secondary,
        )
      }
    }

    HorizontalPager(state = pagerState, Modifier.fillMaxSize(), verticalAlignment = Alignment.Top, userScrollEnabled = appPlatform.isAndroid) { index ->
      // LALAL SCROLLBAR DOESN'T WORK
      ColumnWithScrollBar(
        Modifier
          .fillMaxSize(),
        verticalArrangement = if (index == NewChatOption.INVITE.ordinal && connReqInvitation.isEmpty()) Arrangement.Center else Arrangement.Top) {
        Spacer(Modifier.height(DEFAULT_PADDING))
        when (index) {
          NewChatOption.INVITE.ordinal -> {
            PrepareAndInviteView(rh?.remoteHostId, contactConnection, connReqInvitation, creatingConnReq)
          }
          NewChatOption.CONNECT.ordinal -> {
            ConnectView(rh?.remoteHostId, showQRCodeScanner, pastedLink, close)
          }
        }
        SectionBottomSpacer()
      }
    }
  }
}

@Composable
private fun PrepareAndInviteView(rhId: Long?, contactConnection: MutableState<PendingContactConnection?>, connReqInvitation: String, creatingConnReq: MutableState<Boolean>) {
  if (connReqInvitation.isNotEmpty()) {
    InviteView(
      rhId,
      connReqInvitation = connReqInvitation,
      contactConnection = contactConnection,
    )
  } else if (creatingConnReq.value) {
    CreatingLinkProgressView()
  } else {
    RetryButton { createInvitation(rhId, creatingConnReq, connReqInvitation, contactConnection) }
  }
}

@Composable
private fun CreatingLinkProgressView() {
  DefaultProgressView(stringResource(MR.strings.creating_link))
}

@Composable
private fun RetryButton(onClick: () -> Unit) {
  Column(
    Modifier.fillMaxSize(),
    horizontalAlignment = Alignment.CenterHorizontally,
    verticalArrangement = Arrangement.Center
  ) {
    IconButton(onClick, Modifier.size(30.dp)) {
      Icon(painterResource(MR.images.ic_refresh), null)
    }
    Spacer(Modifier.height(DEFAULT_PADDING))
    Text(stringResource(MR.strings.retry_verb))
  }
}

@Composable
private fun InviteView(rhId: Long?, connReqInvitation: String, contactConnection: MutableState<PendingContactConnection?>) {
  SectionView(stringResource(MR.strings.share_this_1_time_link).uppercase()) {
    LinkTextView(connReqInvitation, true)
  }

  Spacer(Modifier.height(10.dp))

  SectionView(stringResource(MR.strings.or_show_this_qr_code).uppercase()) {
    SimpleXLinkQRCode(connReqInvitation, onShare = { chatModel.markShowingInvitationUsed() })
  }

  Spacer(Modifier.height(10.dp))
  val incognito = remember { mutableStateOf(controller.appPrefs.incognito.get()) }
  IncognitoToggle(controller.appPrefs.incognito, incognito) {
    if (appPlatform.isDesktop) ModalManager.end.closeModals()
    ModalManager.end.showModal { IncognitoView() }
  }
  KeyChangeEffect(incognito.value) {
    withBGApi {
      val contactConn = contactConnection.value ?: return@withBGApi
      val conn = controller.apiSetConnectionIncognito(rhId, contactConn.pccConnId, incognito.value) ?: return@withBGApi
      contactConnection.value = conn
      chatModel.updateContactConnection(rhId, conn)
    }
    chatModel.markShowingInvitationUsed()
  }
  SectionTextFooter(sharedProfileInfo(chatModel, incognito.value))
}

@Composable
private fun AddContactLearnMoreButton() {
  IconButton(
    {
      if (appPlatform.isDesktop) ModalManager.end.closeModals()
      ModalManager.end.showModalCloseable { close ->
        AddContactLearnMore(close)
      }
    }
  ) {
    Icon(
      painterResource(MR.images.ic_info),
      stringResource(MR.strings.learn_more),
    )
  }
}

@Composable
private fun ConnectView(rhId: Long?, showQRCodeScanner: MutableState<Boolean>, pastedLink: MutableState<String>, close: () -> Unit) {
  SectionView(stringResource(MR.strings.paste_the_link_you_received).uppercase()) {
    PasteLinkView(rhId, pastedLink, showQRCodeScanner, close)
  }

  if (appPlatform.isAndroid) {
    Spacer(Modifier.height(10.dp))

    SectionView(stringResource(MR.strings.or_scan_qr_code).uppercase()) {
      QRCodeScanner(showQRCodeScanner) { text ->
        withBGApi {
          val res = verify(rhId, text, close)
          if (!res) {
            AlertManager.shared.showAlertMsg(
              title = generalGetString(MR.strings.invalid_qr_code),
              text = generalGetString(MR.strings.code_you_scanned_is_not_simplex_link_qr_code)
            )
          }
        }
      }
    }
  }
}

@Composable
private fun PasteLinkView(rhId: Long?, pastedLink: MutableState<String>, showQRCodeScanner: MutableState<Boolean>, close: () -> Unit) {
  if (pastedLink.value.isEmpty()) {
    val clipboard = LocalClipboardManager.current
    SectionItemView({
      val str = clipboard.getText()?.text ?: return@SectionItemView
      val link = strHasSingleSimplexLink(str.trim())
      if (link != null) {
        pastedLink.value = link.text
        showQRCodeScanner.value = false
        withBGApi { connect(rhId, link.text, close) { pastedLink.value = "" } }
      } else {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.invalid_contact_link),
          text = generalGetString(MR.strings.the_text_you_pasted_is_not_a_link)
        )
      }
    }) {
      Text(stringResource(MR.strings.tap_to_paste_link))
    }
  } else {
    LinkTextView(pastedLink.value, false)
  }
}

@Composable
fun LinkTextView(link: String, share: Boolean) {
  val clipboard = LocalClipboardManager.current
  Row(Modifier.fillMaxWidth().heightIn(min = 46.dp).padding(horizontal = DEFAULT_PADDING), verticalAlignment = Alignment.CenterVertically) {
    Box(Modifier.weight(1f).clickable {
      chatModel.markShowingInvitationUsed()
      clipboard.shareText(link)
    }) {
      BasicTextField(
        value = link,
        onValueChange = {  },
        enabled = false,
        textStyle = TextStyle(fontSize = 16.sp, color = MaterialTheme.colors.onBackground),
        singleLine = true,
        decorationBox = @Composable { innerTextField ->
          TextFieldDefaults.TextFieldDecorationBox(
            value = link,
            innerTextField = innerTextField,
            contentPadding = PaddingValues(),
            label = null,
            visualTransformation = VisualTransformation.None,
            leadingIcon = null,
            trailingIcon = null,
            singleLine = true,
            enabled = false,
            isError = false,
            interactionSource = remember { MutableInteractionSource() },
          )
        })
    }
    // Element Text() can add ellipsis (...) in random place of the string, sometimes even after half of width of a screen.
    // So using BasicTextField + manual ...
    Text("…", fontSize = 16.sp)
    if (share) {
      Spacer(Modifier.width(DEFAULT_PADDING))
      IconButton({
        chatModel.markShowingInvitationUsed()
        clipboard.shareText(link)
      }, Modifier.size(20.dp)) {
        Icon(painterResource(MR.images.ic_share_filled), null, tint = MaterialTheme.colors.primary)
      }
    }
  }
}

private suspend fun verify(rhId: Long?, text: String?, close: () -> Unit): Boolean {
  if (text != null && strIsSimplexLink(text)) {
    connect(rhId, text, close)
    return true
  }
  return false
}

private suspend fun connect(rhId: Long?, link: String, close: () -> Unit, cleanup: (() -> Unit)? = null) {
  chatModel.newChatConnectionStage.value = NewChatConnectionStage.STARTED

  planAndConnect(
    rhId,
    URI.create(link),
    close = close,
    cleanup = cleanup,
    incognito = null
  )
}

private fun createInvitation(
  rhId: Long?,
  creatingConnReq: MutableState<Boolean>,
  connReqInvitation: String,
  contactConnection: MutableState<PendingContactConnection?>
) {
  if (connReqInvitation.isNotEmpty() || contactConnection.value != null || creatingConnReq.value) return
  chatModel.newChatConnectionStage.value = NewChatConnectionStage.STARTED

  creatingConnReq.value = true
  withBGApi {
    val (r, alert) = controller.apiAddContact(rhId, incognito = controller.appPrefs.incognito.get())
    if (r != null) {
      chatModel.updateContactConnection(rhId, r.second)
      chatModel.showingInvitation.value = ShowingInvitation(connId = r.second.id, connReq = simplexChatLink(r.first), connChatUsed = false)
      contactConnection.value = r.second
    } else {
      creatingConnReq.value = false
      if (alert != null) {
        alert()
      }
    }
  }
}

fun strIsSimplexLink(str: String): Boolean {
  val parsedMd = parseToMarkdown(str)
  return parsedMd != null && parsedMd.size == 1 && parsedMd[0].format is Format.SimplexLink
}

fun strHasSingleSimplexLink(str: String): FormattedText? {
  val parsedMd = parseToMarkdown(str) ?: return null
  val parsedLinks = parsedMd.filter { it.format?.isSimplexLink ?: false }
  if (parsedLinks.size != 1) return null

  return parsedLinks[0]
}

@Composable
fun IncognitoToggle(
  incognitoPref: SharedPreference<Boolean>,
  incognito: MutableState<Boolean>,
  onClickInfo: () -> Unit
) {
  SettingsActionItemWithContent(
    icon = if (incognito.value) painterResource(MR.images.ic_theater_comedy_filled) else painterResource(MR.images.ic_theater_comedy),
    text = null,
    click = onClickInfo,
    iconColor = if (incognito.value) Indigo else MaterialTheme.colors.secondary,
    extraPadding = false
  ) {
    SharedPreferenceToggleWithIcon(
      stringResource(MR.strings.incognito),
      painterResource(MR.images.ic_info),
      stopped = false,
      onClickInfo = onClickInfo,
      preference = incognitoPref,
      preferenceState = incognito
    )
  }
}

fun sharedProfileInfo(
  chatModel: ChatModel,
  incognito: Boolean
): String {
  val name = chatModel.currentUser.value?.displayName ?: ""
  return if (incognito) {
    generalGetString(MR.strings.connect__a_new_random_profile_will_be_shared)
  } else {
    String.format(generalGetString(MR.strings.connect__your_profile_will_be_shared), name)
  }
}
