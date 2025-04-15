package chat.simplex.common.views.newchat

import SectionBottomSpacer
import SectionItemView
import SectionTextFooter
import SectionView
import SectionViewWithButton
import TextIconSpaced
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.foundation.pager.HorizontalPager
import androidx.compose.foundation.pager.rememberPagerState
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.topPaddingToContent
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.MR
import kotlinx.coroutines.*

enum class NewChatOption {
  INVITE, CONNECT
}

@Composable
fun ModalData.NewChatView(rh: RemoteHostInfo?, selection: NewChatOption, showQRCodeScanner: Boolean = false, close: () -> Unit) {
  val selection = remember { stateGetOrPut("selection") { selection } }
  val showQRCodeScanner = remember { stateGetOrPut("showQRCodeScanner") { showQRCodeScanner } }
  val contactConnection: MutableState<PendingContactConnection?> = rememberSaveable(stateSaver = serializableSaver()) { mutableStateOf(chatModel.showingInvitation.value?.conn) }
  val connLinkInvitation by remember { derivedStateOf { chatModel.showingInvitation.value?.connLink ?: CreatedConnLink("", null) } }
  val creatingConnReq = rememberSaveable { mutableStateOf(false) }
  val pastedLink = rememberSaveable { mutableStateOf("") }
  LaunchedEffect(selection.value) {
    if (
      selection.value == NewChatOption.INVITE
      && connLinkInvitation.connFullLink.isEmpty()
      && contactConnection.value == null
      && !creatingConnReq.value
    ) {
      createInvitation(rh?.remoteHostId, creatingConnReq, connLinkInvitation, contactConnection)
    }
  }
  DisposableEffect(Unit) {
    onDispose {
      /** When [AddContactLearnMore] is open, we don't need to drop [ChatModel.showingInvitation].
       * Otherwise, it will be called here AFTER [AddContactLearnMore] is launched and will clear the value too soon.
       * It will be dropped automatically when connection established or when user goes away from this screen.
       **/
      if (chatModel.showingInvitation.value != null && ModalManager.start.openModalCount() <= 1) {
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
                  ModalManager.start.closeModals()
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
        stringResource(MR.strings.one_time_link_short)
      NewChatOption.CONNECT ->
        stringResource(MR.strings.connect_via_link)
    }
  }

  BoxWithConstraints {
    ColumnWithScrollBar {
      AppBarTitle(stringResource(MR.strings.new_chat), hostDevice(rh?.remoteHostId), bottomPadding = DEFAULT_PADDING)
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

      HorizontalPager(state = pagerState, Modifier, pageNestedScrollConnection = LocalAppBarHandler.current!!.connection, verticalAlignment = Alignment.Top, userScrollEnabled = appPlatform.isAndroid) { index ->
        Column(
          Modifier
            .fillMaxWidth()
            .heightIn(min = this@BoxWithConstraints.maxHeight - 150.dp),
          verticalArrangement = if (index == NewChatOption.INVITE.ordinal && connLinkInvitation.connFullLink.isEmpty()) Arrangement.Center else Arrangement.Top
        ) {
          Spacer(Modifier.height(DEFAULT_PADDING))
          when (index) {
            NewChatOption.INVITE.ordinal -> {
              PrepareAndInviteView(rh?.remoteHostId, contactConnection, connLinkInvitation, creatingConnReq)
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
}

@Composable
private fun PrepareAndInviteView(rhId: Long?, contactConnection: MutableState<PendingContactConnection?>, connLinkInvitation: CreatedConnLink, creatingConnReq: MutableState<Boolean>) {
  if (connLinkInvitation.connFullLink.isNotEmpty()) {
    InviteView(
      rhId,
      connLinkInvitation = connLinkInvitation,
      contactConnection = contactConnection,
    )
  } else if (creatingConnReq.value) {
    CreatingLinkProgressView()
  } else {
    RetryButton { createInvitation(rhId, creatingConnReq, connLinkInvitation, contactConnection) }
  }
}

@Composable
private fun CreatingLinkProgressView() {
  DefaultProgressView(stringResource(MR.strings.creating_link))
}

private fun updateShownConnection(conn: PendingContactConnection) {
  chatModel.showingInvitation.value = chatModel.showingInvitation.value?.copy(
    conn = conn,
    connId = conn.id,
    connLink = conn.connLinkInv ?: CreatedConnLink("", null),
    connChatUsed = true
  )
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
private fun ProfilePickerOption(
  title: String,
  selected: Boolean,
  disabled: Boolean,
  onSelected: () -> Unit,
  image: @Composable () -> Unit,
  onInfo: (() -> Unit)? = null
) {
  Row(
    Modifier
      .fillMaxWidth()
      .sizeIn(minHeight = DEFAULT_MIN_SECTION_ITEM_HEIGHT + 8.dp)
      .clickable(enabled = !disabled, onClick = onSelected)
      .padding(horizontal = DEFAULT_PADDING, vertical = 4.dp),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    image()
    TextIconSpaced(false)
    Text(title, modifier = Modifier.align(Alignment.CenterVertically))
    if (onInfo != null) {
      Spacer(Modifier.padding(6.dp))
      Column(Modifier
        .size(48.dp)
        .clip(CircleShape)
        .clickable(
          enabled = !disabled,
          onClick = { ModalManager.start.showModal { IncognitoView() } }
        ),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.Center
      ) {
        Icon(
          painterResource(MR.images.ic_info),
          stringResource(MR.strings.incognito),
          tint = MaterialTheme.colors.primary
        )
      }
    }
    Spacer(Modifier.weight(1f))
    if (selected) {
      Icon(
        painterResource(
          MR.images.ic_check
        ),
        title,
        Modifier.size(20.dp),
        tint = MaterialTheme.colors.primary,
      )
    }
  }
  Divider(
    Modifier.padding(
      start = DEFAULT_PADDING_HALF,
      end = DEFAULT_PADDING_HALF,
    )
  )
}

@Composable
fun ActiveProfilePicker(
  search: MutableState<String>,
  contactConnection: PendingContactConnection?,
  close: () -> Unit,
  rhId: Long?,
  showIncognito: Boolean = true
) {
  val switchingProfile = remember { mutableStateOf(false) }
  val incognito = remember {
    chatModel.showingInvitation.value?.conn?.incognito ?: controller.appPrefs.incognito.get()
  }
  val selectedProfile by remember { chatModel.currentUser }
  val searchTextOrPassword = rememberSaveable { search }
  // Intentionally don't use derivedStateOf in order to NOT change an order after user was selected
  val filteredProfiles = remember(searchTextOrPassword.value) {
    filteredProfiles(chatModel.users.map { it.user }.sortedBy { !it.activeUser }, searchTextOrPassword.value)
  }

  var progressByTimeout by rememberSaveable { mutableStateOf(false) }

  LaunchedEffect(switchingProfile.value) {
    progressByTimeout = if (switchingProfile.value) {
      delay(500)
      switchingProfile.value
    } else {
      false
    }
  }

  @Composable
  fun ProfilePickerUserOption(user: User) {
    val selected = selectedProfile?.userId == user.userId && !incognito

    ProfilePickerOption(
      title = user.chatViewName,
      disabled = switchingProfile.value || selected,
      selected = selected,
      onSelected = {
        switchingProfile.value = true
        withApi {
          try {
            appPreferences.incognito.set(false)
            var updatedConn: PendingContactConnection? = null;

            if (contactConnection != null) {
              updatedConn = controller.apiChangeConnectionUser(rhId, contactConnection.pccConnId, user.userId)
              if (updatedConn != null) {
                withContext(Dispatchers.Main) {
                  chatModel.chatsContext.updateContactConnection(rhId, updatedConn)
                  updateShownConnection(updatedConn)
                }
              }
            }

            if ((contactConnection != null && updatedConn != null) || contactConnection == null) {
              controller.changeActiveUser_(
                rhId = user.remoteHostId,
                toUserId = user.userId,
                viewPwd = if (user.hidden) searchTextOrPassword.value else null
              )

              if (chatModel.currentUser.value?.userId != user.userId) {
                AlertManager.shared.showAlertMsg(generalGetString(
                  MR.strings.switching_profile_error_title),
                  String.format(generalGetString(MR.strings.switching_profile_error_message), user.chatViewName)
                )
              }
            }

            if (updatedConn != null) {
              withContext(Dispatchers.Main) {
                chatModel.chatsContext.updateContactConnection(user.remoteHostId, updatedConn)
              }
            }

            close()
          } finally {
            switchingProfile.value = false
          }
        }
      },
        image = { ProfileImage(size = 42.dp, image = user.image) }
    )
  }

  @Composable
  fun IncognitoUserOption() {
    ProfilePickerOption(
      disabled = switchingProfile.value,
      title = stringResource(MR.strings.incognito),
      selected = incognito,
      onSelected = {
        if (incognito || switchingProfile.value || contactConnection == null) return@ProfilePickerOption

        switchingProfile.value = true
        withApi {
          try {
            appPreferences.incognito.set(true)
            val conn = controller.apiSetConnectionIncognito(rhId, contactConnection.pccConnId, true)
            if (conn != null) {
              withContext(Dispatchers.Main) {
                chatModel.chatsContext.updateContactConnection(rhId, conn)
                updateShownConnection(conn)
              }
              close()
            }
          } finally {
            switchingProfile.value = false
          }
        }
      },
      image = {
        Spacer(Modifier.width(8.dp))
        Icon(
          painterResource(MR.images.ic_theater_comedy_filled),
          contentDescription = stringResource(MR.strings.incognito),
          Modifier.size(32.dp),
          tint = Indigo,
        )
        Spacer(Modifier.width(2.dp))
      },
      onInfo = { ModalManager.start.showModal { IncognitoView() } },
    )
  }

  BoxWithConstraints {
    Column(
      Modifier
        .fillMaxSize()
        .alpha(if (progressByTimeout) 0.6f else 1f)
    ) {
      LazyColumnWithScrollBar(Modifier.padding(top = topPaddingToContent(false)), userScrollEnabled = !switchingProfile.value) {
        item {
          val oneHandUI = remember { appPrefs.oneHandUI.state }
          if (oneHandUI.value) {
            Spacer(Modifier.padding(top = DEFAULT_PADDING + 5.dp))
          }
          AppBarTitle(stringResource(MR.strings.select_chat_profile), hostDevice(rhId), bottomPadding = DEFAULT_PADDING)
        }
        val activeProfile = filteredProfiles.firstOrNull { it.activeUser }

        if (activeProfile != null) {
          val otherProfiles = filteredProfiles.filter { it.userId != activeProfile.userId }
          item {
            when {
              !showIncognito ->
                ProfilePickerUserOption(activeProfile)
              incognito -> {
                IncognitoUserOption()
                ProfilePickerUserOption(activeProfile)
              }
              else -> {
                ProfilePickerUserOption(activeProfile)
                IncognitoUserOption()
              }
            }
          }

          itemsIndexed(otherProfiles) { _, p ->
            ProfilePickerUserOption(p)
          }
        } else {
          if (showIncognito) {
            item {
              IncognitoUserOption()
            }
          }
          itemsIndexed(filteredProfiles) { _, p ->
            ProfilePickerUserOption(p)
          }
        }
        item {
          Spacer(Modifier.imePadding().padding(bottom = DEFAULT_BOTTOM_PADDING))
        }
      }
    }
    if (progressByTimeout) {
      DefaultProgressView("")
    }
  }
}

@Composable
private fun InviteView(rhId: Long?, connLinkInvitation: CreatedConnLink, contactConnection: MutableState<PendingContactConnection?>) {
  val showShortLink = remember { mutableStateOf(true) }
  Spacer(Modifier.height(10.dp))

  SectionView(stringResource(MR.strings.share_this_1_time_link).uppercase(), headerBottomPadding = 5.dp) {
    LinkTextView(connLinkInvitation.simplexChatUri(short = showShortLink.value), true)
  }

  Spacer(Modifier.height(DEFAULT_PADDING))

  SectionViewWithButton(
    stringResource(MR.strings.or_show_this_qr_code).uppercase(),
    titleButton = if (connLinkInvitation.connShortLink != null) {{ ToggleShortLinkButton(showShortLink) }} else null
  ) {
    SimpleXCreatedLinkQRCode(connLinkInvitation, short = showShortLink.value, onShare = { chatModel.markShowingInvitationUsed() })
  }

  Spacer(Modifier.height(DEFAULT_PADDING))
  val incognito by remember(chatModel.showingInvitation.value?.conn?.incognito, controller.appPrefs.incognito.get()) {
    derivedStateOf {
      chatModel.showingInvitation.value?.conn?.incognito ?: controller.appPrefs.incognito.get()
    }
  }
  val currentUser = remember { chatModel.currentUser }.value

  if (currentUser != null) {
    SectionView(stringResource(MR.strings.new_chat_share_profile).uppercase(), headerBottomPadding = 5.dp) {
      SectionItemView(
        padding = PaddingValues(
          top = 0.dp,
          bottom = 0.dp,
          start = 16.dp,
          end = 16.dp
        ),
        click = {
          ModalManager.start.showCustomModal(keyboardCoversBar = false) { close ->
            val search = rememberSaveable { mutableStateOf("") }
            ModalView(
              { close() },
              showSearch = true,
              searchAlwaysVisible = true,
              onSearchValueChanged = { search.value = it },
              content = {
                ActiveProfilePicker(
                  search = search,
                  close = close,
                  rhId = rhId,
                  contactConnection = contactConnection.value
                )
              })
          }
        }
      ) {
        if (incognito) {
          Spacer(Modifier.width(8.dp))
          Icon(
            painterResource(MR.images.ic_theater_comedy_filled),
            contentDescription = stringResource(MR.strings.incognito),
            tint = Indigo,
            modifier = Modifier.size(32.dp)
          )
          Spacer(Modifier.width(2.dp))
        } else {
          ProfileImage(size = 42.dp, image = currentUser.image)
        }
        TextIconSpaced(false)
        Text(
          text = if (incognito) stringResource(MR.strings.incognito) else currentUser.chatViewName,
          color = MaterialTheme.colors.onBackground
        )
        Column(modifier = Modifier.fillMaxWidth(), horizontalAlignment = Alignment.End) {
          Icon(
            painter = painterResource(MR.images.ic_arrow_forward_ios),
            contentDescription = stringResource(MR.strings.new_chat_share_profile),
            tint = MaterialTheme.colors.secondary,
          )
        }
      }
    }
    if (incognito) {
      SectionTextFooter(generalGetString(MR.strings.connect__a_new_random_profile_will_be_shared))
    }
  }
}

@Composable
fun ToggleShortLinkButton(short: MutableState<Boolean>) {
  Text(
    stringResource(if (short.value) MR.strings.full_link_button_text else MR.strings.short_link_button_text),
    modifier = Modifier.clickable(
      interactionSource = remember { MutableInteractionSource() },
      indication = null
    ) { short.value = !short.value },
    style = MaterialTheme.typography.body2, fontSize = 14.sp, color = MaterialTheme.colors.primary
  )
}

@Composable
fun AddContactLearnMoreButton() {
  IconButton(
    {
      ModalManager.start.showModalCloseable { close ->
        AddContactLearnMore(close)
      }
    }
  ) {
    Icon(
      painterResource(MR.images.ic_info),
      stringResource(MR.strings.learn_more),
      tint = MaterialTheme.colors.primary
    )
  }
}

@Composable
private fun ConnectView(rhId: Long?, showQRCodeScanner: MutableState<Boolean>, pastedLink: MutableState<String>, close: () -> Unit) {
  SectionView(stringResource(MR.strings.paste_the_link_you_received).uppercase(), headerBottomPadding = 5.dp) {
    PasteLinkView(rhId, pastedLink, showQRCodeScanner, close)
  }

  if (appPlatform.isAndroid) {
    Spacer(Modifier.height(10.dp))

    SectionView(stringResource(MR.strings.or_scan_qr_code).uppercase(), headerBottomPadding = 5.dp) {
      QRCodeScanner(showQRCodeScanner) { text ->
        val linkVerified = verifyOnly(text)
        if (!linkVerified) {
          AlertManager.shared.showAlertMsg(
            title = generalGetString(MR.strings.invalid_qr_code),
            text = generalGetString(MR.strings.code_you_scanned_is_not_simplex_link_qr_code)
          )
        }
        verifyAndConnect(rhId, text, close)
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
  Row(Modifier.fillMaxWidth().heightIn(min = DEFAULT_MIN_SECTION_ITEM_HEIGHT).padding(horizontal = DEFAULT_PADDING), verticalAlignment = Alignment.CenterVertically) {
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
            colors = TextFieldDefaults.textFieldColors(backgroundColor = Color.Unspecified)
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

private fun filteredProfiles(users: List<User>, searchTextOrPassword: String): List<User> {
  val s = searchTextOrPassword.trim()
  val lower = s.lowercase()
  return users.filter { u ->
    if ((u.activeUser || !u.hidden) && (s == "" || u.anyNameContains(lower))) {
      true
    } else {
      correctPassword(u, s)
    }
  }
}

private fun verifyOnly(text: String?): Boolean = text != null && strIsSimplexLink(text)

private suspend fun verifyAndConnect(rhId: Long?, text: String?, close: () -> Unit): Boolean {
  if (text != null && strIsSimplexLink(text)) {
    return withContext(Dispatchers.Default) {
      connect(rhId, text, close)
    }
  }
  return false
}

private suspend fun connect(rhId: Long?, link: String, close: () -> Unit, cleanup: (() -> Unit)? = null): Boolean =
  planAndConnect(
    rhId,
    link,
    close = close,
    cleanup = cleanup,
    incognito = null
  ).await()

private fun createInvitation(
  rhId: Long?,
  creatingConnReq: MutableState<Boolean>,
  connLinkInvitation: CreatedConnLink,
  contactConnection: MutableState<PendingContactConnection?>
) {
  if (connLinkInvitation.connFullLink.isNotEmpty() || contactConnection.value != null || creatingConnReq.value) return
  creatingConnReq.value = true
  withBGApi {
    val (r, alert) = controller.apiAddContact(rhId, incognito = controller.appPrefs.incognito.get())
    if (r != null) {
      withContext(Dispatchers.Main) {
        chatModel.chatsContext.updateContactConnection(rhId, r.second)
        chatModel.showingInvitation.value = ShowingInvitation(connId = r.second.id, connLink = r.first, connChatUsed = false, conn = r.second)
        contactConnection.value = r.second
      }
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
