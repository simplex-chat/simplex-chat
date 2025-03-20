package chat.simplex.common.views.chatlist

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.items
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.views.helpers.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.themedBackground
import chat.simplex.common.views.chat.topPaddingToContent
import chat.simplex.common.views.newchat.ActiveProfilePicker
import chat.simplex.res.MR

@Composable
fun ShareListView(chatModel: ChatModel, stopped: Boolean) {
  var searchInList by rememberSaveable { mutableStateOf("") }
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  Box(Modifier.fillMaxSize().themedBackground(bgLayerSize = LocalAppBarHandler.current?.backgroundGraphicsLayerSize, bgLayer = LocalAppBarHandler.current?.backgroundGraphicsLayer)) {
    val sharedContent = chatModel.sharedContent.value
    var isMediaOrFileAttachment = false
    var isVoice = false
    var hasSimplexLink = false
    when (sharedContent) {
      is SharedContent.Text ->
        hasSimplexLink = hasSimplexLink(sharedContent.text)
      is SharedContent.Media -> {
        isMediaOrFileAttachment = true
        hasSimplexLink = hasSimplexLink(sharedContent.text)
      }
      is SharedContent.File -> {
        isMediaOrFileAttachment = true
        hasSimplexLink = hasSimplexLink(sharedContent.text)
      }
      is SharedContent.Forward -> {
        sharedContent.chatItems.forEach { ci ->
          val mc = ci.content.msgContent
          if (mc != null) {
            isMediaOrFileAttachment = isMediaOrFileAttachment || mc.isMediaOrFileAttachment
            isVoice = isVoice || mc.isVoice
            hasSimplexLink = hasSimplexLink || hasSimplexLink(mc.text)
          }
        }
      }
      null -> {}
    }
    if (chatModel.chats.value.isNotEmpty()) {
      ShareList(
        chatModel,
        search = searchInList,
        isMediaOrFileAttachment = isMediaOrFileAttachment,
        isVoice = isVoice,
        hasSimplexLink = hasSimplexLink,
      )
    } else {
      EmptyList()
    }
    if (oneHandUI.value) {
      StatusBarBackground()
    } else {
      NavigationBarBackground(oneHandUI.value, true)
    }
    Box(Modifier.align(if (oneHandUI.value) Alignment.BottomStart else Alignment.TopStart)) {
      ShareListToolbar(chatModel, stopped) { searchInList = it.trim() }
    }
  }
}

private fun hasSimplexLink(msg: String): Boolean {
  val parsedMsg = parseToMarkdown(msg) ?: return false
  return parsedMsg.any { ft -> ft.format is Format.SimplexLink }
}

@Composable
private fun EmptyList() {
  Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
    Text(stringResource(MR.strings.you_have_no_chats), color = MaterialTheme.colors.secondary)
  }
}

@Composable
private fun ShareListToolbar(chatModel: ChatModel, stopped: Boolean, onSearchValueChanged: (String) -> Unit) {
  var showSearch by rememberSaveable { mutableStateOf(false) }
  val hideSearchOnBack = { onSearchValueChanged(""); showSearch = false }
  if (showSearch) {
    BackHandler(onBack = hideSearchOnBack)
  }
  val users by remember { derivedStateOf { chatModel.users.filter { u -> u.user.activeUser || !u.user.hidden } } }
  val navButton: @Composable RowScope.() -> Unit = {
    when {
      showSearch -> NavigationButtonBack(hideSearchOnBack)
      (users.size > 1 || chatModel.remoteHosts.isNotEmpty()) && remember { chatModel.sharedContent }.value !is SharedContent.Forward -> {
        val allRead = users
          .filter { u -> !u.user.activeUser && !u.user.hidden }
          .all { u -> u.unreadCount == 0 }
        UserProfileButton(chatModel.currentUser.value?.profile?.image, allRead) {
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
                  rhId = chatModel.remoteHostId,
                  close = close,
                  contactConnection = null,
                  showIncognito = false
                )
              }
            )
          }
        }
      }
      else -> NavigationButtonBack(onButtonClicked = {
        val sharedContent = chatModel.sharedContent.value
        // Drop shared content
        chatModel.sharedContent.value = null
        if (sharedContent is SharedContent.Forward) {
          chatModel.chatId.value = sharedContent.fromChatInfo.id
        }
      })
    }
  }

  DefaultAppBar(
    navigationButton = navButton,
    title = {
      Row(verticalAlignment = Alignment.CenterVertically) {
        Text(
          when (val v = chatModel.sharedContent.value) {
            is SharedContent.Text -> stringResource(MR.strings.share_message)
            is SharedContent.Media -> stringResource(MR.strings.share_image)
            is SharedContent.File -> stringResource(MR.strings.share_file)
            is SharedContent.Forward -> if (v.chatItems.size > 1) stringResource(MR.strings.forward_multiple) else stringResource(MR.strings.forward_message)
            null -> stringResource(MR.strings.share_message)
          },
          color = MaterialTheme.colors.onBackground,
          fontWeight = FontWeight.SemiBold,
        )
      }
    },
    onTitleClick = null,
    showSearch = showSearch,
    onTop = !remember { appPrefs.oneHandUI.state }.value,
    onSearchValueChanged = onSearchValueChanged,
    buttons = {
      if (chatModel.chats.value.size >= 8) {
        IconButton({ showSearch = true }) {
          Icon(painterResource(MR.images.ic_search_500), stringResource(MR.strings.search_verb), tint = MaterialTheme.colors.primary)
        }
      }
      if (stopped) {
        IconButton(onClick = {
          AlertManager.shared.showAlertMsg(
            generalGetString(MR.strings.chat_is_stopped_indication),
            generalGetString(MR.strings.you_can_start_chat_via_setting_or_by_restarting_the_app)
          )
        }) {
          Icon(
            painterResource(MR.images.ic_report_filled),
            generalGetString(MR.strings.chat_is_stopped_indication),
            tint = Color.Red,
          )
        }
      }
    }
  )
}

@Composable
private fun ShareList(
  chatModel: ChatModel,
  search: String,
  isMediaOrFileAttachment: Boolean,
  isVoice: Boolean,
  hasSimplexLink: Boolean,
) {
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  val chats by remember(search) {
    derivedStateOf {
      val sorted = chatModel.chats.value.toList().filter { it.chatInfo.ready }.sortedByDescending { it.chatInfo is ChatInfo.Local }
      filteredChats(mutableStateOf(false), mutableStateOf(null), search, sorted)
    }
  }
  val topPaddingToContent = topPaddingToContent(false)
  LazyColumnWithScrollBar(
    modifier = Modifier.then(if (oneHandUI.value) Modifier.consumeWindowInsets(WindowInsets.navigationBars.only(WindowInsetsSides.Vertical)) else Modifier).imePadding(),
    contentPadding = PaddingValues(
      top = if (oneHandUI.value) WindowInsets.statusBars.asPaddingValues().calculateTopPadding() else topPaddingToContent,
      bottom = if (oneHandUI.value) WindowInsets.navigationBars.asPaddingValues().calculateBottomPadding() + AppBarHeight * fontSizeSqrtMultiplier else 0.dp
    ),
    reverseLayout = oneHandUI.value
  ) {
    items(chats) { chat ->
      ShareListNavLinkView(
        chat,
        chatModel,
        isMediaOrFileAttachment = isMediaOrFileAttachment,
        isVoice = isVoice,
        hasSimplexLink = hasSimplexLink,
      )
    }
  }
}
