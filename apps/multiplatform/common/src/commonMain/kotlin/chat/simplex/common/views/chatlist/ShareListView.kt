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
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.views.helpers.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.newchat.ActiveProfilePicker
import chat.simplex.res.MR

@Composable
fun ShareListView(chatModel: ChatModel, stopped: Boolean) {
  var searchInList by rememberSaveable { mutableStateOf("") }
  val oneHandUI = remember { appPrefs.oneHandUI.state }

  Scaffold(
    contentColor = LocalContentColor.current,
    topBar = {
      if (!oneHandUI.value) {
        Column {
          ShareListToolbar(chatModel, stopped) { searchInList = it.trim() }
          Divider()
        }
      }
    },
    bottomBar = {
      if (oneHandUI.value) {
        Column {
          Divider()
          ShareListToolbar(chatModel, stopped) { searchInList = it.trim() }
        }
      }
    }
  ) {
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
        val mc = sharedContent.chatItem.content.msgContent
        if (mc != null) {
          isMediaOrFileAttachment = mc.isMediaOrFileAttachment
          isVoice = mc.isVoice
          hasSimplexLink = hasSimplexLink(mc.text)
        }
      }
      is SharedContent.BulkForward -> {
        sharedContent.chatItems.forEach {
          val mc = it.content.msgContent
          if (mc != null) {
            isMediaOrFileAttachment = isMediaOrFileAttachment || mc.isMediaOrFileAttachment
            isVoice = isVoice || mc.isVoice
            hasSimplexLink = hasSimplexLink || hasSimplexLink(mc.text)
          }
        }
      }
      null -> {}
    }
    Box(Modifier.padding(it)) {
      Column(
        modifier = Modifier.fillMaxSize()
      ) {
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
      }
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
  val barButtons = arrayListOf<@Composable RowScope.() -> Unit>()
  val users by remember { derivedStateOf { chatModel.users.filter { u -> u.user.activeUser || !u.user.hidden } } }
  val content = remember { chatModel.sharedContent }.value
  val navButton: @Composable RowScope.() -> Unit = {
    when {
      showSearch -> NavigationButtonBack(hideSearchOnBack)
      (users.size > 1 || chatModel.remoteHosts.isNotEmpty()) && content !is SharedContent.Forward && content !is SharedContent.BulkForward -> {
        val allRead = users
          .filter { u -> !u.user.activeUser && !u.user.hidden }
          .all { u -> u.unreadCount == 0 }
        UserProfileButton(chatModel.currentUser.value?.profile?.image, allRead) {
          ModalManager.start.showCustomModal { close ->
            val search = rememberSaveable { mutableStateOf("") }
            ModalView(
              { close() },
              endButtons = {
                SearchTextField(Modifier.fillMaxWidth(), placeholder = stringResource(MR.strings.search_verb), alwaysVisible = true) { search.value = it }
              },
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
  if (chatModel.chats.value.size >= 8) {
    barButtons.add {
      IconButton({ showSearch = true }) {
        Icon(painterResource(MR.images.ic_search_500), stringResource(MR.strings.search_verb), tint = MaterialTheme.colors.primary)
      }
    }
  }
  if (stopped) {
    barButtons.add {
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

  DefaultTopAppBar(
    navigationButton = navButton,
    title = {
      Row(verticalAlignment = Alignment.CenterVertically) {
        Text(
          when (chatModel.sharedContent.value) {
            is SharedContent.Text -> stringResource(MR.strings.share_message)
            is SharedContent.Media -> stringResource(MR.strings.share_image)
            is SharedContent.File -> stringResource(MR.strings.share_file)
            is SharedContent.Forward -> stringResource(MR.strings.forward_message)
            is SharedContent.BulkForward -> stringResource(MR.strings.forward_multiple)
            null -> stringResource(MR.strings.share_message)
          },
          color = MaterialTheme.colors.onBackground,
          fontWeight = FontWeight.SemiBold,
        )
      }
    },
    onTitleClick = null,
    showSearch = showSearch,
    onSearchValueChanged = onSearchValueChanged,
    buttons = barButtons
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
      filteredChats(false, mutableStateOf(false), mutableStateOf(null), search, sorted)
    }
  }
  LazyColumnWithScrollBar(
    modifier = Modifier.fillMaxSize(),
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
