package chat.simplex.common.views

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.layout.layoutId
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.TextRange
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.*
import chat.simplex.common.views.chat.item.CONSOLE_COMPOSE_LAYOUT_ID
import chat.simplex.common.views.chat.item.AdaptingBottomPaddingLayout
import chat.simplex.common.views.chatlist.NavigationBarBackground
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.drop
import kotlinx.coroutines.flow.filter
import kotlinx.coroutines.launch

@Composable
fun TerminalView(floating: Boolean = false) {
  val composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = false)) }
  TerminalLayout(
    composeState,
    floating,
    sendCommand = { sendCommand(chatModel, composeState) },
  )
}

private fun sendCommand(chatModel: ChatModel, composeState: MutableState<ComposeState>) {
  val developerTools = chatModel.controller.appPrefs.developerTools.get()
  val prefPerformLA = chatModel.controller.appPrefs.performLA.get()
  val s = composeState.value.message
  if (s.text.startsWith("/sql") && (!prefPerformLA || !developerTools)) {
    val resp = API.Error(null, ChatError.ChatErrorChat(ChatErrorType.CommandError("Failed reading: empty")))
    chatModel.addTerminalItem(TerminalItem.cmd(null, CC.Console(s.text)))
    chatModel.addTerminalItem(TerminalItem.resp(null, resp))
    composeState.value = ComposeState(useLinkPreviews = false)
  } else {
    withBGApi {
      // show "in progress"
      // TODO show active remote host in chat console?
      chatModel.controller.sendCmd(chatModel.remoteHostId(), CC.Console(s.text))
      composeState.value = ComposeState(useLinkPreviews = false)
      // hide "in progress"
    }
  }
}

@Composable
fun TerminalLayout(
  composeState: MutableState<ComposeState>,
  floating: Boolean,
  sendCommand: () -> Unit,
) {
  val smallFont = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  val textStyle = remember { mutableStateOf(smallFont) }

  fun onMessageChange(s: ComposeMessage) {
    composeState.value = composeState.value.copy(message = s)
  }
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  Box(Modifier.fillMaxSize()) {
    val composeViewHeight = remember { mutableStateOf(0.dp) }
    AdaptingBottomPaddingLayout(Modifier, CONSOLE_COMPOSE_LAYOUT_ID, composeViewHeight) {
      TerminalLog(floating, composeViewHeight)
      Column(
        Modifier
          .layoutId(CONSOLE_COMPOSE_LAYOUT_ID)
          .align(Alignment.BottomCenter)
          .navigationBarsPadding()
          .consumeWindowInsets(PaddingValues(bottom = if (oneHandUI.value) AppBarHeight * fontSizeSqrtMultiplier else 0.dp))
          .imePadding()
          .padding(bottom = if (oneHandUI.value) AppBarHeight * fontSizeSqrtMultiplier else 0.dp)
          .background(MaterialTheme.colors.background)
      ) {
        Divider()
        Surface(Modifier.padding(horizontal = 8.dp), color = MaterialTheme.colors.background, contentColor = MaterialTheme.colors.onBackground) {
          SendMsgView(
            composeState = composeState,
            showVoiceRecordIcon = false,
            recState = remember { mutableStateOf(RecordingState.NotStarted) },
            isDirectChat = false,
            liveMessageAlertShown = SharedPreference(get = { false }, set = {}),
            sendMsgEnabled = true,
            sendButtonEnabled = true,
            nextSendGrpInv = false,
            needToAllowVoiceToContact = false,
            allowedVoiceByPrefs = false,
            userIsObserver = false,
            userCanSend = true,
            allowVoiceToContact = {},
            placeholder = "",
            sendMessage = { sendCommand() },
            sendLiveMessage = null,
            updateLiveMessage = null,
            editPrevMessage = {},
            onMessageChange = ::onMessageChange,
            onFilesPasted = {},
            textStyle = textStyle,
            focusRequester = remember { FocusRequester() }
          )
        }
      }
    }
    if (!oneHandUI.value) {
      NavigationBarBackground(true, oneHandUI.value)
    }
  }
}

@Composable
fun TerminalLog(floating: Boolean, composeViewHeight: State<Dp>) {
  val reversedTerminalItems by remember {
    derivedStateOf { chatModel.terminalItems.value.asReversed() }
  }
  val listState = LocalAppBarHandler.current?.listState ?: rememberLazyListState()
  var autoScrollToBottom = rememberSaveable { mutableStateOf(true) }
  LaunchedEffect(Unit) {
    launch {
      snapshotFlow { listState.layoutInfo.totalItemsCount }
        .filter { autoScrollToBottom.value }
        .collect {
          try {
            listState.scrollToItem(0)
          } catch (e: Exception) {
            Log.e(TAG, e.stackTraceToString())
          }
        }
    }
    var oldNumberOfElements = listState.layoutInfo.totalItemsCount
    launch {
      snapshotFlow { listState.firstVisibleItemIndex }
        .drop(1)
        .collect {
          if (oldNumberOfElements != listState.layoutInfo.totalItemsCount) {
            oldNumberOfElements = listState.layoutInfo.totalItemsCount
            return@collect
          }
          autoScrollToBottom.value = it == 0
        }
    }
  }
  LazyColumnWithScrollBar (
    state = listState,
    contentPadding = PaddingValues(
      top = topPaddingToContent(false),
      bottom = composeViewHeight.value
    ),
    reverseLayout = true,
    additionalBarOffset = composeViewHeight
  ) {
    items(reversedTerminalItems, key = { item -> item.id to item.createdAtNanos }) { item ->
      val clipboard = LocalClipboardManager.current
      val rhId = item.remoteHostId
      val rhIdStr = if (rhId == null) "" else "$rhId "
      Text(
        "$rhIdStr${item.date.toString().subSequence(11, 19)} ${item.label}",
        style = TextStyle(fontFamily = FontFamily.Monospace, fontSize = 18.sp, color = MaterialTheme.colors.primary),
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        modifier = Modifier
          .fillMaxWidth()
          .clickable {
            val modalPlace = if (floating) {
              ModalManager.floatingTerminal
            } else {
              ModalManager.start
            }
            modalPlace.showModal(endButtons = { ShareButton { clipboard.shareText(item.details) } }) {
              ColumnWithScrollBar {
                SelectionContainer {
                  val details = item.details
                    .let {
                      if (it.length < 100_000) it
                      else it.substring(0, 100_000)
                    }
                  Text(details, modifier = Modifier.heightIn(max = 50_000.dp).padding(horizontal = DEFAULT_PADDING).padding(bottom = DEFAULT_PADDING))
                }
              }
            }
          }.padding(horizontal = 8.dp, vertical = 4.dp)
      )
    }
  }
  DisposableEffect(Unit) {
    val terminals = chatModel.terminalsVisible.toMutableSet()
    terminals += floating
    chatModel.terminalsVisible = terminals
    onDispose {
      val terminals = chatModel.terminalsVisible.toMutableSet()
      terminals -= floating
      chatModel.terminalsVisible = terminals
    }
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewTerminalLayout() {
  SimpleXTheme {
    TerminalLayout(
      composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = false)) },
      sendCommand = {},
      floating = false
    )
  }
}
