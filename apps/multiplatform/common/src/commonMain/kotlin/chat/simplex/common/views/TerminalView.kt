package chat.simplex.common.views

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.*
import kotlinx.coroutines.flow.collect
import kotlinx.coroutines.flow.filter
import kotlinx.coroutines.launch

@Composable
fun TerminalView(floating: Boolean = false, close: () -> Unit) {
  val composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = false)) }
  val close = {
    close()
    if (appPlatform.isDesktop) {
      ModalManager.center.closeModals()
    }
  }
  BackHandler(onBack = {
    close()
  })
  TerminalLayout(
    composeState,
    floating,
    sendCommand = { sendCommand(chatModel, composeState) },
    close
  )
}

private fun sendCommand(chatModel: ChatModel, composeState: MutableState<ComposeState>) {
  val developerTools = chatModel.controller.appPrefs.developerTools.get()
  val prefPerformLA = chatModel.controller.appPrefs.performLA.get()
  val s = composeState.value.message
  if (s.startsWith("/sql") && (!prefPerformLA || !developerTools)) {
    val resp = CR.ChatCmdError(null, ChatError.ChatErrorChat(ChatErrorType.CommandError("Failed reading: empty")))
    chatModel.addTerminalItem(TerminalItem.cmd(null, CC.Console(s)))
    chatModel.addTerminalItem(TerminalItem.resp(null, resp))
    composeState.value = ComposeState(useLinkPreviews = false)
  } else {
    withBGApi {
      // show "in progress"
      // TODO show active remote host in chat console?
      chatModel.controller.sendCmd(chatModel.remoteHostId(), CC.Console(s))
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
  close: () -> Unit
) {
  val smallFont = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  val textStyle = remember { mutableStateOf(smallFont) }

  fun onMessageChange(s: String) {
    composeState.value = composeState.value.copy(message = s)
  }

  ProvideWindowInsets(windowInsetsAnimationsEnabled = true) {
    Scaffold(
      topBar = { CloseSheetBar(close) },
      bottomBar = {
        Column {
          Divider()
          Box(Modifier.padding(horizontal = 8.dp)) {
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
              textStyle = textStyle
            )
          }
        }
      },
      contentColor = LocalContentColor.current,
      modifier = Modifier.navigationBarsWithImePadding()
    ) { contentPadding ->
      Surface(
        modifier = Modifier
          .padding(contentPadding)
          .fillMaxWidth(),
        color = MaterialTheme.colors.background,
        contentColor = LocalContentColor.current
      ) {
        TerminalLog(floating)
      }
    }
  }
}

@Composable
fun TerminalLog(floating: Boolean) {
  val reversedTerminalItems by remember {
    derivedStateOf { chatModel.terminalItems.value.asReversed() }
  }
  val clipboard = LocalClipboardManager.current
  val listState = LocalAppBarHandler.current?.listState ?: rememberLazyListState()
  LaunchedEffect(Unit) {
    var autoScrollToBottom = true
    launch {
      snapshotFlow { listState.layoutInfo.totalItemsCount }
        .filter { autoScrollToBottom }
        .collect {
          try {
            listState.scrollToItem(0)
          } catch (e: Exception) {
            Log.e(TAG, e.stackTraceToString())
          }
        }
    }
    launch {
      snapshotFlow { listState.firstVisibleItemIndex }
        .collect {
          autoScrollToBottom = listState.firstVisibleItemIndex == 0
        }
    }
  }
  LazyColumnWithScrollBar(reverseLayout = true, state = listState) {
    items(reversedTerminalItems, key = { item -> item.id to item.createdAtNanos }) { item ->
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
              SelectionContainer(modifier = Modifier.verticalScroll(rememberScrollState())) {
                val details = item.details
                  .let {
                    if (it.length < 100_000) it
                    else it.substring(0, 100_000)
                  }
                Text(details, modifier = Modifier.heightIn(max = 50_000.dp).padding(horizontal = DEFAULT_PADDING).padding(bottom = DEFAULT_PADDING))
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
      floating = false,
      close = {}
    )
  }
}
