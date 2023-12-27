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

@Composable
fun TerminalView(chatModel: ChatModel, close: () -> Unit) {
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
    withApi {
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
        Box(Modifier.padding(horizontal = 8.dp)) {
          SendMsgView(
            composeState = composeState,
            showVoiceRecordIcon = false,
            recState = remember { mutableStateOf(RecordingState.NotStarted) },
            isDirectChat = false,
            liveMessageAlertShown = SharedPreference(get = { false }, set = {}),
            sendMsgEnabled = true,
            nextSendGrpInv = false,
            needToAllowVoiceToContact = false,
            allowedVoiceByPrefs = false,
            userIsObserver = false,
            userCanSend = true,
            allowVoiceToContact = {},
            sendMessage = { sendCommand() },
            sendLiveMessage = null,
            updateLiveMessage = null,
            editPrevMessage = {},
            onMessageChange = ::onMessageChange,
            onFilesPasted = {},
            textStyle = textStyle
          )
        }
      },
      modifier = Modifier.navigationBarsWithImePadding()
    ) { contentPadding ->
      Surface(
        modifier = Modifier
          .padding(contentPadding)
          .fillMaxWidth(),
        color = MaterialTheme.colors.background
      ) {
        TerminalLog()
      }
    }
  }
}

private var lazyListState = 0 to 0

@Composable
fun TerminalLog() {
  val listState = rememberLazyListState(lazyListState.first, lazyListState.second)
  DisposableEffect(Unit) {
    onDispose { lazyListState = listState.firstVisibleItemIndex to listState.firstVisibleItemScrollOffset }
  }
  val reversedTerminalItems by remember {
    derivedStateOf { chatModel.terminalItems.value.asReversed() }
  }
  val clipboard = LocalClipboardManager.current
  LazyColumn(state = listState, reverseLayout = true) {
    items(reversedTerminalItems) { item ->
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
            ModalManager.start.showModal(endButtons = { ShareButton { clipboard.shareText(item.details) } }) {
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
      close = {}
    )
  }
}
