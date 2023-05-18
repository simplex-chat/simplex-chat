package chat.simplex.app.views

import android.content.res.Configuration
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.*
import chat.simplex.app.views.helpers.*
import com.google.accompanist.insets.ProvideWindowInsets
import com.google.accompanist.insets.navigationBarsWithImePadding

@Composable
fun TerminalView(chatModel: ChatModel, close: () -> Unit) {
  val composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = false)) }
  BackHandler(onBack = {
    close()
  })
    TerminalLayout(
      remember { chatModel.terminalItems },
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
    val resp = CR.ChatCmdError(null, ChatError.ChatErrorChat(ChatErrorType.СommandError("Failed reading: empty")))
    chatModel.addTerminalItem(TerminalItem.cmd(CC.Console(s)))
    chatModel.addTerminalItem(TerminalItem.resp(resp))
    composeState.value = ComposeState(useLinkPreviews = false)
  } else {
    withApi {
      // show "in progress"
      chatModel.controller.sendCmd(CC.Console(s))
      composeState.value = ComposeState(useLinkPreviews = false)
      // hide "in progress"
    }
  }
}

@Composable
fun TerminalLayout(
  terminalItems: List<TerminalItem>,
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
            needToAllowVoiceToContact = false,
            allowedVoiceByPrefs = false,
            userIsObserver = false,
            userCanSend = true,
            allowVoiceToContact = {},
            sendMessage = { sendCommand() },
            sendLiveMessage = null,
            updateLiveMessage = null,
            onMessageChange = ::onMessageChange,
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
        TerminalLog(terminalItems)
      }
    }
  }
}

private var lazyListState = 0 to 0

@Composable
fun TerminalLog(terminalItems: List<TerminalItem>) {
  val listState = rememberLazyListState(lazyListState.first, lazyListState.second)
  DisposableEffect(Unit) {
    onDispose { lazyListState = listState.firstVisibleItemIndex to listState.firstVisibleItemScrollOffset }
  }
  val reversedTerminalItems by remember { derivedStateOf { terminalItems.reversed().toList() } }
  val context = LocalContext.current
  LazyColumn(state = listState, reverseLayout = true) {
    items(reversedTerminalItems) { item ->
      Text(
        "${item.date.toString().subSequence(11, 19)} ${item.label}",
        style = TextStyle(fontFamily = FontFamily.Monospace, fontSize = 18.sp, color = MaterialTheme.colors.primary),
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        modifier = Modifier
          .fillMaxWidth()
          .clickable {
            ModalManager.shared.showModal(endButtons = { ShareButton { shareText(context, item.details) } }) {
              SelectionContainer(modifier = Modifier.verticalScroll(rememberScrollState())) {
                Text(item.details, modifier = Modifier.padding(horizontal = DEFAULT_PADDING).padding(bottom = DEFAULT_PADDING))
              }
            }
          }.padding(horizontal = 8.dp, vertical = 4.dp)
      )
    }
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewTerminalLayout() {
  SimpleXTheme {
    TerminalLayout(
      terminalItems = TerminalItem.sampleData,
      composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = false)) },
      sendCommand = {},
      close = {}
    )
  }
}
