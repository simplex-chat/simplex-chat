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
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.ComposeState
import chat.simplex.app.views.chat.SendMsgView
import chat.simplex.app.views.helpers.*
import com.google.accompanist.insets.ProvideWindowInsets
import com.google.accompanist.insets.navigationBarsWithImePadding
import kotlinx.coroutines.launch

@Composable
fun TerminalView(chatModel: ChatModel, close: () -> Unit) {
  val composeState = remember { mutableStateOf(ComposeState()) }
  BackHandler(onBack = close)
  TerminalLayout(
    chatModel.terminalItems,
    composeState,
    sendCommand = {
      withApi {
        // show "in progress"
        chatModel.controller.sendCmd(CC.Console(composeState.value.message))
        // hide "in progress"
      }
    },
    close
  )
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
          SendMsgView(composeState, sendCommand, ::onMessageChange, textStyle)
        }
      },
      modifier = Modifier.navigationBarsWithImePadding()
    ) { contentPadding ->
      Surface(
        modifier = Modifier
          .padding(contentPadding)
          .fillMaxWidth()
          .background(MaterialTheme.colors.background)
      ) {
        TerminalLog(terminalItems)
      }
    }
  }
}

@Composable
fun TerminalLog(terminalItems: List<TerminalItem>) {
  val listState = rememberLazyListState()
  val scope = rememberCoroutineScope()
  LazyColumn(state = listState) {
    items(terminalItems) { item ->
      Text("${item.date.toString().subSequence(11, 19)} ${item.label}",
        style = TextStyle(fontFamily = FontFamily.Monospace, fontSize = 18.sp, color = MaterialTheme.colors.primary),
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        modifier = Modifier
          .padding(horizontal = 8.dp, vertical = 4.dp)
          .clickable {
            ModalManager.shared.showModal {
              SelectionContainer(modifier = Modifier.verticalScroll(rememberScrollState())) {
                Text(item.details)
              }
            }
          }
      )
    }
    val len = terminalItems.count()
    if (len > 1) {
      scope.launch {
        listState.animateScrollToItem(len - 1)
      }
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
      composeState = remember { mutableStateOf(ComposeState()) },
      sendCommand = {},
      close = {}
    )
  }
}
