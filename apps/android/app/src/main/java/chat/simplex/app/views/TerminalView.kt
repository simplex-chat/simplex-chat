package chat.simplex.app.views

import android.content.res.Configuration
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
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
import chat.simplex.app.views.chat.SendMsgView
import chat.simplex.app.views.helpers.*
import com.google.accompanist.insets.ProvideWindowInsets
import com.google.accompanist.insets.navigationBarsWithImePadding
import kotlinx.coroutines.launch

@Composable
fun TerminalView(chatModel: ChatModel, close: () -> Unit) {
  BackHandler(onBack = close)
  TerminalLayout(chatModel.terminalItems, close) { cmd ->
    withApi {
      // show "in progress"
      chatModel.controller.sendCmd(CC.Console(cmd))
      // hide "in progress"
    }
  }
}

@Composable
fun TerminalLayout(terminalItems: List<TerminalItem>, close: () -> Unit, sendCommand: (String) -> Unit) {
  ProvideWindowInsets(windowInsetsAnimationsEnabled = true) {
    Scaffold(
      topBar = { CloseSheetBar(close) },
      bottomBar = {
        SendMsgView(
          msg = remember { mutableStateOf("") },
          linkPreview = remember { mutableStateOf(null) },
          cancelledLinks = remember { mutableSetOf() },
          parseMarkdown = { null },
          sendMessage = sendCommand
        )
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
      close = {},
      sendCommand = {}
    )
  }
}
