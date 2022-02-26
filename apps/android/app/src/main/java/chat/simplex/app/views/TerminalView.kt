package chat.simplex.app.views

import android.content.res.Configuration
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.navigation.NavController
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.SendMsgView
import chat.simplex.app.views.helpers.CloseSheetBar
import chat.simplex.app.views.helpers.withApi
import com.google.accompanist.insets.ProvideWindowInsets
import com.google.accompanist.insets.navigationBarsWithImePadding
import kotlinx.coroutines.DelicateCoroutinesApi
import kotlinx.coroutines.launch

@DelicateCoroutinesApi
@Composable
fun TerminalView(chatModel: ChatModel, nav: NavController) {
  TerminalLayout(chatModel.terminalItems, nav::popBackStack, nav::navigate) { cmd ->
    withApi {
      // show "in progress"
      chatModel.controller.sendCmd(CC.Console(cmd))
      // hide "in progress"
    }
  }
}

@Composable
fun TerminalLayout(terminalItems: List<TerminalItem> , close: () -> Unit, navigate: (String) -> Unit,
                   sendCommand: (String) -> Unit) {
  ProvideWindowInsets(windowInsetsAnimationsEnabled = true) {
    Scaffold(
      topBar = { CloseSheetBar(close) },
      bottomBar = { SendMsgView(sendCommand) },
      modifier = Modifier.navigationBarsWithImePadding()
    ) { contentPadding ->
      Box(
        modifier = Modifier
          .padding(contentPadding)
          .fillMaxWidth()
          .background(MaterialTheme.colors.background)
      ) {
        TerminalLog(terminalItems, navigate)
      }
    }
  }
}

@Composable
fun TerminalLog(terminalItems: List<TerminalItem>, navigate: (String) -> Unit) {
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
            .clickable { navigate("details/${item.id}") })
    }
    val len = terminalItems.count()
    if (len > 1) {
      scope.launch {
        listState.animateScrollToItem(len - 1)
      }
    }
  }
}

@Composable
fun DetailView(identifier: Long, terminalItems: List<TerminalItem>, nav: NavController){
  Surface(
    Modifier
      .background(MaterialTheme.colors.background)
      .fillMaxSize()
  ) {
    Column {
      CloseSheetBar(nav::popBackStack)
      SelectionContainer(modifier = Modifier.verticalScroll(rememberScrollState())) {
        Text((terminalItems.firstOrNull { it.id == identifier })?.details ?: "")
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
      navigate = {},
      sendCommand = {}
    )
  }
}
