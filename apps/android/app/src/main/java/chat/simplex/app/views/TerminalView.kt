package chat.simplex.app.views

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.tooling.preview.Preview
import chat.simplex.app.model.CC
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.TerminalItem
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.SendMsgView


@Composable
fun TerminalView(chatModel: ChatModel) {
  Column {
    TerminalLog(chatModel.terminalItems)
    SendMsgView(sendMessage = { cmd ->
      chatModel.controller.sendCmd(CC.Console(cmd))
    })
  }
}

@Composable
fun TerminalLog(terminalItems: List<TerminalItem>) {
  LazyColumn {
    items(terminalItems) { item ->
      Text(item.label)
    }
  }
}

@Preview
@Composable
fun PreviewSendMsgView() {
  SimpleXTheme {
    TerminalView(chatModel = ChatModel.sampleData)
  }
}
