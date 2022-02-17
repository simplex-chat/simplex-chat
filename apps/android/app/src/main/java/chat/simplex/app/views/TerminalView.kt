package chat.simplex.app.views

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.ClickableText
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.Text
import androidx.compose.material.Button
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.AnnotatedString
import androidx.navigation.*
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.rememberNavController
import chat.simplex.app.model.*
import androidx.navigation.navArgument
import chat.simplex.app.Pages
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.TerminalItem
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.SendMsgView
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext

@Composable
fun TerminalView(chatModel: ChatModel, navController: NavController) {
  Column {
    TerminalLog(chatModel.terminalItems, navController)
    SendMsgView(sendMessage = { cmd ->
      GlobalScope.launch {
        withContext(Dispatchers.Main) {
          // show "in progress"
          chatModel.controller.sendCmd(CC.Console(cmd))
          // hide "in progress"
        }
      }
    })
  }
}

@Composable
fun TerminalLog(terminalItems: List<TerminalItem>, navController: NavController) {
  LazyColumn {
    items(terminalItems) { item ->
      ClickableText(
        AnnotatedString(item.label),
        onClick = { navController.navigate("details/${item.id}") }
      )
    }
  }
}

@Composable
fun DetailView(identifier: Long, terminalItems: List<TerminalItem>, navController: NavController){
  Column(
    modifier=Modifier.verticalScroll(rememberScrollState())
  ) {
    Text((terminalItems.filter {it.id == identifier}).first().details)
    Button(onClick = { navController.popBackStack() }) {
      Text("Back")
    }
  }
}
