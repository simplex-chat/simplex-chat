package chat.simplex.app.views

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.text.ClickableText
import androidx.compose.material.Text
import androidx.compose.material.Button
import androidx.compose.runtime.Composable
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.tooling.preview.Preview
import androidx.navigation.NavController
import androidx.navigation.NavType
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.rememberNavController
import androidx.navigation.navArgument
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.TerminalItem
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.SendMsgView


@Composable
fun TerminalPage(chatModel: ChatModel) {
  val navController = rememberNavController()
  NavHost(navController = navController, startDestination = "terminalView"){
    composable("terminalView") { TerminalView(chatModel, navController) }
    composable(
      "details" + "/{_details}",
      arguments=listOf(
        navArgument("_details"){
          type = NavType.StringType
        }
      )
    ) { entry -> DetailView( entry.arguments?.getString("_details"), navController)}
  }

}

@Composable
fun TerminalView(chatModel: ChatModel, navController: NavController) {
  Column {
    TerminalLog(chatModel.terminalItems, navController)
    SendMsgView(chatModel.controller::sendCmd)
  }
}

@Composable
fun TerminalLog(terminalItems: List<TerminalItem>, navController: NavController) {
  LazyColumn {
    items(terminalItems) { item ->
      ClickableText(
        AnnotatedString(item.label),
        onClick={navController.navigate("details/$item.details")}
      )
    }
  }
}

@Composable
fun DetailView(details: String?, navController: NavController){
  Column {
    Text("$details")

    Button(
      onClick={navController.popBackStack()}
    )
    {Text("Back")}
  }
}

