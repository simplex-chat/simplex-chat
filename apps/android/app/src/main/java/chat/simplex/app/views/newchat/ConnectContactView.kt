package chat.simplex.app.views.newchat

import androidx.compose.runtime.Composable
import androidx.navigation.NavController
import chat.simplex.app.model.ChatController
import chat.simplex.app.model.ChatModel
import com.google.accompanist.permissions.ExperimentalPermissionsApi

@ExperimentalPermissionsApi
@Composable
fun ConnectContactView(chatModel: ChatModel, nav: NavController) {
  ConnectContactLayout(chatModel.controller, nav,
    close = { nav.popBackStack() }
  )
}

@ExperimentalPermissionsApi
@Composable
fun ConnectContactLayout(ctrl: ChatController, nav: NavController, close: () -> Unit) {
  CodeScanner(ctrl, nav)
}