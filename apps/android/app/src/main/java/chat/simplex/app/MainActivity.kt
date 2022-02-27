package chat.simplex.app

import android.app.Application
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.util.Log
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.viewModels
import androidx.compose.foundation.layout.Box
import androidx.compose.runtime.*
import androidx.lifecycle.AndroidViewModel
import androidx.navigation.*
import androidx.navigation.compose.*
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.*
import chat.simplex.app.views.chat.ChatInfoView
import chat.simplex.app.views.chat.ChatView
import chat.simplex.app.views.chatlist.ChatListView
import chat.simplex.app.views.chatlist.openChat
import chat.simplex.app.views.helpers.withApi
import chat.simplex.app.views.newchat.*
import chat.simplex.app.views.usersettings.*
//import kotlinx.serialization.decodeFromString

class MainActivity: ComponentActivity() {
  private val vm by viewModels<SimplexViewModel>()

  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
//    testJson()
    processIntent(intent, vm.chatModel)
//    vm.app.initiateBackgroundWork()
    setContent {
      SimpleXTheme {
        Navigation(vm.chatModel)
      }
    }
  }
}

class SimplexViewModel(application: Application): AndroidViewModel(application) {
  val app = getApplication<SimplexApp>()
  val chatModel = app.chatModel
}

@Composable
fun MainPage(chatModel: ChatModel, nav: NavController) {
  when (chatModel.userCreated.value) {
    null -> SplashView()
    false -> WelcomeView(chatModel) // { nav.navigate(Pages.ChatList.route) }
    true -> if (chatModel.chatId.value == null) {
      ChatListView(chatModel, nav)
    } else {
      ChatView(chatModel)
    }
  }
}

@Composable
fun Navigation(chatModel: ChatModel) {
  val nav = rememberNavController()
  Box {
    NavHost(navController = nav, startDestination = Pages.Home.route) {
      composable(route = Pages.Home.route) {
        MainPage(chatModel, nav)
      }
      composable(route = Pages.Terminal.route) {
        TerminalView(chatModel, nav)
      }
      composable(
        Pages.TerminalItemDetails.route + "/{identifier}",
        arguments = listOf(
          navArgument("identifier") {
            type = NavType.LongType
          }
        )
      ) { entry -> DetailView(entry.arguments!!.getLong("identifier"), chatModel.terminalItems, nav) }
      composable(route = Pages.Help.route) {
        HelpView(chatModel, nav)
      }
    }
    val am = chatModel.alertManager
    if (am.presentAlert.value) am.alertView.value?.invoke()
    val mm = ModalManager.shared
    if (mm.presentModal.value) mm.modalView.value?.invoke(mm::closeModal)
  }
}

sealed class Pages(val route: String) {
  object Home: Pages("home")
  object Terminal: Pages("terminal")
  object TerminalItemDetails: Pages("details")
  object Help: Pages("help")
}

fun processIntent(intent: Intent?, chatModel: ChatModel) {
  when (intent?.action) {
    NtfManager.OpenChatAction -> {
      val chatId = intent.getStringExtra("chatId")
      Log.d("SIMPLEX", "processIntent: OpenChatAction $chatId")
      if (chatId != null) {
        val cInfo = chatModel.getChat(chatId)?.chatInfo
        if (cInfo != null) withApi { openChat(chatModel, cInfo) }
      }
    }
    "android.intent.action.VIEW" -> {
      val uri = intent.data
      if (uri != null) connectIfOpenedViaUri(uri, chatModel)
    }
  }
}

fun connectIfOpenedViaUri(uri: Uri, chatModel: ChatModel) {
  Log.d("SIMPLEX", "connectIfOpenedViaUri: opened via link")
  if (chatModel.currentUser.value == null) {
    // TODO open from chat list view
    chatModel.appOpenUrl.value = uri
  } else {
    withUriAction(chatModel, uri) { action ->
      chatModel.alertManager.showAlertMsg(
        title = "Connect via $action link?",
        text = "Your profile will be sent to the contact that you received this link from.",
        confirmText = "Connect",
        onConfirm = {
          withApi {
            Log.d("SIMPLEX", "connectIfOpenedViaUri: connecting")
            connectViaUri(chatModel, action, uri)
          }
        }
      )
    }
  }
}

//fun testJson() {
//  val str = """
//    {}
//  """.trimIndent()
//
//  println(json.decodeFromString<ChatItem>(str))
//}
