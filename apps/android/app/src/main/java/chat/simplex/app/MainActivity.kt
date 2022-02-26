package chat.simplex.app

import android.app.Application
import android.content.Intent
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
import chat.simplex.app.views.helpers.withApi
import chat.simplex.app.views.newchat.*
import chat.simplex.app.views.usersettings.*
import kotlinx.serialization.decodeFromString

class MainActivity: ComponentActivity() {
  private val vm by viewModels<SimplexViewModel>()

  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
//    testJson()
    processIntent(intent, vm.chatModel)
    vm.app.initiateBackgroundWork()
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
    false -> WelcomeView(chatModel) { nav.navigate(Pages.ChatList.route) }
    true -> ChatListView(chatModel, nav)
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
      composable(route = Pages.Welcome.route) {
        WelcomeView(chatModel) {
          nav.navigate(Pages.Home.route) {
            popUpTo(Pages.Home.route) { inclusive = true }
          }
        }
      }
      composable(route = Pages.ChatList.route) {
        ChatListView(chatModel, nav)
      }
      composable(route = Pages.Chat.route) {
        ChatView(chatModel, nav)
      }
      composable(route = Pages.AddContact.route) {
        AddContactView(chatModel, nav)
      }
      composable(route = Pages.Connect.route) {
        ConnectContactView(chatModel, nav)
      }
      composable(route = Pages.ChatInfo.route) {
        ChatInfoView(chatModel, nav)
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
      composable(route = Pages.UserProfile.route) {
        UserProfileView(chatModel, nav)
      }
      composable(route = Pages.UserAddress.route) {
        UserAddressView(chatModel, nav)
      }
      composable(route = Pages.Help.route) {
        HelpView(chatModel, nav)
      }
      composable(route = Pages.Markdown.route) {
        MarkdownHelpView(nav)
      }
    }
    val am = chatModel.alertManager
    if (am.presentAlert.value) am.alertView.value?.invoke()
  }
  if (chatModel.goToChatWithId.value != null) {
    LaunchedEffect(chatModel.goToChatWithId) {
      chatModel.chatId.value = chatModel.goToChatWithId.value
      chatModel.goToChatWithId.value = null
      nav.navigateUp()
      nav.navigate(Pages.ChatList.route)
      nav.navigate(Pages.Chat.route)
    }
  }
}

sealed class Pages(val route: String) {
  object Home: Pages("home")
  object Terminal: Pages("terminal")
  object Welcome: Pages("welcome")
  object TerminalItemDetails: Pages("details")
  object ChatList: Pages("chats")
  object Chat: Pages("chat")
  object AddContact: Pages("add_contact")
  object Connect: Pages("connect")
  object ChatInfo: Pages("chat_info")
  object UserProfile: Pages("user_profile")
  object UserAddress: Pages("user_address")
  object Help: Pages("help")
  object Markdown: Pages("markdown")
}

fun processIntent(intent: Intent?, chatModel: ChatModel) {
  when (intent?.action) {
    "openChatWithId" -> {
      val chatId = intent.getStringExtra("chatId",)
      withApi {
        val chatPreview = chatModel.getChat(chatId!!)
        if (chatPreview != null) {
          val chat = chatModel.controller.apiGetChat(chatPreview.chatInfo.chatType, chatPreview.chatInfo.apiId)
          if (chat != null) {
            chatModel.goToChatWithId.value = chat.chatInfo.id
            chatModel.chatItems = chat.chatItems.toMutableStateList()
          }
        }
      }
    }
    "android.intent.action.VIEW" -> {
      val uri = intent.data
      if (uri != null) {
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
    }
  }
}

fun testJson() {
  val str = """
    {}
  """.trimIndent()

  println(json.decodeFromString<ChatItem>(str))
}
