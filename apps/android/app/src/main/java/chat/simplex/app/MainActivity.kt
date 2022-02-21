package chat.simplex.app

import android.app.Application
import android.content.Intent
import android.os.Bundle
import android.util.Log
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.viewModels
import androidx.compose.foundation.layout.Box
import androidx.compose.material.ExperimentalMaterialApi
import androidx.compose.runtime.Composable
import androidx.lifecycle.AndroidViewModel
import androidx.navigation.*
import androidx.navigation.compose.*
import chat.simplex.app.model.AccountStatus
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.*
import chat.simplex.app.views.chat.ChatInfoView
import chat.simplex.app.views.chat.ChatView
import chat.simplex.app.views.chatlist.ChatListView
import chat.simplex.app.views.helpers.withApi
import chat.simplex.app.views.newchat.*
import com.google.accompanist.permissions.ExperimentalPermissionsApi
import kotlinx.coroutines.DelicateCoroutinesApi

@DelicateCoroutinesApi
@ExperimentalPermissionsApi
@ExperimentalMaterialApi
class MainActivity: ComponentActivity() {
  private val vm by viewModels<SimplexViewModel>()

  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
    connectIfOpenedViaUri(intent, vm.chatModel)
    setContent {
      SimpleXTheme {
        Navigation(vm.chatModel)
      }
    }
  }
}

@DelicateCoroutinesApi
class SimplexViewModel(application: Application) : AndroidViewModel(application) {
  val chatModel = getApplication<SimplexApp>().chatModel
}

@DelicateCoroutinesApi
@ExperimentalPermissionsApi
@ExperimentalMaterialApi
@Composable
fun MainPage(chatModel: ChatModel, nav: NavController) {
  when (chatModel.accountStatus.value) {
    AccountStatus.NOT_KNOWN -> SplashView()
    AccountStatus.NO_ACCOUNT -> WelcomeView(chatModel) { nav.navigate(Pages.ChatList.route) }
    AccountStatus.ACCOUNT_ACQUIRED -> ChatListView(chatModel, nav)
  }
}

@DelicateCoroutinesApi
@ExperimentalPermissionsApi
@ExperimentalMaterialApi
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
    }
    val am = chatModel.alertManager
    if (am.presentAlert.value) am.alertView.value?.invoke()
  }
}

sealed class Pages(val route: String) {
  object Home : Pages("home")
  object Terminal : Pages("terminal")
  object Welcome : Pages("welcome")
  object TerminalItemDetails : Pages("details")
  object ChatList: Pages("chats")
  object Chat: Pages("chat")
  object AddContact: Pages("add_contact")
  object Connect: Pages("connect")
  object ChatInfo: Pages("chat_info")
}

@DelicateCoroutinesApi
fun connectIfOpenedViaUri(intent: Intent?, chatModel: ChatModel) {
  val uri = intent?.data
  if (intent?.action == "android.intent.action.VIEW" && uri != null) {
    Log.d("SIMPLEX", "connectIfOpenedViaUri: opened via link")
    if (chatModel.currentUser.value == null) {
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
