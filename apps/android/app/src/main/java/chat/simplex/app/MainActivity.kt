package chat.simplex.app

import android.app.Application
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.viewModels
import androidx.compose.material.ExperimentalMaterialApi
import androidx.compose.runtime.Composable
import chat.simplex.app.ui.theme.SimpleXTheme
import androidx.lifecycle.AndroidViewModel
import androidx.navigation.NavType
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.rememberNavController
import androidx.navigation.navArgument
import androidx.navigation.navDeepLink
import chat.simplex.app.model.ChatModel
import chat.simplex.app.views.*
import chat.simplex.app.views.chat.ChatView
import chat.simplex.app.views.chatlist.*
import chat.simplex.app.views.newchat.AddContactView
import chat.simplex.app.views.newchat.ConnectContactView
import com.google.accompanist.permissions.ExperimentalPermissionsApi
import kotlinx.coroutines.DelicateCoroutinesApi

@ExperimentalPermissionsApi
@ExperimentalMaterialApi
class MainActivity: ComponentActivity() {
  private val viewModel by viewModels<SimplexViewModel>()

  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
    setContent {
      SimpleXTheme {
        Navigation(viewModel.chatModel)
      }
    }
  }
}

class SimplexViewModel(application: Application) : AndroidViewModel(application) {
  val chatModel = getApplication<SimplexApp>().chatModel
}

@DelicateCoroutinesApi
@ExperimentalPermissionsApi
@ExperimentalMaterialApi
@Composable
fun Navigation(chatModel: ChatModel) {
  val nav = rememberNavController()
  val uri = "simplex://connect"

  NavHost(navController = nav, startDestination=Pages.Home.route){
    composable(route=Pages.Home.route){
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
    composable(
      route = Pages.ConnectWith.route,
      arguments = listOf(
        navArgument("version"){
          type = NavType.IntType
        },
        navArgument("address"){
          type = NavType.StringType
        }
      ),
      deepLinks = listOf(navDeepLink{ uriPattern = "${uri}/?v={version}&smp={address}" })
    ) {
      ConnectWithView(it.arguments!!.getString("address")!!)
    }
    composable(route = Pages.Terminal.route) {
      TerminalView(chatModel,  nav)
    }
    composable(
      Pages.TerminalItemDetails.route + "/{identifier}",
      arguments = listOf(
        navArgument("identifier"){
          type = NavType.LongType
        }
      )
    ) { entry -> DetailView( entry.arguments!!.getLong("identifier"), chatModel.terminalItems, nav) }
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
  object ConnectWith: Pages("connect_with")
}
