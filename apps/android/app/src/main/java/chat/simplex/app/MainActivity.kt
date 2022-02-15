package chat.simplex.app

import android.app.Application
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.viewModels
import androidx.compose.runtime.Composable
import chat.simplex.app.ui.theme.SimpleXTheme
import androidx.lifecycle.AndroidViewModel
import chat.simplex.app.model.*
import chat.simplex.app.views.TerminalView
import kotlinx.serialization.*
import kotlinx.serialization.json.*
import kotlinx.serialization.modules.*


class MainActivity: ComponentActivity() {
  private val viewModel by viewModels<SimplexViewModel>()
  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
    ExampleSerialisation()  // This is here just to have a simple way to run things.
    setContent {
      SimpleXTheme {
        MainPage(viewModel)
      }
    }
  }
}

class SimplexViewModel(application: Application) : AndroidViewModel(application) {
  val chatModel = getApplication<SimplexApp>().chatModel
}

@Composable
fun MainPage(vm: SimplexViewModel) {
  TerminalView(vm.chatModel)
}

fun ExampleSerialisation(){
  val jsonString = """
{
  "resp": {
    "type": "activeUser",
    "user": {
      "extra": "foo",
      "userId": 1,
      "userContactId": 10,
      "localDisplayName": "test",
      "activeUser": true,
      "profile": {
        "displayName": "prof",
        "fullName": "full"
      }
    }
  }
}
  """

  var resp: CR = CR.ActiveUser(
    User(1, 10, "test", Profile("p", "full"), true)
  )

  println(Json.encodeToString(resp))

  val data = APIResponse.decodeStr(jsonString)
  println(data)
  println(data.responseType)
}