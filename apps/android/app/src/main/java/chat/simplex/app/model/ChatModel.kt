package chat.simplex.app.model

import android.util.Log
import androidx.compose.runtime.mutableStateListOf
import chat.simplex.app.chatSendCmd

class ChatModel(val controller: ChatController) {
  val currentUser: User? = null
  var terminalItems = mutableStateListOf<String>()
    private set
//    private val store = chatInit(filesDir)
//    private val controller: Controller by lazy { // Maybe this shouldn't be lazy
//        this.maybeCreateUser()
//        chatStart(store)
//    }

//    fun maybeCreateUser() {
//        // create user if needed
//        if(chatGetUser(store) == "{}") {
//            chatCreateUser(store, """
//                    {"displayName": "test", "fullName": "android test"}
//                    """.trimIndent())
//        }
//        Log.d("SIMPLEX (user)", chatGetUser(store))
//    }
}

enum class ChatType(val type: String) {
  Direct("@"),
  Group("#"),
  ContactRequest("<@")
}

class User (
  val userId: Int,
  val userContactId: Int,
  val localDisplayName: String,
  val profile: Profile,
  val activeUser: Boolean
) : NamedChat {
  override val displayName: String get() = profile.displayName
  override val fullName: String get() = profile.fullName

  companion object {
    val sampleData = User(
      userId = 1,
      userContactId = 1,
      localDisplayName = "alice",
      profile = Profile.sampleData,
      activeUser = true
    )
  }
}

class Profile(
  val displayName: String,
  val fullName: String
  ) {
  companion object {
    val sampleData = Profile(
      displayName = "alice",
      fullName = "Alice"
    )
  }
}

interface NamedChat {
  abstract val displayName: String
  abstract val fullName: String
  val chatViewName: String
    get() = displayName + (if (fullName == "" || fullName == displayName) "" else " / $fullName")
}
