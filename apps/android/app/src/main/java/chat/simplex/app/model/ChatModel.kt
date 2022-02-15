package chat.simplex.app.model

class ChatModel {
  val currentUser: User? = null
  val terminalItems = mutableListOf<TerminalItem>()
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
