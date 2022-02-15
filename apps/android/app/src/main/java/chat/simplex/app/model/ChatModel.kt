package chat.simplex.app.model

class ChatModel {
  val currentUser: User? = null
  val terminalItems = mutableListOf<TerminalItem>()
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
}

class Profile(
  val displayName: String,
  val fullName: String
  )

interface NamedChat {
  abstract val displayName: String
  abstract val fullName: String
  val chatViewName: String
    get() = displayName + (if (fullName == "" || fullName == displayName) "" else " / $fullName")
}

val sampleProfile = Profile(
  displayName = "alice",
  fullName = "Alice"
)

val sampleUser = User(
  userId = 1,
  userContactId = 1,
  localDisplayName = "alice",
  profile = sampleProfile,
  activeUser = true
)
