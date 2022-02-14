package chat.simplex.app.model

class Profile(
  val displayName: String,
  val fullName: String
  )

val sampleProfile = Profile(
  displayName = "alice",
  fullName = "Alice"
)

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

interface NamedChat {
  abstract val displayName: String
  abstract val fullName: String
  val chatViewName: String
    get() = displayName + (if (fullName == "" || fullName == displayName) "" else " / $fullName")
}

val sampleUser = User(
  userId = 1,
  userContactId = 1,
  localDisplayName = "alice",
  profile = sampleProfile,
  activeUser = true
)
