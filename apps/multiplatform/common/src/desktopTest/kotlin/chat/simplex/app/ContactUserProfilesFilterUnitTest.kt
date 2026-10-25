package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.usersettings.correctPassword
import chat.simplex.common.views.usersettings.filteredUsers
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse

class ContactUserProfilesFilterUnitTest {

  private val pwdHash = UserPwdHash(hash = "hash", salt = "salt")

  private fun user(userId: Long, displayName: String, fullName: String = "", activeUser: Boolean = false, hidden: Boolean = false): UserInfo =
    UserInfo(
      user = User.sampleData.copy(
        userId = userId,
        userContactId = userId,
        localDisplayName = displayName,
        profile = LocalProfile.sampleData.copy(profileId = userId, displayName = displayName, fullName = fullName),
        activeUser = activeUser,
        viewPwdHash = if (hidden) pwdHash else null,
      ),
      unreadCount = 0
    )

  private val alice = user(1, "alice", fullName = "Alice Smith", activeUser = true)
  private val bob = user(2, "bob")
  private val carol = user(3, "carol", hidden = true)
  private val dave = user(4, "dave", activeUser = true, hidden = true)

  private fun names(users: List<UserInfo>): List<String> = users.map { it.user.displayName }

  private fun filtered(users: List<UserInfo>, search: String): List<String> {
    val saved = ChatModel.users.toList()
    ChatModel.users.clear()
    ChatModel.users.addAll(users)
    try {
      return names(filteredUsers(ChatModel, search))
    } finally {
      ChatModel.users.clear()
      ChatModel.users.addAll(saved)
    }
  }

  @Test
  fun emptySearchListsVisibleUsersInOrderWithoutHidden() {
    assertEquals(listOf("alice", "bob"), filtered(listOf(alice, carol, bob), ""))
  }

  @Test
  fun blankSearchIsTreatedAsEmpty() {
    assertEquals(listOf("alice", "bob"), filtered(listOf(alice, carol, bob), "   "))
  }

  @Test
  fun searchMatchesDisplayNameIgnoringCase() {
    assertEquals(listOf("bob"), filtered(listOf(alice, bob), "BOB"))
    assertEquals(listOf("alice"), filtered(listOf(alice, bob), " ali "))
  }

  @Test
  fun searchMatchesFullName() {
    assertEquals(listOf("alice"), filtered(listOf(alice, bob), "smith"))
  }

  @Test
  fun searchWithoutMatchListsNoUsers() {
    assertEquals(emptyList(), filtered(listOf(alice, bob), "zzz"))
  }

  @Test
  fun activeHiddenUserIsListedWithEmptySearch() {
    assertEquals(listOf("dave", "bob"), filtered(listOf(dave, bob), ""))
  }

  @Test
  fun activeHiddenUserIsFoundByName() {
    assertEquals(listOf("dave"), filtered(listOf(dave, bob), "DAV"))
  }

  @Test
  fun passwordIsNeverCorrectForVisibleUser() {
    assertFalse(correctPassword(bob.user, "anything"))
    assertFalse(correctPassword(bob.user, ""))
  }

  @Test
  fun emptyPasswordIsNeverCorrectForHiddenUser() {
    assertFalse(correctPassword(carol.user, ""))
  }
}
