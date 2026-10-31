package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.usersettings.filteredUsers
import kotlin.test.AfterTest
import kotlin.test.BeforeTest
import kotlin.test.Test
import kotlin.test.assertEquals

class ProfilesFilteredUsersUnitTest {
  private val savedUsers = chatModel.users.toList()

  private fun user(id: Long, displayName: String, fullName: String = "", activeUser: Boolean = false, hidden: Boolean = false): UserInfo =
    UserInfo(
      user = User.sampleData.copy(
        userId = id,
        userContactId = id,
        localDisplayName = displayName,
        profile = LocalProfile.sampleData.copy(profileId = id, displayName = displayName, fullName = fullName),
        activeUser = activeUser,
        viewPwdHash = if (hidden) UserPwdHash(hash = "hash", salt = "salt") else null
      ),
      unreadCount = 0
    )

  private val alice = user(1, "alice", fullName = "Alice Liddell", activeUser = true)
  private val work = user(2, "work", fullName = "Office")
  private val bob = user(3, "bob")
  private val secret = user(4, "secret", hidden = true)

  private fun setUsers(vararg users: UserInfo) {
    chatModel.users.clear()
    chatModel.users.addAll(users)
  }

  private fun names(search: String): List<String> = filteredUsers(chatModel, search).map { it.user.displayName }

  @BeforeTest
  fun setUp() = chatModel.users.clear()

  @AfterTest
  fun tearDown() = setUsers(*savedUsers.toTypedArray())

  @Test
  fun emptySearchShowsAllVisibleUsersAndHidesHiddenInactiveOnes() {
    setUsers(alice, work, bob, secret)
    assertEquals(listOf("alice", "work", "bob"), names(""))
  }

  @Test
  fun blankSearchActsLikeEmpty() {
    setUsers(alice, work, bob, secret)
    assertEquals(listOf("alice", "work", "bob"), names("   "))
  }

  @Test
  fun hiddenActiveUserIsShown() {
    val hiddenActive = user(5, "private", activeUser = true, hidden = true)
    setUsers(hiddenActive, work)
    assertEquals(listOf("private", "work"), names(""))
  }

  @Test
  fun searchIsCaseInsensitive() {
    setUsers(alice, work, bob)
    assertEquals(listOf("alice"), names("ALI"))
  }

  @Test
  fun searchIsTrimmedAndMatchesFullName() {
    setUsers(alice, work, bob)
    assertEquals(listOf("work"), names("  office "))
    assertEquals(listOf("alice"), names("liddell"))
  }

  @Test
  fun searchMatchingSeveralUsersKeepsOrder() {
    setUsers(alice, work, bob)
    assertEquals(listOf("work", "bob"), names("o"))
  }

  @Test
  fun searchWithNoMatchIsEmpty() {
    setUsers(alice, work, bob)
    assertEquals(emptyList<String>(), names("zzz"))
  }

  @Test
  fun hiddenActiveUserMatchingSearchIsShown() {
    val hiddenActive = user(5, "private", activeUser = true, hidden = true)
    setUsers(hiddenActive, work)
    assertEquals(listOf("private"), names("priv"))
  }
}
