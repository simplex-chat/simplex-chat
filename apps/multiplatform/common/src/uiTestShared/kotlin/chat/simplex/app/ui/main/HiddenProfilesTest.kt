package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.CliPeer
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.described
import chat.simplex.common.model.GroupProfile
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle
import chat.simplex.common.ui.UiKey
import kotlinx.coroutines.runBlocking
import org.junit.Test

private const val PASSWORD = "hide-me-42"
private const val RESEND_MS = 2_000L
private val SEARCH = Ui.Profiles.Bar.part("search")

class HiddenProfilesTest : UiTest() {
  @Test fun profilesHideProfileWithPassword() = uiTest {
    val alice = app("alice")
    newGroupViaApi("team")
    val aliceId = userId("alice")
    alice.userPicker().profiles().apply { add("work"); close() }
    newGroupViaApi("secret")
    val workId = userId("work")
    val picker = alice.userPicker()
    poll(what = "alice and work in the user picker") { exists(Ui.UserPicker.User(aliceId)) && exists(Ui.UserPicker.User(workId)) }
    picker.switchTo("alice")
    alice.chatList.row("team")

    alice.userPicker().profiles()
    poll(what = "alice and work in the profiles list") { exists(Ui.Profiles.User(aliceId)) && exists(Ui.Profiles.User(workId)) }
    openHideProfile(workId)
    typeSecret(Ui.HideProfile.Password, PASSWORD)
    typeSecret(Ui.HideProfile.Confirm, PASSWORD)
    click(Ui.HideProfile.Save)
    poll(what = "the hide screen to close") { !exists(Ui.HideProfile.Save) }
    poll(what = "work hidden and gone from the profiles list, alice listed") {
      hidden(workId) && !exists(Ui.Profiles.User(workId)) && exists(Ui.Profiles.User(aliceId))
    }
    closeProfiles()

    alice.userPicker()
    poll(what = "alice without work in the user picker") { exists(Ui.UserPicker.User(aliceId)) && !exists(Ui.UserPicker.User(workId)) }
    escape()
    poll(what = "the user picker to close") { !exists(Ui.UserPicker.Sheet) }

    alice.userPicker().profiles()
    typeInto(SEARCH, "work")
    poll(what = "no profile listed for the hidden profile's name") {
      editableText(SEARCH) == "work" && !exists(Ui.Profiles.Add) && !exists(Ui.Profiles.User(workId)) && !exists(Ui.Profiles.User(aliceId))
    }
    replaceText(SEARCH, PASSWORD)
    poll(what = "work listed for its password, alice not") { exists(Ui.Profiles.User(workId)) && !exists(Ui.Profiles.User(aliceId)) }
    click(Ui.Profiles.User(workId))
    poll(what = "work active with its chats loaded") {
      chatModel.currentUser.value?.userId == workId && !chatModel.switchingUsersAndHosts.value &&
        chatModel.chats.value.any { it.chatInfo.chatViewName == "secret" } && chatModel.chats.value.none { it.chatInfo.chatViewName == "team" }
    }
    closeProfiles()
    val secretId = alice.chatList.row("secret").chatId("secret")
    check(!seesText("team")) { "alice's group is listed while the hidden profile is active" }

    alice.userPicker().switchTo("alice")
    alice.chatList.row("team")
    poll(what = "the hidden profile's group gone from the model and the list") {
      chatModel.chats.value.none { it.id == secretId } && !exists(Ui.ChatList.Row(secretId)) && !seesText("secret")
    }
  }

  @Test fun profilesHidePasswordMismatchDisabled() = uiTest {
    val alice = app("alice")
    val aliceId = userId("alice")
    alice.userPicker().profiles().add("work")
    openHideProfile(aliceId)
    typeSecret(Ui.HideProfile.Password, PASSWORD)
    typeSecret(Ui.HideProfile.Confirm, PASSWORD.dropLast(1))
    click(Ui.HideProfile.Save)
    poll(what = "app work started by `Save profile password` to finish") { UiIdle.inFlight().isEmpty() }
    check(!hidden(aliceId) && exists(Ui.HideProfile.Save)) { "`Save profile password` hid the profile with a mismatched confirmation" }
    typeSecret(Ui.HideProfile.Confirm, PASSWORD.takeLast(1))
    click(Ui.HideProfile.Save)
    poll(what = "alice hidden and the Hide profile screen closed once the passwords match") { hidden(aliceId) && !exists(Ui.HideProfile.Save) }
  }

  @Test fun profilesDeleteInactiveWithConnections() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val aliceId = userId("alice")
    alice.userPicker().profiles().apply { add("work"); close() }
    val workId = userId("work")
    connect(bob)
    bob sends "@work hello work"
    poll(what = "bob's message in work's chat list") { "hello work" in alice.chatList.rowTexts("bob") }
    val picker = alice.userPicker()
    poll(what = "alice and work in the user picker") { exists(Ui.UserPicker.User(aliceId)) && exists(Ui.UserPicker.User(workId)) }
    picker.switchTo("alice")

    alice.userPicker().profiles()
    poll(what = "alice and work in the profiles list") { exists(Ui.Profiles.User(aliceId)) && exists(Ui.Profiles.User(workId)) }
    rightClick(Ui.Profiles.User(workId))
    click(Ui.Profiles.UserMenu(workId, "delete"))
    poll(what = "the `Delete chat profile?` alert") { seesText("Delete chat profile?") && exists(Ui.Alert.Option(Ui.AlertSource.Shared, "deleteWithConnections")) }
    click(Ui.Alert.Option(Ui.AlertSource.Shared, "deleteWithConnections"))
    poll(what = "work gone from the users and the profiles list, alice listed") {
      chatModel.users.none { it.user.userId == workId } && !exists(Ui.Profiles.User(workId)) && exists(Ui.Profiles.User(aliceId))
    }
    closeProfiles()

    alice.userPicker()
    poll(what = "alice without work in the user picker") { exists(Ui.UserPicker.User(aliceId)) && !exists(Ui.UserPicker.User(workId)) }
    sendUntilAuthFailed(bob, "work")
  }

  @KnownAppIssue("15.5 item 5", "`Enter password in search` after hiding")
  @Test fun profilesHideShowsPasswordHint() = uiTest {
    val alice = app("alice")
    alice.userPicker().profiles().apply { add("work"); close() }
    val workId = userId("work")
    alice.userPicker().switchTo("alice")
    alice.userPicker().profiles()
    poll(what = "work in the profiles list without the password hint") { exists(Ui.Profiles.User(workId)) && !seesText("Enter password in search") }
    openHideProfile(workId)
    typeSecret(Ui.HideProfile.Password, PASSWORD)
    typeSecret(Ui.HideProfile.Confirm, PASSWORD)
    click(Ui.HideProfile.Save)
    poll(what = "`Enter password in search` after hiding") { !exists(Ui.HideProfile.Save) && seesText("Enter password in search") }
  }

  private fun userId(name: String): Long = chatModel.users.first { it.user.displayName == name }.user.userId

  private fun hidden(userId: Long): Boolean = chatModel.users.firstOrNull { it.user.userId == userId }?.user?.hidden == true

  private fun newGroupViaApi(name: String) {
    runBlocking { chatController.apiNewGroup(null, incognito = false, GroupProfile(name, "", null)) } ?: error("apiNewGroup($name) failed")
  }

  private fun UiScope.openHideProfile(userId: Long) {
    rightClick(Ui.Profiles.User(userId))
    click(Ui.Profiles.UserMenu(userId, "hide"))
    waitFor(Ui.HideProfile.Save)
  }

  private fun UiScope.typeSecret(key: UiKey, text: String) {
    val before = editableText(key).orEmpty().length
    enterText(key, text)
    poll(what = "field ${key.tag} to hold ${before + text.length} characters") { editableText(key)?.length == before + text.length }
  }

  private fun UiScope.closeProfiles() {
    click(described("Back"))
    poll(what = "the profiles screen to close") { !exists(SEARCH) }
  }

  private fun UiScope.sendUntilAuthFailed(peer: CliPeer, contact: String) {
    val failed = Regex("\\[$contact, contactId: \\d+, connId: \\d+] error: connection authorization failed.*")
    var sentAt = 0L
    var n = 0
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "${peer.name}'s messages to $contact to fail with `connection authorization failed`") {
      if (System.currentTimeMillis() - sentAt >= RESEND_MS) {
        peer sends "@$contact are you there ${++n}"
        sentAt = System.currentTimeMillis()
      }
      peer.pending().any { failed.matches(it) }
    }
    peer.eventuallyMatching(failed)
  }
}
