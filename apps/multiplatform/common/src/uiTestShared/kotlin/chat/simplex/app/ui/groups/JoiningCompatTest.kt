package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.joinGroupFromList
import org.junit.Test

class JoiningCompatTest : UiTest() {
  @Test fun groupInvitation_tapRowShowsJoinAlert() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    joinGroupFromList(alice, bob, "team")
  }
}
