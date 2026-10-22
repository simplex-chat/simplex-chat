package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.chatListToolbar
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.groupInfoOf
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.joinGroupFromList
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.common.model.MsgFilter
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

class MentionNotificationTest : UiTest() {
  @Test fun mentions_mentionsOnlyMuteCountsOnlyMentionsForOtherProfile() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    joinGroupFromList(alice, bob, "team")
    backToChatList()
    val info = alice.chatList.open("team").groupInfo()
    waitFor(Ui.GroupInfo.Mute)
    click(Ui.GroupInfo.Mute)
    poll(what = "team muted to mentions only") { groupInfoOf("team").chatSettings.enableNtfs == MsgFilter.Mentions && seesText("Mute all") }
    info.close()
    backToChatList()
    alice.userPicker().profiles().apply { add("work"); close() }
    quiesce()
    fun aliceUser() = chatModel.users.first { it.user.displayName == "alice" }
    val before = aliceUser().unreadCount
    bob sends "#team @alice one"
    bob sends "#team plain"
    bob sends "#team @alice two"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "both mentions counted for the inactive profile alice and the badge on the profile button") {
      aliceUser().unreadCount >= before + 2 && chatListToolbar().profileBadge()
    }
    check(aliceUser().unreadCount == before + 2) { "alice's unread count is ${aliceUser().unreadCount}, expected ${before + 2}: the plain message in the mentions-only group was counted" }
    val picker = alice.userPicker()
    poll(what = "alice's row in the user picker showing ${before + 2}") { "${before + 2}" in textsOf(Ui.UserPicker.User(aliceUser().user.userId)) }
    picker.switchTo("alice")
    val chat = alice.chatList.open("team")
    chat.itemId("bob's `plain` in team") { it.content.text == "plain" }
    chat.itemId("bob's `@alice two` in team") { it.content.text == "@alice two" }
  }
}
