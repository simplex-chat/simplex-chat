package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.GroupMemberStatus
import chat.simplex.common.model.MsgFilter
import chat.simplex.common.ui.Ui
import org.junit.Test

class ChatListMenusPendingTest : UiTest() {
  @Test fun groupMenuMuteModesAllMentionsNone() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    alice.chatList.open("team")
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    bob.eventually("#team: alice joined the group")
    fun team() = (alice.chatList.chat("team").chatInfo as ChatInfo.Group).groupInfo
    poll(what = "alice to be a member of #team") { team().membership.memberStatus !in listOf(GroupMemberStatus.MemInvited, GroupMemberStatus.MemAccepted) }
    alice.chatList.rowMenu("team", "ntfs.Mentions", "Mute")
    poll(what = "mentions-only notifications") { team().chatSettings.enableNtfs == MsgFilter.Mentions }
    alice.chatList.rowMenu("team", "ntfs.None", "Mute all")
    poll(what = "notifications off") { team().chatSettings.enableNtfs == MsgFilter.None }
    alice.chatList.rowMenu("team", "ntfs.All", "Unmute")
    poll(what = "all notifications") { team().chatSettings.enableNtfs == MsgFilter.All }
  }
}
