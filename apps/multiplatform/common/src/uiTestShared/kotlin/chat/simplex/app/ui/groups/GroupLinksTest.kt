package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.connectIncognito
import chat.simplex.app.ui.harness.emptyGroup
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.GroupMemberStatus
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.ModalManager
import org.junit.Assert.assertNotEquals
import org.junit.Test

class GroupLinksTest : UiTest() {
  @Test fun groupLink_cliJoinsViaShortLink() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val info = emptyGroup(alice, "team").groupInfo()
    poll(what = "alice alone in team's members") { "you: creator" in info.ownMemberTexts() && info.memberTexts("bob").isEmpty() }
    val link = info.createLink()
    bob sends "/c $link"
    bob expects "connection request sent!"
    bob.eventually("#team: joining the group...", CONNECT_TIMEOUT_MS)
    bob.eventually("#team: you joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob listed as connected in team's members") { "connected" in info.memberTexts("bob") }
  }

  @Test fun groupInvitation_incognitoContactJoinsIncognito() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val incognito = connectIncognito(bob)
    assertNotEquals("alice", incognito)
    hostGroup(bob, "team", incognito)
    alice.chatList.open("team")
    poll(what = "the `Join group?` alert with `Join incognito`") { seesText("Join group?") && "Join incognito" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    bob.eventually("#team: $incognito joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "membership of #team to be connected") {
      (alice.chatList.chat("team").chatInfo as ChatInfo.Group).groupInfo.membership.memberStatus == GroupMemberStatus.MemConnected
    }
    val info = alice.chatList.open("team").groupInfo()
    poll(what = "alice's own member row named `$incognito`, not `alice`") {
      info.ownMemberTexts().let { incognito in it && "alice" !in it && "you: connected" in it }
    }
  }

  @Test fun moderate_markDeletedForAll() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    val chat = ownGroup(alice, "team", bob, cath)
    bob.eventuallyMatching(Regex("#team: (new )?member cath is connected"), CONNECT_TIMEOUT_MS)
    cath.eventuallyMatching(Regex("#team: (new )?member bob is connected"), CONNECT_TIMEOUT_MS)
    bob sends "#team spam"
    cath.eventuallyMessage("#team bob> spam", CONNECT_TIMEOUT_MS)
    val id = chat.itemId("spam")
    poll(what = "bob's `spam` shown, not moderated") { textsOf(Ui.Chat.Item(id)).let { t -> t.any { "spam" in it } && t.none { "moderated by" in it } } }
    chat.menu("spam", "moderate")
    poll(what = "the `Delete member message?` alert saying the message will be marked as moderated, with `Delete`") {
      seesText("Delete member message?") && seesText("The message will be marked as moderated for all members.") &&
        "Delete" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    cath.eventuallyMessage("#team bob> [marked deleted by alice] spam", CONNECT_TIMEOUT_MS)
    bob.eventuallyMessage("#team bob> [marked deleted by alice] spam", CONNECT_TIMEOUT_MS)
    poll(what = "`moderated by alice` in place of `spam`") { textsOf(Ui.Chat.Item(id)).let { t -> "moderated by alice" in t && t.none { "spam" in it } } }
  }

  @Test fun memberInfo_blockForAllNotifiesMembers() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    val chat = ownGroup(alice, "team", bob, cath)
    bob.eventuallyMatching(Regex("#team: (new )?member cath is connected"), CONNECT_TIMEOUT_MS)
    cath.eventuallyMatching(Regex("#team: (new )?member bob is connected"), CONNECT_TIMEOUT_MS)
    bob sends "#team before block"
    cath.eventuallyMessage("#team bob> before block", CONNECT_TIMEOUT_MS)
    val before = chat.itemId("before block")
    chat.groupInfo().member("bob").blockForAll()
    cath.eventually("#team: alice blocked bob (signed)", CONNECT_TIMEOUT_MS)
    poll(what = "bob's member info offering `Unblock for all`") { seesText("Unblock for all") }
    escape()
    poll(what = "member info to close to group info") { !exists(Ui.MemberInfo.Remove) }
    escape()
    poll(what = "group info to close to team's chat") { !ModalManager.end.hasModalsOpen() }
    bob sends "#team after block"
    val after = chat.itemId("after block")
    poll(what = "bob's new message shown as `blocked by admin`, his earlier one unchanged") {
      textsOf(Ui.Chat.Item(after)).let { t -> "blocked by admin" in t && t.none { "after block" in it } } &&
        textsOf(Ui.Chat.Item(before)).any { "before block" in it }
    }
    chat.send("after the block")
    bob.expectsMessage("#team alice> after the block", CONNECT_TIMEOUT_MS)
    cath.expectsMessage("#team alice> after the block", CONNECT_TIMEOUT_MS)
  }
}
