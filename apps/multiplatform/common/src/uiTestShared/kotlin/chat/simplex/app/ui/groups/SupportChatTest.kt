package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.CliPeer
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.MemberSupportScreen
import chat.simplex.app.ui.harness.SupportChatScreen
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.closeMemberInfo
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.expectsScopedMessage
import chat.simplex.app.ui.harness.goBack
import chat.simplex.app.ui.harness.groupInfoOf
import chat.simplex.app.ui.harness.groupMemberId
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.joinGroupFromList
import chat.simplex.app.ui.harness.memberConnected
import chat.simplex.app.ui.harness.memberOfBobsTeamWithCath
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.app.ui.harness.openSupportRowMenu
import chat.simplex.app.ui.harness.ownTeamWithSupportRequests
import chat.simplex.app.ui.harness.supportBanner
import chat.simplex.app.ui.harness.supportRowBadge
import chat.simplex.app.ui.harness.unreadBadge
import chat.simplex.common.model.GroupMemberRole
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.ModalManager
import org.junit.Test

class SupportChatTest : UiTest() {
  @Test fun supportChat_memberSendsToAdmins() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = memberOfBobsTeamWithCath(alice, bob, cath)
    poll(what = "team's chat open with no support banner") { exists(Ui.Chat.Message) && supportBanner().isEmpty() }
    bob sends "#team (support: alice) first"
    bob sends "#team (support: alice) second"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `2 messages` banner in team's chat") { "2 messages" in supportBanner() }
    val info = chat.groupInfo()
    poll(what = "`Chat with admins` with 2 unread in group info") {
      exists(Ui.GroupInfo.UserSupport) && "Chat with admins" in textsOf(Ui.GroupInfo.UserSupport) && unreadBadge(Ui.GroupInfo.UserSupport) == "2"
    }
    waitStable(Ui.GroupInfo.UserSupport)
    click(Ui.GroupInfo.UserSupport)
    val sc = SupportChatScreen(this)
    sc.waitOpen()
    sc.itemId("first")
    sc.itemId("second")
    poll(what = "bob's two support messages marked read in alice's support chat") {
      sc.items().filter { it.content.text == "first" || it.content.text == "second" }.let { it.size == 2 && it.none { i -> i.isRcvNew } }
    }
    sc.send("hello")
    bob.expectsScopedMessage("#team (support: alice) alice", "hello")
    check(chat.itemTexts().none { it == "hello" || it == "first" || it == "second" }) { "a support message was added to team's main chat" }
    sc.close()
    info.close()
    poll(what = "the support banner gone after the messages were read") { exists(Ui.Chat.Message) && supportBanner().isEmpty() }
    bob sends "#team (support: alice) third"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `1 message` banner in team's chat") { supportBanner().any { it == "1 message" || it == "1 messages" } }
    click(Ui.Chat.SupportChats)
    val again = SupportChatScreen(this)
    again.waitOpen()
    again.itemId("third")
    again.itemId("hello")
    again.close()
    chat.send("for everyone")
    cath.expectsMessage("#team alice> for everyone", CONNECT_TIMEOUT_MS)
    bob.expectsMessage("#team alice> for everyone", CONNECT_TIMEOUT_MS)
  }

  @KnownAppIssue("15.5 item 40", "waiting for the singular `1 message` banner in team's chat")
  @Test fun supportChat_singleUnreadBannerIsSingular() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    memberOfBobsTeamWithCath(alice, bob, cath)
    poll(what = "team's chat open with no support banner") { exists(Ui.Chat.Message) && supportBanner().isEmpty() }
    bob sends "#team (support: alice) only"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "a support banner for bob's message in team's chat") { supportBanner().any { it == "1 message" || it == "1 messages" } }
    poll(what = "the singular `1 message` banner in team's chat") { "1 message" in supportBanner() }
  }

  @Test fun chatsWithMembers_markRead() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    ownTeamWithSupportRequests(alice, bob, cath)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `2 chats with members` banner in team's chat") { supportBanner().any { it.startsWith("2 chat") } }
    click(Ui.Chat.SupportChats)
    MemberSupportScreen(this)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's and cath's support chats each with 1 unread") { supportRowBadge("bob") == "1" && supportRowBadge("cath") == "1" }
    val bobId = openSupportRowMenu("bob")
    click(Ui.MemberSupport.RowMenu(bobId, "markRead"))
    poll(what = "bob's unread badge gone and cath's kept") {
      exists(Ui.MemberSupport.Row(bobId)) && supportRowBadge("bob") == null && supportRowBadge("cath") == "1"
    }
    openSupportRowMenu("bob")
    check(!exists(Ui.MemberSupport.RowMenu(bobId, "markRead"))) { "`Mark read` offered for bob's chat with no unread messages" }
    click(Ui.MemberSupport.RowMenu(bobId, "delete"))
    poll(what = "the `Delete chat with member?` alert with `Delete chat`") {
      seesText("Delete chat with member?") && "Delete chat" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    val cathId = groupMemberId("cath")
    poll(what = "bob's support chat gone from `Chats with members` and cath's kept") {
      !exists(Ui.MemberSupport.Row(bobId)) && exists(Ui.MemberSupport.Row(cathId)) && supportRowBadge("cath") == "1"
    }
    goBack()
    poll(what = "`Chats with members` closed to team's chat showing `1 chat`") {
      !ModalManager.end.hasModalsOpen() && supportBanner().any { it.startsWith("1 chat") }
    }
  }

  @Test fun chatsWithMembers_listSearchAndOpen() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = ownTeamWithSupportRequests(alice, bob, cath)
    val info = chat.groupInfo()
    val cwm = info.chatsWithMembers()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob and cath listed in `Chats with members`") { "bob" in cwm.rowTexts("bob") && "cath" in cwm.rowTexts("cath") }
    typeInto(Ui.MemberSupport.Search, "bo")
    poll(what = "only bob listed for `bo`") { "bob" in cwm.rowTexts("bob") && cwm.rowTexts("cath").isEmpty() }
    replaceText(Ui.MemberSupport.Search, "cat")
    poll(what = "only cath listed for `cat`") { "cath" in cwm.rowTexts("cath") && cwm.rowTexts("bob").isEmpty() }
    hideKeyboard()
    val sc = cwm.open("cath")
    sc.itemId("cath needs help")
    check(sc.items().none { it.content.text == "bob needs help" }) { "bob's support message is shown in cath's support chat" }
    sc.send("hi cath")
    cath.expectsMessage("#team (support) alice> hi cath", CONNECT_TIMEOUT_MS)
    check(chat.itemTexts().none { it == "hi cath" }) { "alice's support reply was added to team's main chat" }
    sc.close()
    cwm.close()
    info.close()
    chat.send("for everyone")
    bob.expectsMessage("#team alice> for everyone", CONNECT_TIMEOUT_MS)
    cath.expectsMessage("#team alice> for everyone", CONNECT_TIMEOUT_MS)
  }

  @Test fun chatsWithMembers_disabledState() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    joinGroupFromList(alice, bob, "team")
    backToChatList()
    val info = alice.chatList.open("team").groupInfo()
    fun supportOn() = groupInfoOf("team").fullGroupPreferences.support.on
    poll(what = "`Chat with admins` and no `Chats with members` in a member's group info while support is on") {
      supportOn() && exists(Ui.GroupInfo.Preferences) && exists(Ui.GroupInfo.UserSupport) && !exists(Ui.GroupInfo.MemberSupport)
    }
    bob.setsSupport("off")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Chat with admins` gone with support off, as alice has no support chat") {
      !supportOn() && exists(Ui.GroupInfo.Preferences) && !exists(Ui.GroupInfo.UserSupport)
    }
    bob.setsSupport("on")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Chat with admins` back with support on") { supportOn() && exists(Ui.GroupInfo.UserSupport) }
    waitStable(Ui.GroupInfo.UserSupport)
    click(Ui.GroupInfo.UserSupport)
    val sc = SupportChatScreen(this)
    sc.waitOpen()
    sc.send("help please")
    bob.eventuallyMatching(Regex("(\\d{2}:\\d{2} )?#team \\(support: alice\\) alice!?> help please"), CONNECT_TIMEOUT_MS)
    sc.close()
    bob.setsSupport("off")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Chat with admins` kept with support off, as alice has a support chat") {
      !supportOn() && exists(Ui.GroupInfo.Preferences) && exists(Ui.GroupInfo.UserSupport)
    }
    bob sends "/mr team alice moderator"
    bob.eventuallyMatching(Regex("#team: you changed the role of alice to moderator( \\(signed\\))?"))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Chats with members` in the group info of the moderator alice") {
      groupInfoOf("team").membership.memberRole == GroupMemberRole.Moderator && exists(Ui.GroupInfo.MemberSupport)
    }
    info.chatsWithMembers()
    poll(what = "`Chats with members are disabled`, not `No chats with members`") {
      seesText("Chats with members are disabled") && !seesText("No chats with members")
    }
  }

  @Test fun supportChat_adminOpensFromMemberInfo() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = ownGroup(alice, "team", bob)
    val info = chat.groupInfo()
    memberConnected(info, "bob")
    val member = info.member("bob")
    poll(what = "`Chat with member` in the member bob's member info") {
      exists(Ui.MemberInfo.SupportChat) && "Chat with member" in textsOf(Ui.MemberInfo.SupportChat)
    }
    member.changeRole(GroupMemberRole.Moderator)
    bob.eventually("#team: alice changed your role from member to moderator (signed)", CONNECT_TIMEOUT_MS)
    poll(what = "no `Chat with member` for the moderator bob, who has no support chat") {
      "moderator" in textsOf(Ui.MemberInfo.Role) && !exists(Ui.MemberInfo.SupportChat)
    }
    member.changeRole(GroupMemberRole.Member)
    bob.eventually("#team: alice changed your role from moderator to member (signed)", CONNECT_TIMEOUT_MS)
    poll(what = "`Chat with member` back for the member bob") { "member" in textsOf(Ui.MemberInfo.Role) && exists(Ui.MemberInfo.SupportChat) }
    closeMemberInfo("bob")
    info.close()
    poll(what = "team's chat with no support banner before bob's support message") { exists(Ui.Chat.Message) && supportBanner().isEmpty() }
    bob sends "#team (support) bob needs help"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `1 chat` banner for bob's support message in team's chat") {
      supportBanner().any { it.startsWith("1 chat") }
    }
    chat.groupInfo().member("bob")
    click(Ui.MemberInfo.SupportChat)
    val sc = SupportChatScreen(this)
    sc.waitOpen()
    sc.itemId("bob needs help")
    sc.send("on it")
    bob.eventuallyMessage("#team (support) alice> on it", CONNECT_TIMEOUT_MS)
    check(chat.itemTexts().none { it == "on it" || it == "bob needs help" }) { "a support message was added to team's main chat" }
  }

  private fun CliPeer.setsSupport(state: String) {
    this sends "/set support #team $state"
    eventually("updated group preferences:")
    this expects "Chat with admins: $state"
  }
}
