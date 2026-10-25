package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.ChatScreen
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.confirmAlert
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.emptyGroup
import chat.simplex.app.ui.harness.groupMemberId
import chat.simplex.app.ui.harness.inviteMembers
import chat.simplex.app.ui.harness.membersTitleShown
import chat.simplex.app.ui.harness.openMemberMenu
import chat.simplex.app.ui.harness.ownGroupOfThree
import chat.simplex.app.ui.harness.scrollToMembersEnd
import chat.simplex.app.ui.harness.selectMembers
import chat.simplex.app.ui.harness.selectionEnded
import chat.simplex.app.ui.harness.setRoleViaApi
import chat.simplex.common.model.GroupMemberRole
import chat.simplex.common.ui.Ui
import org.junit.Test

private val ROLE_LABELS = listOf("owner", "admin", "moderator", "observer", "member")

class MemberListTest : UiTest() {
  @Test fun memberList_rolesAndStatuses() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob)
    connect(cath)
    val info = emptyGroup(alice, "team").groupInfo()
    poll(what = "`1 members` with alice's own row `you: creator`, `owner`") {
      info.ownMemberTexts().let { "you: creator" in it && "owner" in it } && membersTitleShown(1)
    }
    val first = inviteMembers()
    poll(what = "cath offered") { first.listed("cath") }
    first.toggle("cath")
    first.invite()
    cath.expectsMessage("#team: alice invites you to join the group as member", CONNECT_TIMEOUT_MS)
    cath expects "use /j team to accept"
    val second = inviteMembers()
    poll(what = "bob offered") { second.listed("bob") }
    second.role(GroupMemberRole.Admin)
    poll(what = "`New member role` set to `admin`") { "admin" in second.roleTexts() }
    second.toggle("bob")
    second.invite()
    bob.expectsMessage("#team: alice invites you to join the group as admin", CONNECT_TIMEOUT_MS)
    bob expects "use /j team to accept"
    poll(what = "`3 members` once bob and cath are invited") { info.ownMemberTexts().isNotEmpty() && membersTitleShown(3) && !membersTitleShown(1) }
    cath sends "/j team"
    cath.eventually("#team: you joined the group", CONNECT_TIMEOUT_MS)
    bob sends "/j team"
    bob.eventually("#team: you joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob `connected` as `admin`, cath `connected` without a role label") {
      info.memberTexts("bob").let { "connected" in it && "admin" in it } &&
        info.memberTexts("cath").let { t -> "connected" in t && ROLE_LABELS.none { it in t } }
    }
    poll(what = "the admin bob listed above cath, who was invited first") {
      textsOf(Ui.GroupInfo.Content).let { t -> t.indexOf("bob") >= 0 && t.indexOf("bob") < t.indexOf("cath") }
    }
  }

  @KnownAppIssue("15.5 item 15", "FOREIGN KEY constraint failed")
  @Test fun memberSelection_blockUnblockAndRemoveMultiple() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath"); val dan = cli("dan")
    var info = ownGroupOfThree(alice, bob, cath, dan).groupInfo()
    poll(what = "`4 members` in team's info") { info.ownMemberTexts().isNotEmpty() && membersTitleShown(4) }
    val cathId = groupMemberId("cath")
    scrollToMembersEnd()
    val bobId = openMemberMenu("bob")
    poll(what = "bob's menu with `Block for all`, `Remove member` and `Select`") {
      listOf("blockForAll", "remove", "select").all { exists(Ui.GroupInfo.MemberMenu(bobId, it)) } && !exists(Ui.GroupInfo.MemberMenu(bobId, "unblockForAll")) &&
        listOf("Block for all", "Remove member", "Select").all { seesText(it) }
    }
    click(Ui.GroupInfo.MemberMenu(bobId, "select"))
    poll(what = "`Selected 1` after `Select` on bob") { ChatScreen(this).selectionTitle() == "Selected 1" }
    click(Ui.GroupInfo.Member(cathId))
    poll(what = "`Selected 2` after tapping cath") { ChatScreen(this).selectionTitle() == "Selected 2" }
    click(Ui.GroupInfo.MembersToolbar.part("block"))
    confirmAlert("Block members for all?", "All new messages from these members will be hidden!", "Block for all")
    dan.eventuallyAll(lines = listOf("#team: alice blocked bob (signed)", "#team: alice blocked cath (signed)"), timeoutMs = CONNECT_TIMEOUT_MS)
    selectionEnded()
    poll(what = "bob and cath listed as `blocked`, dan not") {
      "blocked" in info.memberTexts("bob") && "blocked" in info.memberTexts("cath") && "blocked" !in info.memberTexts("dan")
    }
    openMemberMenu("bob")
    poll(what = "bob's menu offering `Unblock for all` instead of `Block for all`") {
      exists(Ui.GroupInfo.MemberMenu(bobId, "unblockForAll")) && !exists(Ui.GroupInfo.MemberMenu(bobId, "blockForAll"))
    }
    click(Ui.GroupInfo.MemberMenu(bobId, "select"))
    poll(what = "`Selected 1` after `Select` on the blocked bob") { ChatScreen(this).selectionTitle() == "Selected 1" }
    click(Ui.GroupInfo.Member(cathId))
    poll(what = "`Selected 2` after tapping the blocked cath") { ChatScreen(this).selectionTitle() == "Selected 2" }
    click(Ui.GroupInfo.MembersToolbar.part("block"))
    confirmAlert("Unblock members for all?", "Messages from these members will be shown!", "Unblock for all")
    dan.eventuallyAll(lines = listOf("#team: alice unblocked bob (signed)", "#team: alice unblocked cath (signed)"), timeoutMs = CONNECT_TIMEOUT_MS)
    selectionEnded()
    poll(what = "bob and cath no longer `blocked`") { "blocked" !in info.memberTexts("bob") && "blocked" !in info.memberTexts("cath") }
    selectMembers("bob", "cath")
    click(Ui.GroupInfo.MembersToolbar.part("remove"))
    val remove = Ui.Alert.Option(Ui.AlertSource.Shared, "remove")
    poll(what = "the `Remove members?` alert with `Remove` and `Remove and delete messages`") {
      seesText("Remove members?") && seesText("Members will be removed from group - this cannot be undone!") &&
        exists(remove) && "Remove" in textsOf(remove) && seesText("Remove and delete messages")
    }
    click(remove)
    dan.eventuallyAll(lines = listOf("#team: alice removed bob from the group (signed)", "#team: alice removed cath from the group (signed)"), timeoutMs = CONNECT_TIMEOUT_MS)
    selectionEnded()
    info.close()
    info = ChatScreen(this).groupInfo()
    poll(what = "`2 members` with dan listed and bob and cath gone") {
      info.ownMemberTexts().isNotEmpty() && membersTitleShown(2) && "dan" in info.memberTexts("dan") &&
        !exists(Ui.GroupInfo.Member(bobId)) && !exists(Ui.GroupInfo.Member(cathId))
    }
    bob.eventually("#team: alice removed you from the group (signed)", CONNECT_TIMEOUT_MS)
    cath.eventually("#team: alice removed you from the group (signed)", CONNECT_TIMEOUT_MS)
  }

  @Test fun memberSelection_selectAllExcludesModerators() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath"); val dan = cli("dan")
    val chat = ownGroupOfThree(alice, bob, cath, dan)
    setRoleViaApi("team", "dan", GroupMemberRole.Moderator)
    dan.eventually("#team: alice changed your role from member to moderator (signed)", CONNECT_TIMEOUT_MS)
    val info = chat.groupInfo()
    poll(what = "dan listed as `moderator`, bob and cath without a role label") {
      "moderator" in info.memberTexts("dan") && "moderator" !in info.memberTexts("bob") && "moderator" !in info.memberTexts("cath")
    }
    scrollToMembersEnd()
    selectMembers("bob")
    click(Ui.GroupInfo.Member(groupMemberId("dan")))
    check(ChatScreen(this).selectionTitle() == "Selected 1") { "tapping the moderator dan changed the selection to `${ChatScreen(this).selectionTitle()}`" }
    click(Ui.Chat.Selection.part("selectAll"))
    poll(what = "`Selected 2` (bob and cath) after select all, the moderator dan left out") { ChatScreen(this).selectionTitle() == "Selected 2" }
    click(Ui.GroupInfo.MembersToolbar.part("role"))
    confirmAlert("Change role?", "The role will be changed to \"observer\". Everyone in the group will be notified.", "Change")
    bob.eventually("#team: alice changed your role from member to observer (signed)", CONNECT_TIMEOUT_MS)
    cath.eventually("#team: alice changed your role from member to observer (signed)", CONNECT_TIMEOUT_MS)
    dan.eventuallyAll(
      lines = listOf("#team: alice changed the role of bob from member to observer (signed)", "#team: alice changed the role of cath from member to observer (signed)"),
      timeoutMs = CONNECT_TIMEOUT_MS,
    )
    selectionEnded()
    poll(what = "bob and cath listed as `observer`, dan still `moderator`") {
      "observer" in info.memberTexts("bob") && "observer" in info.memberTexts("cath") && info.memberTexts("dan").let { "moderator" in it && "observer" !in it }
    }
    info.close()
    chat.send("done")
    dan.expectsMessage("#team alice> done", CONNECT_TIMEOUT_MS)
  }
}
