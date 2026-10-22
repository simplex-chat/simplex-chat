package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.eventIds
import chat.simplex.app.ui.harness.groupInfoOf
import chat.simplex.app.ui.harness.inviteMembers
import chat.simplex.app.ui.harness.emptyGroup
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.itemMenu
import chat.simplex.app.ui.harness.joinGroupFromList
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.common.model.CIContent
import chat.simplex.common.model.GroupFeature
import chat.simplex.common.model.RcvGroupEvent
import chat.simplex.common.ui.Ui
import org.junit.Test

private val TWO_CONNECTED = Regex("(bob and cath|cath and bob) connected")

class GroupEventsTest : UiTest() {
  @Test fun groupEvents_memberConnectedMerged() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob)
    connect(cath)
    ownGroup(alice, "team", bob, cath)
    val (older, newer) = eventIds("the `connected` events of bob and cath in team", 2) { it.memberConnected != null }
    fun rowTexts(id: Long) = textsOf(Ui.Chat.Item(id))
    poll(what = "one row `… and … connected` in place of the two events") {
      exists(Ui.Chat.Item(newer)) && rowTexts(newer).any { TWO_CONNECTED.containsMatchIn(it) } && !exists(Ui.Chat.Item(older))
    }
    itemMenu(newer, "expand")
    poll(what = "each `connected` event on a row of its own") {
      exists(Ui.Chat.Item(older)) &&
        listOf(older, newer).all { id -> rowTexts(id).let { t -> t.any { "connected" in it } && t.none { TWO_CONNECTED.containsMatchIn(it) } } }
    }
    itemMenu(newer, "collapse")
    poll(what = "the two events merged into one row again") {
      !exists(Ui.Chat.Item(older)) && rowTexts(newer).any { TWO_CONNECTED.containsMatchIn(it) }
    }
  }

  @Test fun groupEvents_roleChangeItems() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    joinGroupFromList(alice, bob, "team")
    backToChatList()
    val chat = alice.chatList.open("team")
    bob sends "/mr team alice admin"
    bob.eventuallyMatching(Regex("#team: you changed the role of alice to admin( \\(signed\\))?"))
    val role = chat.itemId("bob's `changed your role to admin` in team") {
      (it.content as? CIContent.RcvGroupEventContent)?.rcvGroupEvent is RcvGroupEvent.UserRole
    }
    poll(what = "`changed your role to admin` on its own row") { textsOf(Ui.Chat.Item(role)).any { "changed your role to admin" in it } }
    for (welcome in listOf("Hello", "Hello again")) {
      bob sends "/set welcome #team $welcome"
      bob.eventually("welcome message changed to:")
      bob expects welcome
    }
    val (older, newer) = eventIds("bob's two `updated group profile` events in team", 2) {
      (it.content as? CIContent.RcvGroupEventContent)?.rcvGroupEvent is RcvGroupEvent.GroupUpdated
    }
    poll(what = "`2 group events` in place of the two profile updates, the role change still on its own row") {
      textsOf(Ui.Chat.Item(newer)).any { "2 group events" in it } && !exists(Ui.Chat.Item(older)) &&
        textsOf(Ui.Chat.Item(role)).any { "changed your role to admin" in it }
    }
    itemMenu(newer, "expand")
    poll(what = "both `updated group profile` events on rows of their own") {
      listOf(older, newer).all { id -> exists(Ui.Chat.Item(id)) && textsOf(Ui.Chat.Item(id)).let { t -> t.any { "updated group profile" in it } && t.none { "2 group events" in it } } }
    }
  }

  @Test fun history_newCliMemberReceivesHistory() = uiTest {
    val alice = app("alice"); val cath = cli("cath"); val dan = cli("dan")
    connect(cath)
    connect(dan)
    val chat = emptyGroup(alice, "team")
    chat.send("hello")
    val info = chat.groupInfo()
    inviteMembers().apply {
      poll(what = "cath offered") { listed("cath") }
      toggle("cath")
      invite()
    }
    cath.expectsMessage("#team: alice invites you to join the group as member", CONNECT_TIMEOUT_MS)
    cath expects "use /j team to accept"
    cath sends "/j team"
    cath.eventuallyAll(lines = listOf("#team: you joined the group"), messages = listOf("#team alice> hello [>>]"), timeoutMs = CONNECT_TIMEOUT_MS)
    val prefs = info.preferences()
    prefs.toggle(GroupFeature.History)
    prefs.save()
    cath.eventually("alice updated group #team: (signed)", CONNECT_TIMEOUT_MS)
    cath expects "updated group preferences:"
    cath expects "Recent history: off"
    poll(what = "visible history off for team") { !groupInfoOf("team").fullGroupPreferences.history.on }
    prefs.backClosing()
    inviteMembers().apply {
      poll(what = "dan offered") { listed("dan") }
      toggle("dan")
      invite()
    }
    dan.expectsMessage("#team: alice invites you to join the group as member", CONNECT_TIMEOUT_MS)
    dan expects "use /j team to accept"
    dan sends "/j team"
    dan.expectsMessage("#team: you joined the group", CONNECT_TIMEOUT_MS)
    dan.expectsMessage("#team: member cath is connected", CONNECT_TIMEOUT_MS)
    info.close()
    chat.send("after")
    dan.expectsMessage("#team alice> after", CONNECT_TIMEOUT_MS)
  }
}
