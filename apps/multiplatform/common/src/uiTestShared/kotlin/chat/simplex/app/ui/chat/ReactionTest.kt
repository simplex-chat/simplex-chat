package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.PeerFlags
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowText
import chat.simplex.app.ui.harness.closeContactInfo
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.offeredReactions
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.common.model.ChatFeature
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class ReactionTest : UiTest() {
  @KnownAppIssue("15.5 item 17", "waiting for the chip menu listing `bob` and `you`", desktopOnly = true)
  @Test fun reaction_chipClickOthers_adds() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(reactions = true))
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("hi")
    bob expectsMessage "alice> hi"
    bob sends "+1 @alice hi"
    bob expects "added 👍"
    val hi = chat.itemId("hi")
    val chip = Ui.Chat.ReactionChip(hi, "👍")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's 👍 chip on `hi` without a count") { exists(chip) && chat.reactionCount("hi", "👍") == null }
    assertTrue("the menu of `hi` does not offer 👍 before alice reacts", "👍" in offeredReactions(hi))
    click(chip)
    bob.expectsMessage("alice> > hi", CONNECT_TIMEOUT_MS)
    bob expects "    + 👍"
    poll(what = "the 👍 chip counting 2") { chat.reactionCount("hi", "👍") == "2" }
    offeredReactions(hi).let { assertTrue("the menu still offers 👍 or no longer offers 👎 after alice reacted: $it", "👍" !in it && "👎" in it) }
    click(chip)
    bob.expectsMessage("alice> > hi", CONNECT_TIMEOUT_MS)
    bob expects "    - 👍"
    poll(what = "bob's 👍 chip left without a count") { exists(chip) && chat.reactionCount("hi", "👍") == null }
    assertTrue("the menu does not offer 👍 again after alice removed hers", "👍" in offeredReactions(hi))
    click(chip)
    bob.expectsMessage("alice> > hi", CONNECT_TIMEOUT_MS)
    bob expects "    + 👍"
    poll(what = "the 👍 chip counting 2 again") { chat.reactionCount("hi", "👍") == "2" }
    rightClick(chip)
    poll(what = "the chip menu listing `bob` and `you`") { exists(Ui.Chat.ReactionMember(hi, "bob")) && exists(Ui.Chat.ReactionMember(hi, "you")) }
    click(Ui.Chat.ReactionMember(hi, "bob"))
    poll(what = "bob's contact info opened from the chip menu") { exists(Ui.Info.Alias) && !exists(Ui.Chat.ReactionMember(hi, "bob")) }
    closeContactInfo()
  }

  @Test fun reaction_chipMenu_group_membersAndOpenInfo() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    val chat = ownGroup(alice, "team", bob, cath)
    chat.send("hi")
    bob.eventuallyMessage("#team alice> hi", CONNECT_TIMEOUT_MS)
    cath.eventuallyMessage("#team alice> hi", CONNECT_TIMEOUT_MS)
    bob sends "+1 #team hi"
    bob.eventually("added 👍")
    cath sends "+1 #team hi"
    cath.eventually("added 👍")
    val hi = chat.itemId("hi")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the 👍 chip on `hi` counting 2") { chat.reactionCount("hi", "👍") == "2" }
    rightClick(Ui.Chat.ReactionChip(hi, "👍"))
    poll(what = "the chip menu listing bob and cath but not alice") {
      exists(Ui.Chat.ReactionMember(hi, "bob")) && exists(Ui.Chat.ReactionMember(hi, "cath")) && !exists(Ui.Chat.ReactionMember(hi, "alice"))
    }
    click(Ui.Chat.ReactionMember(hi, "cath"))
    poll(what = "member info with `Change role` showing `member`") { exists(Ui.MemberInfo.Role) && "member" in textsOf(Ui.MemberInfo.Role) }
    click(Ui.MemberInfo.BlockForAll)
    poll(what = "the `Block member for all?` alert naming cath") { seesText("Block member for all?") && seesText("All new messages from cath will be hidden!") }
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the alert closed on cath's member info") { !seesText("Block member for all?") && exists(Ui.MemberInfo.BlockForAll) }
  }

  @Test fun reaction_disabled_noMenuRow() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hi"
    awaitRowText(alice, "bob", "hi")
    val chat = alice.chatList.open("bob")
    val hi = chat.itemId("hi")
    assertTrue("the menu of `hi` does not offer 👍 while reactions are allowed", "👍" in offeredReactions(hi))
    val aliceId = bob.contactId("alice")
    bob sends """/_set prefs @$aliceId {"reactions": {"allow": "no"}}"""
    bob expects "you updated preferences for alice:"
    bob.eventuallyMatching(Regex("Message reactions: off .*"))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's `no` for reactions to reach alice") {
      !alice.chatList.chat("bob").chatInfo.featureEnabled(ChatFeature.Reactions)
    }
    assertEquals("the menu of `hi` still offers reactions", emptyList<String>(), offeredReactions(hi))
  }
}
