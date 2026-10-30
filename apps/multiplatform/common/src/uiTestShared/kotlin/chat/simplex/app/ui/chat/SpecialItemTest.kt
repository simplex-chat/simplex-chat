package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.eventIds
import chat.simplex.app.ui.harness.itemMenu
import chat.simplex.app.ui.harness.loadedItemId
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.common.model.CIContent
import chat.simplex.common.model.ChatFeature
import chat.simplex.common.model.ChatItem
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

class SpecialItemTest : UiTest() {
  @Test fun featureItems_merged_collapseExpandHide() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.open("bob")
    val initial = featureItemIds("the five feature items created on connection", INITIAL.size)
    poll(what = "the five feature items collapsed into one row of icons without text") { collapsedInto(initial.max(), initial) }
    bob sends "/set voice @alice no"
    bob expects "you updated preferences for alice:"
    bob.eventuallyMatching(Regex("Voice messages: off \\(you allow: no.*"))
    val all = featureItemIds("bob's `Voice messages: off` feature item", INITIAL.size + 1)
    val voiceOff = all.max()
    poll(what = "bob's change joining the collapsed row of feature icons without text") { collapsedInto(voiceOff, all) }
    itemMenu(voiceOff, "expand")
    poll(what = "every feature item shown with its text after Expand") {
      shows(voiceOff, "Voice messages: off") && INITIAL.all { (feature, text) -> shows(initialItem(feature), text) }
    }
    itemMenu(voiceOff, "collapse")
    poll(what = "the feature items collapsed into the icon row again after Hide") { collapsedInto(voiceOff, all) }
  }

  @Test fun events_merged_deleteRange() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    val chat = ownGroup(alice, "team", bob, cath)
    val (older, newer) = eventIds("the `connected` events of bob and cath in team", 2) { it.memberConnected != null }
    chat.send("keep")
    bob.eventuallyMessage("#team alice> keep", CONNECT_TIMEOUT_MS)
    val keep = chat.itemId("keep")
    val e2ee = loadedItemId("the encryption info of team") { it.content is CIContent.SndGroupE2EEInfo }
    poll(what = "the two `connected` events merged into one row") { exists(Ui.Chat.Item(newer)) && !exists(Ui.Chat.Item(older)) }
    hideKeyboard()
    itemMenu(newer, "delete")
    poll(what = "the alert to delete the merged events for alice only") {
      seesText("Messages will be deleted - this cannot be undone!") && exists(EVENTS_FOR_ME) && !exists(EVENTS_FOR_EVERYONE)
    }
    click(EVENTS_FOR_ME)
    poll(what = "both `connected` events deleted, `keep` and the encryption info kept") {
      chat.item(older) == null && chat.item(newer) == null && !exists(Ui.Chat.Item(newer)) && exists(Ui.Chat.Item(keep)) && chat.item(e2ee) != null
    }
  }

  private fun UiScope.featureItemIds(what: String, count: Int): List<Long> {
    var ids = emptyList<Long>()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = what) {
      ids = chatModel.chatsContext.chatItems.value.filter { it.content is CIContent.RcvChatFeature }.map { it.id }
      ids.size == count
    }
    return ids
  }

  private fun initialItem(feature: ChatFeature): Long =
    chatModel.chatsContext.chatItems.value.first { it.isFeature(feature) }.id

  private fun ChatItem.isFeature(feature: ChatFeature) = (content as? CIContent.RcvChatFeature)?.feature == feature

  private fun UiScope.collapsedInto(row: Long, ids: List<Long>): Boolean =
    exists(Ui.Chat.Item(row)) && (ids - row).none { exists(Ui.Chat.Item(it)) } && textsOf(Ui.Chat.Item(row)).none { t -> INITIAL.any { (_, text) -> text.substringBefore(":") in t } }

  private fun UiScope.shows(id: Long, text: String): Boolean = exists(Ui.Chat.Item(id)) && textsOf(Ui.Chat.Item(id)).any { text in it }

  private companion object {
    val EVENTS_FOR_ME = Ui.Alert.Option(Ui.AlertSource.Shared, "forMe")
    val EVENTS_FOR_EVERYONE = Ui.Alert.Option(Ui.AlertSource.Shared, "forEveryone")
    val INITIAL = listOf(
      ChatFeature.TimedMessages to "Disappearing messages: off",
      ChatFeature.FullDelete to "Delete for everyone: off",
      ChatFeature.Reactions to "Message reactions: enabled",
      ChatFeature.Voice to "Voice messages: enabled",
      ChatFeature.Calls to "Audio/video calls: enabled",
    )
  }
}
