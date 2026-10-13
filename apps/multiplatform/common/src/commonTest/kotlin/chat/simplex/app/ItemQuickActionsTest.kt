package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.chat.item.*
import kotlinx.datetime.Clock
import kotlin.test.Test
import kotlin.test.assertEquals

class ItemQuickActionsTest {
  private val direct = ChatInfo.Direct.sampleData

  private fun rcvItem(id: Long = 1, content: MsgContent = MsgContent.MCText("hi"), itemDeleted: CIDeleted? = null, reactions: List<CIReactionCount> = emptyList()) =
    ChatItem(CIDirection.DirectRcv(), CIMeta.getSample(id, Clock.System.now(), content.text, itemDeleted = itemDeleted), CIContent.RcvMsgContent(content), reactions = reactions)

  private fun reacted(emoji: MREmojiChar) = CIReactionCount(MsgReaction.Emoji(emoji), userReacted = true, totalReacted = 1)

  @Test
  fun allActionsForRegularMessage() {
    assertEquals(ItemQuickActions(react = true, reply = true, forward = true), itemQuickActions(direct, rcvItem(), live = false))
  }

  @Test
  fun reactDisabledWhenFeatureOff() {
    val contact = Contact.sampleData.copy(
      mergedPreferences = ContactUserPreferences.sampleData.copy(
        reactions = ContactUserPreferences.sampleData.reactions.copy(enabled = FeatureEnabled(forUser = false, forContact = true))
      )
    )
    assertEquals(false, itemQuickActions(ChatInfo.Direct(contact), rcvItem(), live = false).react)
  }

  @Test
  fun reactDisabledAfterThreeUserReactions() {
    val item = rcvItem(reactions = listOf(reacted(MREmojiChar.ThumbsUp), reacted(MREmojiChar.Smile), reacted(MREmojiChar.Heart)))
    assertEquals(false, itemQuickActions(direct, item, live = false).react)
  }

  @Test
  fun replyHiddenForLocalNote() {
    val note = ChatItem(CIDirection.LocalSnd(), CIMeta.getSample(1, Clock.System.now(), "note"), CIContent.SndMsgContent(MsgContent.MCText("note")), reactions = emptyList())
    assertEquals(ItemQuickActions(react = false, reply = false, forward = true), itemQuickActions(ChatInfo.Local.sampleData, note, live = false))
  }

  @Test
  fun replyHiddenWhenContactCannotReceive() {
    val deletedContact = ChatInfo.Direct(Contact.sampleData.copy(contactStatus = ContactStatus.Deleted))
    assertEquals(false, itemQuickActions(deletedContact, rcvItem(), live = false).reply)
  }

  @Test
  fun replyAndForwardHiddenWhenLive() {
    assertEquals(ItemQuickActions(react = true, reply = false, forward = false), itemQuickActions(direct, rcvItem(), live = true))
  }

  @Test
  fun noQuickActionsForReport() {
    val report = rcvItem(content = MsgContent.MCReport("spam", ReportReason.Spam))
    assertEquals(ItemQuickActions(react = false, reply = false, forward = false), itemQuickActions(direct, report, live = false))
  }

  @Test
  fun noQuickActionsForLiveDummy() {
    assertEquals(ItemQuickActions(react = false, reply = false, forward = false), itemQuickActions(direct, rcvItem(id = ChatItem.TEMP_LIVE_CHAT_ITEM_ID), live = false))
  }

  @Test
  fun noQuickActionsForDeletedItem() {
    val deleted = rcvItem(itemDeleted = CIDeleted.Deleted(null))
    assertEquals(ItemQuickActions(react = false, reply = false, forward = false), itemQuickActions(direct, deleted, live = false))
  }

  @Test
  fun noQuickActionsForEventItem() {
    val event = ChatItem(CIDirection.DirectRcv(), CIMeta.getSample(1, Clock.System.now(), ""), CIContent.RcvChatFeature(ChatFeature.Voice, FeatureEnabled(forUser = true, forContact = true)), reactions = emptyList())
    assertEquals(ItemQuickActions(react = false, reply = false, forward = false), itemQuickActions(direct, event, live = false))
  }

  @Test
  fun availableReactionsExcludeUserReacted() {
    val item = rcvItem(reactions = listOf(reacted(MREmojiChar.ThumbsUp), CIReactionCount(MsgReaction.Emoji(MREmojiChar.Heart), userReacted = false, totalReacted = 2)))
    val available = availableReactions(item).map { it.text }
    assertEquals(MsgReaction.supported.map { it.text } - MsgReaction.Emoji(MREmojiChar.ThumbsUp).text, available)
  }
}
