package chat.simplex.app

import chat.simplex.common.model.*
import kotlinx.datetime.Clock
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

class ChatItemPredicatesUnitTest {
  private val member = GroupMember.sampleData.copy(groupMemberId = 2, memberId = "bob", localDisplayName = "bob", memberRole = GroupMemberRole.Member)

  private fun group(userRole: GroupMemberRole): ChatInfo.Group =
    ChatInfo.Group(GroupInfo.sampleData.copy(membership = GroupMember.sampleData.copy(memberRole = userRole)), groupChatScope = null)

  private fun item(
    dir: CIDirection,
    content: CIContent = CIContent.RcvMsgContent(MsgContent.MCText("hello")),
    id: Long = 1,
    itemDeleted: CIDeleted? = null,
    itemLive: Boolean = false,
    reactions: List<CIReactionCount> = emptyList()
  ): ChatItem =
    ChatItem(dir, CIMeta.getSample(id, Clock.System.now(), "hello", itemDeleted = itemDeleted, itemLive = itemLive), content, reactions = reactions)

  private fun reaction(emoji: MREmojiChar, userReacted: Boolean): CIReactionCount =
    CIReactionCount(MsgReaction.Emoji(emoji), userReacted = userReacted, totalReacted = 1)

  @Test
  fun memberCannotModerateOwnSentMessage() {
    assertNull(item(CIDirection.GroupSnd(), CIContent.SndMsgContent(MsgContent.MCText("hello"))).memberToModerate(group(GroupMemberRole.Member)))
  }

  @Test
  fun channelAndDirectMessagesCannotBeModerated() {
    assertNull(item(CIDirection.ChannelRcv()).memberToModerate(group(GroupMemberRole.Owner)), "channel message")
    assertNull(item(CIDirection.DirectRcv()).memberToModerate(ChatInfo.Direct.sampleData), "direct message")
    assertNull(item(CIDirection.GroupRcv(member)).memberToModerate(ChatInfo.Direct.sampleData), "group item in direct chat")
  }

  @Test
  fun reactionCanBeAddedToRegularMessage() {
    assertTrue(item(CIDirection.DirectRcv()).allowAddReaction)
  }

  @Test
  fun reactionCannotBeAddedToDeletedMessage() {
    assertFalse(item(CIDirection.DirectRcv(), itemDeleted = CIDeleted.Deleted(null)).allowAddReaction)
  }

  @Test
  fun reactionCannotBeAddedToLiveDummy() {
    assertFalse(item(CIDirection.DirectSnd(), CIContent.SndMsgContent(MsgContent.MCText("")), id = ChatItem.TEMP_LIVE_CHAT_ITEM_ID).allowAddReaction)
  }

  @Test
  fun ownReactionsAreLimitedToThree() {
    val two = listOf(reaction(MREmojiChar.ThumbsUp, true), reaction(MREmojiChar.Heart, true))
    assertTrue(item(CIDirection.DirectRcv(), reactions = two).allowAddReaction, "2 own")
    assertTrue(item(CIDirection.DirectRcv(), reactions = two + reaction(MREmojiChar.Smile, false)).allowAddReaction, "2 own + 1 other")
    assertFalse(item(CIDirection.DirectRcv(), reactions = two + reaction(MREmojiChar.Smile, true)).allowAddReaction, "3 own")
  }

  @Test
  fun messagesEventsAndDeletedItemsCanBeDeletedForSelf() {
    assertTrue(item(CIDirection.DirectRcv()).canBeDeletedForSelf, "received text")
    assertTrue(item(CIDirection.DirectSnd(), CIContent.SndMsgContent(MsgContent.MCText("hello")), itemLive = true).canBeDeletedForSelf, "live text")
    assertTrue(item(CIDirection.DirectRcv(), CIContent.RcvDeleted(CIDeleteMode.cidmBroadcast)).canBeDeletedForSelf, "deleted content")
    assertTrue(item(CIDirection.DirectRcv(), itemDeleted = CIDeleted.Deleted(null)).canBeDeletedForSelf, "marked deleted")
    assertTrue(item(CIDirection.GroupRcv(member), CIContent.RcvGroupEventContent(RcvGroupEvent.MemberConnected())).canBeDeletedForSelf, "group event")
  }

  @Test
  fun encryptionInfoAndBannerCannotBeDeletedForSelf() {
    assertFalse(item(CIDirection.DirectSnd(), CIContent.SndDirectE2EEInfo(E2EEInfo(pqEnabled = true))).canBeDeletedForSelf, "direct snd e2ee")
    assertFalse(item(CIDirection.DirectRcv(), CIContent.RcvDirectE2EEInfo(E2EEInfo(pqEnabled = false))).canBeDeletedForSelf, "direct rcv e2ee")
    assertFalse(item(CIDirection.GroupSnd(), CIContent.SndGroupE2EEInfo(E2EEInfo(pqEnabled = null))).canBeDeletedForSelf, "group snd e2ee")
    assertFalse(item(CIDirection.GroupRcv(member), CIContent.RcvGroupE2EEInfo(E2EEInfo(pqEnabled = null))).canBeDeletedForSelf, "group rcv e2ee")
    assertFalse(item(CIDirection.DirectRcv(), CIContent.ChatBanner).canBeDeletedForSelf, "banner")
  }

  @Test
  fun reportReasonsOfferedInOrder_toleratesNullEntry_item36() {
    val reasons = ReportReason.supportedReasons
    val expected = listOf(ReportReason.Spam, ReportReason.Illegal, ReportReason.Community, ReportReason.Profile, ReportReason.Other)
    assertEquals(5, reasons.size)
    expected.zip(reasons).forEach { (e, r) -> assertTrue(r == null || r == e, "$r") }
  }
}
