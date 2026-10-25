package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.model.*
import chat.simplex.common.views.chat.*
import kotlinx.datetime.Clock
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class GroupEventMergeUnitTest {

  private fun groupMember(id: Long, name: String): GroupMember =
    GroupMember.sampleData.copy(
      groupMemberId = id,
      memberId = "member-$id",
      localDisplayName = name,
      memberProfile = LocalProfile.sampleData.copy(profileId = id, displayName = name, fullName = "")
    )

  private val bob = groupMember(2, "bob")
  private val alice = groupMember(3, "alice")
  private val cath = groupMember(4, "cath")

  private fun rcvEvent(id: Long, event: RcvGroupEvent, member: GroupMember = bob): ChatItem =
    ChatItem(
      chatDir = CIDirection.GroupRcv(member),
      meta = CIMeta.getSample(id, Clock.System.now(), text = ""),
      content = CIContent.RcvGroupEventContent(event),
      reactions = emptyList()
    )

  private fun sndEvent(id: Long, event: SndGroupEvent): ChatItem =
    ChatItem(
      chatDir = CIDirection.GroupSnd(),
      meta = CIMeta.getSample(id, Clock.System.now(), text = ""),
      content = CIContent.SndGroupEventContent(event),
      reactions = emptyList()
    )

  private fun connected(id: Long, member: GroupMember): ChatItem = rcvEvent(id, RcvGroupEvent.MemberConnected(), member)

  private fun groupUpdated(id: Long): ChatItem = rcvEvent(id, RcvGroupEvent.GroupUpdated(GroupProfile.sampleData))

  private fun userRole(id: Long): ChatItem = rcvEvent(id, RcvGroupEvent.UserRole(GroupMemberRole.Admin))

  @Test
  fun mergeableReceivedGroupEventsShareOneCategory() {
    assertEquals(CIMergeCategory.RcvGroupEvent, connected(1, bob).mergeCategory)
    assertEquals(CIMergeCategory.RcvGroupEvent, groupUpdated(1).mergeCategory)
    assertEquals(CIMergeCategory.RcvGroupEvent, rcvEvent(1, RcvGroupEvent.MemberLeft()).mergeCategory)
    assertEquals(CIMergeCategory.RcvGroupEvent, rcvEvent(1, RcvGroupEvent.MemberRole(5, Profile.sampleData, GroupMemberRole.Admin)).mergeCategory)
    assertEquals(CIMergeCategory.RcvGroupEvent, rcvEvent(1, RcvGroupEvent.MemberAdded(5, Profile.sampleData)).mergeCategory)
  }

  @Test
  fun receivedEventsAboutTheUserOrTheGroupLifecycleAreNeverMerged() {
    assertNull(userRole(1).mergeCategory)
    assertNull(rcvEvent(1, RcvGroupEvent.UserDeleted()).mergeCategory)
    assertNull(rcvEvent(1, RcvGroupEvent.GroupDeleted()).mergeCategory)
    assertNull(rcvEvent(1, RcvGroupEvent.MemberCreatedContact()).mergeCategory)
    assertNull(rcvEvent(1, RcvGroupEvent.NewMemberPendingReview()).mergeCategory)
  }

  @Test
  fun sentGroupEventsMergeExceptUserRoleLeftAcceptedAndPendingReview() {
    assertEquals(CIMergeCategory.SndGroupEvent, sndEvent(1, SndGroupEvent.MemberRole(5, Profile.sampleData, GroupMemberRole.Admin)).mergeCategory)
    assertEquals(CIMergeCategory.SndGroupEvent, sndEvent(1, SndGroupEvent.GroupUpdated(GroupProfile.sampleData)).mergeCategory)
    assertEquals(CIMergeCategory.SndGroupEvent, sndEvent(1, SndGroupEvent.MemberDeleted(5, Profile.sampleData)).mergeCategory)
    assertNull(sndEvent(1, SndGroupEvent.UserRole(GroupMemberRole.Admin)).mergeCategory)
    assertNull(sndEvent(1, SndGroupEvent.UserLeft()).mergeCategory)
    assertNull(sndEvent(1, SndGroupEvent.MemberAccepted(5, Profile.sampleData)).mergeCategory)
    assertNull(sndEvent(1, SndGroupEvent.UserPendingReview()).mergeCategory)
  }

  @Test
  fun deletedItemsMergeByDirectionWhateverTheirContent() {
    val deleted = CIDeleted.Deleted(null)
    assertEquals(CIMergeCategory.SndItemDeleted, ChatItem.getSampleData(1, CIDirection.GroupSnd(), itemDeleted = deleted).mergeCategory)
    assertEquals(CIMergeCategory.RcvItemDeleted, ChatItem.getSampleData(1, CIDirection.GroupRcv(bob), itemDeleted = deleted).mergeCategory)
    val deletedEvent = userRole(1).let { it.copy(meta = it.meta.copy(itemDeleted = deleted)) }
    assertEquals(CIMergeCategory.RcvItemDeleted, deletedEvent.mergeCategory)
  }

  @Test
  fun groupFeatureItemsMergeAsChatFeatureAndMessagesNeverMerge() {
    val feature = ChatItem(
      chatDir = CIDirection.GroupRcv(bob),
      meta = CIMeta.getSample(1, Clock.System.now(), text = ""),
      content = CIContent.RcvGroupFeature(GroupFeature.Voice, GroupPreference(GroupFeatureEnabled.ON), memberRole_ = null),
      reactions = emptyList()
    )
    assertEquals(CIMergeCategory.ChatFeature, feature.mergeCategory)
    assertNull(ChatItem.getSampleData(1, CIDirection.GroupRcv(bob)).mergeCategory)
  }

  @Test
  fun connectedMemberNamesAreCollectedNewestFirst() {
    val reversed = listOf(connected(2, bob), connected(1, alice))
    assertEquals(2 to listOf("bob", "alice"), ChatModel.getConnectedMemberNames(reversed[0], reversed))
  }

  @Test
  fun unmergedEventEndsTheConnectedRun() {
    val reversed = listOf(connected(4, bob), connected(3, alice), userRole(2), connected(1, cath))
    assertEquals(2 to listOf("bob", "alice"), ChatModel.getConnectedMemberNames(reversed[0], reversed))
    assertEquals(1 to listOf("cath"), ChatModel.getConnectedMemberNames(reversed[3], reversed))
    assertEquals(0 to emptyList<String>(), ChatModel.getConnectedMemberNames(reversed[2], reversed))
  }

  @Test
  fun otherGroupEventsCountButAddNoNames() {
    val reversed = listOf(connected(4, bob), groupUpdated(3), connected(2, alice), connected(1, cath))
    assertEquals(4 to listOf("bob", "alice", "cath"), ChatModel.getConnectedMemberNames(reversed[0], reversed))
    assertEquals(3 to listOf("alice", "cath"), ChatModel.getConnectedMemberNames(reversed[1], reversed))
  }

  @Test
  fun groupUpdatesOnlyGiveCountWithoutNames() {
    val reversed = listOf(groupUpdated(2), groupUpdated(1))
    assertEquals(2 to emptyList<String>(), ChatModel.getConnectedMemberNames(reversed[0], reversed))
  }

  @Test
  fun itemNotInTheListGivesNoNames() {
    val reversed = listOf(connected(2, bob), connected(1, alice))
    assertEquals(0 to emptyList<String>(), ChatModel.getConnectedMemberNames(connected(9, cath), reversed))
  }

  private fun itemIds(merged: MergedItems): List<List<Long>> =
    merged.items.map {
      when (it) {
        is MergedItem.Grouped -> it.items.map { listItem -> listItem.item.id }
        is MergedItem.Single -> listOf(it.item.item.id)
      }
    }

  @Test
  fun userRoleEventSplitsMergedConnectedEvents() {
    val reversed = listOf(connected(4, bob), connected(3, alice), userRole(2), connected(1, cath))
    val merged = MergedItems.create(reversed, mutableStateOf(0), emptySet(), ActiveChatState())
    assertEquals(listOf(listOf(4L, 3L), listOf(2L), listOf(1L)), itemIds(merged))
    assertEquals(listOf(0, 2, 3), merged.items.map { it.startIndexInReversedItems })
    assertEquals(listOf(false, true, false), merged.items.map { it is MergedItem.Single })
  }

  @Test
  fun connectedAndUpdatedEventsMergeIntoOneRow() {
    val reversed = listOf(connected(4, bob), groupUpdated(3), connected(2, alice), connected(1, cath))
    val merged = MergedItems.create(reversed, mutableStateOf(0), emptySet(), ActiveChatState())
    assertEquals(listOf(listOf(4L, 3L, 2L, 1L)), itemIds(merged))
    assertEquals(listOf(false), merged.items.map { (it as MergedItem.Grouped).revealed })
  }

  @Test
  fun revealedEventsAreShownOneRowEach() {
    val reversed = listOf(connected(2, bob), connected(1, alice))
    val merged = MergedItems.create(reversed, mutableStateOf(0), setOf(2L, 1L), ActiveChatState())
    assertEquals(listOf(listOf(2L), listOf(1L)), itemIds(merged))
    assertEquals(listOf(true, true), merged.items.map { (it as MergedItem.Grouped).revealed })
  }
}
