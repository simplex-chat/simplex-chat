package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.ChatsContext
import kotlinx.coroutines.runBlocking
import kotlin.test.*

/**
 * Correctness + performance test for the batched group-member removal path
 * ([ChatsContext.upsertGroupMembers] and the batched [ChatsContext.removeMemberItems]).
 *
 * Reproduces the reported slowdown scenario: removing 100 members from a group whose
 * chat is open and holds many items. The old code rebuilt the whole item list once per
 * removed member (O(members * items)); the batched code does it once (O(members + items)).
 *
 * The test runs the OLD per-member loop and the NEW batched call on identical fresh state,
 * asserts they produce the same result (and the correct result), and prints the speedup.
 */
class RemoveMembersPerfTest {
  private val rhId: Long? = null
  private val N_MEMBERS = 100
  private val ITEMS_PER_MEMBER = 20
  private val KEEPER_ITEMS = 50
  private val KEEPER_ID = 500L
  private val MEMBERSHIP_ID = 1000L

  // the user's own membership (owner) — never part of the removed set
  private val membership = GroupMember.sampleData.copy(
    groupMemberId = MEMBERSHIP_ID, groupId = 1L, memberId = "self", memberRole = GroupMemberRole.Owner
  )

  // group with fullDelete ON so removeMemberItems takes the (expensive) removeAllAndNotify branch
  private val groupInfo = GroupInfo.sampleData.copy(
    groupId = 1L,
    membership = membership,
    fullGroupPreferences = GroupInfo.sampleData.fullGroupPreferences.copy(
      fullDelete = GroupPreference(GroupFeatureEnabled.ON, role = null)
    )
  )

  // a member that is NOT removed — its items and entry must survive
  private val keeper = GroupMember.sampleData.copy(
    groupMemberId = KEEPER_ID, groupId = 1L, memberId = "keeper", localDisplayName = "keeper"
  )

  // the 100 members present in the group before removal
  private val presentMembers: List<GroupMember> = (1L..N_MEMBERS).map { id ->
    GroupMember.sampleData.copy(
      groupMemberId = id, groupId = 1L, memberId = "mid_$id", localDisplayName = "member_$id",
      memberStatus = GroupMemberStatus.MemComplete
    )
  }

  // what apiRemoveMembers returns: the same members with an updated status
  private val removedMembers: List<GroupMember> = presentMembers.map { it.copy(memberStatus = GroupMemberStatus.MemRemoved) }

  private fun buildItems(): List<ChatItem> {
    var id = 1L
    val items = ArrayList<ChatItem>(N_MEMBERS * ITEMS_PER_MEMBER + KEEPER_ITEMS)
    for (m in presentMembers) {
      repeat(ITEMS_PER_MEMBER) { items.add(ChatItem.getSampleData(id = id++, dir = CIDirection.GroupRcv(m))) }
    }
    repeat(KEEPER_ITEMS) { items.add(ChatItem.getSampleData(id = id++, dir = CIDirection.GroupRcv(keeper))) }
    return items
  }

  // chatId / groupMembers / groupMembersIndexes are shared state on the ChatModel object;
  // only chatItems (and chatState) are per-context.
  private fun ChatsContext.seed() {
    ChatModel.chatId.value = groupInfo.id
    chatItems.replaceAll(buildItems())
    val all = presentMembers + keeper
    ChatModel.groupMembers.value = all
    ChatModel.groupMembersIndexes.value = all.mapIndexed { i, m -> m.groupMemberId to i }.toMap()
  }

  private data class Snapshot(val itemIds: List<Long>, val members: List<GroupMember>)

  private fun ChatsContext.snapshot() = Snapshot(
    itemIds = chatItems.value.map { it.id },
    members = ChatModel.groupMembers.value.toList()
  )

  private fun assertCorrect(s: Snapshot) {
    // only the keeper's items remain; all removed members' items are gone
    assertEquals(KEEPER_ITEMS, s.itemIds.size, "only keeper items should remain")
    // every removed member is now marked removed, keeper is untouched
    val byId = s.members.associateBy { it.groupMemberId }
    assertTrue((1L..N_MEMBERS).all { byId[it]?.memberStatus == GroupMemberStatus.MemRemoved }, "removed members must be updated")
    assertEquals(GroupMemberStatus.MemComplete, byId[KEEPER_ID]?.memberStatus, "keeper must be untouched")
  }

  @Test
  fun batchedRemovalIsCorrectAndFaster() {
    val ctx = ChatModel.chatsContext

    // OLD: per-member loop (mirrors the pre-fix removeMembers body)
    ctx.seed()
    val oldNanos = measure {
      runBlocking {
        for (m in removedMembers) {
          ctx.upsertGroupMember(rhId, groupInfo, m)
          ctx.removeMemberItems(rhId, m, byMember = membership, groupInfo)
        }
      }
    }
    val oldResult = ctx.snapshot()

    // NEW: batched calls (the fixed removeMembers body)
    ctx.seed()
    val newNanos = measure {
      runBlocking {
        ctx.upsertGroupMembers(rhId, groupInfo, removedMembers)
        ctx.removeMemberItems(rhId, removedMembers, byMember = membership, groupInfo)
      }
    }
    val newResult = ctx.snapshot()

    // correctness
    assertCorrect(oldResult)
    assertCorrect(newResult)
    assertEquals(oldResult.itemIds.sorted(), newResult.itemIds.sorted(), "batched must remove the same items as the loop")
    assertEquals(oldResult.members, newResult.members, "batched must produce the same members list as the loop")

    // performance
    val oldMs = oldNanos / 1_000_000.0
    val newMs = newNanos / 1_000_000.0
    val speedup = oldNanos.toDouble() / newNanos.coerceAtLeast(1)
    println(
      """
      |
      |========= remove-members performance =========
      | members removed : $N_MEMBERS
      | chat items      : ${N_MEMBERS * ITEMS_PER_MEMBER + KEEPER_ITEMS}
      | old (per-member): ${"%.1f".format(oldMs)} ms   O(members * items)
      | new (batched)   : ${"%.1f".format(newMs)} ms   O(members + items)
      | speedup         : ${"%.1f".format(speedup)}x
      |==============================================
      """.trimMargin()
    )

    assertTrue(newNanos * 2 < oldNanos, "batched should be clearly faster: old=${"%.1f".format(oldMs)}ms new=${"%.1f".format(newMs)}ms (${"%.1f".format(speedup)}x)")
  }

  private inline fun measure(block: () -> Unit): Long {
    val start = System.nanoTime()
    block()
    return System.nanoTime() - start
  }
}
