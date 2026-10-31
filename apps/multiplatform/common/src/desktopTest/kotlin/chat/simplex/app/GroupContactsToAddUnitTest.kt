package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.chat.group.getContactsToAdd
import kotlin.test.AfterTest
import kotlin.test.Test
import kotlin.test.assertEquals

class GroupContactsToAddUnitTest {

  private fun contact(id: Long, name: String, fullName: String = "", alias: String = ""): Contact =
    Contact.sampleData.copy(
      contactId = id,
      localDisplayName = name,
      profile = LocalProfile.sampleData.copy(profileId = id, displayName = name, fullName = fullName, localAlias = alias)
    )

  private fun member(contactId: Long, status: GroupMemberStatus): GroupMember =
    GroupMember.sampleData.copy(groupMemberId = 100 + contactId, memberContactId = contactId, memberStatus = status)

  private fun setChats(vararg contacts: Contact, members: List<GroupMember> = emptyList()) {
    chatModel.chatsContext.chats.replaceAll(contacts.map { Chat(remoteHostId = null, chatInfo = ChatInfo.Direct(it), chatItems = emptyList()) })
    chatModel.groupMembers.value = members
  }

  private fun names(search: String = ""): List<String> = getContactsToAdd(chatModel, search).map { it.localDisplayName }

  @AfterTest
  fun resetChatModel() {
    chatModel.chatsContext.chats.replaceAll(emptyList())
    chatModel.groupMembers.value = emptyList()
  }

  @Test
  fun readyContactIsListed() {
    setChats(contact(1, "bob"))
    assertEquals(listOf("bob"), names())
  }

  @Test
  fun contactWhoIsCurrentMemberIsExcluded() {
    setChats(contact(1, "bob"), contact(2, "cath"), members = listOf(member(1, GroupMemberStatus.MemConnected)))
    assertEquals(listOf("cath"), names())
  }

  @Test
  fun invitedMemberIsStillOffered() {
    setChats(contact(1, "bob"), members = listOf(member(1, GroupMemberStatus.MemInvited)))
    assertEquals(listOf("bob"), names())
  }

  @Test
  fun memberWhoLeftOrWasRemovedIsOfferedAgain() {
    setChats(contact(1, "bob"), contact(2, "cath"), members = listOf(member(1, GroupMemberStatus.MemLeft), member(2, GroupMemberStatus.MemRemoved)))
    assertEquals(listOf("bob", "cath"), names())
  }

  @Test
  fun contactStillToConnectIsExcluded() {
    setChats(contact(1, "bob").copy(contactGroupMemberId = 9, contactGrpInvSent = false), contact(2, "cath"))
    assertEquals(listOf("cath"), names())
  }

  @Test
  fun deletedContactIsExcluded() {
    setChats(contact(1, "bob").copy(contactStatus = ContactStatus.Deleted), contact(2, "cath"))
    assertEquals(listOf("cath"), names())
  }

  @Test
  fun contactNotReadyIsExcluded() {
    setChats(contact(1, "bob").copy(activeConn = Connection.sampleData.copy(connStatus = ConnStatus.Joined)), contact(2, "cath"))
    assertEquals(listOf("cath"), names())
  }

  @Test
  fun groupChatsAreNotOffered() {
    chatModel.chatsContext.chats.replaceAll(
      listOf(
        Chat(remoteHostId = null, chatInfo = ChatInfo.Group(GroupInfo.sampleData, groupChatScope = null), chatItems = emptyList()),
        Chat(remoteHostId = null, chatInfo = ChatInfo.Direct(contact(2, "cath")), chatItems = emptyList()),
      )
    )
    assertEquals(listOf("cath"), names())
  }

  @Test
  fun searchIgnoresCaseAndSurroundingSpaces() {
    setChats(contact(1, "bob"), contact(2, "cath"))
    assertEquals(listOf("cath"), names("CA"))
    assertEquals(listOf("cath"), names("  cA "))
  }

  @Test
  fun searchMatchesFullNameAndLocalAlias() {
    setChats(contact(1, "bob", fullName = "Robert Smith"), contact(2, "cath", alias = "kitty"), contact(3, "dan"))
    assertEquals(listOf("bob"), names("smith"))
    assertEquals(listOf("cath"), names("KIT"))
  }

  @Test
  fun resultsAreSortedByShownNameIgnoringCase() {
    setChats(contact(1, "cath"), contact(2, "Alice"), contact(3, "bob"), contact(4, "aaron", alias = "zed"))
    assertEquals(listOf("Alice", "bob", "cath", "aaron"), names())
  }
}
