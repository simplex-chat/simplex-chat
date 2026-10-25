package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.newchat.ContactType
import chat.simplex.common.views.newchat.chatContactType
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class ContactTypeUnitTest {

  private val readyConn = Connection.sampleData
  private val preparedConn = Connection.sampleData.copy(connStatus = ConnStatus.Prepared)
  private val newConn = Connection.sampleData.copy(connStatus = ConnStatus.New)
  private val address = "simplex:/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw?h=smp6.simplex.im"
  private val directInvitation = GroupDirectInvitation(
    groupDirectInvLink = address,
    fromGroupId_ = 1,
    fromGroupMemberId_ = 2,
    fromGroupMemberConnId_ = 3,
    groupDirectInvStartedConnection = false
  )

  private fun contact(
    activeConn: Connection? = readyConn,
    contactStatus: ContactStatus = ContactStatus.Active,
    contactLink: String? = null,
    preparedContact: PreparedContact? = null,
    contactRequestId: Long? = null,
    groupDirectInv: GroupDirectInvitation? = null,
    chatDeleted: Boolean = false,
    displayName: String = "alice",
    fullName: String = "Alice",
    localAlias: String = "",
  ): Contact =
    Contact.sampleData.copy(
      profile = LocalProfile.sampleData.copy(displayName = displayName, fullName = fullName, localAlias = localAlias, contactLink = contactLink),
      activeConn = activeConn,
      contactStatus = contactStatus,
      preparedContact = preparedContact,
      contactRequestId = contactRequestId,
      groupDirectInv = groupDirectInv,
      chatDeleted = chatDeleted,
    )

  private fun type(contact: Contact): ContactType =
    chatContactType(Chat(remoteHostId = null, chatInfo = ChatInfo.Direct(contact), chatItems = emptyList()))

  @Test
  fun contactRequestIsRequest() {
    assertEquals(ContactType.REQUEST, chatContactType(Chat(remoteHostId = null, chatInfo = ChatInfo.ContactRequest(UserContactRequest.sampleData), chatItems = emptyList())))
  }

  @Test
  fun groupIsUnlisted() {
    assertEquals(ContactType.UNLISTED, chatContactType(Chat(remoteHostId = null, chatInfo = ChatInfo.Group(GroupInfo.sampleData, groupChatScope = null), chatItems = emptyList())))
  }

  @Test
  fun contactWithPendingRequestIsContactWithRequest() {
    assertEquals(ContactType.CONTACT_WITH_REQUEST, type(contact(activeConn = null, contactRequestId = 1)))
    assertEquals(ContactType.CONTACT_WITH_REQUEST, type(contact(activeConn = newConn, contactRequestId = 1)))
    assertEquals(ContactType.CONTACT_WITH_REQUEST, type(contact(activeConn = preparedConn, contactRequestId = 1)))
    assertEquals(ContactType.CONTACT_WITH_REQUEST, type(contact(activeConn = null, groupDirectInv = directInvitation)))
  }

  @Test
  fun deletedContactWithStaleRequestIsUnlisted() {
    assertEquals(ContactType.UNLISTED, type(contact(activeConn = null, contactRequestId = 1, contactStatus = ContactStatus.Deleted)))
  }

  @Test
  fun acceptedRequestWithReadyConnectionIsRecent() {
    assertEquals(ContactType.RECENT, type(contact(activeConn = readyConn, contactRequestId = 1)))
  }

  @Test
  fun contactWithRequestWinsOverDeletedChat() {
    assertEquals(ContactType.CONTACT_WITH_REQUEST, type(contact(activeConn = null, contactRequestId = 1, chatDeleted = true)))
  }

  @Test
  fun addressWithoutConnectionIsCard() {
    assertEquals(ContactType.CARD, type(contact(activeConn = null, contactLink = address)))
    assertEquals(ContactType.CARD, type(contact(activeConn = preparedConn, contactLink = address)))
  }

  @Test
  fun cardWinsOverDeletedChat() {
    assertEquals(ContactType.CARD, type(contact(activeConn = null, contactLink = address, chatDeleted = true)))
  }

  @Test
  fun addressWithReadyConnectionIsRecent() {
    assertEquals(ContactType.RECENT, type(contact(activeConn = readyConn, contactLink = address)))
  }

  @Test
  fun preparedContactWithAddressIsNotCard() {
    val prepared = PreparedContact(connLinkToConnect = CreatedConnLink(connFullLink = address, connShortLink = null), uiConnLinkType = ConnectionMode.Con)
    assertEquals(ContactType.RECENT, type(contact(activeConn = null, contactLink = address, preparedContact = prepared)))
  }

  @Test
  fun deletedContactWithAddressIsNotCard() {
    assertEquals(ContactType.UNLISTED, type(contact(activeConn = null, contactLink = address, contactStatus = ContactStatus.Deleted)))
  }

  @Test
  fun deletedChatIsChatDeleted() {
    assertEquals(ContactType.CHAT_DELETED, type(contact(chatDeleted = true)))
    assertEquals(ContactType.CHAT_DELETED, type(contact(chatDeleted = true, contactStatus = ContactStatus.DeletedByUser)))
  }

  @Test
  fun activeContactIsRecent() {
    assertEquals(ContactType.RECENT, type(contact()))
  }

  @Test
  fun inactiveContactWithChatIsUnlisted() {
    assertEquals(ContactType.UNLISTED, type(contact(contactStatus = ContactStatus.Deleted)))
    assertEquals(ContactType.UNLISTED, type(contact(contactStatus = ContactStatus.DeletedByUser)))
  }

  @Test
  fun contactTypesAreOrderedForTheNewChatList() {
    assertEquals(
      listOf(ContactType.CARD, ContactType.CONTACT_WITH_REQUEST, ContactType.REQUEST, ContactType.RECENT, ContactType.CHAT_DELETED, ContactType.UNLISTED),
      ContactType.entries
    )
  }

  @Test
  fun contactSearchIgnoresCaseAndSurroundingSpaces() {
    val c = contact(displayName = "cath", fullName = "")
    assertTrue(c.anyNameContains("CA"))
    assertTrue(c.anyNameContains("  ath "))
    assertFalse(c.anyNameContains("zzz"))
  }

  @Test
  fun contactSearchMatchesFullName() {
    assertTrue(contact(displayName = "bob", fullName = "Robert Smith").anyNameContains("smith"))
  }

  @Test
  fun contactSearchMatchesLocalAliasAndStillMatchesNames() {
    val c = contact(displayName = "bob", fullName = "Robert", localAlias = "dentist")
    assertTrue(c.anyNameContains("DENT"))
    assertTrue(c.anyNameContains("bob"))
    assertTrue(c.anyNameContains("robert"))
  }
}
