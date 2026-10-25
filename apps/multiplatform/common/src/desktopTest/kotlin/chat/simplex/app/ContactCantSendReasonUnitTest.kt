package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.*
import dev.icerock.moko.resources.StringResource
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class ContactCantSendReasonUnitTest {

  private val readyConn = Connection.sampleData
  private val joinedConn = Connection.sampleData.copy(connStatus = ConnStatus.Joined)
  private val link = CreatedConnLink(connFullLink = "simplex:/contact#/?v=2-7", connShortLink = null)

  private fun contact(
    activeConn: Connection? = readyConn,
    contactStatus: ContactStatus = ContactStatus.Active,
    preparedContact: PreparedContact? = null,
    contactRequestId: Long? = null,
    contactGroupMemberId: Long? = null,
    contactGrpInvSent: Boolean = false,
  ): Contact =
    Contact.sampleData.copy(
      activeConn = activeConn,
      contactStatus = contactStatus,
      preparedContact = preparedContact,
      contactRequestId = contactRequestId,
      contactGroupMemberId = contactGroupMemberId,
      contactGrpInvSent = contactGrpInvSent,
    )

  private fun prepared(mode: ConnectionMode) = PreparedContact(connLinkToConnect = link, uiConnLinkType = mode)

  private fun stats(state: RatchetSyncState) = ConnectionStats(
    connAgentVersion = 1,
    rcvQueuesInfo = emptyList(),
    sndQueuesInfo = emptyList(),
    ratchetSyncState = state,
    ratchetSyncSupported = true,
    subStatus = null
  )

  private fun reason(contact: Contact): Pair<String, String?>? = ChatInfo.Direct(contact).userCantSendReason()

  private fun expected(id: StringResource): Pair<String, String?> = generalGetString(id) to null

  @Test
  fun readyContactCanSend() {
    assertNull(reason(contact()))
  }

  @Test
  fun sndReadyContactCanSend() {
    assertNull(reason(contact(activeConn = readyConn.copy(connStatus = ConnStatus.SndReady))))
  }

  @Test
  fun preparedContactBeforeConnectCanSend() {
    assertNull(reason(contact(activeConn = null, preparedContact = prepared(ConnectionMode.Inv))))
    assertNull(reason(contact(activeConn = null, preparedContact = prepared(ConnectionMode.Con))))
    assertNull(reason(contact(activeConn = readyConn.copy(connStatus = ConnStatus.Prepared), preparedContact = prepared(ConnectionMode.Con))))
  }

  @Test
  fun memberContactWithUnsentInvitationCanSend() {
    assertNull(reason(contact(activeConn = null, contactGroupMemberId = 5)))
  }

  @Test
  fun memberContactWithSentInvitationIsNotReady() {
    assertEquals(expected(MR.strings.cant_send_message_contact_not_ready), reason(contact(activeConn = null, contactGroupMemberId = 5, contactGrpInvSent = true)))
  }

  @Test
  fun sendToConnectWinsOverPendingRequest() {
    assertNull(reason(contact(activeConn = null, preparedContact = prepared(ConnectionMode.Con), contactRequestId = 3)))
  }

  @Test
  fun contactWithPendingRequestCannotSend() {
    assertEquals(expected(MR.strings.cant_send_message_generic), reason(contact(activeConn = null, contactRequestId = 3)))
    assertEquals(expected(MR.strings.cant_send_message_generic), reason(contact(activeConn = readyConn.copy(connStatus = ConnStatus.New), contactRequestId = 3)))
  }

  @Test
  fun deletedContactCannotSend() {
    assertEquals(expected(MR.strings.cant_send_message_contact_deleted), reason(contact(contactStatus = ContactStatus.Deleted)))
    assertEquals(expected(MR.strings.cant_send_message_contact_deleted), reason(contact(contactStatus = ContactStatus.DeletedByUser)))
  }

  @Test
  fun deletedWinsOverNotSynchronized() {
    assertEquals(
      expected(MR.strings.cant_send_message_contact_deleted),
      reason(contact(activeConn = readyConn.copy(connectionStats = stats(RatchetSyncState.Required)), contactStatus = ContactStatus.Deleted))
    )
  }

  @Test
  fun connectingViaAddressShowsRequestSent() {
    assertEquals(expected(MR.strings.cant_send_message_request_is_sent), reason(contact(activeConn = joinedConn, preparedContact = prepared(ConnectionMode.Con))))
  }

  @Test
  fun connectingViaOneTimeLinkIsNotReady() {
    assertEquals(expected(MR.strings.cant_send_message_contact_not_ready), reason(contact(activeConn = joinedConn, preparedContact = prepared(ConnectionMode.Inv))))
  }

  @Test
  fun notReadyContactWithoutPreparedLinkIsNotReady() {
    assertEquals(expected(MR.strings.cant_send_message_contact_not_ready), reason(contact(activeConn = joinedConn)))
    assertEquals(expected(MR.strings.cant_send_message_contact_not_ready), reason(contact(activeConn = null)))
  }

  @Test
  fun ratchetSyncInProgressCannotSend() {
    for (state in listOf(RatchetSyncState.Required, RatchetSyncState.Started, RatchetSyncState.Agreed)) {
      assertEquals(expected(MR.strings.cant_send_message_contact_not_synchronized), reason(contact(activeConn = readyConn.copy(connectionStats = stats(state)))), "$state")
    }
  }

  @Test
  fun ratchetSyncOkOrAllowedCanSend() {
    for (state in listOf(RatchetSyncState.Ok, RatchetSyncState.Allowed)) {
      assertNull(reason(contact(activeConn = readyConn.copy(connectionStats = stats(state)))), "$state")
    }
  }

  @Test
  fun disabledConnectionAfterTenAuthErrorsCannotSend() {
    assertEquals(expected(MR.strings.cant_send_message_contact_disabled), reason(contact(activeConn = readyConn.copy(authErrCounter = 10))))
    assertNull(reason(contact(activeConn = readyConn.copy(authErrCounter = 9))))
  }

  @Test
  fun notSynchronizedWinsOverDisabled() {
    assertEquals(
      expected(MR.strings.cant_send_message_contact_not_synchronized),
      reason(contact(activeConn = readyConn.copy(connectionStats = stats(RatchetSyncState.Started), authErrCounter = 10)))
    )
  }
}
