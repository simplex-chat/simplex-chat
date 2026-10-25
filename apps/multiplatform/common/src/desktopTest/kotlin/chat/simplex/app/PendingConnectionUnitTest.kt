package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.*
import kotlinx.datetime.Clock
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

class PendingConnectionUnitTest {

  private fun pending(
    status: ConnStatus,
    viaContactUri: Boolean = false,
    groupLinkId: String? = null,
    incognito: Boolean = false,
    localAlias: String = "",
  ): PendingContactConnection =
    PendingContactConnection(
      pccConnId = 7,
      pccAgentConnId = "abcd",
      pccConnStatus = status,
      viaContactUri = viaContactUri,
      groupLinkId = groupLinkId,
      customUserProfileId = if (incognito) 2L else null,
      localAlias = localAlias,
      createdAt = Clock.System.now(),
      updatedAt = Clock.System.now()
    )

  @Test
  fun connStatusInitiatedByStatus() {
    assertEquals(true, ConnStatus.New.initiated)
    assertEquals(false, ConnStatus.Prepared.initiated)
    assertEquals(false, ConnStatus.Joined.initiated)
    assertEquals(true, ConnStatus.Requested.initiated)
    assertEquals(true, ConnStatus.Accepted.initiated)
    assertNull(ConnStatus.Ready.initiated)
    assertNull(ConnStatus.Deleted.initiated)
    assertNull(ConnStatus.Failed("error").initiated)
  }

  @Test
  fun sharedOneTimeLinkIsInvitedToConnect() {
    val c = pending(ConnStatus.New)
    assertEquals(generalGetString(MR.strings.display_name_invited_to_connect), c.displayName)
    assertEquals(generalGetString(MR.strings.description_you_shared_one_time_link), c.description)
    assertTrue(c.initiated)
  }

  @Test
  fun sharedOneTimeLinkIncognitoDescription() {
    val c = pending(ConnStatus.New, incognito = true)
    assertEquals(generalGetString(MR.strings.display_name_invited_to_connect), c.displayName)
    assertEquals(generalGetString(MR.strings.description_you_shared_one_time_link_incognito), c.description)
    assertTrue(c.incognito)
  }

  @Test
  fun acceptedByInviterIsStillInvitedToConnect() {
    val c = pending(ConnStatus.Accepted)
    assertEquals(generalGetString(MR.strings.display_name_invited_to_connect), c.displayName)
    assertEquals(generalGetString(MR.strings.description_you_shared_one_time_link), c.description)
  }

  @Test
  fun joinedOneTimeLinkIsAcceptedInvitation() {
    val c = pending(ConnStatus.Joined)
    assertEquals(generalGetString(MR.strings.display_name_accepted_invitation), c.displayName)
    assertEquals(generalGetString(MR.strings.description_via_one_time_link), c.description)
    assertFalse(c.initiated)
  }

  @Test
  fun preparedOneTimeLinkIsAcceptedInvitation() {
    val c = pending(ConnStatus.Prepared)
    assertEquals(generalGetString(MR.strings.display_name_accepted_invitation), c.displayName)
    assertEquals(generalGetString(MR.strings.description_via_one_time_link), c.description)
  }

  @Test
  fun joinedOneTimeLinkIncognitoDescription() {
    assertEquals(generalGetString(MR.strings.description_via_one_time_link_incognito), pending(ConnStatus.Joined, incognito = true).description)
  }

  @Test
  fun addressRequestIsRequestedToConnect() {
    val c = pending(ConnStatus.Requested, viaContactUri = true)
    assertEquals(generalGetString(MR.strings.display_name_requested_to_connect), c.displayName)
    assertEquals(generalGetString(MR.strings.description_via_contact_address_link), c.description)
    assertFalse(c.initiated)
  }

  @Test
  fun addressRequestIncognitoDescription() {
    assertEquals(generalGetString(MR.strings.description_via_contact_address_link_incognito), pending(ConnStatus.Requested, viaContactUri = true, incognito = true).description)
  }

  @Test
  fun groupLinkRequestDescriptions() {
    val c = pending(ConnStatus.Requested, viaContactUri = true, groupLinkId = "group-link")
    assertEquals(generalGetString(MR.strings.display_name_requested_to_connect), c.displayName)
    assertEquals(generalGetString(MR.strings.description_via_group_link), c.description)
    assertEquals(generalGetString(MR.strings.description_via_group_link_incognito), pending(ConnStatus.Requested, viaContactUri = true, groupLinkId = "group-link", incognito = true).description)
  }

  @Test
  fun joinedAddressIsRequestedToConnect() {
    val c = pending(ConnStatus.Joined, viaContactUri = true)
    assertEquals(generalGetString(MR.strings.display_name_requested_to_connect), c.displayName)
    assertEquals(generalGetString(MR.strings.description_via_contact_address_link), c.description)
  }

  @Test
  fun sndReady_showsConnectionEstablished_currentBehaviour_item18() {
    assertNull(ConnStatus.SndReady.initiated)
    val c = pending(ConnStatus.SndReady)
    assertEquals(generalGetString(MR.strings.display_name_connection_established), c.displayName)
    assertEquals("", c.description)
    assertFalse(c.initiated)
  }

  @Test
  fun completedStatusesShowConnectionEstablishedWithoutDescription() {
    for (status in listOf(ConnStatus.Ready, ConnStatus.Deleted, ConnStatus.Failed("error"))) {
      val c = pending(status)
      assertEquals(generalGetString(MR.strings.display_name_connection_established), c.displayName, "$status")
      assertEquals("", c.description, "$status")
      assertFalse(c.initiated, "$status")
    }
  }

  @Test
  fun localAliasReplacesDisplayNameButNotDescription() {
    val invited = pending(ConnStatus.New, localAlias = "dentist")
    assertEquals("dentist", invited.displayName)
    assertEquals(generalGetString(MR.strings.description_you_shared_one_time_link), invited.description)
    assertEquals("dentist", pending(ConnStatus.SndReady, localAlias = "dentist").displayName)
    assertEquals("dentist", pending(ConnStatus.Requested, viaContactUri = true, localAlias = "dentist").displayName)
  }

  @Test
  fun localDisplayNameUsesConnectionId() {
    assertEquals(String.format(generalGetString(MR.strings.connection_local_display_name), 7L), pending(ConnStatus.New).localDisplayName)
  }
}
