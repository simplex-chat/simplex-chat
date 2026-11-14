package chat.simplex.app

import chat.simplex.common.model.ChatPeerType
import chat.simplex.common.model.MsgChatLink
import chat.simplex.common.model.Profile
import chat.simplex.common.model.peerTypeIcon
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotEquals

class PeerTypeIconTest {
  private fun contact(peerType: ChatPeerType?, business: Boolean = false) = MsgChatLink.Contact("", Profile.sampleData.copy(peerType = peerType), business)

  private fun invitation(peerType: ChatPeerType?) = MsgChatLink.Invitation("", Profile.sampleData.copy(peerType = peerType))

  @Test
  fun linkCardsShowThePeerType() {
    assertEquals(peerTypeIcon(ChatPeerType.Agent), contact(ChatPeerType.Agent).iconRes)
    assertEquals(peerTypeIcon(ChatPeerType.Bot), contact(ChatPeerType.Bot).iconRes)
    assertEquals(peerTypeIcon(null), contact(ChatPeerType.Human).iconRes)
    assertEquals(peerTypeIcon(null), contact(null).iconRes)
    assertEquals(peerTypeIcon(ChatPeerType.Agent), invitation(ChatPeerType.Agent).iconRes)
    assertEquals(contact(null, business = true).iconRes, contact(ChatPeerType.Agent, business = true).iconRes)
    assertNotEquals(peerTypeIcon(ChatPeerType.Agent), contact(ChatPeerType.Agent, business = true).iconRes)
  }

  @Test
  fun smallLinkIconsShowThePeerType() {
    assertEquals(peerTypeIcon(ChatPeerType.Agent), contact(ChatPeerType.Agent).smallIconRes)
    assertEquals(peerTypeIcon(ChatPeerType.Bot), contact(ChatPeerType.Bot).smallIconRes)
    assertEquals(contact(null).smallIconRes, contact(ChatPeerType.Human).smallIconRes)
    assertNotEquals(peerTypeIcon(null), contact(null).smallIconRes)
    assertEquals(peerTypeIcon(ChatPeerType.Bot), invitation(ChatPeerType.Bot).smallIconRes)
    assertEquals(contact(null, business = true).smallIconRes, contact(ChatPeerType.Agent, business = true).smallIconRes)
  }
}
