package chat.simplex.app

import chat.simplex.common.model.ChatPeerType
import chat.simplex.common.model.MsgChatLink
import chat.simplex.common.model.Profile
import chat.simplex.res.MR
import kotlin.test.Test
import kotlin.test.assertEquals

class PeerTypeIconTest {
  private fun contact(peerType: ChatPeerType?, business: Boolean = false) = MsgChatLink.Contact("", Profile.sampleData.copy(peerType = peerType), business)

  private fun invitation(peerType: ChatPeerType?) = MsgChatLink.Invitation("", Profile.sampleData.copy(peerType = peerType))

  @Test
  fun linkCardsShowThePeerType() {
    assertEquals(MR.images.ic_agent, contact(ChatPeerType.Agent).iconRes)
    assertEquals(MR.images.ic_cube, contact(ChatPeerType.Bot).iconRes)
    assertEquals(MR.images.ic_account_circle_filled, contact(ChatPeerType.Human).iconRes)
    assertEquals(MR.images.ic_account_circle_filled, contact(null).iconRes)
    assertEquals(MR.images.ic_agent, invitation(ChatPeerType.Agent).iconRes)
    assertEquals(MR.images.ic_work_filled_padded, contact(ChatPeerType.Agent, business = true).iconRes)
  }

  @Test
  fun smallLinkIconsShowThePeerType() {
    assertEquals(MR.images.ic_agent, contact(ChatPeerType.Agent).smallIconRes)
    assertEquals(MR.images.ic_cube, contact(ChatPeerType.Bot).smallIconRes)
    assertEquals(MR.images.ic_person, contact(ChatPeerType.Human).smallIconRes)
    assertEquals(MR.images.ic_person, contact(null).smallIconRes)
    assertEquals(MR.images.ic_cube, invitation(ChatPeerType.Bot).smallIconRes)
    assertEquals(MR.images.ic_work, contact(ChatPeerType.Agent, business = true).smallIconRes)
  }
}
