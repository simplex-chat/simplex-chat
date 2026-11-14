package chat.simplex.app

import chat.simplex.common.model.*
import kotlinx.datetime.Clock
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class ChatLinkSignedTest {
  private val card = MsgContent.MCChat(
    "",
    MsgChatLink.Invitation("https://simplex.chat/invitation", Profile.sampleData),
    LinkOwnerSig(chatBinding = "", ownerSig = "")
  )

  private fun cardItem(chatDir: CIDirection, content: CIContent) =
    ChatItem(chatDir = chatDir, meta = CIMeta.getSample(1, Clock.System.now(), ""), content = content, reactions = listOf())

  @Test
  fun sentCardIsSigned() {
    assertTrue(cardItem(CIDirection.DirectSnd(), CIContent.SndMsgContent(card)).chatLinkSigned)
  }

  @Test
  fun receivedCardIsNotSigned() {
    assertFalse(cardItem(CIDirection.DirectRcv(), CIContent.RcvMsgContent(card)).chatLinkSigned)
  }
}
