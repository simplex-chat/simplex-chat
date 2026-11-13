package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import kotlinx.datetime.Clock
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class AutoReceiveFileTest {
  private fun imageItem(itemDeleted: CIDeleted?) =
    ChatItem(
      chatDir = CIDirection.DirectRcv(),
      meta = CIMeta.getSample(1, Clock.System.now(), "", CIStatus.RcvRead(), itemDeleted = itemDeleted),
      content = CIContent.RcvMsgContent(MsgContent.MCImage("", "")),
      reactions = listOf(),
      file = CIFile.getSample(fileName = "image.jpg", fileSize = 1000, filePath = null, fileStatus = CIFileStatus.RcvInvitation)
    )

  @Test
  fun receivesImage() {
    appPrefs.privacyAcceptImages.set(true)
    val item = imageItem(null)
    assertEquals(item.file, item.autoReceiveFile())
  }

  @Test
  fun skipsImageOfBlockedMember() {
    appPrefs.privacyAcceptImages.set(true)
    assertNull(imageItem(CIDeleted.Blocked(null)).autoReceiveFile())
    assertNull(imageItem(CIDeleted.BlockedByAdmin(null)).autoReceiveFile())
  }
}
