package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.views.helpers.MAX_IMAGE_SIZE_AUTO_RCV
import kotlinx.datetime.Clock
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class AutoReceiveFileTest {
  private fun fileItem(
    mc: MsgContent,
    itemDeleted: CIDeleted? = null,
    fileSize: Long = 1000,
    fileStatus: CIFileStatus = CIFileStatus.RcvInvitation
  ) =
    ChatItem(
      chatDir = CIDirection.DirectRcv(),
      meta = CIMeta.getSample(1, Clock.System.now(), "", CIStatus.RcvRead(), itemDeleted = itemDeleted),
      content = CIContent.RcvMsgContent(mc),
      reactions = listOf(),
      file = CIFile.getSample(fileName = "file", fileSize = fileSize, filePath = null, fileStatus = fileStatus)
    )

  private val image = MsgContent.MCImage("", "")
  private val video = MsgContent.MCVideo("", "", 1)
  private val voice = MsgContent.MCVoice("", 1)

  private fun assertReceived(item: ChatItem) = assertEquals(item.file, item.autoReceiveFile())

  @Test
  fun receivesImage() {
    appPrefs.privacyAcceptImages.set(true)
    assertReceived(fileItem(image))
  }

  @Test
  fun receivesVideoAndVoice() {
    appPrefs.privacyAcceptImages.set(true)
    assertReceived(fileItem(video))
    assertReceived(fileItem(voice))
  }

  @Test
  fun skipsAcceptedVoice() {
    appPrefs.privacyAcceptImages.set(true)
    assertNull(fileItem(voice, fileStatus = CIFileStatus.RcvAccepted).autoReceiveFile())
  }

  @Test
  fun skipsLargeImage() {
    appPrefs.privacyAcceptImages.set(true)
    assertNull(fileItem(image, fileSize = MAX_IMAGE_SIZE_AUTO_RCV + 1).autoReceiveFile())
  }

  @Test
  fun skipsImageWhenNotAccepted() {
    appPrefs.privacyAcceptImages.set(false)
    assertNull(fileItem(image).autoReceiveFile())
    appPrefs.privacyAcceptImages.set(true)
  }

  @Test
  fun skipsImageOfBlockedMember() {
    appPrefs.privacyAcceptImages.set(true)
    assertNull(fileItem(image, CIDeleted.Blocked(null)).autoReceiveFile())
    assertNull(fileItem(image, CIDeleted.BlockedByAdmin(null)).autoReceiveFile())
  }

  @Test
  fun skipsImageOfDeletedItem() {
    appPrefs.privacyAcceptImages.set(true)
    assertNull(fileItem(image, CIDeleted.Deleted(null)).autoReceiveFile())
  }
}
