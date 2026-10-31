package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.res.*
import dev.icerock.moko.resources.StringResource
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

class ChatFileCancelActionUnitTest {
  private fun file(status: CIFileStatus, protocol: FileProtocol = FileProtocol.XFTP): CIFile =
    CIFile(fileId = 1, fileName = "test.txt", fileSize = 100, fileSource = CryptoFile.plain("test.txt"), fileStatus = status, fileProtocol = protocol)

  private fun actionTexts(f: CIFile): List<StringResource>? =
    f.cancelAction?.let { listOf(it.uiActionId, it.alert.titleId, it.alert.messageId, it.alert.confirmId) }

  private val stopSnd = listOf(MR.strings.stop_file__action, MR.strings.stop_snd_file__title, MR.strings.stop_snd_file__message, MR.strings.stop_file__confirm)
  private val revoke = listOf(MR.strings.revoke_file__action, MR.strings.revoke_file__title, MR.strings.revoke_file__message, MR.strings.revoke_file__confirm)
  private val stopRcv = listOf(MR.strings.stop_file__action, MR.strings.stop_rcv_file__title, MR.strings.stop_rcv_file__message, MR.strings.stop_file__confirm)

  @Test
  fun sendingFileCanBeStopped() {
    assertEquals(stopSnd, actionTexts(file(CIFileStatus.SndStored)))
    assertEquals(stopSnd, actionTexts(file(CIFileStatus.SndTransfer(sndProgress = 10, sndTotal = 100))))
    assertEquals(stopSnd, actionTexts(file(CIFileStatus.SndWarning(FileError.NoFile()))))
  }

  @Test
  fun uploadedXftpFileCanBeRevoked() {
    assertEquals(revoke, actionTexts(file(CIFileStatus.SndComplete, FileProtocol.XFTP)))
  }

  @Test
  fun completedSmpOrLocalFileHasNoCancelAction() {
    assertNull(file(CIFileStatus.SndComplete, FileProtocol.SMP).cancelAction)
    assertNull(file(CIFileStatus.SndComplete, FileProtocol.LOCAL).cancelAction)
  }

  @Test
  fun cancelledOrFailedSentFileHasNoCancelAction() {
    assertNull(file(CIFileStatus.SndCancelled).cancelAction, "cancelled")
    assertNull(file(CIFileStatus.SndError(FileError.Auth())).cancelAction, "error")
  }

  @Test
  fun receivingFileCanBeStopped() {
    assertEquals(stopRcv, actionTexts(file(CIFileStatus.RcvAccepted)))
    assertEquals(stopRcv, actionTexts(file(CIFileStatus.RcvTransfer(rcvProgress = 10, rcvTotal = 100))))
    assertEquals(stopRcv, actionTexts(file(CIFileStatus.RcvWarning(FileError.NoFile()))))
  }

  @Test
  fun receivedFileOutsideTransferHasNoCancelAction() {
    assertNull(file(CIFileStatus.RcvInvitation).cancelAction, "invitation")
    assertNull(file(CIFileStatus.RcvAborted).cancelAction, "aborted")
    assertNull(file(CIFileStatus.RcvCancelled).cancelAction, "cancelled")
    assertNull(file(CIFileStatus.RcvComplete).cancelAction, "complete")
    assertNull(file(CIFileStatus.RcvError(FileError.Auth())).cancelAction, "error")
    assertNull(file(CIFileStatus.Invalid("bad")).cancelAction, "invalid")
  }

  @Test
  fun sentFilesAreLoaded() {
    assertTrue(file(CIFileStatus.SndStored).loaded, "stored")
    assertTrue(file(CIFileStatus.SndTransfer(sndProgress = 10, sndTotal = 100)).loaded, "transfer")
    assertTrue(file(CIFileStatus.SndComplete).loaded, "complete")
    assertTrue(file(CIFileStatus.SndCancelled).loaded, "cancelled")
    assertTrue(file(CIFileStatus.SndError(FileError.Auth())).loaded, "error")
    assertTrue(file(CIFileStatus.SndWarning(FileError.NoFile())).loaded, "warning")
  }

  @Test
  fun receivedFileIsLoadedOnlyWhenComplete() {
    assertTrue(file(CIFileStatus.RcvComplete).loaded, "complete")
    assertFalse(file(CIFileStatus.RcvInvitation).loaded, "invitation")
    assertFalse(file(CIFileStatus.RcvAccepted).loaded, "accepted")
    assertFalse(file(CIFileStatus.RcvTransfer(rcvProgress = 10, rcvTotal = 100)).loaded, "transfer")
    assertFalse(file(CIFileStatus.RcvAborted).loaded, "aborted")
    assertFalse(file(CIFileStatus.RcvCancelled).loaded, "cancelled")
    assertFalse(file(CIFileStatus.RcvError(FileError.Auth())).loaded, "error")
    assertFalse(file(CIFileStatus.RcvWarning(FileError.NoFile())).loaded, "warning")
    assertFalse(file(CIFileStatus.Invalid("bad")).loaded, "invalid")
  }
}
