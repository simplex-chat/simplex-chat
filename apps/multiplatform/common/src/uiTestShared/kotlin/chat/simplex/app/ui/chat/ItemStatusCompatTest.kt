package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.ownGroupWithTwo
import chat.simplex.common.model.CIStatus
import chat.simplex.common.model.MsgReceiptStatus
import chat.simplex.common.model.SndCIStatusProgress
import org.junit.Test

class ItemStatusCompatTest : UiTest() {
  @Test fun status_group_partialThenComplete() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = ownGroupWithTwo(alice, bob, cath)
    cath.stop()
    chat.send("hi")
    bob.eventuallyMessage("#team alice> hi", CONNECT_TIMEOUT_MS)
    val id = chat.itemId("hi")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`hi` delivered to bob only: delivered, partial") {
      (chat.item(id)?.meta?.itemStatus as? CIStatus.SndRcvd)?.let { it.msgRcptStatus == MsgReceiptStatus.Ok && it.sndProgress == SndCIStatusProgress.Partial } == true
    }
    cath.start()
    cath.eventuallyMessage("#team alice> hi", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`hi` delivered to bob and cath: delivered, complete") {
      (chat.item(id)?.meta?.itemStatus as? CIStatus.SndRcvd)?.let { it.msgRcptStatus == MsgReceiptStatus.Ok && it.sndProgress == SndCIStatusProgress.Complete } == true
    }
  }
}
