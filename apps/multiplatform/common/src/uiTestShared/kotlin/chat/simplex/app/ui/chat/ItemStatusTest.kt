package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.TestServers
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitConnectionDeletedOnServer
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.contactOf
import chat.simplex.common.model.CIStatus
import chat.simplex.common.model.SndError
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class ItemStatusTest : UiTest() {
  @Test fun status_authError_clickShowsDeliveryError() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("before")
    bob.expectsMessage("alice> before", CONNECT_TIMEOUT_MS)
    val before = chat.itemId("before")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`before` delivered to bob") { chat.item(before)?.meta?.itemStatus is CIStatus.SndRcvd }
    click(Ui.Chat.Item(before))
    settle()
    assertFalse("clicking a delivered message shows a delivery alert", seesText(DELIVERY_ERROR))
    bob sends "/d alice notify=off"
    bob expects "alice: contact is deleted"
    awaitConnectionDeletedOnServer(bob)
    chat.send("after")
    val after = chat.itemId("after")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`after` failed with an authorization error") {
      (chat.item(after)?.meta?.itemStatus as? CIStatus.CISSndError)?.agentError is SndError.Auth
    }
    assertFalse("the delivery alert is shown before the failed message is clicked", seesText(DELIVERY_ERROR))
    click(Ui.Chat.Item(after))
    poll(what = "the `$DELIVERY_ERROR` alert explaining the authorization error") { seesText(DELIVERY_ERROR) && seesText(AUTH_ERROR_INFO) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the delivery error alert to close") { !seesText(DELIVERY_ERROR) }
  }

  @Test fun status_quotaExceeded_warningAlert() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice's contact with bob ready") { contactOf("bob").ready }
    chat.send("drained")
    bob.expectsMessage("alice> drained", CONNECT_TIMEOUT_MS)
    val drained = chat.itemId("drained")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`drained` delivered to bob, so bob's queue is empty") { chat.item(drained)?.meta?.itemStatus is CIStatus.SndRcvd }
    bob.stop()
    chat.send("m1")
    val first = chat.itemId("m1")
    for (i in 2..QUOTA + 1) chat.send("m$i")
    val last = chat.itemId("m${QUOTA + 1}")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the message over the queue capacity marked with a delivery warning") {
      (chat.item(last)?.meta?.itemStatus as? CIStatus.SndWarning)?.agentError is SndError.Quota
    }
    assertFalse("a message within the queue capacity has a delivery warning", chat.item(first)?.meta?.itemStatus is CIStatus.SndWarning)
    click(Ui.Chat.Item(last))
    poll(what = "the `$DELIVERY_WARNING` alert explaining the exceeded capacity") { seesText(DELIVERY_WARNING) && seesText(QUOTA_INFO) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the delivery warning alert to close") { !seesText(DELIVERY_WARNING) }
  }

  @Test fun status_pending_whileServerDown() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice's contact with bob ready") { contactOf("bob").ready }
    chat.send("before")
    bob.expectsMessage("alice> before", CONNECT_TIMEOUT_MS)
    val before = chat.itemId("before")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`before` sent with smp1 up") { chat.item(before)?.meta?.itemStatus.isSentOrDelivered() }
    TestServers.stop("smp1")
    chat.send("while down")
    val pending = chat.itemId("while down")
    poll(what = "the send of `while down` to return with smp1 down") { UiIdle.inFlight().isEmpty() }
    chat.item(pending)?.meta?.itemStatus.let { assertTrue("`while down` is not pending with smp1 down: $it", it is CIStatus.SndNew) }
    TestServers.start("smp1")
    bob.eventuallyMessage("alice> while down", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`while down` sent once smp1 is back") { chat.item(pending)?.meta?.itemStatus.isSentOrDelivered() }
  }

  private fun CIStatus?.isSentOrDelivered() = this is CIStatus.SndSent || this is CIStatus.SndRcvd

  private companion object {
    const val QUOTA = 16
    const val DELIVERY_ERROR = "Message delivery error"
    const val DELIVERY_WARNING = "Message delivery warning"
    const val AUTH_ERROR_INFO = "Wrong key or unknown connection - most likely this connection is deleted."
    const val QUOTA_INFO = "Capacity exceeded - recipient did not receive previously sent messages."
  }
}
