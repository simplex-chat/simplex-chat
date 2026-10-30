package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.*
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

class ChatItemStatusUnitTest {
  private val errorTitle get() = generalGetString(MR.strings.message_delivery_error_title)
  private val warningTitle get() = generalGetString(MR.strings.message_delivery_warning_title)

  @Test
  fun statusesWithoutAlertHaveNoStatusInfo() {
    assertNull(CIStatus.SndNew().statusInto, "SndNew")
    assertNull(CIStatus.SndSent(SndCIStatusProgress.Complete).statusInto, "SndSent")
    assertNull(CIStatus.SndRcvd(MsgReceiptStatus.Ok, SndCIStatusProgress.Complete).statusInto, "SndRcvd ok")
    assertNull(CIStatus.SndRcvd(MsgReceiptStatus.BadMsgHash, SndCIStatusProgress.Partial).statusInto, "SndRcvd bad hash")
    assertNull(CIStatus.RcvNew().statusInto, "RcvNew")
    assertNull(CIStatus.RcvRead().statusInto, "RcvRead")
  }

  @Test
  fun authErrorShowsDeliveryErrorWithDeletedConnectionText() {
    assertEquals(errorTitle to generalGetString(MR.strings.message_delivery_error_desc), CIStatus.SndErrorAuth().statusInto)
  }

  @Test
  fun sndErrorShowsDeliveryErrorWithAgentErrorText() {
    assertEquals(errorTitle to generalGetString(MR.strings.snd_error_auth), CIStatus.CISSndError(SndError.Auth()).statusInto)
    assertEquals(errorTitle to generalGetString(MR.strings.snd_error_expired), CIStatus.CISSndError(SndError.Expired()).statusInto)
    assertEquals(errorTitle to generalGetString(MR.strings.ci_status_other_error).format("NETWORK"), CIStatus.CISSndError(SndError.Other("NETWORK")).statusInto)
  }

  @Test
  fun quotaWarningShowsDeliveryWarningWithCapacityText() {
    assertEquals(warningTitle to generalGetString(MR.strings.snd_error_quota), CIStatus.SndWarning(SndError.Quota()).statusInto)
  }

  @Test
  fun serverWarningsIncludeServerErrorAndProxyHost() {
    assertEquals(
      warningTitle to generalGetString(MR.strings.snd_error_relay).format(generalGetString(MR.strings.srv_error_host)),
      CIStatus.SndWarning(SndError.Relay(SrvError.Host())).statusInto
    )
    assertEquals(
      warningTitle to generalGetString(MR.strings.snd_error_proxy).format("smp.example.com", generalGetString(MR.strings.srv_error_version)),
      CIStatus.SndWarning(SndError.Proxy("smp.example.com", SrvError.Version())).statusInto
    )
    assertEquals(
      warningTitle to generalGetString(MR.strings.snd_error_proxy_relay).format("smp.example.com", "TIMEOUT"),
      CIStatus.SndWarning(SndError.ProxyRelay("smp.example.com", SrvError.Other("TIMEOUT"))).statusInto
    )
  }

  @Test
  fun proxyErrorTextNamesTheForwardingServer() {
    val text = SndError.Proxy("smp.example.com", SrvError.Other("TIMEOUT")).errorInfo
    assertTrue(text.contains("smp.example.com"), text)
    assertTrue(text.contains("TIMEOUT"), text)
  }

  @Test
  fun invalidStatusShowsItsText() {
    assertEquals("Invalid status" to "bad status", CIStatus.Invalid("bad status").statusInto)
  }

  @Test
  fun groupMemberDeliveryStatusInfo() {
    assertNull(GroupSndStatus.New().statusInto, "New")
    assertNull(GroupSndStatus.Sent().statusInto, "Sent")
    assertNull(GroupSndStatus.Rcvd(MsgReceiptStatus.Ok).statusInto, "Rcvd")
    assertEquals(generalGetString(MR.strings.message_forwarded_title) to generalGetString(MR.strings.message_forwarded_desc), GroupSndStatus.Forwarded().statusInto)
    assertEquals(generalGetString(MR.strings.member_inactive_title) to generalGetString(MR.strings.member_inactive_desc), GroupSndStatus.Inactive().statusInto)
    assertEquals(errorTitle to generalGetString(MR.strings.snd_error_auth), GroupSndStatus.Error(SndError.Auth()).statusInto)
    assertEquals(warningTitle to generalGetString(MR.strings.snd_error_quota), GroupSndStatus.Warning(SndError.Quota()).statusInto)
  }

  @Test
  fun receivedStatusIsNotReplacedByAnOlderSndStatus() {
    val rcvd = CIStatus.SndRcvd(MsgReceiptStatus.Ok, SndCIStatusProgress.Complete)
    assertTrue(shouldKeepOldSndCIStatus(rcvd, CIStatus.SndSent(SndCIStatusProgress.Complete)), "rcvd then sent")
    assertTrue(shouldKeepOldSndCIStatus(rcvd, CIStatus.SndNew()), "rcvd then new")
    assertTrue(shouldKeepOldSndCIStatus(rcvd, CIStatus.SndWarning(SndError.Quota())), "rcvd then warning")
    assertFalse(shouldKeepOldSndCIStatus(rcvd, CIStatus.SndRcvd(MsgReceiptStatus.BadMsgHash, SndCIStatusProgress.Complete)), "rcvd then rcvd")
  }

  @Test
  fun sentStatusIsNotReplacedByNew() {
    assertTrue(shouldKeepOldSndCIStatus(CIStatus.SndSent(SndCIStatusProgress.Complete), CIStatus.SndNew()), "sent then new")
    assertTrue(shouldKeepOldSndCIStatus(CIStatus.SndErrorAuth(), CIStatus.SndNew()), "auth error then new")
    assertTrue(shouldKeepOldSndCIStatus(CIStatus.CISSndError(SndError.Expired()), CIStatus.SndNew()), "error then new")
    assertTrue(shouldKeepOldSndCIStatus(CIStatus.SndWarning(SndError.Quota()), CIStatus.SndNew()), "warning then new")
  }

  @Test
  fun forwardStatusTransitionsAreApplied() {
    assertFalse(shouldKeepOldSndCIStatus(CIStatus.SndNew(), CIStatus.SndSent(SndCIStatusProgress.Complete)), "new then sent")
    assertFalse(shouldKeepOldSndCIStatus(CIStatus.SndSent(SndCIStatusProgress.Complete), CIStatus.SndRcvd(MsgReceiptStatus.Ok, SndCIStatusProgress.Complete)), "sent then rcvd")
    assertFalse(shouldKeepOldSndCIStatus(CIStatus.SndWarning(SndError.Quota()), CIStatus.SndSent(SndCIStatusProgress.Complete)), "warning then sent")
    assertFalse(shouldKeepOldSndCIStatus(CIStatus.SndNew(), CIStatus.SndNew()), "new then new")
  }
}
