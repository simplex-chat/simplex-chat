package chat.simplex.app

import chat.simplex.common.model.*
import kotlin.test.Test
import kotlin.test.assertEquals

class ReportReasonTest {
  @Test
  fun inappropriateContentReportIsSentAsContent() {
    val cmd = CC.ApiReportMessage(groupId = 1, chatItemId = 2, reportReason = ReportReason.Illegal, reportText = "Scam shop link")
    assertEquals("/_report #1 2 reason=content Scam shop link", cmd.cmdString)
  }

  @Test
  fun contentReportFromCoreIsInappropriateContent() {
    val mc = json.decodeFromString<MsgContent>("""{"type":"report","text":"Scam shop link","reason":"content"}""")
    assertEquals(ReportReason.Illegal, (mc as MsgContent.MCReport).reason)
  }
}
