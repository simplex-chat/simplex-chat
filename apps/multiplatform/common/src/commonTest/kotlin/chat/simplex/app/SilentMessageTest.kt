package chat.simplex.app

import chat.simplex.common.model.*
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class SilentMessageTest {
  private fun rcvItem(priority: String): ChatItem {
    val meta = json.decodeFromString(
      CIMeta.serializer(),
      """{"itemId":1,"itemTs":"2026-11-13T10:00:00Z","itemText":"hi","itemStatus":{"type":"rcvNew"},"itemEdited":false,"userMention":false,$priority"hasLink":false,"deletable":false,"editable":false,"showGroupAsSender":false,"createdAt":"2026-11-13T10:00:00Z","updatedAt":"2026-11-13T10:00:00Z"}"""
    )
    return ChatItem(CIDirection.DirectRcv(), meta, CIContent.RcvMsgContent(MsgContent.MCText("hi")), reactions = listOf())
  }

  @Test
  fun silentItemShowsNoNotification() {
    assertFalse(rcvItem(""""priority":-1,""").showNotification)
  }

  @Test
  fun normalItemShowsNotification() {
    assertTrue(rcvItem("").showNotification)
  }
}
