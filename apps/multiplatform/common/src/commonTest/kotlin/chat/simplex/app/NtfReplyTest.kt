package chat.simplex.app

import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.ChatType
import chat.simplex.common.model.NotificationPreviewMode
import chat.simplex.common.platform.chatIdToChatTypeAndId
import chat.simplex.common.platform.ntfReplyAllowed
import chat.simplex.common.platform.shouldOfferReply
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

class NtfReplyTest {
  @Test
  fun testChatIdParsing() {
    assertEquals(ChatType.Direct to 123L, chatIdToChatTypeAndId("@123"))
    assertEquals(ChatType.Group to 45L, chatIdToChatTypeAndId("#45"))
    assertEquals(ChatType.Local to 7L, chatIdToChatTypeAndId("*7"))
    assertNull(chatIdToChatTypeAndId("@"))      // no numeric id
    assertNull(chatIdToChatTypeAndId("@abc"))   // non-numeric id
    assertNull(chatIdToChatTypeAndId("123"))    // no type prefix
  }

  @Test
  fun testNtfReplyAllowed() {
    // lock off: follows quickReply flag
    assertTrue(ntfReplyAllowed(quickReplyEnabled = true, appLockEnabled = false, allowWhenLocked = false))
    assertFalse(ntfReplyAllowed(quickReplyEnabled = false, appLockEnabled = false, allowWhenLocked = false))
    // lock on: requires opt-in
    assertFalse(ntfReplyAllowed(quickReplyEnabled = true, appLockEnabled = true, allowWhenLocked = false))
    assertTrue(ntfReplyAllowed(quickReplyEnabled = true, appLockEnabled = true, allowWhenLocked = true))
    // master flag off always wins
    assertFalse(ntfReplyAllowed(quickReplyEnabled = false, appLockEnabled = true, allowWhenLocked = true))
  }

  @Test
  fun testShouldOfferReply() {
    val direct = ChatInfo.Direct.sampleData       // writable direct chat fixture
    val group = ChatInfo.Group.sampleData         // writable group chat fixture
    val request = ChatInfo.ContactRequest.sampleData
    val msg = NotificationPreviewMode.MESSAGE.name
    val hidden = NotificationPreviewMode.HIDDEN.name

    assertTrue(shouldOfferReply(replyAllowed = true, previewModeName = msg, cInfo = direct))
    assertTrue(shouldOfferReply(replyAllowed = true, previewModeName = msg, cInfo = group))
    assertFalse(shouldOfferReply(replyAllowed = false, previewModeName = msg, cInfo = direct))   // gate: setting
    assertFalse(shouldOfferReply(replyAllowed = true, previewModeName = hidden, cInfo = direct)) // gate: preview
    assertFalse(shouldOfferReply(replyAllowed = true, previewModeName = msg, cInfo = request))   // gate: type
  }
}
