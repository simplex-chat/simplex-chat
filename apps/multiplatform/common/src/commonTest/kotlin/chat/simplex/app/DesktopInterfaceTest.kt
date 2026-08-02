package chat.simplex.app

import chat.simplex.common.*
import chat.simplex.common.platform.*
import chat.simplex.common.model.NotificationPreviewMode
import chat.simplex.common.views.chat.*
import java.net.URI
import kotlin.test.*

class DesktopInterfaceTest {
  @AfterTest
  fun resetLayout() {
    DesktopLayoutState.resetSidebarWidth()
    DesktopLayoutState.sidebarCollapsed.value = false
  }

  @Test
  fun sidebarWidthIsClampedAndResettable() {
    DesktopLayoutState.setSidebarWidth(100f)
    assertEquals(240f, DesktopLayoutState.sidebarWidth.floatValue)
    DesktopLayoutState.setSidebarWidth(900f)
    assertEquals(480f, DesktopLayoutState.sidebarWidth.floatValue)
    DesktopLayoutState.resetSidebarWidth()
    assertEquals(320f, DesktopLayoutState.sidebarWidth.floatValue)
  }

  @Test
  fun desktopDensityOnlyExpandsSpacing() {
    val compact = DesktopChatDensity.Compact.tokens()
    val comfortable = DesktopChatDensity.Comfortable.tokens()
    val spacious = DesktopChatDensity.Spacious.tokens()
    assertTrue(compact.chatRowVerticalPadding < comfortable.chatRowVerticalPadding)
    assertTrue(comfortable.chatRowVerticalPadding < spacious.chatRowVerticalPadding)
    assertTrue(compact.messageVerticalPadding < comfortable.messageVerticalPadding)
    assertTrue(comfortable.composerVerticalPadding < spacious.composerVerticalPadding)
  }

  @Test
  fun rangeSelectionKeepsOrderAndSkipsUnselectableItems() {
    val items = listOf(1L to true, 2L to false, 3L to true, 4L to true)
    assertEquals(linkedSetOf(1L, 3L, 4L), desktopRangeSelection(items, 4L, 1L))
    assertTrue(desktopRangeSelection(items, 99L, 1L).isEmpty())
  }

  @Test
  fun attachmentReorderingAndPartialFailurePreserveOrder() {
    val attachments = listOf("one", "two", "three").map {
      PendingAttachment(it, URI("file:///$it"), it, PendingAttachmentKind.File)
    }
    val reordered = reorderPendingAttachments(attachments, 2, 0)
    assertEquals(listOf("three", "one", "two"), reordered.map { it.id })
    assertEquals(listOf("one", "two"), pendingAttachmentsAfterFailure(reordered, 1).map { it.id })
  }

  @Test
  fun notificationSuppressionRequiresExactActiveRoute() {
    val route = NotificationRoute(7, 9, "@42", 100)
    assertTrue(suppressDesktopNotification(true, 7, 9, "@42", route))
    assertFalse(suppressDesktopNotification(true, 7, 9, "@43", route))
    assertFalse(suppressDesktopNotification(false, 7, 9, "@42", route))
  }

  @Test
  fun notificationPrivacyModesExposeOnlyAllowedContent() {
    val message = desktopNotificationPreview(NotificationPreviewMode.MESSAGE, "Alice", "hello", "Somebody", "New message")
    assertEquals(DesktopNotificationPreview("Alice", "hello"), message)
    val contact = desktopNotificationPreview(NotificationPreviewMode.CONTACT, "Alice", "hello", "Somebody", "New message")
    assertEquals(DesktopNotificationPreview("Alice", "New message"), contact)
    val hidden = desktopNotificationPreview(NotificationPreviewMode.HIDDEN, "Alice", "hello", "Somebody", "New message")
    assertEquals(DesktopNotificationPreview("Somebody", "New message"), hidden)
  }

  @Test
  fun notificationIdentifiersAreStableAndRouteQueueWaitsUntilReady() {
    val route = NotificationRoute(7, null, "#team chat", 100)
    assertEquals("simplex.7.-1._team_chat.100", desktopNotificationIdentifier(route, 999))
    val queue = NotificationRouteQueue()
    queue.enqueue(route)
    assertTrue(queue.consumeIfReady(false).isEmpty())
    assertEquals(listOf(route), queue.consumeIfReady(true))
    assertTrue(queue.consumeIfReady(true).isEmpty())
  }
}
