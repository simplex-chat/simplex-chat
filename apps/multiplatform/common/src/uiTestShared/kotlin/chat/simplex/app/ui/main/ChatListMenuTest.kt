package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.common.ui.Ui
import org.junit.Test

class ChatListMenuTest : UiTest() {
  @Test fun contactMenuMarkReadAndUnread() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice one"
    poll(what = "unread badge 1") { "1" in alice.chatList.rowTexts("bob") }
    alice.chatList.rowMenu("bob", "markRead")
    poll(what = "unread badge gone") { "1" !in alice.chatList.rowTexts("bob") && alice.chatList.chat("bob").chatStats.unreadCount == 0 }
    alice.chatList.rowMenu("bob", "markUnread")
    poll(what = "the chat marked unread") { alice.chatList.chat("bob").chatStats.unreadChat }
    alice.chatList.rowMenu("bob", "markRead")
    poll(what = "the unread mark cleared") { !alice.chatList.chat("bob").chatStats.unreadChat }
  }

  @Test fun contactMenuFavoriteUnfavorite() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.rowMenu("bob", "favorite")
    poll(what = "the favorite star on the row") { "Favorite" in alice.chatList.rowDescriptions("bob") }
    alice.chatList.rowMenu("bob", "unfavorite")
    poll(what = "the favorite star gone") { "Favorite" !in alice.chatList.rowDescriptions("bob") }
  }

  @Test fun contactMenuMuteUnmute() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.rowMenu("bob", "ntfs.None")
    poll(what = "the muted icon on the row") { "Notifications" in alice.chatList.rowDescriptions("bob") }
    alice.chatList.rowMenu("bob", "ntfs.All")
    poll(what = "the muted icon gone") { "Notifications" !in alice.chatList.rowDescriptions("bob") }
  }

  @Test fun contactMenuClearChat() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice to be cleared"
    poll(what = "the message preview on the row") { "to be cleared" in alice.chatList.rowTexts("bob") }
    alice.chatList.rowMenu("bob", "clear")
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the preview gone") { "to be cleared" !in alice.chatList.rowTexts("bob") }
  }
}
