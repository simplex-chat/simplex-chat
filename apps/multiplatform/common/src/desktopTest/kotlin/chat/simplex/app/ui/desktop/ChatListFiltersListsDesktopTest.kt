package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.lists
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

class ChatListFiltersListsDesktopTest : UiTest() {
  @Test fun editListNameAndEmoji() = uiTest {
    val alice = app("alice")
    alice.chatList.row("Private notes")
    val lists = lists()
    lists.addList().apply {
      typeInto(Ui.TagEditor.Emoji, "🚀")
      typeName("work")
      save()
    }
    val work = lists.tag("work")
    check(work.chatTagEmoji == "🚀") { "list created with emoji ${work.chatTagEmoji}" }
    poll(what = "the work chip with the emoji") { lists.chipTexts("work").containsAll(listOf("🚀", "work")) }
    val id = work.chatTagId
    lists.menu("work", "edit")
    val editor = lists.editor()
    poll(what = "the editor with the list's emoji and name") { editor.emojiText() == "🚀" && editor.nameText() == "work" && editor.saveLabel() == "Save list" }
    editor.saveIgnored()
    replaceText(Ui.TagEditor.Emoji, "🏠")
    editor.replaceName("home")
    editor.save()
    poll(what = "the chip with the new emoji and name") {
      chatModel.userTags.value.single { it.chatTagId == id }.let { it.chatTagText == "home" && it.chatTagEmoji == "🏠" } &&
        textsOf(Ui.ChatList.UserTag(id)).containsAll(listOf("🏠", "home")) && "work" !in textsOf(Ui.ChatList.UserTag(id))
    }
  }
}
