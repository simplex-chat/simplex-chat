package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatTag
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.chatlist.ActiveFilter

class UserListsScreen(private val s: UiScope) {
  fun tag(name: String): ChatTag {
    var t: ChatTag? = null
    s.poll(what = "list `$name` in the model") {
      t = chatModel.userTags.value.firstOrNull { it.chatTagText == name }
      t != null
    }
    return t!!
  }

  fun chip(name: String) = Ui.ChatList.UserTag(tag(name).chatTagId)

  fun chipTexts(name: String): List<String> = s.textsOf(chip(name))

  fun activeTagId(): Long? = (chatModel.activeChatTagFilter.value as? ActiveFilter.UserTag)?.tag?.chatTagId

  fun tagsOf(chat: String): List<Long> = ChatListScreen(s).chat(chat).chatInfo.chatTags.orEmpty()

  fun editor() = ListEditorScreen(s)

  fun addList(): ListEditorScreen {
    s.click(Ui.ChatList.AddList)
    return editor()
  }

  fun create(name: String): ChatTag {
    addList().apply { typeName(name); save() }
    return tag(name)
  }

  fun menu(name: String, action: String) {
    val id = tag(name).chatTagId
    s.openContextMenu(Ui.ChatList.UserTag(id))
    s.click(Ui.ChatList.UserTagMenu(id, action))
  }

  fun filter(name: String) {
    val id = tag(name).chatTagId
    s.click(Ui.ChatList.UserTag(id))
    s.poll(what = "the `$name` list filter to be active") { activeTagId() == id }
  }

  fun assign(chat: String, list: String) {
    val id = tag(list).chatTagId
    ChatListScreen(s).rowMenu(chat, "tagList")
    s.click(Ui.TagList.Tag(id))
    s.poll(what = "`$chat` in list `$list` and the list sheet closed") { tagsOf(chat) == listOf(id) && !s.exists(Ui.TagList.Tag(id)) }
  }
}

fun UiScope.lists() = UserListsScreen(this)

class ListEditorScreen(private val s: UiScope) {
  fun shown(): Boolean = s.exists(Ui.TagEditor.Name)

  fun typeName(text: String) = s.typeInto(Ui.TagEditor.Name, text)

  fun replaceName(text: String) = s.replaceText(Ui.TagEditor.Name, text)

  fun nameText(): String? = s.editableText(Ui.TagEditor.Name)

  fun emojiText(): String? = s.editableText(Ui.TagEditor.Emoji) ?: s.textsOf(Ui.TagEditor.Emoji).firstOrNull()

  fun saveLabel(): String? = s.textOf(Ui.TagEditor.Save)

  fun save() {
    s.click(Ui.TagEditor.Save)
    s.poll(what = "the list editor to close") { !shown() }
  }

  fun saveIgnored() {
    s.click(Ui.TagEditor.Save)
    s.settle()
    s.quiesce()
    check(shown()) { "the list editor closed although its save button is disabled" }
  }
}
