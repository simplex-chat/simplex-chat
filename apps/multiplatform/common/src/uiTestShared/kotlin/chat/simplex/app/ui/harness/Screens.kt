package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.ChatItem
import chat.simplex.common.model.GroupMemberRole
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui

class App(private val s: UiScope) {
  val chatList = ChatListScreen(s)

  fun userPicker(): UserPickerScreen {
    s.click(Ui.ChatList.UserPicker)
    return UserPickerScreen(s)
  }

  fun profile(): ProfileScreen {
    s.click(Ui.ChatList.UserPicker)
    s.click(Ui.UserPicker.User(chatModel.currentUser.value!!.userId))
    return ProfileScreen(s)
  }

  fun appearance(): AppearanceScreen {
    s.click(Ui.ChatList.UserPicker)
    s.click(Ui.UserPicker.Settings)
    s.click(Ui.Settings.Appearance)
    return AppearanceScreen(s)
  }

  fun settings() {
    s.click(Ui.ChatList.UserPicker)
    s.click(Ui.UserPicker.Settings)
  }

  fun newChat(): NewChatScreen {
    s.click(Ui.ChatList.NewChat)
    return NewChatScreen(s)
  }
}

class ChatListScreen(private val s: UiScope) {
  fun chatId(name: String): String {
    var id: String? = null
    s.poll(what = "chat `$name` in the model") {
      id = chatModel.chats.value.firstOrNull { it.chatInfo.chatViewName == name }?.id
      id != null
    }
    return id!!
  }

  fun row(name: String): ChatListScreen {
    s.waitFor(Ui.ChatList.Row(chatId(name)))
    return this
  }

  fun request(name: String): ChatListScreen {
    row(name)
    s.poll(what = "the `Open to accept` request row for `$name`") { s.seesText("Open to accept", substring = true) }
    return this
  }

  fun rowMenu(name: String, action: String, label: String? = null) {
    val id = chatId(name)
    val item = Ui.ChatList.RowMenu(id, action)
    s.openContextMenu(Ui.ChatList.Row(id))
    s.poll(what = "row menu item `$action`" + (label?.let { " labelled `$it`" } ?: "") + " (model settings ${chatModel.chats.value.firstOrNull { it.id == id }?.chatInfo?.chatSettings})") {
      s.exists(item) && (label == null || label in s.textsOf(item))
    }
    s.click(item)
  }

  fun gone(name: String) {
    s.poll(what = "chat `$name` to disappear") { chatModel.chats.value.none { it.chatInfo.chatViewName == name } }
  }

  fun open(name: String): ChatScreen {
    s.click(Ui.ChatList.Row(chatId(name)))
    return ChatScreen(s)
  }

  fun search(text: String) = s.typeInto(Ui.ChatList.Search, text)

  fun shown(name: String): Boolean = s.exists(Ui.ChatList.Row(chatId(name)))

  fun shareTo(name: String) = s.click(Ui.Share.Row(chatId(name)))

  fun rowTexts(name: String): List<String> = s.textsOf(Ui.ChatList.Row(chatId(name)))

  fun rowDescriptions(name: String): List<String> = s.descriptionsOf(Ui.ChatList.Row(chatId(name)))

  fun chat(name: String) = chatModel.chats.value.first { it.chatInfo.chatViewName == name }
}

class NewChatScreen(private val s: UiScope) {
  fun createGroup(name: String) {
    s.click(Ui.NewChat.CreateGroup)
    s.typeInto(Ui.NewGroup.Name, name)
    s.click(Ui.NewGroup.Create)
  }

  fun invite(vararg names: String) {
    for (name in names) s.click(Ui.InviteMembers.Contact(contactId(name)))
    s.click(Ui.InviteMembers.Invite)
    s.poll(what = "the invite sheet to close") { !s.exists(Ui.InviteMembers.Invite) }
  }

  fun createOneTimeLink(): String {
    s.click(Ui.NewChat.CreateLink)
    var link: String? = null
    s.poll(timeoutMs = 20_000, what = "the one-time link") {
      link = s.editableText(Ui.Link.Text)?.takeIf { it.isNotBlank() }
      link != null
    }
    return link!!
  }

  private fun contactId(name: String): Long =
    chatModel.chats.value.firstNotNullOf { (it.chatInfo as? ChatInfo.Direct)?.contact?.takeIf { c -> c.displayName == name }?.contactId }
}

class UserPickerScreen(private val s: UiScope) {
  fun createAddress(): String {
    val a = address()
    return a.create().also { a.close() }
  }

  fun address(): AddressScreen {
    s.click(Ui.UserPicker.Address)
    return AddressScreen(s)
  }

  fun profiles(): ProfilesScreen {
    s.click(Ui.UserPicker.Profiles)
    return ProfilesScreen(s)
  }

  fun switchTo(name: String) {
    s.click(Ui.UserPicker.User(chatModel.users.first { it.user.displayName == name }.user.userId))
    s.poll(what = "`$name` to become the active user") { chatModel.currentUser.value?.displayName == name && !chatModel.switchingUsersAndHosts.value }
    s.quiesce()
  }
}

class ProfilesScreen(private val s: UiScope) {
  fun add(name: String) {
    s.click(Ui.Profiles.Add)
    s.typeInto(Ui.NewProfile.Name, name)
    s.click(Ui.NewProfile.Create)
    s.poll(what = "`$name` to become the active user and the new profile screen to close") {
      chatModel.currentUser.value?.displayName == name && !s.exists(Ui.NewProfile.Create)
    }
  }

  fun close() {
    s.escape()
    s.poll(what = "the profiles screen to close") { !s.exists(Ui.Profiles.Add) }
  }
}

class AddressScreen(private val s: UiScope) {
  fun create(): String {
    s.click(Ui.Address.Create)
    s.click(Ui.Address.Share)
    var link: String? = null
    s.poll(what = "the address on the clipboard") { link = s.clipboardText(); link != null }
    return link!!
  }

  fun close() {
    s.escape()
    s.poll(what = "the address screen to close") { !s.exists(Ui.Address.Share) }
  }
}

class AppearanceScreen(private val s: UiScope) {
  fun colorMode(mode: String) {
    s.click(Ui.Appearance.ColorMode)
    s.click(Ui.Appearance.ColorMode.option(mode))
  }

  fun colorModeText(): String? = s.textOf(Ui.Appearance.ColorMode)

  var fontScale: Float
    get() = s.sliderValue(Ui.Appearance.FontScale)
    set(v) = s.setSlider(Ui.Appearance.FontScale, v)

  var zoom: Float
    get() = s.sliderValue(Ui.Appearance.Zoom)
    set(v) = s.setSlider(Ui.Appearance.Zoom, v)
}

class ProfileScreen(private val s: UiScope) {
  fun rename(name: String) {
    s.replaceText(Ui.Profile.Name, name)
    s.poll(what = "save to be enabled") { s.isEnabled(Ui.Profile.Save) }
    s.click(Ui.Profile.Save)
  }
}

class ChatScreen(private val s: UiScope) {
  fun info() {
    s.click(Ui.Chat.Bar.part("title"))
  }

  fun contactInfo(): ContactInfoScreen {
    info()
    return ContactInfoScreen(s)
  }

  fun groupInfo(): GroupInfoScreen {
    info()
    return GroupInfoScreen(s)
  }

  fun itemId(text: String): Long {
    var id: Long? = null
    s.poll(what = "chat item `$text`") {
      id = chatModel.chatsContext.chatItems.value.lastOrNull { it.content.text == text }?.id
      id != null && s.exists(Ui.Chat.Item(id!!))
    }
    return id!!
  }

  fun item(id: Long): ChatItem? = chatModel.chatsContext.chatItems.value.firstOrNull { it.id == id }

  fun itemTexts(): List<String> = chatModel.chatsContext.chatItems.value.map { it.content.text }

  fun menu(text: String, action: String) {
    val id = itemId(text)
    s.rightClick(Ui.Chat.Item(id))
    s.click(Ui.Chat.ItemMenu(id, action))
  }

  fun react(text: String, emoji: String) {
    val id = itemId(text)
    s.rightClick(Ui.Chat.Item(id))
    s.click(Ui.Chat.ItemReaction(id, emoji))
  }

  fun reactionCount(text: String, emoji: String): String? =
    s.textsOf(Ui.Chat.ReactionChip(itemId(text), emoji)).firstOrNull { it.all(Char::isDigit) }

  fun forwardTo(text: String, chatName: String) {
    menu(text, "forward")
    ChatListScreen(s).shareTo(chatName)
    s.poll(what = "the send button to be enabled for forwarding") { s.isEnabled(Ui.Chat.Send) }
    s.click(Ui.Chat.Send)
  }

  fun openSearch() {
    s.click(Ui.Chat.BarMenu)
    s.click(Ui.Chat.BarMenuItem("search"))
  }

  fun search(text: String) {
    openSearch()
    s.typeInto(Ui.Chat.Bar.part("search"), text)
  }

  fun send(text: String) {
    s.typeInto(Ui.Chat.Message, text)
    enterWhenSendable()
  }

  fun replaceAndSend(text: String) {
    s.replaceText(Ui.Chat.Message, text)
    enterWhenSendable()
  }

  private fun enterWhenSendable() {
    s.poll(what = "the send button to be enabled") { s.isEnabled(Ui.Chat.Send) }
    s.pressEnter(Ui.Chat.Message)
    if (fieldClears(ENTER_RETRY_MS)) return
    if (!s.editableText(Ui.Chat.Message).isNullOrEmpty() && s.isEnabled(Ui.Chat.Send)) runCatching { s.pressEnter(Ui.Chat.Message) }
    s.poll(what = "the message field to clear after the second Enter") { s.editableText(Ui.Chat.Message).isNullOrEmpty() }
  }

  private fun fieldClears(timeoutMs: Long): Boolean =
    runCatching { s.poll(timeoutMs = timeoutMs, what = "the message field to clear", screenshot = false) { s.editableText(Ui.Chat.Message).isNullOrEmpty() } }.isSuccess

  private companion object {
    const val ENTER_RETRY_MS = 5_000L
  }
}

class ContactInfoScreen(private val s: UiScope) {
  fun securityCode(): String {
    s.click(Ui.Info.VerifyCode)
    return s.textOf(Ui.VerifyCode.Code)!!.filterNot(Char::isWhitespace)
  }

  fun markVerified() {
    s.click(Ui.Info.VerifyCode)
    s.click(Ui.VerifyCode.Mark)
    s.poll(what = "the security code screen to close") { !s.exists(Ui.VerifyCode.Mark) }
  }

  fun verifyRowText(): String? = s.textOf(Ui.Info.VerifyCode)
}

class GroupInfoScreen(private val s: UiScope) {
  fun member(name: String): MemberInfoScreen {
    var id: Long? = null
    s.poll(what = "member `$name` in the model") {
      id = chatModel.groupMembers.value.firstOrNull { it.memberProfile.displayName == name }?.groupMemberId
      id != null
    }
    s.scrollTo(Ui.GroupInfo.Member(id!!))
    s.click(Ui.GroupInfo.Member(id!!))
    return MemberInfoScreen(s)
  }
}

class MemberInfoScreen(private val s: UiScope) {
  fun changeRole(role: GroupMemberRole) {
    s.click(Ui.MemberInfo.Role)
    s.click(Ui.MemberInfo.Role.option(role))
    s.click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  }

  fun remove() {
    s.click(Ui.MemberInfo.Remove)
    s.click(Ui.Alert.Option(Ui.AlertSource.Shared, "remove"))
  }
}

fun UiScope.ownGroup(app: App, group: String, vararg members: CliPeer): ChatScreen {
  val me = chatModel.currentUser.value!!.displayName
  app.newChat().apply {
    createGroup(group)
    invite(*members.map { it.name }.toTypedArray())
  }
  for (m in members) {
    m.eventually("#$group: $me invites you to join the group as member")
    m.eventually("use /j $group to accept")
    m sends "/j $group"
    m.eventually("#$group: you joined the group", CONNECT_TIMEOUT_MS)
  }
  quiesce()
  return ChatScreen(this)
}
