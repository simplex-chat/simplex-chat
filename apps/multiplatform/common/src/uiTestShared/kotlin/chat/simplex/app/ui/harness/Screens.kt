package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.ChatItem
import chat.simplex.common.model.GroupMemberRole
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiKey
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.ModalManager

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
    val id = chatId(name)
    s.poll(what = "the `Open to accept` request row for `$name`") { s.textsOf(Ui.ChatList.Row(id)).any { "Open to accept" in it } }
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
    val id = chatModel.chats.value.firstOrNull { it.chatInfo.chatViewName == name }?.id
    s.poll(what = "chat `$name` to disappear from the model and the list") {
      chatModel.chats.value.none { it.chatInfo.chatViewName == name } && (id == null || !s.exists(Ui.ChatList.Row(id)))
    }
  }

  fun open(name: String): ChatScreen {
    val id = chatId(name)
    s.quiesce()
    s.click(Ui.ChatList.Row(id))
    s.poll(what = "chat `$name` to open, or an alert instead") {
      chatModel.chatId.value == id || AlertManager.shared.hasAlertsShown() || AlertManager.privacySensitive.hasAlertsShown()
    }
    return ChatScreen(s)
  }

  fun search(text: String) = s.typeInto(Ui.ChatList.Search, text)

  fun shown(name: String): Boolean = s.exists(Ui.ChatList.Row(chatId(name)))

  fun shareTo(name: String) = s.click(Ui.Share.Row(chatId(name)))

  fun shareSearch(text: String) {
    s.click(described("Search"))
    s.typeInto(Ui.Share.Bar.part("search"), text)
  }

  fun shareShown(name: String): Boolean = s.exists(Ui.Share.Row(chatId(name)))

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
    s.poll(what = "`$name` to become the active user and the user picker to close") {
      chatModel.currentUser.value?.displayName == name && !chatModel.switchingUsersAndHosts.value && !s.exists(Ui.UserPicker.Sheet)
    }
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
    s.poll(what = "the address shared") { link = s.sharedText(); link != null }
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

  fun darkModeColors(theme: String) {
    s.click(Ui.Appearance.DarkModeColors)
    s.click(Ui.Appearance.DarkModeColors.option(theme))
  }

  fun darkModeColorsText(): String? = s.textOf(Ui.Appearance.DarkModeColors)

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
    var revealed = false
    s.poll(what = "chat item `$text`") {
      id = chatModel.chatsContext.chatItems.value.lastOrNull { it.content.text == text }?.id
      val shown = id != null && s.exists(Ui.Chat.Item(id!!))
      if (id != null && !shown && !revealed && s.exists(Ui.Chat.ScrollToBottom)) {
        revealed = true
        s.click(Ui.Chat.ScrollToBottom)
      }
      shown
    }
    return id!!
  }

  fun item(id: Long): ChatItem? = chatModel.chatsContext.chatItems.value.firstOrNull { it.id == id }

  fun scrollToBottom() {
    s.settle()
    if (!s.exists(Ui.Chat.ScrollToBottom)) return
    s.click(Ui.Chat.ScrollToBottom)
    s.poll(what = "the chat scrolled to the bottom") { !s.exists(Ui.Chat.ScrollToBottom) }
  }

  fun itemTexts(): List<String> = chatModel.chatsContext.chatItems.value.map { it.content.text }

  fun menu(text: String, action: String) {
    val id = itemId(text)
    s.rightClick(Ui.Chat.Item(id))
    s.click(Ui.Chat.ItemMenu(id, action))
  }

  fun select(text: String) {
    menu(text, "select")
    s.waitFor(Ui.Chat.Selection.part("title"))
  }

  fun toggle(id: Long) = s.click(Ui.Chat.Item(id))

  fun selectionTitle(): String? = Ui.Chat.Selection.part("title").let { k -> if (s.exists(k)) s.textsOf(k).firstOrNull() else null }

  fun itemInfo(text: String): ItemInfoScreen {
    menu(text, "info")
    s.poll(what = "the item info with `Sent message` and `History`") { s.seesText("Sent message") && s.seesText("History") }
    return ItemInfoScreen(s)
  }

  fun receivedItemInfo(text: String): ItemInfoScreen {
    menu(text, "info")
    s.poll(what = "the item info with `Received message`") { s.seesText("Received message") }
    return ItemInfoScreen(s)
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

  fun itemId(what: String, match: (ChatItem) -> Boolean): Long {
    var id: Long? = null
    var revealed = false
    s.poll(timeoutMs = CONNECT_TIMEOUT_MS, what = what) {
      val found = chatModel.chatsContext.chatItems.value.lastOrNull(match)?.id
      id = found
      val shown = found != null && s.exists(Ui.Chat.Item(found))
      if (found != null && !shown && !revealed && s.exists(Ui.Chat.ScrollToBottom)) {
        revealed = true
        s.click(Ui.Chat.ScrollToBottom)
      }
      shown
    }
    return id!!
  }

  fun forwardItemTo(id: Long, chatName: String) {
    s.rightClick(Ui.Chat.Item(id))
    s.click(Ui.Chat.ItemMenu(id, "forward"))
    ChatListScreen(s).shareTo(chatName)
    s.poll(what = "the send button to be enabled for forwarding") { s.isEnabled(Ui.Chat.Send) }
    s.click(Ui.Chat.Send)
  }

  fun forwardSelected() = s.click(Ui.Chat.Selection.part("forward"))

  fun sendComposed() {
    s.click(Ui.Chat.Send)
    s.poll(what = "the message field to clear after Send") { s.editableText(Ui.Chat.Message).isNullOrEmpty() }
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
    s.poll(what = "the message field to clear after one Enter") { s.editableText(Ui.Chat.Message).isNullOrEmpty() }
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
    val id = memberId(name)
    s.scrollTo(Ui.GroupInfo.Member(id))
    s.click(Ui.GroupInfo.Member(id))
    return MemberInfoScreen(s)
  }

  fun memberTexts(name: String): List<String> {
    val id = chatModel.groupMembers.value.firstOrNull { it.memberProfile.displayName == name }?.groupMemberId ?: return emptyList()
    s.scrollTo(Ui.GroupInfo.Member(id))
    return s.textsOf(Ui.GroupInfo.Member(id))
  }

  fun ownMemberTexts(): List<String> {
    val id = (chatModel.chats.value.first { it.id == chatModel.chatId.value }.chatInfo as ChatInfo.Group).groupInfo.membership.groupMemberId
    s.scrollTo(Ui.GroupInfo.Member(id))
    return s.textsOf(Ui.GroupInfo.Member(id))
  }

  fun createLink(): String {
    s.click(Ui.GroupInfo.CreateLink)
    s.waitFor(Ui.GroupLink.Share, CONNECT_TIMEOUT_MS)
    s.click(Ui.GroupLink.Share)
    var link: String? = null
    s.poll(what = "the group link shared") { link = s.sharedText()?.takeIf { it.isNotBlank() }; link != null }
    s.escape()
    s.poll(what = "the group link screen to close") { !s.exists(Ui.GroupLink.Share) }
    return link!!
  }

  fun preferences(): GroupPreferencesScreen {
    s.click(Ui.GroupInfo.Preferences)
    return GroupPreferencesScreen(s)
  }

  fun chatsWithMembers(): MemberSupportScreen {
    s.click(Ui.GroupInfo.MemberSupport)
    return MemberSupportScreen(s)
  }

  fun reportsTexts(): List<String> = s.textsOf(Ui.GroupInfo.Reports)

  fun reports(): MemberReportsScreen {
    s.click(Ui.GroupInfo.Reports)
    return MemberReportsScreen(s)
  }

  fun close() {
    s.escape()
    s.poll(what = "group info to close") { !ModalManager.end.hasModalsOpen() }
  }

  private fun memberId(name: String): Long {
    var id: Long? = null
    s.poll(what = "member `$name` in the model") {
      id = chatModel.groupMembers.value.firstOrNull { it.memberProfile.displayName == name }?.groupMemberId
      id != null
    }
    return id!!
  }
}

class MemberInfoScreen(private val s: UiScope) {
  fun blockForAll() {
    s.click(Ui.MemberInfo.BlockForAll)
    s.poll(what = "the `Block member for all?` alert with `Block for all`") { s.seesText("Block member for all?") && "Block for all" in s.textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
    s.click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  }

  fun changeRole(role: GroupMemberRole) {
    s.click(Ui.MemberInfo.Role)
    s.click(Ui.MemberInfo.Role.option(role))
    s.poll(what = "the `Change role?` alert with `Change`") { s.seesText("Change role?") && "Change" in s.textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
    s.click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  }

  fun remove() {
    s.click(Ui.MemberInfo.Remove)
    s.click(Ui.Alert.Option(Ui.AlertSource.Shared, "remove"))
  }

  fun removeWithMessages() {
    s.click(Ui.MemberInfo.Remove)
    val option = Ui.Alert.Option(Ui.AlertSource.Shared, "removeWithMessages")
    s.poll(what = "the `Remove member?` alert with `Remove and delete messages`") {
      s.seesText("Remove member?") && s.exists(option) && "Remove and delete messages" in s.textsOf(option)
    }
    s.click(option)
  }

  fun pickRole(role: GroupMemberRole) {
    s.click(Ui.MemberInfo.Role)
    s.click(Ui.MemberInfo.Role.option(role))
    s.poll(what = "the `Change role?` alert with `Change`") { s.seesText("Change role?") && "Change" in s.textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
  }

  fun block(name: String) = confirm(Ui.MemberInfo.Block, "Block member?", "All new messages from $name will be hidden!", "Block")

  fun unblock(name: String) = confirm(Ui.MemberInfo.Unblock, "Unblock member?", "Messages from $name will be shown!", "Unblock")

  fun unblockForAll(name: String) = confirm(Ui.MemberInfo.UnblockForAll, "Unblock member for all?", "Messages from $name will be shown!", "Unblock for all")

  private fun confirm(button: UiKey, title: String, text: String, action: String) {
    s.click(button)
    s.poll(what = "the `$title` alert saying `$text` with `$action`") {
      s.seesText(title) && s.seesText(text) && action in s.textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    s.click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  }

  fun message() = s.click(Ui.MemberInfo.Message)
}

fun UiScope.emptyGroup(app: App, group: String): ChatScreen {
  app.newChat().createGroup(group)
  val id = app.chatList.chatId(group)
  poll(what = "the invite members sheet over `$group`") { chatModel.chatId.value == id && ModalManager.end.hasModalsOpen() && seesText("Invite members") }
  escape()
  poll(what = "the invite members sheet to close") { !ModalManager.end.hasModalsOpen() && !seesText("Invite members") }
  backToChatList()
  return app.chatList.open(group)
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
