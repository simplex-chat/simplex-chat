package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.ChatItem
import chat.simplex.common.model.GroupFeature
import chat.simplex.common.model.GroupInfo
import chat.simplex.common.model.GroupMemberRole
import chat.simplex.common.model.MemberCriteria
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.views.helpers.ModalViewId

fun groupInfoOf(name: String): GroupInfo =
  (chatModel.chats.value.first { it.chatInfo.chatViewName == name }.chatInfo as ChatInfo.Group).groupInfo

class GroupPreferencesScreen(private val s: UiScope) {
  init {
    s.poll(what = "the `Group preferences` screen") { s.seesText("Group preferences") && s.exists(Ui.GroupPrefs.MemberAdmission) }
    s.waitStable(Ui.GroupPrefs.MemberAdmission)
  }

  fun toggle(f: GroupFeature) = s.click(Ui.GroupPrefs.Toggle(f))

  fun role(f: GroupFeature, r: GroupMemberRole?) {
    s.click(Ui.GroupPrefs.Role(f))
    s.click(Ui.GroupPrefs.Role(f).option(r ?: "null"))
  }

  fun ttl(seconds: Int) {
    s.click(Ui.GroupPrefs.Ttl)
    s.click(Ui.GroupPrefs.Ttl.option(seconds))
  }

  fun reset() = s.click(Ui.GroupPrefs.Reset)

  fun save() = s.click(Ui.GroupPrefs.Save)

  fun admission(): MemberAdmissionScreen {
    s.click(Ui.GroupPrefs.MemberAdmission)
    return MemberAdmissionScreen(s)
  }

  fun backExpectingAlert(title: String) {
    s.goBack()
    s.poll(what = "the `$title` alert") { s.seesText(title) }
  }

  fun backClosing() {
    s.goBack()
    s.poll(what = "group preferences to close without an alert") { !s.exists(Ui.GroupPrefs.MemberAdmission) && !s.seesText("Save preferences?") }
  }
}

class MemberAdmissionScreen(private val s: UiScope) {
  init {
    s.poll(what = "the `Member admission` screen") { s.seesText("Member admission") && s.seesText("Review members before admitting (\"knocking\").") }
  }

  fun review(): List<String> = s.textsOf(Ui.MemberAdmission.Review)

  fun select(c: MemberCriteria?) {
    s.waitStable(Ui.MemberAdmission.Review)
    s.click(Ui.MemberAdmission.Review)
    s.click(Ui.MemberAdmission.Review.option(c ?: "null"))
  }

  fun save() = s.click(Ui.MemberAdmission.Save)
}

fun UiScope.enableReviewAll(info: GroupInfoScreen, group: String) {
  val adm = info.preferences().admission()
  poll(what = "`Review members` off before enabling review") { "off" in adm.review() }
  adm.select(MemberCriteria.All)
  adm.save()
  poll(what = "review of all members saved for #$group") { groupInfoOf(group).groupProfile.memberAdmission?.review == MemberCriteria.All }
  goBack()
  poll(what = "member admission to close to group preferences") { !seesText("Review members before admitting (\"knocking\").") && exists(Ui.GroupPrefs.MemberAdmission) }
  GroupPreferencesScreen(this).backClosing()
}

class MemberSupportScreen(private val s: UiScope) {
  init {
    s.poll(what = "the `Chats with members` screen") { s.seesText("Chats with members") && !s.exists(Ui.GroupInfo.MemberSupport) }
  }

  fun empty() = s.poll(what = "`No chats with members`") { s.seesText("No chats with members") }

  fun close() {
    s.goBack()
    s.poll(what = "`Chats with members` to close to group info") { s.exists(Ui.GroupInfo.MemberSupport) }
    s.waitStable(Ui.GroupInfo.MemberSupport)
  }

  fun rowTexts(name: String): List<String> {
    val id = chatModel.groupMembers.value.firstOrNull { it.memberProfile.displayName == name }?.groupMemberId ?: return emptyList()
    return if (s.exists(Ui.MemberSupport.Row(id))) s.textsOf(Ui.MemberSupport.Row(id)) else emptyList()
  }

  fun open(name: String): SupportChatScreen {
    s.click(Ui.MemberSupport.Row(memberId(name)))
    return SupportChatScreen(s).also { it.waitOpen() }
  }

  fun menuAccept(name: String) {
    val id = memberId(name)
    s.openContextMenu(Ui.MemberSupport.Row(id))
    s.click(Ui.MemberSupport.RowMenu(id, "accept"))
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

class SupportChatScreen(private val s: UiScope) {
  fun waitOpen() = s.poll(what = "the support chat") {
    chatModel.secondaryChatsContext.value != null && ModalManager.end.hasModalOpen(ModalViewId.SECONDARY_CHAT)
  }

  fun items(): List<ChatItem> = chatModel.secondaryChatsContext.value?.chatItems?.value.orEmpty()

  fun itemId(text: String): Long {
    var id: Long? = null
    s.poll(what = "support chat item `$text`") {
      id = items().lastOrNull { it.content.text == text }?.id
      id != null && s.exists(Ui.Chat.Item(id!!))
    }
    return id!!
  }

  fun send(text: String) {
    s.typeInto(Ui.Chat.ScopeMessage, text)
    s.poll(what = "the support chat send button to be enabled") { s.isEnabled(Ui.Chat.ScopeSend) }
    s.pressEnter(Ui.Chat.ScopeMessage)
    s.poll(what = "the support chat message field to clear after one Enter") { s.editableText(Ui.Chat.ScopeMessage).isNullOrEmpty() }
    s.hideKeyboard()
  }

  fun acceptVia(option: String) {
    s.click(Ui.Chat.PendingAccept)
    s.acceptAlert(option)
  }

  fun reject() = s.click(Ui.Chat.PendingReject)

  fun close() {
    s.goBack()
    s.poll(what = "the support chat to close") { chatModel.secondaryChatsContext.value == null && !ModalManager.end.hasModalOpen(ModalViewId.SECONDARY_CHAT) }
  }
}

fun UiScope.acceptAlert(option: String) {
  poll(what = "the `Accept member` alert with `Accept as member` and `Accept as observer`") {
    seesText("Accept member") && seesText("Member will join the group, accept member?") && seesText("Accept as member") && seesText("Accept as observer")
  }
  click(Ui.Alert.Option(Ui.AlertSource.Shared, option))
}

fun UiScope.pendingLinkJoin(app: App, joiner: CliPeer, group: String): ChatScreen {
  val me = chatModel.currentUser.value!!.displayName
  val chat = emptyGroup(app, group)
  val info = chat.groupInfo()
  info.chatsWithMembers().apply { empty() }.close()
  enableReviewAll(info, group)
  val link = info.createLink()
  info.close()
  joiner sends "/c $link"
  joiner expects "connection request sent!"
  joiner.eventuallyAll(
    lines = listOf(
      "#$group: $me accepted you to the group, pending review",
      "#$group: joining the group...",
      "#$group: you joined the group, connecting to group moderators for admission to group",
    ),
    timeoutMs = CONNECT_TIMEOUT_MS,
  )
  quiesce()
  return chat
}
