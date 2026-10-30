package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import kotlinx.coroutines.runBlocking

class ChannelLinkScreen(private val s: UiScope, private val channel: String) {
  init {
    s.poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Channel link` screen of `$channel` with `Share link` and `Continue`") {
      s.seesText("Channel link") && s.exists(Ui.GroupLink.Share) && s.exists(Ui.GroupLink.Continue)
    }
  }

  fun share(): String {
    s.click(Ui.GroupLink.Share)
    var link: String? = null
    s.poll(what = "the channel link of `$channel` shared") { link = s.sharedText()?.takeIf { it.isNotBlank() }; link != null }
    return link!!
  }

  fun continueToChannel(): ChatScreen {
    s.click(Ui.GroupLink.Continue)
    s.poll(what = "the `Channel link` screen to close with `$channel` open") {
      !s.exists(Ui.GroupLink.Share) && chatModel.chatId.value != null && chatModel.chatId.value == channelChatId(channel)
    }
    return ChatScreen(s)
  }
}

fun channelChatId(name: String): String? =
  chatModel.chats.value.singleOrNull { (it.chatInfo as? ChatInfo.Group)?.groupInfo?.let { g -> g.useRelays && g.groupProfile.displayName == name } == true }?.id

fun UiScope.createChannel(app: App, name: String): ChannelLinkScreen {
  app.newChat()
  click(Ui.NewChat.CreateChannel)
  typeInto(Ui.NewChannel.Name, name)
  click(Ui.NewChannel.Create)
  return ChannelLinkScreen(this, name)
}

fun channelLinkViaApi(name: String): String {
  val groupId = groupInfoOf(name).groupId
  val link = runBlocking { chatController.apiGetGroupLink(null, groupId) } ?: error("apiGetGroupLink failed for $name")
  return link.connLinkContact.simplexChatUri(short = true)
}

fun CliPeer.joinChannel(channel: String, link: String, vararg relays: CliPeer) {
  this sends "/c $link"
  this expects "#$channel: connection started"
  eventuallyAll(
    relays.flatMap { listOf("#$channel: joining the group (connecting to relay ${it.name})...", "#$channel: you joined the group (connected to relay ${it.name})") },
    timeoutMs = CONNECT_TIMEOUT_MS
  )
  for (r in relays) {
    r.eventually("$name: accepting request to join group #$channel...", CONNECT_TIMEOUT_MS)
    r.eventually("#$channel: $name joined the group", CONNECT_TIMEOUT_MS)
  }
}

fun CliPeer.createChannel(channel: String, relay: CliPeer): String {
  this sends "/relays name=${relay.name} ${relay.relayAddress}"
  this expects "ok"
  this sends "/public group relays=1 #$channel"
  this expects "group #$channel is created"
  this expects "wait for selected relay(s) to join, then you can invite members via group link"
  eventually("  - relay id 1: active", CONNECT_TIMEOUT_MS)
  this expects "group link:"
  val link = nextLine()
  relay.eventually("#$channel: you joined the group as relay", CONNECT_TIMEOUT_MS)
  return link
}

fun UiScope.openPreparedChannelFromSearch(link: String, name: String): ChatScreen {
  enterLinkInSearch(link)
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the plan alert for channel `$name` with `Open channel`") {
    exists(planConfirm) && "Open channel" in textsOf(planConfirm) && seesText(name)
  }
  click(planConfirm)
  poll(what = "`$name`'s prepared channel open") {
    val id = channelChatId(name)
    id != null && id == chatModel.chatId.value && chatModel.chats.value.first { it.id == id }.chatInfo.nextConnectPrepared
  }
  return ChatScreen(this)
}

fun UiScope.joinPreparedChannel(name: String, owner: CliPeer, relay: CliPeer) {
  poll(what = "`Join channel` in place of the message field") { exists(Ui.Chat.Connect) && "Join channel" in textsOf(Ui.Chat.Connect) }
  click(Ui.Chat.Connect)
  val me = chatModel.currentUser.value!!.displayName
  owner.eventually("#$name: ${relay.name} introduced $me in the channel", CONNECT_TIMEOUT_MS)
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the message field in place of `Join channel` in `$name`") {
    !exists(Ui.Chat.Connect) && exists(Ui.Chat.Message) && channelChatId(name)?.let { id -> chatModel.chats.value.first { it.id == id }.chatInfo.nextConnectPrepared } == false
  }
  quiesce()
}
