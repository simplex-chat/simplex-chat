package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.chatListToolbar
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.GroupProfile
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.database.stopChatAsync
import kotlinx.coroutines.runBlocking
import org.junit.Test

class ChatListToolbarSearchTest : UiTest() {
  @Test fun chatListTitleAndProfileButton() = uiTest {
    val alice = app("alice")
    poll(what = "the title `Chats`") { "Chats" in chatListToolbar().titleTexts() }
    check(!exists(Ui.UserPicker.Sheet)) { "the user picker is open before the profile button is clicked" }
    alice.userPicker()
    poll(what = "the user picker with `Create SimpleX address`") { exists(Ui.UserPicker.Sheet) && textOf(Ui.UserPicker.Address) == "Create SimpleX address" }
  }

  @Test fun chatListScrollToTopOnTitleClick() = uiTest {
    app("alice")
    repeat(GROUPS) { i ->
      runBlocking { chatController.apiNewGroup(null, incognito = false, GroupProfile("group$i", "", null)) } ?: error("apiNewGroup failed")
    }
    restart()
    var ids = emptyList<String>()
    poll(what = "all chats in the model") { ids = chatModel.chats.value.map { it.id }; ids.size > GROUPS }
    val bar = chatListToolbar()
    waitFor(Ui.ChatList.Row(ids.first()))
    check(!bar.canScrollToTop()) { "the title is clickable while the list is at the top" }
    scrollTo(Ui.ChatList.Row(ids.last()))
    poll(what = "the title to become clickable with the list scrolled") { bar.canScrollToTop() }
    bar.scrollToTop()
    poll(what = "the list back at the top with the first chat shown") { !bar.canScrollToTop() && exists(Ui.ChatList.Row(ids.first())) }
  }

  @Test fun chatListUnreadDotOnProfileButtonForOtherUsers() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    alice.userPicker().profiles().apply { add("work"); close() }
    connect(bob)
    alice.userPicker().switchTo("alice")
    val bar = chatListToolbar()
    quiesce()
    check(!bar.profileBadge()) { "the profile button has a badge before bob writes" }
    bob sends "@work hello"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the badge on the profile button") { bar.profileBadge() }
    alice.userPicker().switchTo("work")
    alice.chatList.open("bob")
    poll(what = "bob's chat to be read") { alice.chatList.chat("bob").chatStats.unreadCount == 0 }
    backToChatList()
    alice.userPicker().switchTo("alice")
    poll(what = "the badge gone") { !bar.profileBadge() }
  }

  @Test fun chatListStoppedChatIndicator() = uiTest {
    app("alice")
    check(!exists(Ui.ChatList.Stopped)) { "the stopped indicator is shown while chat runs" }
    runBlocking { stopChatAsync(chatModel) }
    poll(what = "the `Chat is stopped` indicator") { exists(Ui.ChatList.Stopped) && "Chat is stopped" in descriptionsOf(Ui.ChatList.Stopped) }
    click(Ui.ChatList.Stopped)
    poll(what = "the chat stopped alert") { seesText("Chat is stopped") && seesText(STOPPED_ALERT_TEXT) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the alert to close") { !seesText(STOPPED_ALERT_TEXT) }
  }

  @Test fun subscriptionIndicatorConnected() = uiTest {
    app("alice")
    appPrefs.networkShowSubscriptionPercentage.set(true)
    val bar = chatListToolbar()
    poll(what = "`%` with no subscriptions") { bar.percentage() == "%" }
    runBlocking { chatController.apiAddContact(null, incognito = false) }.first ?: error("apiAddContact failed")
    poll(what = "`100%` with the invitation queue subscribed") { bar.percentage() == "100%" }
  }

  @Test fun subscriptionIndicatorPercentageToggle() = uiTest {
    app("alice")
    val bar = chatListToolbar()
    check(bar.percentage() == null) { "the percentage is shown by default" }
    bar.toggleShowPercentage()
    poll(what = "the percentage next to the indicator") { bar.percentage() == "%" && storedSetting("ShowSubscriptionPercentage") == "true" }
    bar.toggleShowPercentage()
    poll(what = "the percentage hidden") { bar.percentage() == null && storedSetting("ShowSubscriptionPercentage") == "false" }
  }

  private companion object {
    const val GROUPS = 30
    const val STOPPED_ALERT_TEXT = "You can start chat via app Settings / Database or by restarting the app."
  }
}
