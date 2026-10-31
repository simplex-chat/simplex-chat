package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.openRowMenu
import chat.simplex.common.model.CC
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertNotEquals
import org.junit.Test

class ChatListRequestsCardsCompatTest : UiTest() {
  @Test fun contactRequestIncognitoHiddenWithShortLinkData() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = alice.userPicker().createAddress()
    poll(what = "alice's address with short-link data") { chatModel.userAddress.value?.shortLinkDataSet == true }
    bob sends "/c $address"; bob expects "connection request sent!"
    alice.chatList.request("bob")
    val id = openRowMenu("bob")
    poll(what = "the request menu with `Accept` and `Reject` but no `Accept incognito`") {
      exists(Ui.ChatList.RowMenu(id, "accept")) && exists(Ui.ChatList.RowMenu(id, "reject")) &&
        !exists(Ui.ChatList.RowMenu(id, "acceptIncognito")) && !seesText("Accept incognito")
    }
    runBlocking { chatController.sendCmd(null, CC.Console("/sql chat UPDATE user_contact_links SET short_link_data_set = 0 WHERE group_id IS NULL")) }
    restart()
    poll(what = "alice's address without short-link data after the restart") { chatModel.userAddress.value?.shortLinkDataSet == false }
    alice.chatList.request("bob")
    openRowMenu("bob")
    poll(what = "the request menu with `Accept incognito`") {
      exists(Ui.ChatList.RowMenu(id, "accept")) && exists(Ui.ChatList.RowMenu(id, "reject")) &&
        exists(Ui.ChatList.RowMenu(id, "acceptIncognito")) && "Accept incognito" in textsOf(Ui.ChatList.RowMenu(id, "acceptIncognito"))
    }
    click(Ui.ChatList.RowMenu(id, "acceptIncognito"))
    val shared = bob.eventuallyMatching(Regex("(\\S+): contact is connected"), CONNECT_TIMEOUT_MS).groupValues[1]
    assertNotEquals("alice", shared)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob connected to alice's incognito profile") {
      (alice.chatList.chat("bob").chatInfo as? ChatInfo.Direct)?.contact?.let { it.contactConnIncognito && it.sndReady } == true
    }
  }
}
