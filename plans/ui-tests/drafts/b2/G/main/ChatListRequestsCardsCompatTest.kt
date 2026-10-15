package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.openRowMenu
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

open class ChatListRequestsCardsCompatTest : UiTest() {
  @Test fun contactRequestIncognitoHiddenWithShortLinkData() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = alice.userPicker().createAddress()
    poll(what = "alice's address with short-link data") { chatModel.userAddress.value?.shortLinkDataSet == true }
    bob sends "/c $address"; bob expects "connection request sent!"
    alice.chatList.request("bob")
    val id = openRowMenu(alice, "bob")
    poll(what = "the request menu with `Accept` and `Reject` but no `Accept incognito`") {
      exists(Ui.ChatList.RowMenu(id, "accept")) && exists(Ui.ChatList.RowMenu(id, "reject")) &&
        !exists(Ui.ChatList.RowMenu(id, "acceptIncognito")) && !seesText("Accept incognito")
    }
  }
}
