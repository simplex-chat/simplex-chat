package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.common.ui.Ui
import org.junit.Test

class ContactsTest : UiTest() {
  @Test fun contactRequestMenuAccept() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = alice.userPicker().createAddress()
    bob sends "/c $address"; bob expects "connection request sent!"
    alice.chatList.request("bob")
    alice.chatList.rowMenu("bob", "accept")
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
  }

  @Test fun contactRequestMenuReject() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = alice.userPicker().createAddress()
    bob sends "/c $address"; bob expects "connection request sent!"
    alice.chatList.request("bob")
    alice.chatList.rowMenu("bob", "reject")
    alice.chatList.gone("bob")
    bob sends "/u"
    bob expects "user profile: bob"
  }

  @Test fun deleteContactAndNotify() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.open("bob").info()
    click(Ui.Info.DeleteContact)
    click(Ui.Alert.Option(Ui.AlertSource.Shared, "deleteContact"))
    click(Ui.Alert.Option(Ui.AlertSource.Shared, "deleteAndNotify"))
    bob.eventually("alice deleted contact with you", CONNECT_TIMEOUT_MS)
    alice.chatList.gone("bob")
  }

  @Test fun editProfileDisplayName() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.profile().rename("alice2")
    bob.eventually("contact alice changed to alice2", CONNECT_TIMEOUT_MS)
  }
}
