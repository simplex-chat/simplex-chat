package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.AddressScreen
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.addressPlanOk
import chat.simplex.app.ui.harness.addressSettings
import chat.simplex.app.ui.harness.addressShared
import chat.simplex.app.ui.harness.awaitAddressGone
import chat.simplex.app.ui.harness.awaitAddressScreenAfterSettings
import chat.simplex.app.ui.harness.awaitAppIdle
import chat.simplex.app.ui.harness.closeTopScreen
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.labelled
import chat.simplex.app.ui.harness.leaveAddressSettingsExpectingSaveAlert
import chat.simplex.app.ui.harness.pickerRowOpens
import chat.simplex.app.ui.harness.savedAddressSettings
import chat.simplex.app.ui.harness.shareAddress
import chat.simplex.common.model.BusinessChatType
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle
import chat.simplex.common.views.helpers.AlertManager
import org.junit.Test

class AddressTest : UiTest() {
  @Test fun addressDelete() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob)
    alice.chatList.row("bob")
    alice.userPicker().address()
    click(Ui.Address.Create)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Share address with SimpleX contacts?` alert with `Share`") {
      seesText("Share address with SimpleX contacts?") && "Share" in textsOf(CONFIRM)
    }
    click(CONFIRM)
    bob.eventually("alice set new contact address, use /info alice to view", CONNECT_TIMEOUT_MS)
    val address = shareAddress()
    check(cath.addressPlanOk(address)) { "cath's plan rejects the address before it is deleted" }
    click(Ui.Address.Delete)
    poll(what = "the `Delete address?` alert saying contacts stay connected and get a profile update") {
      seesText("Delete address?") && seesText("All your contacts will remain connected. Profile update will be sent to your contacts.") && "Delete" in textsOf(CONFIRM)
    }
    click(CONFIRM)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the address screen back to `Create SimpleX address`") {
      chatModel.userAddress.value == null && exists(Ui.Address.Create) && !exists(Ui.Address.Delete)
    }
    bob.eventually("alice removed contact address", CONNECT_TIMEOUT_MS)
    cath.awaitAddressGone(address)
    cath sends "/c $address"
    cath.nextLine(CONNECT_TIMEOUT_MS).let { check("error: connection authorization failed" in it) { "cath's `/c` via the deleted address printed `$it`" } }
  }

  @Test fun addressAutoAcceptWelcomeMessage() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath"); val dave = cli("dave")
    val address = alice.userPicker().createAddress()
    bob sends "/c $address"
    bob expects "connection request sent!"
    alice.chatList.request("bob")
    alice.userPicker().address()
    val settings = addressSettings()
    settings.toggleAutoAccept()
    poll(what = "auto-accept saved") { savedAddressSettings()?.autoAccept != null }
    settings.welcome("hello!")
    settings.save()
    poll(what = "the welcome message saved") { savedAddressSettings()?.autoReply?.text == "hello!" }
    cath sends "/c $address"
    cath expects "connection request sent!"
    cath.eventuallyAll(listOf("alice: contact is connected"), messages = listOf("alice> hello!"), timeoutMs = CONNECT_TIMEOUT_MS)
    settings.toggleAutoAccept()
    poll(what = "auto-accept off saved with no welcome message") { savedAddressSettings()?.let { it.autoAccept == null && it.autoReply == null } == true }
    settings.close()
    AddressScreen(this).close()
    dave sends "/c $address"
    dave expects "connection request sent!"
    alice.chatList.request("dave")
    alice.chatList.request("bob")
    poll(what = "cath connected as a contact without a request") {
      (alice.chatList.chat("cath").chatInfo as? ChatInfo.Direct)?.contact?.let { it.sndReady && !it.nextAcceptContactRequest } == true
    }
  }

  @Test fun addressBusinessToggleAutoEnablesAutoAccept() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val address = alice.userPicker().address().create()
    waitFor(Ui.Address.Business)
    check(!seesText(TEAM_FOOTER) && savedAddressSettings()?.let { !it.businessAddress && it.autoAccept == null } == true) { "a new address is a business address or auto-accepts" }
    click(Ui.Address.Business)
    poll(what = "the business footer shown and business with auto-accept saved") {
      seesText(TEAM_FOOTER) && savedAddressSettings()?.let { it.businessAddress && it.autoAccept != null } == true
    }
    val settings = addressSettings()
    check(!settings.autoAcceptEnabled()) { "`Auto-accept` can be turned off for a business address" }
    settings.close()
    bob sends "/c $address"
    bob expects "connection request sent!"
    bob.eventually("#alice: joining the group...", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "a customer business chat with bob") {
      chatModel.chats.value.any { (it.chatInfo as? ChatInfo.Group)?.groupInfo?.businessChat?.chatType == BusinessChatType.Customer && it.chatInfo.chatViewName == "bob" }
    }
    click(Ui.Address.Business)
    poll(what = "the business footer gone and a plain auto-accepting address saved") {
      !seesText(TEAM_FOOTER) && savedAddressSettings()?.let { !it.businessAddress && it.autoAccept != null } == true
    }
    cath sends "/c $address"
    cath expects "connection request sent!"
    cath.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "cath connected as a direct contact") {
      chatModel.chats.value.any { (it.chatInfo as? ChatInfo.Direct)?.contact?.let { c -> c.displayName == "cath" && c.sndReady } == true }
    }
  }

  @Test fun addressScreenNoAddressShowsCreate() = uiTest {
    val alice = app("alice")
    alice.userPicker().address()
    poll(what = "`For social media` › `Create SimpleX address`, `Or to share privately` › `Create 1-time link`, and `SimpleX address or 1-time link?`") {
      seesText("For social media") && labelled(Ui.Address.Create, "Create SimpleX address") &&
        seesText("Or to share privately") && labelled(Ui.Address.CreateLink, "Create 1-time link") &&
        labelled(Ui.Address.LearnMore, "SimpleX address or 1-time link?") && !exists(Ui.Address.Share) && !exists(Ui.Address.Delete)
    }
    click(Ui.Address.LearnMore)
    poll(what = "`Address or 1-time link?` with its three sections") {
      seesText("Address or 1-time link?") && seesText("Share address publicly") && seesText("Share 1-time link with a friend") &&
        seesText("Connection security") && !exists(Ui.Address.Create)
    }
    closeTopScreen("`Address or 1-time link?`") { !seesText("Address or 1-time link?") && exists(Ui.Address.Create) }
    click(Ui.Address.CreateLink)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "New chat on the 1-time link with the link created") {
      seesText("Share this 1-time invite link") && !editableText(Ui.Link.Text).isNullOrBlank() && chatModel.showingInvitation.value != null &&
        !exists(Ui.Address.CreateLink)
    }
    check(chatModel.userAddress.value == null) { "`Create 1-time link` created a SimpleX address" }
  }

  @Test fun createAddressNoContactsSharesAutomatically() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    alice.userPicker()
    poll(what = "the user picker row `Create SimpleX address`") { labelled(Ui.UserPicker.Address, "Create SimpleX address") }
    click(Ui.UserPicker.Address)
    click(Ui.Address.Create)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the address created and shared with no alert") {
      chatModel.userAddress.value != null && exists(Ui.Address.Share) && addressShared() && UiIdle.inFlight().isEmpty()
    }
    check(!AlertManager.shared.hasAlertsShown() && !seesText("Share address with SimpleX contacts?")) { "an alert asked to share the address with no contacts" }
    AddressScreen(this).close()
    alice.userPicker()
    poll(what = "the user picker row `Your SimpleX address`") { labelled(Ui.UserPicker.Address, "Your SimpleX address") && !labelled(Ui.UserPicker.Address, "Create SimpleX address") }
    click(Ui.UserPicker.Address)
    poll(what = "the address screen for the existing address") { exists(Ui.Address.Share) && exists(Ui.Address.Delete) && !exists(Ui.Address.Create) }
    connect(bob)
    bob sends "/i @alice"
    bob.eventuallyMatching(Regex("contact address: \\S+"))
  }

  @Test fun addressSettingsShareWithContactsOnOff() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob")
    alice.userPicker().address()
    click(Ui.Address.Create)
    awaitShareAlert(CONNECT_TIMEOUT_MS)
    click(DISMISS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the address created and not shared after `Cancel`") {
      chatModel.userAddress.value != null && exists(Ui.Address.Settings) && !AlertManager.shared.hasAlertsShown() && UiIdle.inFlight().isEmpty()
    }
    check(!addressShared()) { "`Cancel` on `Share address with SimpleX contacts?` shared the address" }
    addressSettings()

    click(Ui.Address.ShareWithContacts)
    awaitShareAlert()
    click(DISMISS)
    poll(what = "the share alert closed") { !AlertManager.shared.hasAlertsShown() }
    awaitAppIdle("`Cancel` on the share alert")
    check(!addressShared()) { "`Cancel` on the share alert shared the address" }
    click(Ui.Address.ShareWithContacts)
    awaitShareAlert()
    click(CONFIRM)
    bob.eventually("alice set new contact address, use /info alice to view", CONNECT_TIMEOUT_MS)
    poll(what = "the address shared") { addressShared() }

    click(Ui.Address.ShareWithContacts)
    awaitStopSharingAlert()
    click(DISMISS)
    poll(what = "the stop-sharing alert closed") { !AlertManager.shared.hasAlertsShown() }
    awaitAppIdle("`Cancel` on the stop-sharing alert")
    check(addressShared()) { "`Cancel` on `Stop sharing address?` stopped sharing" }
    click(Ui.Address.ShareWithContacts)
    awaitStopSharingAlert()
    click(CONFIRM)
    bob.eventually("alice removed contact address", CONNECT_TIMEOUT_MS)
    poll(what = "the address no longer shared") { !addressShared() }
  }

  @Test fun addressShareViaChat() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = alice.userPicker().createAddress()
    connect(bob)
    val bobId = alice.chatList.chatId("bob")
    pickerRowOpens(alice, Ui.UserPicker.Address, "the address screen") { exists(Ui.Address.ShareViaChat) }
    click(Ui.Address.ShareViaChat)
    poll(what = "the share list with bob and without the notes folder") {
      alice.chatList.shareShown("bob") && !alice.chatList.shareShown("Private notes") && !exists(Ui.Address.ShareViaChat)
    }
    alice.chatList.shareTo("bob")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's chat with the address card ready to send and no typed text") {
      chatModel.chatId.value == bobId && editableText(Ui.Chat.Message).isNullOrEmpty() && isEnabled(Ui.Chat.Send)
    }
    click(Ui.Chat.Send)
    bob.eventuallyMessage("alice> contact address of @alice (signed):", CONNECT_TIMEOUT_MS)
    val link = bob.nextLine().trim()
    check(link == address) { "bob got the link `$link`, not alice's address `$address`" }
  }

  @Test fun addressSettingsUnsavedChangesAlertExitAndSave() = uiTest {
    val alice = app("alice")
    alice.userPicker().address().create()
    val first = addressSettings()
    poll(what = "an empty welcome message") { editableText(Ui.Address.WelcomeMessage).isNullOrEmpty() }
    awaitAppIdle("creating and sharing the address")
    val saved = chatModel.userAddress.value
    click(Ui.Address.SaveSettings)
    awaitAppIdle("`Save` with unchanged settings")
    check(chatModel.userAddress.value === saved) { "`Save` saved unchanged address settings" }

    first.welcome("first")
    leaveAddressSettingsExpectingSaveAlert()
    click(DISMISS)
    awaitAddressScreenAfterSettings()
    check(chatModel.userAddress.value === saved && savedAddressSettings()?.autoReply == null) { "`Exit without saving` saved the welcome message" }

    val settings = addressSettings()
    poll(what = "the welcome message still empty after `Exit without saving`") { editableText(Ui.Address.WelcomeMessage).isNullOrEmpty() }
    settings.welcome("second")
    leaveAddressSettingsExpectingSaveAlert()
    click(CONFIRM)
    awaitAddressScreenAfterSettings()
    poll(what = "the welcome message `second` saved") { savedAddressSettings()?.autoReply?.text == "second" }

    val again = addressSettings()
    poll(what = "the saved welcome message shown") { editableText(Ui.Address.WelcomeMessage) == "second" }
    again.replaceWelcome("second again")
    again.save()
    poll(what = "the welcome message `second again` saved") { savedAddressSettings()?.autoReply?.text == "second again" }
    again.close()
  }

  private fun UiScope.awaitShareAlert(timeoutMs: Long = 10_000) =
    poll(timeoutMs = timeoutMs, what = "the `Share address with SimpleX contacts?` alert with `Share` and `Cancel`") {
      seesText("Share address with SimpleX contacts?") && labelled(CONFIRM, "Share") && labelled(DISMISS, "Cancel") && !seesText("Stop sharing address?")
    }

  private fun UiScope.awaitStopSharingAlert() =
    poll(what = "the `Stop sharing address?` alert with `Stop sharing` and `Cancel`") {
      seesText("Stop sharing address?") && labelled(CONFIRM, "Stop sharing") && labelled(DISMISS, "Cancel") && !seesText("Share address with SimpleX contacts?")
    }

  private companion object {
    val CONFIRM = Ui.Alert.Confirm(Ui.AlertSource.Shared)
    val DISMISS = Ui.Alert.Dismiss(Ui.AlertSource.Shared)
    const val TEAM_FOOTER = "Add your team members to the conversations."
  }
}
