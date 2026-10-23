package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.NEW_RANDOM_PROFILE
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitInvitationGone
import chat.simplex.app.ui.harness.awaitInviteClosed
import chat.simplex.app.ui.harness.chooseLinkProfile
import chat.simplex.app.ui.harness.closeNewChatSheet
import chat.simplex.app.ui.harness.goBack
import chat.simplex.app.ui.harness.hideUserViaApi
import chat.simplex.app.ui.harness.leaveInviteExpectingKeepAlert
import chat.simplex.app.ui.harness.pendingConnectionIds
import chat.simplex.app.ui.harness.userIdOf
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.AlertManager
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotEquals
import org.junit.Test

private const val INCOGNITO_PREF = "Incognito"
private const val HIDDEN_PASSWORD = "hide-me-42"
private val PROFILE_SEARCH = Ui.Link.ProfileBar.part("search")

class OneTimeLinkTest : UiTest() {
  @Test fun oneTimeLinkSwitchToIncognito() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = alice.newChat().createOneTimeLink()
    assertFalse(seesText("A new random profile will be shared."))
    click(Ui.Link.ShareProfile)
    click(Ui.Link.ProfileIncognito)
    poll(what = "the incognito footer") { seesText("A new random profile will be shared.") }
    bob sends "/c $link"; bob expects "confirmation sent!"
    val shared = bob.eventuallyMatching(Regex("(\\S+): contact is connected"), CONNECT_TIMEOUT_MS).groupValues[1]
    assertNotEquals("alice", shared)
    poll(what = "an incognito contact bob") {
      chatModel.chats.value.any { (it.chatInfo as? ChatInfo.Direct)?.contact?.let { c -> c.displayName == "bob" && c.contactConnIncognito } == true }
    }
  }

  @Test fun createOneTimeLinkShowsLinkAndQr() = uiTest {
    val alice = app("alice")
    val sheet = alice.newChat()
    poll(what = "the New chat sheet without a link or a QR code") { exists(Ui.NewChat.CreateLink) && !exists(Ui.Link.Text) && !seesDescription("QR Code") }
    sheet.createOneTimeLink()
    poll(what = "the link section, the QR code and alice in `Share profile`") {
      seesText("Share this 1-time invite link") && seesText("OR SHOW THIS CODE") && seesDescription("QR Code") && "alice" in textsOf(Ui.Link.ShareProfile)
    }
  }

  @Test fun oneTimeLinkKeepUnusedAlert() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val deleted = alice.newChat().createOneTimeLink()
    val deletedId = pendingConnectionIds().single()
    leaveInviteExpectingKeepAlert()
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    closeNewChatSheet()
    poll(what = "the deleted link's pending connection gone") { deletedId !in pendingConnectionIds() && !exists(Ui.ChatList.Row(deletedId)) }

    val shared = alice.newChat().createOneTimeLink()
    click(Ui.Link.Text)
    poll(what = "the link shared") { clipboardText() == shared }
    goBack()
    awaitInviteClosed()
    check(!AlertManager.shared.hasAlertsShown() && !seesText("Keep unused invitation?")) { "`Keep unused invitation?` shown for a shared link" }
    closeNewChatSheet()
    poll(what = "the shared link's pending connection listed") { pendingConnectionIds().size == 1 }

    val kept = alice.newChat().createOneTimeLink()
    leaveInviteExpectingKeepAlert()
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    closeNewChatSheet()
    poll(what = "the kept link's pending connection listed next to the shared one") { pendingConnectionIds().size == 2 }

    bob sends "/c $kept"; bob expects "confirmation sent!"
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    alice.chatList.row("bob")
    awaitInvitationGone(bob, deleted)
  }

  @Test fun oneTimeLinkChangeToOtherProfile() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    alice.userPicker().profiles().apply { add("work"); close() }
    val workId = userIdOf("work")
    alice.userPicker().switchTo("alice")
    val first = alice.newChat().createOneTimeLink()
    poll(what = "alice in `Share profile`") { "alice" in textsOf(Ui.Link.ShareProfile) }
    click(Ui.Link.ShareProfile)
    val second = chooseLinkProfile(workId, first)
    poll(what = "work in `Share profile`") { "work" in textsOf(Ui.Link.ShareProfile) && "alice" !in textsOf(Ui.Link.ShareProfile) }
    bob sends "/c $second"; bob expects "confirmation sent!"
    bob.eventually("work: contact is connected", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob in work's chats") {
      chatModel.currentUser.value?.userId == workId && chatModel.chats.value.any { it.chatInfo is ChatInfo.Direct && it.chatInfo.chatViewName == "bob" }
    }
    awaitInvitationGone(bob, first)
  }

  @Test fun oneTimeLinkChangeToHiddenProfileViaPassword() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    alice.userPicker().profiles().apply { add("work"); close() }
    val workId = userIdOf("work"); val aliceId = userIdOf("alice")
    alice.userPicker().switchTo("alice")
    hideUserViaApi(workId, HIDDEN_PASSWORD)
    val first = alice.newChat().createOneTimeLink()
    click(Ui.Link.ShareProfile)
    poll(what = "alice listed in `Select chat profile` and the hidden work not") { exists(Ui.Link.Profile(aliceId)) && !exists(Ui.Link.Profile(workId)) }
    typeInto(PROFILE_SEARCH, "work")
    poll(what = "no profile listed for the hidden profile's name") {
      editableText(PROFILE_SEARCH) == "work" && exists(Ui.Link.ProfileIncognito) && !exists(Ui.Link.Profile(workId)) && !exists(Ui.Link.Profile(aliceId))
    }
    replaceText(PROFILE_SEARCH, HIDDEN_PASSWORD)
    poll(what = "work listed for its password") { exists(Ui.Link.Profile(workId)) }
    hideKeyboard()
    val link = chooseLinkProfile(workId, first)
    poll(what = "work in `Share profile`") { "work" in textsOf(Ui.Link.ShareProfile) }
    bob sends "/c $link"; bob expects "confirmation sent!"
    bob.eventually("work: contact is connected", CONNECT_TIMEOUT_MS)
  }

  @Test fun oneTimeLinkSwitchBackFromIncognito() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val aliceId = userIdOf("alice")
    check(storedSetting(INCOGNITO_PREF) != "true") { "incognito is stored as the default before the test chooses it" }
    val sheet = alice.newChat()
    sheet.createOneTimeLink()
    click(Ui.Link.ShareProfile)
    click(Ui.Link.ProfileIncognito)
    poll(what = "the incognito footer and incognito stored as the default") { seesText(NEW_RANDOM_PROFILE) && storedSetting(INCOGNITO_PREF) == "true" }
    goBack()
    awaitInviteClosed()

    val incognitoLink = sheet.createOneTimeLink()
    poll(what = "the new link incognito by default") { seesText(NEW_RANDOM_PROFILE) && "Incognito" in textsOf(Ui.Link.ShareProfile) }
    click(Ui.Link.ShareProfile)
    val link = chooseLinkProfile(aliceId, incognitoLink)
    poll(what = "alice in `Share profile`, no footer and incognito no longer the default") {
      "alice" in textsOf(Ui.Link.ShareProfile) && !seesText(NEW_RANDOM_PROFILE) && storedSetting(INCOGNITO_PREF) == "false"
    }
    bob sends "/c $link"; bob expects "confirmation sent!"
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "a contact bob that is not incognito") {
      chatModel.chats.value.any { (it.chatInfo as? ChatInfo.Direct)?.contact?.let { c -> c.displayName == "bob" && !c.contactConnIncognito } == true }
    }
  }
}
