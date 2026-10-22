package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.NEW_RANDOM_PROFILE
import chat.simplex.app.ui.harness.PeerFlags
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.closePendingConnectionInfo
import chat.simplex.app.ui.harness.connectionRequestSentToGroup
import chat.simplex.app.ui.harness.createFullGroupLink
import chat.simplex.app.ui.harness.createFullOneTimeLink
import chat.simplex.app.ui.harness.described
import chat.simplex.app.ui.harness.enterLinkInSearch
import chat.simplex.app.ui.harness.joinOneTimeLinkInSearch
import chat.simplex.app.ui.harness.keepIncognitoOneTimeLink
import chat.simplex.app.ui.harness.keepOneTimeLink
import chat.simplex.app.ui.harness.openChatById
import chat.simplex.app.ui.harness.pendingConnectionIds
import chat.simplex.app.ui.harness.planOption
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

class PendingConnectionTest : UiTest() {
  @Test fun pendingInfoInvitedSharesWorkingLink() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    keepOneTimeLink(alice)
    alice.chatList.open(INVITED)
    poll(what = "`You invited a contact` with the one-time link section and `Share 1-time link`") {
      seesText("You invited a contact") && seesText("One-time invitation link") && seesText(CONTACT_ONLINE) && exists(SHARE) &&
        !seesText("You accepted connection")
    }
    setClipboardText(null)
    check(clipboardText() == null) { "the clipboard is not empty before sharing the link" }
    click(SHARE)
    var link: String? = null
    poll(what = "the one-time link shared") { link = clipboardText()?.takeIf { it.isNotBlank() }; link != null }
    bob sends "/c $link"
    bob expects "confirmation sent!"
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob connected in place of the pending connection") {
      chatModel.chats.value.any { (it.chatInfo as? ChatInfo.Direct)?.contact?.let { c -> c.displayName == "bob" && c.sndReady } == true } &&
        chatModel.chats.value.none { it.chatInfo is ChatInfo.ContactConnection }
    }
  }

  @Test fun pendingInfoDelete() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = keepOneTimeLink(alice)
    alice.chatList.open(INVITED)
    poll(what = "`You invited a contact` with `Delete`") { seesText("You invited a contact") && exists(DELETE) }
    click(DELETE)
    poll(what = "the `Delete pending connection?` alert for the invitation with `Delete`") {
      seesText("Delete pending connection?") && seesText("$INVITED\n\nThe contact you shared this link with will NOT be able to connect!") &&
        "Delete" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    alice.chatList.gone(INVITED)
    poll(what = "the pending connection info closed") { chatModel.chatId.value == null && !seesText("You invited a contact") }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the deleted link to fail its plan on the server") {
      bob sends "/_connect plan 1 $link"
      bob.eventuallyMatching(PLAN_RESULT).value.startsWith("error")
    }
    bob sends "/c $link"
    bob.eventuallyMatching(AUTH_FAILED)
  }

  @KnownAppIssue("15.5 item 18", "waiting for the pending row named `accepted invitation` with `via one-time link`")
  @Test fun pendingInfoAcceptedShowsNoLink() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(fullLinks = true))
    val full = bob.createFullOneTimeLink()
    bob.stop()
    alice.chatList.row("Private notes")
    joinOneTimeLinkInSearch(full)
    val pending = chatModel.chats.value.first { it.chatInfo is ChatInfo.ContactConnection }.id
    poll(what = "the pending row named `$ACCEPTED` with `via one-time link`") {
      textsOf(Ui.ChatList.Row(pending)).let { ACCEPTED in it && "via one-time link" in it }
    }
    alice.chatList.open(ACCEPTED)
    poll(what = "`You accepted connection` with the name field and `Learn more`, without the one-time link or `Share 1-time link`") {
      seesText("You accepted connection") && seesText(CONTACT_ONLINE) && exists(Ui.Info.Alias) && exists(described("Learn more")) &&
        !seesText("You invited a contact") && !seesText("One-time invitation link") && !exists(SHARE)
    }
    typeInto(Ui.Info.Alias, "dentist")
    poll(what = "the alias `dentist` saved for the pending connection") {
      chatModel.chats.value.any { (it.chatInfo as? ChatInfo.ContactConnection)?.contactConnection?.localAlias == "dentist" }
    }
    bob.start()
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob connected under the alias `dentist` and no pending connection left") {
      chatModel.chats.value.any { (it.chatInfo as? ChatInfo.Direct)?.contact?.let { c -> c.profile.displayName == "bob" && c.localAlias == "dentist" && c.sndReady } == true } &&
        chatModel.chats.value.none { it.chatInfo is ChatInfo.ContactConnection }
    }
    alice.chatList.row("dentist")
  }

  @Test fun pendingInfoIncognitoShown() = uiTest {
    val alice = app("alice")
    keepOneTimeLink(alice)
    val plain = pendingConnectionIds().single()
    keepIncognitoOneTimeLink(alice)
    val incognito = pendingConnectionIds().single { it != plain }
    openChatById(plain)
    poll(what = "the plain invitation's info sharing alice's profile, without `Incognito`") {
      seesText("You invited a contact") && seesText(OWN_PROFILE) && !seesText("Incognito") && !seesText(NEW_RANDOM_PROFILE)
    }
    closePendingConnectionInfo()
    openChatById(incognito)
    poll(what = "the incognito invitation's info with `Incognito` and the random profile footer") {
      seesText("You invited a contact") && seesText("Incognito") && seesText(NEW_RANDOM_PROFILE) && !seesText(OWN_PROFILE)
    }
    check(!seesText(SCAN_TO_CONNECT, substring = true)) { "the `Learn more` text is shown before `Learn more` is clicked" }
    click(described("Learn more"))
    poll(what = "the `One-time invitation link` page explaining how to connect") { seesText(SCAN_TO_CONNECT, substring = true) }
  }

  @Test fun pendingInfoGroupLinkHidesAlias() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(fullLinks = true))
    val full = bob.createFullGroupLink("team")
    bob.stop()
    keepOneTimeLink(alice)
    val invitation = pendingConnectionIds().single()
    enterLinkInSearch(full)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Join group?` alert with `Use current profile`") {
      seesText("Join group?") && exists(planOption("useCurrentProfile"))
    }
    click(planOption("useCurrentProfile"))
    connectionRequestSentToGroup()
    var joining: String? = null
    poll(what = "a pending connection for the group link next to the invitation") {
      joining = pendingConnectionIds().singleOrNull { it != invitation }
      joining != null
    }
    openChatById(invitation)
    poll(what = "the invitation's info with the name field") { seesText("You invited a contact") && exists(Ui.Info.Alias) }
    closePendingConnectionInfo()
    openChatById(joining!!)
    poll(what = "the group link connection's info waiting for the host, without the name field") {
      seesText(GROUP_HOST_ONLINE) && !seesText("You invited a contact") && !exists(Ui.Info.Alias)
    }
  }

  private companion object {
    const val INVITED = "invited to connect"
    const val ACCEPTED = "accepted invitation"
    const val CONTACT_ONLINE = "You will be connected when your contact's device is online, please wait or check later!"
    const val GROUP_HOST_ONLINE = "You will be connected to group when the group host's device is online, please wait or check later!"
    const val OWN_PROFILE = "Your profile alice will be shared."
    const val SCAN_TO_CONNECT = "To connect, your contact can scan QR code or use the link in the app."
    val SHARE = described("Share 1-time link")
    val DELETE = described("Delete")
    val PLAN_RESULT = Regex("invitation link: ok to connect|error: connection authorization failed.*")
    val AUTH_FAILED = Regex("error: connection authorization failed - .*")
  }
}
