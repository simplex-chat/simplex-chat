package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.CliPeer
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.TestServers
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.clearChatFromInfo
import chat.simplex.app.ui.harness.closeContactInfo
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.connectIncognito
import chat.simplex.app.ui.harness.contactOf
import chat.simplex.app.ui.harness.contactPrefs
import chat.simplex.app.ui.harness.copiedServer
import chat.simplex.app.ui.harness.createAddress
import chat.simplex.app.ui.harness.described
import chat.simplex.app.ui.harness.goBack
import chat.simplex.app.ui.harness.infoNameText
import chat.simplex.app.ui.harness.isServer
import chat.simplex.app.ui.harness.itemStatus
import chat.simplex.app.ui.harness.loadedText
import chat.simplex.app.ui.harness.localSmpServer
import chat.simplex.app.ui.harness.markContactVerifiedViaApi
import chat.simplex.app.ui.harness.openNewChatFromSearch
import chat.simplex.app.ui.harness.setContactAliasViaApi
import chat.simplex.app.ui.harness.switchAwayFromChat
import chat.simplex.common.model.CIContent
import chat.simplex.common.model.ChatFeature
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.ChatItemTTL
import chat.simplex.common.model.FeatureAllowed
import chat.simplex.common.model.MsgFilter
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle
import chat.simplex.common.views.helpers.ModalManager
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class ContactInfoTest : UiTest() {
  @Test fun contactInfoVerifyCodeMatchesCli() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val shown = alice.chatList.open("bob").contactInfo().securityCode()
    bob sends "/code alice"
    bob.eventually("alice security code:")
    assertEquals(bob.nextLine().filterNot(Char::isWhitespace), shown)
  }

  @Test fun contactInfoMarkVerified() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val info = alice.chatList.open("bob").contactInfo()
    poll(what = "the row `Verify security code`") { info.verifyRowText() == "Verify security code" }
    info.markVerified()
    poll(what = "bob verified in the model") { (alice.chatList.chat("bob").chatInfo as ChatInfo.Direct).contact.verified }
    poll(what = "the row `View security code`") { info.verifyRowText() == "View security code" }
    click(Ui.Info.VerifyCode)
    poll(what = "`bob is verified`") { seesText("bob is verified") }
  }

  @Test fun contactInfoSetLocalAlias() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob")
    assertTrue("bob" in alice.chatList.rowTexts("bob"))
    alice.chatList.open("bob").info()
    typeInto(Ui.Info.Alias, "friend")
    poll(what = "the row to read `friend`") { chatModel.chats.value.any { it.chatInfo.chatViewName == "friend" } }
    poll(what = "the row text `friend` instead of `bob`") { alice.chatList.rowTexts("friend").let { "friend" in it && "bob" !in it } }
  }

  @Test fun contactInfoClearLocalAlias() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    setContactAliasViaApi("bob", "friend")
    restart()
    val id = alice.chatList.chatId("friend")
    poll(what = "bob's row named `friend`") { alice.chatList.rowTexts("friend").let { "friend" in it && "bob" !in it } }
    alice.chatList.open("friend").info()
    poll(what = "the alias `friend` in the name field of contact info") { exists(Ui.Info.Alias) && editableText(Ui.Info.Alias) == "friend" }
    replaceText(Ui.Info.Alias, "")
    poll(what = "bob's alias cleared in the model") { (chatModel.chats.value.first { it.id == id }.chatInfo as ChatInfo.Direct).contact.localAlias.isEmpty() }
    closeContactInfo()
    backToChatList()
    poll(what = "bob's row named `bob` again") { alice.chatList.rowTexts("bob").let { "bob" in it && "friend" !in it } }
    switchAwayFromChat(alice)
    check(chatModel.chatId.value != id) { "bob is the open chat, which is listed whatever the search text" }
    alice.chatList.search("fri")
    poll(what = "bob hidden for the cleared alias") { editableText(Ui.ChatList.Search) == "fri" && !alice.chatList.shown("bob") }
    replaceText(Ui.ChatList.Search, "bo")
    poll(what = "bob listed for his name") { editableText(Ui.ChatList.Search) == "bo" && alice.chatList.shown("bob") }
  }

  @Test fun contactInfoClearVerification() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    markContactVerifiedViaApi("bob")
    restart()
    alice.chatList.row("bob")
    val info = alice.chatList.open("bob").contactInfo()
    poll(what = "the row `View security code` and the shield before bob's name") {
      info.verifyRowText() == "View security code" && infoNameText() == "${SHIELD}bob"
    }
    click(Ui.Info.VerifyCode)
    poll(what = "`bob is verified` with `Clear verification`") {
      seesText("bob is verified") && exists(described("Clear verification")) && !exists(Ui.VerifyCode.Mark)
    }
    click(described("Clear verification"))
    poll(what = "the security code screen still open with `bob is not verified` and `Mark verified`") {
      seesText("bob is not verified") && !seesText("bob is verified") && exists(Ui.VerifyCode.Mark) && !exists(described("Clear verification"))
    }
    poll(what = "bob not verified in the model") { !contactOf("bob").verified }
    goBack()
    poll(what = "the row `Verify security code` and no shield before bob's name") {
      !exists(Ui.VerifyCode.Mark) && info.verifyRowText() == "Verify security code" && infoNameText() == "bob"
    }
  }

  @Test fun contactInfoHeaderShowsNameBioCopy() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.info()
    poll(what = "bob's name in the header with the name field, and no full name or bio") {
      infoNameText() == "bob" && exists(Ui.Info.Alias) && textsOf(Ui.Info.Header).none { FULL_NAME in it || BIO in it }
    }
    closeContactInfo()
    bob.updateProfile()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's full name and bio in the model") {
      bobProfile()?.let { it.fullName == FULL_NAME && it.shortDescr == BIO } == true
    }
    chat.info()
    poll(what = "bob's name, full name and bio in the header") {
      infoNameText() == "bob" && textsOf(Ui.Info.Header).let { t -> t.any { FULL_NAME in it } && t.any { BIO in it } }
    }
    setClipboardText(null)
    check(clipboardText() == null) { "the clipboard is not empty before copying the name" }
    click(Ui.Info.Name)
    poll(what = "bob's name on the clipboard") { clipboardText() == "bob" }
  }

  @KnownAppIssue("15.5 item 16", "waiting for the full name `Robert Brown` and the bio `Plays the cello` in the open contact info")
  @Test fun contactInfoHeaderFollowsProfileUpdate() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.open("bob").info()
    poll(what = "bob's name in the header, and no full name or bio") {
      infoNameText() == "bob" && textsOf(Ui.Info.Header).none { FULL_NAME in it || BIO in it }
    }
    bob.updateProfile()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's full name and bio in the model") {
      bobProfile()?.let { it.fullName == FULL_NAME && it.shortDescr == BIO } == true
    }
    poll(what = "the full name `$FULL_NAME` and the bio `$BIO` in the open contact info") {
      textsOf(Ui.Info.Header).let { t -> FULL_NAME in t && t.any { BIO in it } }
    }
  }

  @Test fun contactInfoMuteButtonToggles() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    fun muteIcon() = "Notifications" in alice.chatList.rowDescriptions("bob")
    alice.chatList.row("bob")
    poll(what = "bob's row without the mute icon") { !muteIcon() && contactOf("bob").chatSettings.enableNtfs == MsgFilter.All }
    val chat = alice.chatList.open("bob")
    chat.info()
    poll(what = "`Mute` in contact info") { exists(Ui.Info.Mute) && seesText("Mute") && !seesText("Unmute") }
    click(Ui.Info.Mute)
    poll(what = "`Unmute` with bob's notifications off") { seesText("Unmute") && !seesText("Mute") && contactOf("bob").chatSettings.enableNtfs == MsgFilter.None }
    closeContactInfo()
    backToChatList()
    poll(what = "the mute icon on bob's row") { muteIcon() }
    alice.chatList.open("bob").info()
    poll(what = "`Unmute` in reopened contact info") { exists(Ui.Info.Mute) && seesText("Unmute") }
    click(Ui.Info.Mute)
    poll(what = "`Mute` with bob's notifications on") { seesText("Mute") && !seesText("Unmute") && contactOf("bob").chatSettings.enableNtfs == MsgFilter.All }
    closeContactInfo()
    backToChatList()
    poll(what = "the mute icon gone from bob's row") { !muteIcon() }
  }

  @Test fun contactInfoServersSection() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val smp = localSmpServer()
    alice.chatList.open("bob").info()
    poll(what = "`Receiving via` and `Sending via` the local server, and `Network status`") {
      exists(Ui.Info.ReceivingVia) && exists(Ui.Info.SendingVia) && exists(described("Network status")) &&
        textsOf(Ui.Info.ReceivingVia).let { "Receiving via" in it && it.any { t -> smp.hostnames.first() in t } } &&
        textsOf(Ui.Info.SendingVia).let { "Sending via" in it && it.any { t -> smp.hostnames.first() in t } }
    }
    val receiving = copiedServer(Ui.Info.ReceivingVia)
    check(isServer(receiving, smp)) { "`Receiving via` copied `$receiving`, not the local server $smp" }
    val sending = copiedServer(Ui.Info.SendingVia)
    check(isServer(sending, smp)) { "`Sending via` copied `$sending`, not the local server $smp" }
    check(!seesText(NETWORK_STATUS_TEXT)) { "the network status explanation is shown before `Network status` is clicked" }
    click(described("Network status"))
    poll(what = "the `Network status` alert saying alice is connected to bob's receiving server") {
      seesText(NETWORK_STATUS_TEXT) && exists(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the `Network status` alert to close") { !seesText(NETWORK_STATUS_TEXT) }
  }

  @Test fun contactInfoSwitchAddress() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.info()
    poll(what = "`Change receiving address` without `Abort changing address`") { exists(Ui.Info.SwitchAddress) && !seesText(ABORT_SWITCH) }
    bob.stop()
    click(Ui.Info.SwitchAddress)
    poll(what = "the `Change receiving address?` alert with `Change`") {
      seesText("Change receiving address?") &&
        seesText("Receiving address will be changed to a different server. Address change will complete after sender comes online.") &&
        "Change" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "contact info closed after `Change`") { !ModalManager.end.hasModalsOpen() }
    chat.itemId("`changing address…` in bob's chat") { it.content is CIContent.SndConnEventContent && it.content.text == "changing address…" }
    chat.info()
    poll(what = "`Abort changing address` while bob is offline") { exists(Ui.Info.SwitchAddress) && seesText(ABORT_SWITCH) }
    closeContactInfo()
    bob.start()
    bob.eventually("alice started changing address for you", CONNECT_TIMEOUT_MS)
    bob.eventually("alice changed address for you", CONNECT_TIMEOUT_MS)
    chat.itemId("`you changed address` in bob's chat") { it.content is CIContent.SndConnEventContent && it.content.text == "you changed address" }
    chat.info()
    poll(what = "`Change receiving address` back without `Abort changing address`") { exists(Ui.Info.SwitchAddress) && !seesText(ABORT_SWITCH) }
    closeContactInfo()
    bob sends "@alice after switch"
    chat.itemId("bob's `after switch` on the new address") { it.content.text == "after switch" }
  }

  @Test fun contactInfoAbortSwitchAddress() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's contact ready") { contactOf("bob").ready }
    chat.info()
    poll(what = "`Change receiving address` without `Abort changing address`") { exists(Ui.Info.SwitchAddress) && !exists(Ui.Info.AbortSwitchAddress) }
    TestServers.stop("smp1")
    click(Ui.Info.SwitchAddress)
    poll(what = "the `Change receiving address?` alert with `Change`") {
      seesText("Change receiving address?") && "Change" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "contact info closed after `Change`") { !ModalManager.end.hasModalsOpen() }
    chat.info()
    poll(what = "`Abort changing address` while smp1 is down") { exists(Ui.Info.AbortSwitchAddress) && ABORT_SWITCH in textsOf(Ui.Info.AbortSwitchAddress) }
    click(Ui.Info.AbortSwitchAddress)
    poll(what = "the `Abort changing address?` alert with `Abort`") {
      seesText("Abort changing address?") && "Abort" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "`Abort changing address` gone and `Change receiving address` back after `Abort`") {
      !seesText("Abort changing address?") && !exists(Ui.Info.AbortSwitchAddress) && exists(Ui.Info.SwitchAddress)
    }
    closeContactInfo()
    TestServers.start("smp1")
    chat.send("after abort")
    bob.eventuallyMessage("alice> after abort", CONNECT_TIMEOUT_MS)
    bob sends "@alice reply after abort"
    chat.itemId("reply after abort")
    assertTrue("alice's chat shows an address change after the abort: ${chat.itemTexts()}", chat.itemTexts().none { "changing address" in it || "changed address" in it })
  }

  @Test fun contactInfoClearChat() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("hello")
    bob.expectsMessage("alice> hello", CONNECT_TIMEOUT_MS)
    bob sends "@alice hi there"
    val hello = chat.itemId("hello")
    val hi = chat.itemId("bob's `hi there`") { it.content.text == "hi there" }
    hideKeyboard()
    chat.info()
    clearChatFromInfo("bob")
    poll(what = "contact info closed with `hello` and `hi there` gone from bob's chat") {
      !ModalManager.end.hasModalsOpen() && !exists(Ui.Chat.Item(hello)) && !exists(Ui.Chat.Item(hi)) &&
        chatModel.chatsContext.chatItems.value.none { it.content.text == "hello" || it.content.text == "hi there" }
    }
    check(contactOf("bob").let { it.active && it.ready }) { "bob's contact is not active and ready after clearing the chat" }
    bob sends "/tail @alice 2"
    bob.expectsMessage("alice> hello")
    bob.expectsMessage("@alice hi there")
    bob sends "@alice after clear"
    chat.itemId("bob's `after clear`") { it.content.text == "after clear" }
    chat.send("still here")
    bob.expectsMessage("alice> still here", CONNECT_TIMEOUT_MS)
    backToChatList()
    alice.chatList.row("bob")
  }

  @Test fun contactInfoContactAddressSection() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob.createAddress(autoAccept = false)
    alice.chatList.open("bob").info()
    poll(what = "contact info without bob's address") { exists(Ui.Info.DeleteContact) && !exists(Ui.Address.Share) && !seesText(ADDRESS_FOOTER) }
    bob sends "/pa on"
    bob.eventually("new contact address set")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's address with `Share address…` in the open contact info") {
      exists(Ui.Address.Share) && seesText(ADDRESS_FOOTER) && contactOf("bob").contactLink != null
    }
    setClipboardText(null)
    click(Ui.Address.Share)
    var shared: String? = null
    poll(what = "bob's address shared") { shared = clipboardText()?.takeIf { it.isNotBlank() }; shared != null }
    bob sends "/_connect plan 1 $shared"
    bob.eventually("contact address: own address")
  }

  @Test fun contactInfoNotReadyContactHidesSections() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = bob.createAddress(autoAccept = false)
    alice.chatList.row("Private notes")
    val chat = openNewChatFromSearch(address, "bob")
    click(Ui.Chat.Connect)
    poll(what = "`Send contact request?` with `Send request without message`") {
      seesText("Send contact request?") && "Send request without message" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    bob.eventuallyAll(
      listOf("alice wants to connect to you!", "to accept: /ac alice", "to reject: /rc alice (the sender will NOT be notified)"),
      timeoutMs = CONNECT_TIMEOUT_MS
    )
    poll(what = "bob's contact connecting and not ready") { contactOf("bob").let { it.activeConn != null && !it.ready } }
    hideKeyboard()
    chat.info()
    poll(what = "contact info of the not-ready bob without verification, preferences, servers, and with `Mute` disabled") {
      exists(Ui.Info.DeleteContact) && exists(Ui.Info.Mute) && !isEnabled(Ui.Info.Mute) &&
        !exists(Ui.Info.VerifyCode) && !exists(Ui.Info.Preferences) && !exists(Ui.Info.ReceivingVia) && !exists(Ui.Info.SwitchAddress)
    }
    closeContactInfo()
    bob sends "/ac alice"
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's contact ready") { contactOf("bob").ready }
    chat.info()
    poll(what = "verification, preferences, servers and an enabled `Mute` in the contact info of the ready bob") {
      exists(Ui.Info.VerifyCode) && exists(Ui.Info.Preferences) && exists(Ui.Info.ReceivingVia) && exists(Ui.Info.SwitchAddress) && isEnabled(Ui.Info.Mute)
    }
  }

  @Test fun contactInfoAllowCallsAlert() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.info()
    val prefs = contactPrefs()
    poll(what = "calls allowed by both") { seesText(CALLS_BOTH) && !seesText(CALLS_OFF) }
    prefs.allow(ChatFeature.Calls, "NO")
    poll(what = "calls prohibited after the change") { seesText(CALLS_OFF) }
    prefs.save()
    assertEquals("alice updated preferences for you:", bob.nextLine(CONNECT_TIMEOUT_MS))
    bob expects "Audio/video calls: off (you allow: default (yes), contact allows: no)"
    poll(what = "alice's `no` for calls saved in bob's contact") { contactOf("bob").mergedPreferences.calls.userPreference.pref.allow == FeatureAllowed.NO }
    prefs.backClosing()
    closeContactInfo()
    chat.info()
    waitFor(Ui.Info.Call)
    check(!seesText(ALLOW_CALLS_TEXT)) { "`Allow calls?` is shown before `call` is clicked" }
    click(Ui.Info.Call)
    poll(what = "the `Allow calls?` alert with `Allow`") {
      seesText("Allow calls?") && seesText(ALLOW_CALLS_TEXT) && "Allow" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    assertEquals("alice updated preferences for you:", bob.nextLine(CONNECT_TIMEOUT_MS))
    bob expects "Audio/video calls: enabled (you allow: default (yes), contact allows: yes)"
    poll(what = "calls enabled for alice after `Allow`") { contactOf("bob").mergedPreferences.calls.enabled.forUser }
    bob sends "/set calls @alice no"
    bob expects "you updated preferences for alice:"
    bob expects "Audio/video calls: off (you allow: no, contact allows: yes)"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's `no` for calls in alice's contact") {
      contactOf("bob").mergedPreferences.calls.contactPreference.allow == FeatureAllowed.NO
    }
    closeContactInfo()
    chat.info()
    waitFor(Ui.Info.Call)
    check(!seesText(CALLS_PROHIBITED_TEXT)) { "`Calls prohibited!` is shown before `call` is clicked" }
    click(Ui.Info.Call)
    poll(what = "the `Calls prohibited!` alert asking to have calls enabled, not `Allow calls?`") {
      seesText("Calls prohibited!") && seesText(CALLS_PROHIBITED_TEXT) && !seesText("Allow calls?")
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the `Calls prohibited!` alert to close") { !seesText("Calls prohibited!") }
  }

  @Test fun contactInfoSendReceiptsNo() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val direct = "@${bob.contactId("alice")}"
    bob sends "@alice r0"
    val chat = alice.chatList.open("bob")
    chat.itemId("r0")
    awaitReceipt(bob, direct, "r0")
    chat.info()
    poll(what = "`Send receipts` reading `default (yes)`") { "default (yes)" in textsOf(Ui.Info.SendReceipts) && contactOf("bob").chatSettings.sendRcpts == null }
    click(Ui.Info.SendReceipts)
    click(Ui.Info.SendReceipts.option(false))
    poll(what = "`Send receipts` reading `no`, saved for bob") { "no" in textsOf(Ui.Info.SendReceipts) && contactOf("bob").chatSettings.sendRcpts == false }
    bob sends "@alice r1"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's `r1` received while receipts are off") { loadedText("r1") }
    click(Ui.Info.SendReceipts)
    click(Ui.Info.SendReceipts.option("default"))
    poll(what = "`Send receipts` back to `default (yes)`, saved for bob") {
      "default (yes)" in textsOf(Ui.Info.SendReceipts) && contactOf("bob").chatSettings.sendRcpts == null
    }
    bob sends "@alice r2"
    awaitReceipt(bob, direct, "r2")
    assertEquals("r1 received while receipts were off", "sndSent", bob.itemStatus(direct, "r1"))
  }

  @Test fun contactInfoIncognitoSection() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val incognito = connectIncognito(bob)
    connect(cath)
    check(incognito != "alice") { "bob sees alice's own name for an incognito connection" }
    alice.chatList.row("bob")
    alice.chatList.open("cath").info()
    poll(what = "cath's contact info without `Your random profile`") {
      infoNameText() == "cath" && exists(Ui.Info.DeleteContact) && !seesText(RANDOM_PROFILE) && !seesText(incognito)
    }
    closeContactInfo()
    backToChatList()
    alice.chatList.open("bob").info()
    poll(what = "bob's contact info with `Your random profile` named `$incognito`, the name bob sees") {
      infoNameText() == "bob" && seesText(RANDOM_PROFILE) && seesText(incognito)
    }
  }

  @Test fun contactInfoChatTtlEnable() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.info()
    poll(what = "`Delete messages after` reading `default (never)`") {
      "default (never)" in textsOf(Ui.Info.Ttl) && contactOf("bob").chatItemTTL == null
    }
    pickTtl(ChatItemTTL.Day)
    poll(what = "the `Enable automatic message deletion?` alert with `Delete messages`") {
      seesText(ENABLE_TTL) && "Delete messages" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the picker back at `default (never)` after `Cancel`") { !seesText(ENABLE_TTL) && "default (never)" in textsOf(Ui.Info.Ttl) }
    check(contactOf("bob").chatItemTTL == null) { "`Cancel` set a message TTL for bob" }
    pickTtl(ChatItemTTL.Day)
    poll(what = "the `Enable automatic message deletion?` alert again") { seesText(ENABLE_TTL) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "bob's chat TTL of 1 day read back from the core, and `1 day` in the picker") {
      contactOf("bob").chatItemTTL == 86400L && "1 day" in textsOf(Ui.Info.Ttl)
    }
    closeContactInfo()
    chat.info()
    poll(what = "`1 day` in reopened contact info") { "1 day" in textsOf(Ui.Info.Ttl) }
    pickTtl(ChatItemTTL.None)
    poll(what = "the `Disable automatic message deletion?` alert with `Disable delete messages`") {
      seesText("Disable automatic message deletion?") && "Disable delete messages" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "bob's chat TTL set to never in the core and `never` in the picker") {
      contactOf("bob").chatItemTTL == 0L && "never" in textsOf(Ui.Info.Ttl)
    }
  }

  private fun UiScope.pickTtl(ttl: ChatItemTTL) {
    poll(what = "the previous message deletion change to finish before opening the picker") { UiIdle.inFlight().isEmpty() }
    click(Ui.Info.Ttl)
    click(Ui.Info.Ttl.option(ttl))
  }

  private fun UiScope.awaitReceipt(peer: CliPeer, chatRef: String, text: String) =
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "${peer.name} to get alice's receipt for `$text`") { peer.itemStatus(chatRef, text) == "sndRcvd" }

  private fun bobProfile() =
    chatModel.chats.value.firstNotNullOfOrNull { (it.chatInfo as? ChatInfo.Direct)?.contact?.profile?.takeIf { p -> p.displayName == "bob" } }

  private fun CliPeer.updateProfile() {
    this sends """/_profile 1 {"displayName":"bob","fullName":"$FULL_NAME","shortDescr":"$BIO"}"""
    eventuallyMatching(Regex("user full name changed to $FULL_NAME \\(your 1 contacts are notified.*\\)"), CONNECT_TIMEOUT_MS)
  }

  private companion object {
    const val SHIELD = "�"
    const val FULL_NAME = "Robert Brown"
    const val BIO = "Plays the cello"
    const val NETWORK_STATUS_TEXT = "You are connected to the server used to receive messages from this connection."
    const val ABORT_SWITCH = "Abort changing address"
    const val ADDRESS_FOOTER = "You can share this address with your contacts to let them connect with bob."
    const val CALLS_BOTH = "Both you and your contact can make calls."
    const val CALLS_OFF = "Audio/video calls are prohibited."
    const val ALLOW_CALLS_TEXT = "You need to allow your contact to call to be able to call them."
    const val CALLS_PROHIBITED_TEXT = "Please ask your contact to enable calls."
    const val RANDOM_PROFILE = "Your random profile"
    const val ENABLE_TTL = "Enable automatic message deletion?"
  }
}
