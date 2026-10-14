package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.closeInfoToChatList
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.connectIncognito
import chat.simplex.app.ui.harness.createAddress
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.joinGroupFromList
import chat.simplex.app.ui.harness.switchAwayFromChat
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.MsgContent
import chat.simplex.common.model.MsgFilter
import chat.simplex.common.model.getTimestampText
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.newchat.planAndConnect
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertFalse
import org.junit.Test

private const val PNG_1PX = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="

class ChatListRowsTest : UiTest() {
  @Test fun previewShowsLastTextMessage() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice first message"
    poll(what = "the first message on the row") { "first message" in alice.chatList.rowTexts("bob") }
    bob sends "@alice second message"
    poll(what = "the second message in place of the first") {
      alice.chatList.rowTexts("bob").let { "second message" in it && "first message" !in it }
    }
    alice.chatList.open("bob").send("my reply")
    bob.eventuallyMessage("alice> my reply")
    backToChatList()
    poll(what = "alice's own reply on the row") {
      alice.chatList.rowTexts("bob").let { "my reply" in it && "second message" !in it }
    }
  }

  @Test fun previewShowsDraft() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hello"
    poll(what = "bob's message on the row") { "hello" in alice.chatList.rowTexts("bob") }
    alice.chatList.open("bob")
    typeInto(Ui.Chat.Message, "unsent draft")
    switchAwayFromChat(alice)
    poll(what = "the draft with the edit icon in place of the message") {
      chatModel.draftChatId.value == alice.chatList.chatId("bob") &&
        alice.chatList.rowTexts("bob").let { t -> t.any { it.endsWith(" unsent draft") } && "hello" !in t }
    }
  }

  @Test fun previewLink() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val id = bob.contactId("alice")
    bob sends """/_send @$id json [{"msgContent":{"type":"link","text":"https://simplex.chat","preview":{"uri":"https://simplex.chat","title":"SimpleX Chat","description":"Private messenger","image":"$PNG_1PX"}}}]"""
    bob.eventuallyMessage("@alice https://simplex.chat")
    poll(what = "the link text and the link image on the row") {
      "https://simplex.chat" in alice.chatList.rowTexts("bob") && alice.chatList.chat("bob").chatItems.lastOrNull()?.content?.msgContent is MsgContent.MCLink
    }
  }

  @Test fun previewDeletedMessage() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice to be deleted"
    poll(what = "the message on the row") { "to be deleted" in alice.chatList.rowTexts("bob") }
    bob sends "\\@alice to be deleted"
    bob.eventually("message marked deleted")
    poll(what = "`marked deleted` in place of the text") {
      alice.chatList.rowTexts("bob").let { "marked deleted" in it && "to be deleted" !in it }
    }
  }

  @Test fun previewContactCardMessage() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    bob sends "/p bob Coffee lover"
    bob.eventually("user bio changed to Coffee lover (your 0 contacts are notified)")
    bob.createAddress()
    connect(bob)
    bob sends "/share address @alice"
    bob.eventuallyMatching(Regex(".*@alice contact address of @bob.*:"))
    poll(what = "the card's name and bio on the row") { "bob\nCoffee lover" in alice.chatList.rowTexts("bob") }
  }

  @Test fun previewGroupSenderPrefix() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    joinGroupFromList(alice, bob, "team")
    alice.chatList.open("team").send("mine")
    bob.eventuallyMessage("#team alice> mine", CONNECT_TIMEOUT_MS)
    backToChatList()
    poll(what = "alice's own message without a sender prefix") {
      alice.chatList.rowTexts("team").let { t -> "mine" in t && t.none { it.endsWith(": mine") } }
    }
    bob sends "#team hello all"
    poll(what = "bob's message with the `bob: ` prefix") { "bob: hello all" in alice.chatList.rowTexts("team") }
  }

  @Test fun unreadCounterGrayWhenMuted() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.rowMenu("bob", "ntfs.None")
    poll(what = "bob muted") { alice.chatList.chat("bob").chatInfo.chatSettings?.enableNtfs == MsgFilter.None }
    bob sends "@alice one"
    poll(what = "unread badge 1") { "1" in alice.chatList.rowTexts("bob") }
    bob sends "@alice two"
    poll(what = "unread badge 2 in place of the mute icon") {
      "2" in alice.chatList.rowTexts("bob") && "Notifications" !in alice.chatList.rowDescriptions("bob") &&
        alice.chatList.chat("bob").chatInfo.chatSettings?.enableNtfs == MsgFilter.None
    }
  }

  @Test fun mentionBadgeSingleUnread() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    joinGroupFromList(alice, bob, "team")
    poll(what = "no @ badge before the mention") { "Notifications" !in alice.chatList.rowDescriptions("team") }
    bob sends "#team hello @alice"
    poll(what = "the round @ badge without a count") {
      alice.chatList.chat("team").chatStats.let { it.unreadCount == 1 && it.unreadMentions == 1 } &&
        "Notifications" in alice.chatList.rowDescriptions("team") && "1" !in alice.chatList.rowTexts("team")
    }
  }

  @Test fun mentionIconWithMultipleUnread() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    joinGroupFromList(alice, bob, "team")
    bob sends "#team hello @alice"
    poll(what = "the round @ badge") { "Notifications" in alice.chatList.rowDescriptions("team") }
    bob sends "#team and one more"
    poll(what = "the @ icon next to the badge 2") {
      alice.chatList.chat("team").chatStats.let { it.unreadCount == 2 && it.unreadMentions == 1 } &&
        "Notifications" in alice.chatList.rowDescriptions("team") && "2" in alice.chatList.rowTexts("team")
    }
  }

  @Test fun mutedIconWhenNoUnread() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.rowMenu("bob", "ntfs.None")
    poll(what = "the mute icon on the row") { "Notifications" in alice.chatList.rowDescriptions("bob") }
    bob sends "@alice hi"
    poll(what = "the unread badge in place of the mute icon") {
      "1" in alice.chatList.rowTexts("bob") && "Notifications" !in alice.chatList.rowDescriptions("bob")
    }
    alice.chatList.rowMenu("bob", "markRead")
    poll(what = "the mute icon back once read") {
      "Notifications" in alice.chatList.rowDescriptions("bob") && "1" !in alice.chatList.rowTexts("bob")
    }
  }

  @Test fun favoriteStarWhenNoUnread() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.rowMenu("bob", "favorite")
    poll(what = "the star on the row") { "Favorite" in alice.chatList.rowDescriptions("bob") }
    bob sends "@alice hi"
    poll(what = "the unread badge in place of the star") {
      "1" in alice.chatList.rowTexts("bob") && "Favorite" !in alice.chatList.rowDescriptions("bob")
    }
    alice.chatList.rowMenu("bob", "markRead")
    poll(what = "the star back once read") {
      "Favorite" in alice.chatList.rowDescriptions("bob") && "1" !in alice.chatList.rowTexts("bob")
    }
  }

  @Test fun incognitoIconOnRow() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connectIncognito(bob)
    poll(what = "bob connected incognito") {
      (alice.chatList.chat("bob").chatInfo as? ChatInfo.Direct)?.contact?.contactConnIncognito == true
    }
    assertFalse(alice.chatList.chat("Private notes").chatInfo.incognito)
  }

  @Test fun verifiedIconOnRow() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob")
    assertFalse((alice.chatList.chat("bob").chatInfo as ChatInfo.Direct).contact.verified)
    alice.chatList.open("bob").contactInfo().markVerified()
    poll(what = "bob verified in the model") { (alice.chatList.chat("bob").chatInfo as ChatInfo.Direct).contact.verified }
    closeInfoToChatList()
  }

  @Test fun timestampShown() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice what time is it"
    poll(what = "the message time on the row") {
      val item = alice.chatList.chat("bob").chatItems.lastOrNull()?.takeIf { it.content.text == "what time is it" }
      item != null && getTimestampText(item.meta.itemTs) in alice.chatList.rowTexts("bob")
    }
  }

  @Test fun previewContactShouldAcceptForAddress() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    bob sends "/ad"
    bob expects "Your new chat address is created!"
    bob expects ""
    val address = bob.nextLine()
    runBlocking { planAndConnect(null, address, close = null) }
    click(Ui.Alert.Confirm(Ui.AlertSource.PrivacySensitive))
    click(Ui.Chat.Connect)
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    bob.eventually("alice wants to connect to you!", CONNECT_TIMEOUT_MS)
    backToChatList()
    poll(what = "`contact should accept…` on bob's row") { "contact should accept…" in alice.chatList.rowTexts("bob") }
  }

  @Test fun previewGroupInvitationJoinAs() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob)
    hostGroup(bob, "team", "alice")
    poll(what = "`You are invited to group` on the team row") { "You are invited to group" in alice.chatList.rowTexts("team") }
    val incognito = connectIncognito(cath)
    hostGroup(cath, "club", incognito)
    poll(what = "`Join as $incognito` on the club row") { "Join as $incognito" in alice.chatList.rowTexts("club") }
  }
}
