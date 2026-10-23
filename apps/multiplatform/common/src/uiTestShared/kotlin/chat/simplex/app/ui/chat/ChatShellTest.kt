package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.becomeBot
import chat.simplex.app.ui.harness.closeContactInfo
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.contactOf
import chat.simplex.app.ui.harness.emptyGroup
import chat.simplex.app.ui.harness.groupInfoOf
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.infoNameText
import chat.simplex.app.ui.harness.joinGroupFromList
import chat.simplex.common.model.CIContent
import chat.simplex.common.model.MsgFilter
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.ModalManager
import org.junit.Assert.assertFalse
import org.junit.Test

class ChatShellTest : UiTest() {
  @Test fun directChat_opens_showsToolbarAndBanner() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    assertFalse("a chat toolbar is shown before a chat is opened", exists(TITLE))
    val chat = alice.chatList.open("bob")
    val e2ee = chat.itemId("bob's encryption info") { it.content is CIContent.RcvDirectE2EEInfo || it.content is CIContent.SndDirectE2EEInfo }
    poll(what = "bob's name in the toolbar, `Your contact` in the banner and quantum resistant encryption info") {
      textsOf(TITLE).let { t -> "bob" in t && FULL_NAME !in t } && seesText("Your contact") &&
        textsOf(Ui.Chat.Item(e2ee)).let { t -> t.any { PQ_E2EE in it } && t.none { "end-to-end encryption" in it } }
    }
    bob sends """/_profile 1 {"displayName":"bob","fullName":"$FULL_NAME"}"""
    bob.eventuallyMatching(Regex("user full name changed to $FULL_NAME \\(your 1 contacts are notified.*\\)"), CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's full name under his name in the toolbar") { textsOf(TITLE).let { "bob" in it && FULL_NAME in it } }
    assertFalse("contact info is open before the title is clicked", exists(Ui.Info.Alias))
    chat.info()
    poll(what = "bob's contact info with the contact name field") { exists(Ui.Info.Alias) && infoNameText() == "bob" }
    closeContactInfo()
  }

  @Test fun chatBanner_contextText_perChatType() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.open("bob")
    poll(what = "`Your contact` in bob's banner") { seesText("Your contact") && !seesText("Bot") }
    backToChatList()
    val team = emptyGroup(alice, "team")
    val e2ee = team.itemId("the encryption info of #team") { it.content is CIContent.SndGroupE2EEInfo }
    poll(what = "`team` in the toolbar, `Your group` in the banner and the encryption info without quantum resistance") {
      "team" in textsOf(TITLE) && seesText("Your group") && !seesText("Your contact") &&
        textsOf(Ui.Chat.Item(e2ee)).let { t -> t.any { GROUP_E2EE in it } && t.none { "quantum resistant" in it } }
    }
    backToChatList()
    hostGroup(bob, "club", "alice")
    joinGroupFromList(alice, bob, "club")
    alice.chatList.open("club")
    poll(what = "`Group` in the banner of bob's #club") { "club" in textsOf(TITLE) && seesText("Group") && !seesText("Your group") }
    backToChatList()
    bob.becomeBot()
    alice.chatList.open("bob")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Bot` instead of `Your contact` in bob's banner once his profile says he is a bot") {
      seesText("Bot") && !seesText("Your contact")
    }
  }

  @Test fun toolbarMenu_direct_items() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.open("bob")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice's contact with bob ready") { contactOf("bob").ready }
    openBarMenu()
    poll(what = "`Search` and `Mute` in bob's toolbar menu, without `Invite members`") {
      exists(SEARCH) && ntfLabel() == "Mute" && !exists(ADD_MEMBERS)
    }
    click(NTF)
    poll(what = "bob muted") { contactOf("bob").chatSettings.enableNtfs == MsgFilter.None }
    openBarMenu()
    poll(what = "`Unmute` in bob's toolbar menu") { ntfLabel() == "Unmute" }
    click(NTF)
    poll(what = "bob unmuted") { contactOf("bob").chatSettings.enableNtfs == MsgFilter.All }
    openBarMenu()
    poll(what = "`Mute` in bob's toolbar menu again") { ntfLabel() == "Mute" }
    click(SEARCH)
    waitFor(Ui.Chat.Bar.part("search"))
  }

  @Test fun toolbarMenu_group_items() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    emptyGroup(alice, "team")
    openBarMenu()
    poll(what = "`Invite members`, `Search` and `Mute` in the toolbar menu of alice's #team") {
      exists(ADD_MEMBERS) && exists(SEARCH) && ntfLabel() == "Mute"
    }
    click(ADD_MEMBERS)
    poll(what = "the invite members sheet listing bob") { ModalManager.end.hasModalsOpen() && exists(Ui.InviteMembers.Contact(contactOf("bob").contactId)) }
    escape()
    poll(what = "the invite members sheet to close") { !ModalManager.end.hasModalsOpen() }
    cycleGroupNtfs("team")
    backToChatList()
    hostGroup(bob, "club", "alice")
    joinGroupFromList(alice, bob, "club")
    alice.chatList.open("club")
    openBarMenu()
    poll(what = "`Search` and `Mute` without `Invite members` or `Group link` in the toolbar menu of bob's #club") {
      exists(SEARCH) && ntfLabel() == "Mute" && !exists(ADD_MEMBERS) && !seesText("Group link") && !seesText("Invite members")
    }
    click(SEARCH)
    waitFor(Ui.Chat.Bar.part("search"))
  }

  private fun UiScope.cycleGroupNtfs(group: String) {
    openBarMenu()
    poll(what = "`Mute` in the toolbar menu of #$group") { ntfLabel() == "Mute" }
    click(NTF)
    poll(what = "#$group muted to mentions only") { groupInfoOf(group).chatSettings.enableNtfs == MsgFilter.Mentions }
    openBarMenu()
    poll(what = "`Mute all` in the toolbar menu of #$group") { ntfLabel() == "Mute all" }
    click(NTF)
    poll(what = "#$group muted") { groupInfoOf(group).chatSettings.enableNtfs == MsgFilter.None }
    openBarMenu()
    poll(what = "`Unmute` in the toolbar menu of #$group") { ntfLabel() == "Unmute" }
    click(NTF)
    poll(what = "#$group unmuted") { groupInfoOf(group).chatSettings.enableNtfs == MsgFilter.All }
  }

  private fun UiScope.openBarMenu() {
    hideKeyboard()
    click(Ui.Chat.BarMenu)
    waitFor(NTF)
  }

  private fun UiScope.ntfLabel(): String? = if (exists(NTF)) textsOf(NTF).firstOrNull() else null

  private companion object {
    val TITLE = Ui.Chat.Bar.part("title")
    val SEARCH = Ui.Chat.BarMenuItem("search")
    val NTF = Ui.Chat.BarMenuItem("ntf")
    val ADD_MEMBERS = Ui.Chat.BarMenuItem("addMembers")
    const val FULL_NAME = "Robert Brown"
    const val PQ_E2EE = "protected by quantum resistant e2e encryption"
    const val GROUP_E2EE = "protected by end-to-end encryption with perfect forward secrecy"
  }
}
