package chat.simplex.app.ui.back

import chat.simplex.app.ui.harness.ChatScreen
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.views.helpers.SharedContent
import chat.simplex.common.views.onboarding.OnboardingStage
import org.junit.Test

class BackKeyTest : UiTest() {
  @Test fun back_chatToChatList() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val bobId = alice.chatList.chatId("bob")
    val bobRow = Ui.ChatList.Row(bobId)
    alice.chatList.open("bob")
    click(Ui.Chat.Message)
    poll(what = "bob's chat covering the chat list, with the soft keyboard shown") { exists(bobRow) && !displayed(bobRow) && keyboardShown() }
    typeInto(Ui.Chat.Message, "draft")
    escape()
    poll(what = "the soft keyboard to hide") { !keyboardShown() }
    check(chatModel.chatId.value == bobId && exists(Ui.Chat.Bar.part("title"))) { "the first Back closed the chat" }
    escape()
    poll(what = "the chat list showing bob's row") { chatModel.chatId.value == null && !exists(Ui.Chat.Bar.part("title")) && displayed(bobRow) }
    alice.chatList.open("bob")
    poll(what = "the draft back in the message field") { editableText(Ui.Chat.Message) == "draft" }
  }

  @Test fun back_closesChatSearchBeforeChat() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice apple"
    bob sends "@alice banana"
    val bobId = alice.chatList.chatId("bob")
    val chat = alice.chatList.open("bob")
    val apple = Ui.Chat.Item(chat.itemId("apple"))
    val banana = Ui.Chat.Item(chat.itemId("banana"))
    chat.openSearch()
    typeInto(Ui.Chat.Bar.part("search"), "apple")
    poll(what = "search results showing `apple` without `banana`") { exists(apple) && !exists(banana) }
    hideKeyboard()
    escape()
    poll(what = "the search to close and both messages to show again") {
      !exists(Ui.Chat.Bar.part("search")) && exists(Ui.Chat.Bar.part("title")) && exists(apple) && exists(banana)
    }
    check(chatModel.chatId.value == bobId) { "the first Back left the chat" }
    escape()
    poll(what = "the chat list") { chatModel.chatId.value == null && !exists(Ui.Chat.Bar.part("title")) }
  }

  @Test fun back_exitsSelectionMode() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice one"
    bob sends "@alice two"
    val bobId = alice.chatList.chatId("bob")
    val chat = alice.chatList.open("bob")
    val two = chat.itemId("two")
    chat.menu("one", "select")
    click(Ui.Chat.Item(two))
    poll(what = "two messages selected") { exists(Ui.Chat.Selection.part("title")) && "Selected 2" in textsOf(Ui.Chat.Selection.part("title")) }
    escape()
    poll(what = "selection mode to end") { !exists(Ui.Chat.Selection.part("title")) && exists(Ui.Chat.Bar.part("title")) }
    check(chatModel.chatId.value == bobId) { "Back in selection mode left the chat" }
  }

  @KnownAppIssue("15.5 item 6", "the invite members sheet to close to team's chat", intermittent = true)
  @Test fun back_closesContactAndGroupInfo() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val bobId = alice.chatList.chatId("bob")
    val bobInvitee = Ui.InviteMembers.Contact((alice.chatList.chat("bob").chatInfo as ChatInfo.Direct).contact.contactId)
    alice.chatList.open("bob").info()
    waitFor(Ui.Info.Alias)
    escape()
    poll(what = "contact info to close to bob's chat") { !ModalManager.end.hasModalsOpen() && !exists(Ui.Info.Alias) && chatModel.chatId.value == bobId }
    escape()
    poll(what = "the chat list") { chatModel.chatId.value == null && !exists(Ui.Chat.Bar.part("title")) }
    alice.newChat().createGroup("team")
    val teamId = alice.chatList.chatId("team")
    poll(what = "the invite members sheet listing bob over team") { chatModel.chatId.value == teamId && exists(bobInvitee) }
    hideKeyboard()
    escape()
    poll(what = "the invite members sheet to close to team's chat") {
      !exists(bobInvitee) && !ModalManager.end.hasModalsOpen() && chatModel.chatId.value == teamId && exists(Ui.Chat.Bar.part("title"))
    }
    ChatScreen(this).info()
    waitFor(Ui.Info.Alias)
    escape()
    poll(what = "group info to close to team's chat") { !ModalManager.end.hasModalsOpen() && !exists(Ui.Info.Alias) && chatModel.chatId.value == teamId }
  }

  @Test fun back_clearsChatListSearch() = uiTest {
    app("alice")
    typeInto(Ui.ChatList.Search, "zz")
    hideKeyboard()
    escape()
    poll(what = "the search text to clear") { editableText(Ui.ChatList.Search) == "" }
    check(!mainActivityFinished()) { "Back with search text finished MainActivity" }
    backFromRoot()
  }

  @Test fun back_closesUserPicker() = uiTest {
    val alice = app("alice")
    alice.userPicker()
    waitFor(Ui.UserPicker.Sheet)
    escape()
    poll(what = "the user picker to hide") { !exists(Ui.UserPicker.Sheet) }
    check(chatModel.chatId.value == null && !ModalManager.start.hasModalsOpen() && !mainActivityFinished()) { "Back did not return to the chat list" }
  }

  @Test fun back_closesNewChatSheet() = uiTest {
    val alice = app("alice")
    alice.newChat()
    waitFor(Ui.NewChat.CreateLink)
    escape()
    poll(what = "the new chat sheet to close") { !chatModel.newChatSheetVisible.value && !ModalManager.start.hasModalsOpen() && !exists(Ui.NewChat.CreateLink) }
    check(!mainActivityFinished()) { "Back on the new chat sheet finished MainActivity" }
  }

  @Test fun back_closesModalsOneByOne() = uiTest {
    val alice = app("alice")
    alice.appearance()
    poll(what = "Appearance over Your settings") { ModalManager.start.openModalCount() == 2 && exists(Ui.Appearance.ColorMode) }
    escape()
    poll(what = "Your settings again") { ModalManager.start.openModalCount() == 1 && exists(Ui.Settings.Appearance) && !exists(Ui.Appearance.ColorMode) }
    escape()
    poll(what = "the chat list with nothing open") { !ModalManager.start.hasModalsOpen() && !exists(Ui.Settings.Appearance) && !exists(Ui.UserPicker.Sheet) }
    check(!mainActivityFinished()) { "Back on Your settings finished MainActivity" }
    backFromRoot()
  }

  @Test fun back_onChatListRootFinishesActivity() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob")
    val ctrl = chatController.getChatCtrl()
    backFromRoot()
    relaunch {
      alice.chatList.row("bob")
      check(chatModel.chatRunning.value == true && chatController.getChatCtrl() == ctrl) { "the core was restarted" }
    }
  }

  @Test fun back_duringForward_returnsToSourceChat() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hello"
    val bobId = alice.chatList.chatId("bob")
    alice.chatList.open("bob").menu("hello", "forward")
    poll(what = "the forward list") { chatModel.sharedContent.value is SharedContent.Forward && !exists(Ui.Chat.Bar.part("title")) }
    escape()
    poll(what = "bob's chat again") { chatModel.sharedContent.value == null && chatModel.chatId.value == bobId && exists(Ui.Chat.Bar.part("title")) }
    check(!mainActivityFinished()) { "Back during a forward finished MainActivity" }
  }

  @Test fun back_onboardingProfile_returnsToWelcome() = uiTest {
    app(null)
    click(Ui.Onboarding.GetStarted)
    waitFor(Ui.Onboarding.ProfileName)
    hideKeyboard()
    escape()
    poll(what = "the welcome screen") {
      appPrefs.onboardingStage.get() == OnboardingStage.Step1_SimpleXInfo && exists(Ui.Onboarding.GetStarted) && !exists(Ui.Onboarding.ProfileName)
    }
    check(chatModel.currentUser.value == null && chatModel.users.isEmpty()) { "a profile was created" }
  }

  @Test fun back_onboardingYourNetwork_finishesActivity() = uiTest {
    app(null)
    click(Ui.Onboarding.GetStarted)
    typeInto(Ui.Onboarding.ProfileName, "alice")
    hideKeyboard()
    poll(what = "Create profile to be enabled") { isEnabled(Ui.Onboarding.CreateProfile) }
    click(Ui.Onboarding.CreateProfile)
    poll(timeoutMs = 30_000, what = "the Your network step") { appPrefs.onboardingStage.get() == OnboardingStage.Step3_ChooseServerOperators && seesText("Your network") }
    backFromRoot()
    check(appPrefs.onboardingStage.get() == OnboardingStage.Step3_ChooseServerOperators) { "Back changed the onboarding stage" }
    relaunch {
      poll(what = "Your network again") { seesText("Your network") }
      check(appPrefs.onboardingStage.get() == OnboardingStage.Step3_ChooseServerOperators) { "relaunch changed the onboarding stage" }
    }
  }
}
