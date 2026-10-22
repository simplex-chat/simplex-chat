package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.closeTopScreen
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.labelled
import chat.simplex.app.ui.harness.pickerRowInert
import chat.simplex.app.ui.harness.pickerRowOpens
import chat.simplex.app.ui.harness.unreadBadge
import chat.simplex.app.ui.harness.userIdOf
import chat.simplex.common.model.GroupProfile
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.database.stopChatAsync
import kotlinx.coroutines.runBlocking
import org.junit.Ignore
import org.junit.Test

class UserPickerTest : UiTest() {
  @Test fun userPickerSwitchProfile() = uiTest {
    val alice = app("alice")
    runBlocking { chatController.apiNewGroup(null, incognito = false, GroupProfile("team", "", null)) } ?: error("apiNewGroup failed")
    val aliceNotes = alice.chatList.row("Private notes").chatId("Private notes")
    alice.userPicker().profiles().apply { add("work"); close() }
    poll(what = "work's chat list without alice's chats") {
      val notes = chatModel.chats.value.firstOrNull { it.chatInfo.chatViewName == "Private notes" }?.id
      notes != null && notes != aliceNotes && exists(Ui.ChatList.Row(notes)) && !exists(Ui.ChatList.Row(aliceNotes))
    }
    alice.userPicker().switchTo("alice")
    alice.chatList.row("team")
  }

  @Test fun userPickerUnreadBadgePerUser() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.userPicker().profiles().apply { add("work"); close() }
    connect(bob)
    val aliceId = userIdOf("alice"); val workId = userIdOf("work")
    alice.userPicker().switchTo("alice")
    alice.userPicker()
    poll(what = "work's avatar in the user picker without a badge") { exists(Ui.UserPicker.User(workId)) && unreadBadge(Ui.UserPicker.User(workId)) == null }
    bob sends "@work one"
    bob sends "@work two"
    bob sends "@alice three"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "badge 2 on work's avatar") { unreadBadge(Ui.UserPicker.User(workId)) == "2" }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's message unread in alice's chat") { alice.chatList.chat("bob").chatStats.unreadCount == 1 }
    check(unreadBadge(Ui.UserPicker.User(aliceId)) == null) { "the active user alice has an unread badge in the user picker" }
    escape()
    poll(what = "the user picker to close") { !exists(Ui.UserPicker.Sheet) }
    alice.userPicker().switchTo("work")
    alice.userPicker()
    poll(what = "badge 1 on alice's avatar and none on the active work's") {
      unreadBadge(Ui.UserPicker.User(aliceId)) == "1" && exists(Ui.UserPicker.User(workId)) && unreadBadge(Ui.UserPicker.User(workId)) == null
    }
    click(Ui.UserPicker.Profiles)
    poll(what = "badge 1 on alice's row in the profiles list and none on work's") {
      unreadBadge(Ui.Profiles.User(aliceId)) == "1" && exists(Ui.Profiles.User(workId)) && unreadBadge(Ui.Profiles.User(workId)) == null
    }
  }

  @Test fun userPickerOpensAndShowsCurrentUser() = uiTest {
    val alice = app("alice")
    val aliceId = userIdOf("alice")
    alice.userPicker()
    poll(what = "the user picker with alice and the rows `Create SimpleX address`, `Chat preferences`, `Your chat profiles`, `$DEVICES` and `Settings`") {
      labelled(Ui.UserPicker.User(aliceId), "alice") && labelled(Ui.UserPicker.Address, "Create SimpleX address") &&
        labelled(Ui.UserPicker.ChatPreferences, "Chat preferences") && labelled(Ui.UserPicker.Profiles, "Your chat profiles") &&
        labelled(devicesRow, DEVICES) && labelled(Ui.UserPicker.Settings, "Settings")
    }
    click(Ui.UserPicker.User(aliceId))
    poll(what = "`Your current profile` with alice, and the user picker closed") {
      seesText("Your current profile") && editableText(Ui.Profile.Name) == "alice" && !exists(Ui.UserPicker.Sheet)
    }
    closeTopScreen("the profile editor") { !exists(Ui.Profile.Name) }

    pickerRowOpens(alice, Ui.UserPicker.ChatPreferences, "`Your preferences`") { seesText("Your preferences") }
    closeTopScreen("`Your preferences`") { !seesText("Your preferences") }

    pickerRowOpens(alice, Ui.UserPicker.Settings, "`Your settings`") { seesText("Your settings") && exists(Ui.Settings.Appearance) }
    closeTopScreen("`Your settings`") { !exists(Ui.Settings.Appearance) }

    if (appPlatform.isDesktop) {
      pickerRowOpens(alice, devicesRow, "`Link a mobile` with `This device name`") { seesText("This device name") }
    }
  }

  @Ignore("plan §15.4 item 55: disabled user picker rows are merged into the picker's clickable container, so the harness cannot find them")
  @Test fun userPickerDisabledWhenChatStopped() = uiTest {
    val alice = app("alice")
    val aliceId = userIdOf("alice")
    pickerRowOpens(alice, Ui.UserPicker.ChatPreferences, "`Your preferences` while chat runs") { seesText("Your preferences") }
    closeTopScreen("`Your preferences`") { !seesText("Your preferences") }

    runBlocking { stopChatAsync(chatModel) }
    poll(what = "chat stopped") { chatModel.chatRunning.value == false && exists(Ui.ChatList.Stopped) }
    pickerRowInert(alice, Ui.UserPicker.User(aliceId), "`Your current profile`") { exists(Ui.Profile.Name) }
    pickerRowInert(alice, Ui.UserPicker.Address, "the address screen") { exists(Ui.Address.Create) || exists(Ui.Address.Share) }
    pickerRowInert(alice, Ui.UserPicker.ChatPreferences, "`Your preferences`") { seesText("Your preferences") }
    pickerRowInert(alice, Ui.UserPicker.Profiles, "`Your chat profiles`") { exists(Ui.Profiles.Add) }
    pickerRowInert(alice, devicesRow, "`$DEVICES`") { seesText("This device name") || seesText("Connect to desktop") }
    pickerRowOpens(alice, Ui.UserPicker.Settings, "`Your settings` with chat stopped") { seesText("Your settings") && exists(Ui.Settings.Appearance) }
  }

  private companion object {
    val DEVICES: String get() = if (appPlatform.isDesktop) "Link a mobile" else "Use from desktop"
  }

  private val devicesRow get() = if (appPlatform.isAndroid) Ui.UserPicker.UseFromDesktop else Ui.UserPicker.LinkMobile
}
