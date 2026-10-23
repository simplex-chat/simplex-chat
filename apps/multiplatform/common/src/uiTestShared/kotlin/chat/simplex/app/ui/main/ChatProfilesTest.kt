package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.PROFILES_SEARCH
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitAppIdle
import chat.simplex.app.ui.harness.closeProfilesScreen
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.hideUserViaApi
import chat.simplex.app.ui.harness.labelled
import chat.simplex.app.ui.harness.newGroupsViaApi
import chat.simplex.app.ui.harness.openProfileMenu
import chat.simplex.app.ui.harness.sendUntilAuthFailed
import chat.simplex.app.ui.harness.typePassword
import chat.simplex.app.ui.harness.userHidden
import chat.simplex.app.ui.harness.userIdOf
import chat.simplex.app.ui.harness.userShowsNtfs
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.AlertManager
import org.junit.Test

private const val MUTE_ALERT = "ShowMuteProfileAlert"
private const val HIDDEN_NOTICE = "ShowHiddenProfilesNotice"
private const val SECRET_PASSWORD = "secret-pass-7"

class ChatProfilesTest : UiTest() {
  @Test fun profilesAddProfile() = uiTest {
    val alice = app("alice")
    val profiles = alice.userPicker().profiles()
    waitFor(Ui.Profiles.Add)
    poll(what = "the profiles list with `alice` and without `work`") { seesText("alice") && !seesText("work") }
    profiles.add("work")
    poll(what = "the profiles list with `alice` and `work`") { exists(Ui.Profiles.Add) && seesText("alice") && seesText("work") }
  }

  @Test fun profilesMuteUnmute() = uiTest {
    val alice = app("alice")
    appPrefs.showMuteProfileAlert.set(true)
    alice.userPicker().profiles().apply { add("work"); close() }
    val workId = userIdOf("work")
    alice.userPicker().switchTo("alice")
    alice.userPicker().profiles()
    waitFor(Ui.Profiles.User(workId))
    poll(what = "work not muted and the mute alert stored as on") { userShowsNtfs(workId) == true && storedSetting(MUTE_ALERT) == "true" }
    openProfileMenu(workId, "mute")
    check(!exists(Ui.Profiles.UserMenu(workId, "unmute"))) { "`Unmute` offered for a profile that is not muted" }
    click(Ui.Profiles.UserMenu(workId, "mute"))
    awaitMutedAlert()
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "work muted and the alert closed") { userShowsNtfs(workId) == false && !AlertManager.shared.hasAlertsShown() }
    check(storedSetting(MUTE_ALERT) == "true") { "`OK` on the mute alert turned the alert off" }

    openProfileMenu(workId, "unmute")
    check(!exists(Ui.Profiles.UserMenu(workId, "mute"))) { "`Mute` offered for a muted profile" }
    click(Ui.Profiles.UserMenu(workId, "unmute"))
    poll(what = "work unmuted") { userShowsNtfs(workId) == true }

    openProfileMenu(workId, "mute")
    click(Ui.Profiles.UserMenu(workId, "mute"))
    awaitMutedAlert()
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "work muted and the mute alert stored as off") {
      userShowsNtfs(workId) == false && storedSetting(MUTE_ALERT) == "false" && !AlertManager.shared.hasAlertsShown()
    }

    openProfileMenu(workId, "unmute")
    click(Ui.Profiles.UserMenu(workId, "unmute"))
    poll(what = "work unmuted again") { userShowsNtfs(workId) == true }
    openProfileMenu(workId, "mute")
    click(Ui.Profiles.UserMenu(workId, "mute"))
    poll(what = "work muted again") { userShowsNtfs(workId) == false }
    openProfileMenu(workId, "unmute")
    check(!AlertManager.shared.hasAlertsShown() && !seesText("Muted when inactive!")) { "the mute alert was shown after `Don't show again`" }

    restart().userPicker().profiles()
    waitFor(Ui.Profiles.User(workId))
    openProfileMenu(workId, "unmute")
    check(userShowsNtfs(workId) == false && !exists(Ui.Profiles.UserMenu(workId, "mute"))) { "work is not muted after restart" }
  }

  @Test fun profilesDeleteActiveSwitchesToNext() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val aliceId = userIdOf("alice")
    alice.userPicker().profiles().apply { add("work"); close() }
    newGroupsViaApi("team")
    val workId = userIdOf("work")
    alice.userPicker().switchTo("alice")
    alice.chatList.row("bob")
    val profiles = alice.userPicker().profiles()
    poll(what = "alice and work in the profiles list") { exists(Ui.Profiles.User(aliceId)) && exists(Ui.Profiles.User(workId)) }
    openProfileMenu(aliceId, "delete")
    click(Ui.Profiles.UserMenu(aliceId, "delete"))
    val withConnections = Ui.Alert.Option(Ui.AlertSource.Shared, "deleteWithConnections")
    poll(what = "the `Delete chat profile?` alert with `Profile and server connections`") { seesText("Delete chat profile?") && exists(withConnections) }
    click(withConnections)
    poll(what = "work active, alice gone from the users and the profiles list") {
      chatModel.currentUser.value?.userId == workId && chatModel.users.none { it.user.userId == aliceId } &&
        !exists(Ui.Profiles.User(aliceId)) && exists(Ui.Profiles.User(workId))
    }
    profiles.close()
    alice.chatList.row("team")
    poll(what = "alice's contact bob gone from the chats") { chatModel.chats.value.none { it.chatInfo.chatViewName == "bob" } }
    sendUntilAuthFailed(bob, "alice")
  }

  @Test fun profilesListShowsAllVisible() = uiTest {
    val alice = app("alice")
    newGroupsViaApi("team")
    val aliceId = userIdOf("alice")
    alice.userPicker().profiles().apply { add("work"); close() }
    newGroupsViaApi("office")
    val workId = userIdOf("work")
    alice.userPicker().profiles().apply { add("secret"); close() }
    val secretId = userIdOf("secret")
    hideUserViaApi(secretId, SECRET_PASSWORD)
    alice.userPicker().switchTo("alice")
    alice.chatList.row("team")

    alice.userPicker().profiles()
    poll(what = "alice and work listed with `Add profile` and `Tap to activate profile.`, the hidden secret not") {
      exists(Ui.Profiles.User(aliceId)) && exists(Ui.Profiles.User(workId)) && !exists(Ui.Profiles.User(secretId)) &&
        exists(Ui.Profiles.Add) && seesText("Tap to activate profile.")
    }
    click(Ui.Profiles.User(workId))
    poll(what = "work active with its chats loaded") {
      chatModel.currentUser.value?.userId == workId && !chatModel.switchingUsersAndHosts.value &&
        chatModel.chats.value.any { it.chatInfo.chatViewName == "office" } && chatModel.chats.value.none { it.chatInfo.chatViewName == "team" }
    }
    typeInto(PROFILES_SEARCH, "ALI")
    poll(what = "only alice listed for `ALI`, without `Add profile`") {
      exists(Ui.Profiles.User(aliceId)) && !exists(Ui.Profiles.User(workId)) && !exists(Ui.Profiles.User(secretId)) && !exists(Ui.Profiles.Add)
    }
    closeProfilesScreen()
    alice.chatList.row("office")
    check(!seesText("team")) { "alice's group is listed after activating work" }
  }

  @Test fun profilesAddProfileInvalidName() = uiTest {
    val alice = app("alice")
    alice.userPicker().profiles()
    click(Ui.Profiles.Add)
    typeInto(Ui.NewProfile.Name, "?bob")
    waitFor(INVALID_NAME)
    click(Ui.NewProfile.Create)
    awaitAppIdle("`Create profile` with an invalid name")
    check(chatModel.users.size == 1 && exists(Ui.NewProfile.Create) && !seesText("Duplicate display name!")) { "`Create profile` acted on the invalid name `?bob`" }
    click(INVALID_NAME)
    poll(what = "the `Invalid name!` alert offering `bob`") { seesText("Invalid name!") && seesText("Correct name to bob?") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the name corrected to `bob` without the invalid-name icon") { editableText(Ui.NewProfile.Name) == "bob" && !exists(INVALID_NAME) && !seesText("Invalid name!") }

    replaceText(Ui.NewProfile.Name, "alice")
    check(!exists(INVALID_NAME)) { "`alice` is flagged as an invalid name" }
    click(Ui.NewProfile.Create)
    poll(what = "the `Duplicate display name!` alert") {
      seesText("Duplicate display name!") && seesText("You already have a chat profile with the same display name. Please choose another name.")
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the alert closed with the Create profile screen kept and no second profile") {
      !seesText("Duplicate display name!") && exists(Ui.NewProfile.Create) && chatModel.users.size == 1 && chatModel.currentUser.value?.displayName == "alice"
    }
  }

  @Test fun profilesHiddenNoticeFirstTime() = uiTest {
    val alice = app("alice")
    appPrefs.showHiddenProfilesNotice.set(true)
    poll(what = "the hidden-profiles notice stored as on") { storedSetting(HIDDEN_NOTICE) == "true" }
    val profiles = alice.userPicker().profiles()
    waitFor(Ui.Profiles.Add)
    settle()
    check(!seesText("Make profile private!")) { "the hidden-profiles notice was shown with one profile" }
    profiles.close()
    awaitPickerClosed()
    appPrefs.showHiddenProfilesNotice.set(false)
    alice.userPicker().profiles().apply { add("work"); close() }
    awaitPickerClosed()
    appPrefs.showHiddenProfilesNotice.set(true)

    alice.userPicker().profiles()
    awaitHiddenNotice()
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the notice closed and still on") { !seesText("Make profile private!") && storedSetting(HIDDEN_NOTICE) == "true" }
    profiles.close()
    awaitPickerClosed()

    alice.userPicker().profiles()
    awaitHiddenNotice()
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the notice closed and stored as off") { !seesText("Make profile private!") && storedSetting(HIDDEN_NOTICE) == "false" }
    profiles.close()
    awaitPickerClosed()

    alice.userPicker().profiles()
    waitFor(Ui.Profiles.Add)
    settle()
    check(!seesText("Make profile private!")) { "the hidden-profiles notice was shown after `Don't show again`" }
  }

  @Test fun profilesUnhideActiveRequiresPassword() = uiTest {
    val alice = app("alice")
    val aliceId = userIdOf("alice")
    alice.userPicker().profiles().apply { add("work"); close() }
    val workId = userIdOf("work")
    hideUserViaApi(workId, SECRET_PASSWORD)
    alice.userPicker().profiles()
    poll(what = "the active hidden work and alice listed") { exists(Ui.Profiles.User(workId)) && exists(Ui.Profiles.User(aliceId)) && userHidden(workId) }

    openProfileMenu(aliceId, "mute")
    check(!exists(Ui.Profiles.UserMenu(aliceId, "hide"))) { "`Hide` offered for the only visible profile" }
    click(Ui.Profiles.UserMenu(aliceId, "mute"))
    poll(what = "alice muted and the menu closed") { userShowsNtfs(aliceId) == false && !exists(Ui.Profiles.UserMenu(aliceId, "delete")) }

    openProfileMenu(workId, "unhide")
    check(!exists(Ui.Profiles.UserMenu(workId, "hide")) && !exists(Ui.Profiles.UserMenu(workId, "mute"))) { "the hidden profile's menu offers `Hide` or `Mute`" }
    click(Ui.Profiles.UserMenu(workId, "unhide"))
    poll(what = "`Unhide profile` asking for `Profile password` with `Unhide chat profile`") {
      seesText("Unhide profile") && seesText("Profile password") && exists(Ui.ProfileAction.Password) && labelled(Ui.ProfileAction.Submit, "Unhide chat profile")
    }
    typePassword(Ui.ProfileAction.Password, SECRET_PASSWORD.dropLast(1))
    click(Ui.ProfileAction.Submit)
    awaitAppIdle("`Unhide chat profile` with a wrong password")
    check(userHidden(workId) && exists(Ui.ProfileAction.Submit)) { "`Unhide chat profile` acted on a wrong password" }
    typePassword(Ui.ProfileAction.Password, SECRET_PASSWORD.takeLast(1))
    click(Ui.ProfileAction.Submit)
    poll(what = "work unhidden and the Unhide profile screen closed") { !userHidden(workId) && !exists(Ui.ProfileAction.Password) }
    poll(what = "work listed as a visible profile") { exists(Ui.Profiles.User(workId)) && chatModel.currentUser.value?.userId == workId }
  }

  private fun UiScope.awaitPickerClosed() =
    poll(what = "the user picker to close after `Your chat profiles` closed") { !exists(Ui.UserPicker.Sheet) && exists(Ui.ChatList.Search) }

  private fun UiScope.awaitHiddenNotice() =
    poll(what = "the `Make profile private!` notice with `OK` and `Don't show again`") {
      seesText("Make profile private!") && seesText("You can hide or mute a user profile - hold it for the menu.") &&
        labelled(Ui.Alert.Confirm(Ui.AlertSource.Shared), "OK") && labelled(Ui.Alert.Dismiss(Ui.AlertSource.Shared), "Don't show again")
    }

  private fun UiScope.awaitMutedAlert() =
    poll(what = "the `Muted when inactive!` alert with `OK` and `Don't show again`") {
      seesText("Muted when inactive!") && seesText("You will still receive calls and notifications from muted profiles when they are active.") &&
        "Don't show again" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    }

  private companion object {
    val INVALID_NAME = Ui.NewProfile.Name.part("invalid")
  }
}
