package chat.simplex.app.ui.desktop

import androidx.compose.ui.input.key.Key
import androidx.compose.ui.test.onRoot
import androidx.compose.ui.test.performKeyInput
import androidx.compose.ui.test.pressKey
import androidx.compose.ui.test.withKeyDown
import chat.simplex.app.ui.harness.ChatDataScreen
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.LockScreen
import chat.simplex.app.ui.harness.LockScreen.Companion.CONFIRM_PASSCODE
import chat.simplex.app.ui.harness.LockScreen.Companion.ENTER_PASSCODE
import chat.simplex.app.ui.harness.LockScreen.Companion.NEW_PASSCODE
import chat.simplex.app.ui.harness.LockScreen.Companion.PASSCODE_SET
import chat.simplex.app.ui.harness.PasscodeScreen
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.chatRunningInCore
import chat.simplex.app.ui.harness.closeDeveloperTools
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.developerTools
import chat.simplex.app.ui.harness.newGroupsViaApi
import chat.simplex.app.ui.harness.openProfileMenu
import chat.simplex.app.ui.harness.userIdOf
import chat.simplex.app.ui.harness.userShowsNtfs
import chat.simplex.app.ui.harness.yourPrivacy
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.AppLock
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.AlertManager
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Test
import java.util.Base64

class LockDesktopTest : UiTest() {
  @Test fun lock_enable_setsPasscode() = uiTest {
    val alice = app("alice")
    val privacy = yourPrivacy(alice)
    check("Off" in textsOf(Ui.Privacy.Lock)) { "SimpleX Lock before enabling: ${textsOf(Ui.Privacy.Lock)}" }
    val lock = privacy.lock()
    check(!exists(Ui.Lock.Delay) && !exists(Ui.Lock.ChangePasscode) && !exists(Ui.Lock.SelfDestruct)) { "lock options shown while the lock is off" }
    check(storedSetting(PERFORM_LA) != "true" && storedSetting(APP_PASSCODE) == null) { "the lock is stored as on before enabling" }
    click(Ui.Lock.Enable)
    lock.passcode.await(NEW_PASSCODE)
    lock.passcode.enter(CODE)
    click(Ui.Passcode.Cancel)
    poll(what = "the New Passcode screen to close") { lock.passcode.gone() && exists(Ui.Lock.Enable) }
    check(storedSetting(PERFORM_LA) != "true" && storedSetting(APP_PASSCODE) == null && !exists(Ui.Lock.Delay)) { "Cancel turned the lock on" }
    click(Ui.Lock.Enable)
    lock.passcode.await(NEW_PASSCODE)
    lock.passcode.enter(CODE)
    lock.passcode.submit()
    lock.passcode.await(CONFIRM_PASSCODE)
    assertEquals("Confirm", lock.passcode.submitLabel())
    lock.passcode.enter(MISMATCH)
    lock.passcode.submit()
    check(seesText(CONFIRM_PASSCODE) && !AlertManager.shared.hasAlertsShown() && storedSetting(APP_PASSCODE) == null) { "a mismatched confirmation was accepted" }
    lock.passcode.backspace()
    lock.passcode.backspace()
    lock.passcode.enter(CODE.takeLast(2))
    lock.passcode.submit()
    lock.passcode.confirmAlert(PASSCODE_SET)
    poll(what = "the lock stored as on with the passcode") { storedSetting(PERFORM_LA) == "true" && storedSetting(LOCK_DELAY) == "30" && decoded(APP_PASSCODE) == CODE }
    check(textOf(Ui.Lock.Delay) == "30 seconds" && exists(Ui.Lock.ChangePasscode) && exists(Ui.Lock.SelfDestruct)) { "lock options after enabling: ${texts().take(30)}" }
    escape()
    poll(what = "Your privacy showing SimpleX Lock as Passcode") { !exists(Ui.Lock.Enable) && "Passcode" in textsOf(Ui.Privacy.Lock) }
  }

  @Test fun lock_screenOnStartup_wrongThenCorrect() = uiTest {
    val alice = app("alice")
    yourPrivacy(alice).lock().apply { enable(CODE); close() }
    val current = restart()
    val passcode = PasscodeScreen(this)
    passcode.await(ENTER_PASSCODE)
    check(seesText("Unlock") && passcode.submitLabel() == "Submit" && AppLock.userAuthorized.value == false) { "the startup passcode screen: ${texts().take(30)}" }
    passcode.enter(WRONG)
    passcode.submit()
    passcode.authFailed()
    poll(what = "the Unlock button instead of the app") { exists(Ui.Lock.Unlock) && passcode.gone() }
    check(AppLock.userAuthorized.value == false) { "a wrong passcode unlocked the app" }
    click(Ui.Lock.Unlock)
    passcode.await(ENTER_PASSCODE)
    passcode.enter(CODE)
    passcode.submit()
    awaitUnlocked(passcode)
    val notes = current.chatList.open(NOTES)
    notes.send(NOTE)
    notes.itemId(NOTE)
  }

  @Test fun lock_changePasscode_newPasscodeUnlocks() = uiTest {
    val alice = app("alice")
    val lock = yourPrivacy(alice).lock()
    lock.enable(CODE)
    click(Ui.Lock.ChangePasscode)
    lock.passcode.await(CURRENT_PASSCODE)
    lock.passcode.enter(WRONG)
    lock.passcode.submit()
    lock.passcode.authFailed()
    check(decoded(APP_PASSCODE) == CODE && !seesText(NEW_PASSCODE)) { "a wrong current passcode opened the change" }
    click(Ui.Lock.ChangePasscode)
    lock.passcode.await(CURRENT_PASSCODE)
    lock.passcode.enter(CODE)
    lock.passcode.submit()
    lock.passcode.await(NEW_PASSCODE)
    check(seesText("App passcode")) { "the New Passcode screen does not name the app passcode: ${texts().take(30)}" }
    lock.passcode.enter(NEW_CODE)
    lock.passcode.submit()
    lock.passcode.await(CONFIRM_PASSCODE)
    lock.passcode.enter(NEW_CODE)
    lock.passcode.submit()
    lock.passcode.confirmAlert("Passcode changed!")
    poll(what = "the new passcode stored") { decoded(APP_PASSCODE) == NEW_CODE }
    lock.close()
    restart()
    val passcode = PasscodeScreen(this)
    passcode.await(ENTER_PASSCODE)
    passcode.enter(NEW_CODE)
    passcode.submit()
    awaitUnlocked(passcode)
  }

  @Test fun lock_disable_requiresPasscode() = uiTest {
    val alice = app("alice")
    val lock = yourPrivacy(alice).lock()
    lock.enable(CODE)
    click(Ui.Lock.Enable)
    lock.passcode.await(ENTER_PASSCODE)
    check(seesText("Disable SimpleX Lock")) { "the passcode prompt does not say it disables the lock: ${texts().take(30)}" }
    lock.passcode.enter(WRONG)
    lock.passcode.submit()
    lock.passcode.authFailed()
    check(storedSetting(PERFORM_LA) == "true" && decoded(APP_PASSCODE) == CODE && exists(Ui.Lock.ChangePasscode)) { "a wrong passcode turned the lock off" }
    click(Ui.Lock.Enable)
    lock.passcode.await(ENTER_PASSCODE)
    lock.passcode.enter(CODE)
    lock.passcode.submit()
    poll(what = "the lock stored as off and its options hidden") {
      lock.passcode.gone() && storedSetting(PERFORM_LA) == "false" && storedSetting(APP_PASSCODE) == null && !exists(Ui.Lock.ChangePasscode) && !exists(Ui.Lock.Delay)
    }
    lock.close()
    val after = restart()
    poll(what = "the app open without a passcode screen") { AppLock.userAuthorized.value == true && after.chatList.shown(NOTES) }
    check(!seesText(ENTER_PASSCODE) && !exists(Ui.Passcode.Submit)) { "a passcode screen after the lock was turned off" }
  }

  @Test fun lock_relocksAfterDelay() = uiTest {
    val alice = app("alice")
    val lock = yourPrivacy(alice).lock()
    lock.enable(CODE)
    lock.close()
    hideAndReturn(hiddenMs = 29_000)
    settle()
    check(AppLock.userAuthorized.value == true && !exists(Ui.Passcode.Submit)) { "the app locked on return after 29 seconds hidden, before the 30 second delay" }
    val passcode = PasscodeScreen(this)
    hideAndReturn(hiddenMs = 31_000)
    passcode.await(ENTER_PASSCODE)
    check(AppLock.userAuthorized.value == false) { "the app did not lock after 31 seconds hidden" }
    passcode.enter(CODE)
    passcode.submit()
    awaitUnlocked(passcode)
    val again = yourPrivacy(alice).lock()
    click(Ui.Lock.Delay)
    for ((delay, name) in DELAYS) {
      check(name in textsOf(Ui.Lock.Delay.option(delay))) { "Lock after option $delay is not `$name`: ${textsOf(Ui.Lock.Delay.option(delay))}" }
    }
    click(Ui.Lock.Delay.option(0))
    poll(what = "Immediately shown and stored") { textOf(Ui.Lock.Delay) == "Immediately" && storedSetting(LOCK_DELAY) == "0" }
    again.close()
    hideAndReturn()
    passcode.await(ENTER_PASSCODE)
    check(AppLock.userAuthorized.value == false) { "the passcode screen shows but the app is authorized" }
    passcode.enter(CODE)
    passcode.submit()
    awaitUnlocked(passcode)
  }

  @Test fun selfDestruct_triggeredOnLockScreen() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice $BEFORE_WIPE"
    poll(what = "bob's message on the row") { BEFORE_WIPE in alice.chatList.rowTexts("bob") }
    val lock = yourPrivacy(alice).lock()
    lock.enable(CODE)
    check(storedSetting(SELF_DESTRUCT) != "true" && !exists(Ui.Lock.SelfDestructName)) { "self-destruct is on before enabling" }
    click(Ui.Lock.SelfDestruct)
    lock.passcode.await(CURRENT_PASSCODE)
    lock.passcode.enter(CODE)
    lock.passcode.submit()
    lock.passcode.await(SET_PASSCODE)
    check(seesText("Enable self-destruct passcode")) { "the Set passcode screen does not name self-destruct: ${texts().take(30)}" }
    lock.passcode.enter(CODE)
    lock.passcode.submit()
    check(seesText(SET_PASSCODE) && !seesText(CONFIRM_PASSCODE) && lock.passcode.submitLabel() == "Save") { "the app passcode was accepted as the self-destruct passcode" }
    lock.passcode.clear()
    lock.passcode.enter(SD_CODE)
    lock.passcode.submit()
    lock.passcode.await(CONFIRM_PASSCODE)
    lock.passcode.enter(SD_CODE)
    lock.passcode.submit()
    lock.passcode.confirmAlert("Self-destruct passcode enabled!")
    poll(what = "self-destruct stored as on with its passcode") { storedSetting(SELF_DESTRUCT) == "true" && decoded(SD_PASSCODE) == SD_CODE && exists(Ui.Lock.SelfDestructName) }
    typeInto(Ui.Lock.SelfDestructName, WIPED_NAME)
    poll(what = "the new display name stored") { storedSetting(SD_NAME) == WIPED_NAME }
    lock.close()
    restart()
    val passcode = PasscodeScreen(this)
    passcode.await(ENTER_PASSCODE)
    passcode.enter(SD_CODE)
    passcode.submit()
    poll(timeoutMs = 30_000, what = "the app opened as $WIPED_NAME without bob") {
      AppLock.userAuthorized.value == true && chatModel.currentUser.value?.displayName == WIPED_NAME && chatModel.chats.value.none { it.chatInfo.chatViewName == "bob" }
    }
    assertEquals(listOf(WIPED_NAME), coreUserNames())
    check(decoded(APP_PASSCODE) == SD_CODE && storedSetting(SD_PASSCODE) == null && storedSetting(SELF_DESTRUCT) == "false" && storedSetting(SD_NAME) == null) {
      "after the wipe: app passcode ${decoded(APP_PASSCODE)}, self-destruct passcode stored ${storedSetting(SD_PASSCODE) != null}, self-destruct ${storedSetting(SELF_DESTRUCT)}, name ${storedSetting(SD_NAME)}"
    }
    restart()
    passcode.await(ENTER_PASSCODE)
    passcode.enter(SD_CODE)
    passcode.submit()
    awaitUnlocked(passcode)
    check(chatModel.currentUser.value?.displayName == WIPED_NAME && coreUserNames() == listOf(WIPED_NAME)) { "the self-destruct code wiped the data again: ${coreUserNames()}" }
  }

  @Test fun lock_protectedActionsPrompt() = uiTest {
    val alice = app("alice")
    yourPrivacy(alice).lock().apply { enable(CODE); close() }
    val passcode = PasscodeScreen(this)
    developerTools(alice)
    click(Ui.DevTools.Console)
    passcode.await("Open chat console")
    check(seesText(LOG_IN_REASON) && !exists(Ui.Chat.Message)) { "the console prompt: ${texts().take(30)}" }
    passcode.enter(CODE)
    passcode.submit()
    poll(what = "the chat console after the passcode") { passcode.gone() && !exists(Ui.Lock.Unlock) && exists(Ui.Chat.Message) }
    escape()
    poll(what = "the chat console to close") { !exists(Ui.Chat.Message) && exists(Ui.DevTools.Console) }
    closeDeveloperTools()
    alice.settings()
    click(Ui.Settings.Migrate)
    passcode.await("Open migration screen")
    passcode.enter(WRONG)
    passcode.submit()
    poll(what = "the passcode screen closed with only Unlock left") { passcode.gone() && exists(Ui.Lock.Unlock) }
    check(!seesText(MIGRATE_TITLE)) { "a wrong passcode opened the migration screen" }
    click(Ui.Lock.Unlock)
    passcode.await("Open migration screen")
    click(Ui.Passcode.Cancel)
    poll(what = "the passcode screen cancelled with Unlock left") { passcode.gone() && exists(Ui.Lock.Unlock) }
    check(!seesText(MIGRATE_TITLE)) { "cancelling the passcode opened the migration screen" }
    escape()
    poll(what = "Unlock closed back to Your settings") { !exists(Ui.Lock.Unlock) && exists(Ui.Settings.Database) }
    check(!seesText(MIGRATE_TITLE)) { "closing Unlock opened the migration screen" }
    click(Ui.Settings.Database)
    ChatDataScreen(this).management()
    stopChatAndConfirm()
    passcode.await("Stop chat")
    passcode.enter(WRONG)
    passcode.submit()
    passcode.authFailed()
    check(chatRunningInCore() && chatModel.chatRunning.value == true && seesText("Chat is running")) { "a wrong passcode stopped the chat" }
    stopChatAndConfirm()
    passcode.await("Stop chat")
    passcode.enter(CODE)
    passcode.submit()
    poll(what = "the chat stopped after the passcode") { passcode.gone() && seesText("Chat is stopped") && !chatRunningInCore() }
  }

  @Test fun profiles_actionsRequireAuthOnceWhenLocked() = uiTest {
    val alice = app("alice")
    yourPrivacy(alice).lock().apply { enable(CODE); close() }
    val aliceId = userIdOf("alice")
    val passcode = PasscodeScreen(this)
    val profiles = alice.userPicker().profiles()
    waitFor(Ui.Profiles.Add)
    check(!exists(Ui.Passcode.Submit) && !exists(Ui.Lock.Unlock)) { "opening Your chat profiles asked for the passcode" }
    click(Ui.Profiles.Add)
    passcode.await(CHANGE_PROFILES)
    check(!exists(Ui.NewProfile.Name)) { "the new profile screen opened before the passcode" }
    passcode.enter(CODE)
    passcode.submit()
    poll(what = "the new profile screen after the passcode") { passcode.gone() && !exists(Ui.Lock.Unlock) && exists(Ui.NewProfile.Name) }
    typeInto(Ui.NewProfile.Name, WORK)
    click(Ui.NewProfile.Create)
    poll(what = "`$WORK` active and the new profile screen closed") { chatModel.currentUser.value?.displayName == WORK && !exists(Ui.NewProfile.Create) }
    waitFor(Ui.Profiles.User(aliceId))
    openProfileMenu(aliceId, "mute")
    click(Ui.Profiles.UserMenu(aliceId, "mute"))
    poll(what = "alice muted") { userShowsNtfs(aliceId) == false }
    check(!exists(Ui.Passcode.Submit) && !exists(Ui.Lock.Unlock)) { "the second profile action asked for the passcode again" }
    profiles.close()
    alice.userPicker().profiles()
    waitFor(Ui.Profiles.User(aliceId))
    openProfileMenu(aliceId, "unmute")
    click(Ui.Profiles.UserMenu(aliceId, "unmute"))
    passcode.await(CHANGE_PROFILES)
    check(userShowsNtfs(aliceId) == false) { "alice was unmuted before the passcode" }
    passcode.enter(CODE)
    passcode.submit()
    poll(what = "alice unmuted after the passcode in the reopened profiles screen") { passcode.gone() && userShowsNtfs(aliceId) == true }
  }

  @KnownAppIssue("15.5 item 23", "waiting for the SimpleX Lock notice with `Turn on`", intermittent = true)
  @Test fun lock_noticeOfferedWithManyChats() = uiTest {
    app("alice")
    clearLockNoticeShownViaApi()
    var current = restart()
    poll(what = "the chat list after the restart") { current.chatList.shown(NOTES) }
    settle()
    check(chatModel.chats.value.size <= 3 && !AlertManager.shared.hasAlertsShown() && storedSetting(LA_NOTICE_SHOWN) == "false") {
      "with ${chatModel.chats.value.size} chats: alert shown ${AlertManager.shared.hasAlertsShown()}, notice stored ${storedSetting(LA_NOTICE_SHOWN)}"
    }
    newGroupsViaApi("first", "second", "third")
    current = restart()
    poll(what = "the SimpleX Lock notice with `Turn on`") { seesText(LOCK_NOTICE_TEXT, substring = true) && "Turn on" in textsOf(CONFIRM) }
    check(current.chatList.shown(NOTES) && chatModel.chats.value.size > 3 && storedSetting(LA_NOTICE_SHOWN) == "true" && storedSetting(PERFORM_LA) != "true") {
      "the notice with ${chatModel.chats.value.size} chats: notice stored ${storedSetting(LA_NOTICE_SHOWN)}, lock ${storedSetting(PERFORM_LA)}"
    }
    click(CONFIRM)
    val passcode = PasscodeScreen(this)
    passcode.await(NEW_PASSCODE)
    passcode.enter(CODE)
    passcode.submit()
    passcode.await(CONFIRM_PASSCODE)
    passcode.enter(CODE)
    passcode.submit()
    passcode.confirmAlert("SimpleX Lock turned on")
    poll(what = "the lock stored as on with the passcode") { storedSetting(PERFORM_LA) == "true" && decoded(APP_PASSCODE) == CODE && passcode.gone() }
  }

  @Test fun lock_passcodeKeypad_digitsBackspaceClearReveal() = uiTest {
    val alice = app("alice")
    val lock = yourPrivacy(alice).lock()
    click(Ui.Lock.Enable)
    lock.passcode.await(NEW_PASSCODE)
    check(entered().isNullOrEmpty()) { "the passcode field is not empty: ${entered()}" }
    lock.passcode.enter("12345")
    poll(what = "five masked digits") { entered() == "●●●●●" }
    lock.passcode.backspace()
    poll(what = "four masked digits after backspace") { entered() == "●●●●" }
    click(Ui.Passcode.Entered)
    poll(what = "the digits shown after clicking them") { entered() == "1234" }
    click(Ui.Passcode.Entered)
    poll(what = "the digits masked again") { entered() == "●●●●" }
    lock.passcode.clear()
    poll(what = "the field cleared") { entered().isNullOrEmpty() }
    lock.passcode.submit()
    settle()
    check(seesText(NEW_PASSCODE) && !seesText(CONFIRM_PASSCODE)) { "Save accepted an empty passcode" }
    lock.passcode.enter("123")
    lock.passcode.submit()
    settle()
    check(seesText(NEW_PASSCODE) && !seesText(CONFIRM_PASSCODE)) { "Save accepted three digits" }
    lock.passcode.enter("4")
    lock.passcode.submit()
    lock.passcode.await(CONFIRM_PASSCODE)
    lock.passcode.enter(CODE)
    lock.passcode.submit()
    lock.passcode.confirmAlert(PASSCODE_SET)
    poll(what = "the lock stored as on with the passcode") { storedSetting(PERFORM_LA) == "true" && decoded(APP_PASSCODE) == CODE }
  }

  @Test fun lock_passcodeKeyboardKeys() = uiTest {
    val alice = app("alice")
    val lock = yourPrivacy(alice).lock()
    click(Ui.Lock.Enable)
    lock.passcode.await(NEW_PASSCODE)
    for (k in listOf(Key.One, Key.Two, Key.Three)) press(k)
    poll(what = "three masked digits typed with the keyboard") { entered() == "●●●" }
    click(Ui.Passcode.Entered)
    poll(what = "the typed digits shown") { entered() == "123" }
    press(Key.Enter)
    check(seesText(NEW_PASSCODE) && !seesText(CONFIRM_PASSCODE)) { "Enter submitted three digits" }
    press(Key.NumPad4)
    poll(what = "the NumPad digit added") { entered() == "1234" }
    press(Key.Backspace)
    poll(what = "the last digit removed by Backspace") { entered() == "123" }
    val root = ui.onRoot()
    ui.runOnUiThread { root.performKeyInput { withKeyDown(Key.CtrlLeft) { pressKey(Key.Backspace) } } }
    poll(what = "the field cleared by Ctrl+Backspace") { entered().isNullOrEmpty() }
    for (k in listOf(Key.NumPad5, Key.Six, Key.NumPad7, Key.Eight)) press(k)
    poll(what = "the new digits typed") { entered() == NEW_CODE }
    press(Key.Enter)
    lock.passcode.await(CONFIRM_PASSCODE)
    for (k in listOf(Key.Five, Key.Six, Key.Seven, Key.Eight)) press(k)
    poll(what = "four masked digits on the confirm step") { entered() == "●●●●" }
    press(Key.NumPadEnter)
    lock.passcode.confirmAlert(PASSCODE_SET)
    poll(what = "the passcode typed with the keyboard stored") { storedSetting(PERFORM_LA) == "true" && decoded(APP_PASSCODE) == NEW_CODE }
  }

  @Test fun selfDestruct_changePasscode() = uiTest {
    val alice = app("alice")
    val lock = yourPrivacy(alice).lock()
    lock.enable(CODE)
    click(Ui.Lock.SelfDestructInfo)
    poll(what = "the Self-destruct page with its three items") { SD_INFO.all { seesText(it) } && !exists(Ui.Lock.Enable) }
    check(storedSetting(SELF_DESTRUCT) != "true") { "the info row turned self-destruct on" }
    escape()
    poll(what = "back on SimpleX Lock") { exists(Ui.Lock.Enable) && !seesText(SD_INFO.first()) }
    enableSelfDestruct(lock, SD_CODE)
    click(Ui.Lock.ChangeSelfDestruct)
    lock.passcode.await(CURRENT_PASSCODE)
    lock.passcode.enter(CODE)
    lock.passcode.submit()
    lock.passcode.await(NEW_PASSCODE)
    check(seesText("Self-destruct")) { "the New Passcode screen does not name self-destruct: ${texts().take(30)}" }
    lock.passcode.enter(NEW_SD_CODE)
    lock.passcode.submit()
    lock.passcode.await(CONFIRM_PASSCODE)
    lock.passcode.enter(NEW_SD_CODE)
    lock.passcode.submit()
    lock.passcode.confirmAlert("Self-destruct passcode changed!")
    poll(what = "the new self-destruct passcode stored, the app passcode kept") { decoded(SD_PASSCODE) == NEW_SD_CODE && decoded(APP_PASSCODE) == CODE }
    click(Ui.Lock.SelfDestruct)
    lock.passcode.await(CURRENT_PASSCODE)
    lock.passcode.enter(WRONG)
    lock.passcode.submit()
    lock.passcode.authFailed()
    check(storedSetting(SELF_DESTRUCT) == "true" && decoded(SD_PASSCODE) == NEW_SD_CODE) { "a wrong passcode turned self-destruct off" }
    click(Ui.Lock.SelfDestruct)
    lock.passcode.await(CURRENT_PASSCODE)
    lock.passcode.enter(CODE)
    lock.passcode.submit()
    poll(what = "self-destruct off, its passcode removed and its options hidden") {
      lock.passcode.gone() && storedSetting(SELF_DESTRUCT) == "false" && storedSetting(SD_PASSCODE) == null && storedSetting(SD_IV) == null &&
        !exists(Ui.Lock.SelfDestructName) && !exists(Ui.Lock.ChangeSelfDestruct)
    }
    lock.close()
    restart()
    val passcode = PasscodeScreen(this)
    passcode.await(ENTER_PASSCODE)
    passcode.enter(NEW_SD_CODE)
    passcode.submit()
    passcode.authFailed()
    check(AppLock.userAuthorized.value == false && coreUserNames() == listOf("alice")) { "the old self-destruct passcode acted after self-destruct was turned off: ${coreUserNames()}" }
    poll(what = "the Unlock button") { exists(Ui.Lock.Unlock) && passcode.gone() }
    click(Ui.Lock.Unlock)
    passcode.await(ENTER_PASSCODE)
    passcode.enter(CODE)
    passcode.submit()
    awaitUnlocked(passcode)
  }

  private fun UiScope.awaitUnlocked(passcode: PasscodeScreen) =
    poll(what = "the passcode screen to close and the app to unlock") { passcode.gone() && !exists(Ui.Lock.Unlock) && AppLock.userAuthorized.value == true }

  private fun UiScope.hideAndReturn(hiddenMs: Long = 0) {
    ui.runOnUiThread {
      AppLock.appWasHidden()
      AppLock.enteredBackground.value = AppLock.enteredBackground.value?.minus(hiddenMs)
      AppLock.recheckAuthState()
    }
    ui.waitForIdle()
  }

  private fun UiScope.stopChatAndConfirm() {
    click(Ui.Database.RunChat)
    poll(what = "the Stop chat? alert with Stop") { seesText("Stop chat?") && "Stop" in textsOf(CONFIRM) }
    click(CONFIRM)
    poll(what = "the Stop chat? alert to close") { !seesText("Stop chat?") }
  }

  private fun UiScope.enableSelfDestruct(lock: LockScreen, code: String) {
    click(Ui.Lock.SelfDestruct)
    lock.passcode.await(CURRENT_PASSCODE)
    lock.passcode.enter(CODE)
    lock.passcode.submit()
    lock.passcode.await(SET_PASSCODE)
    lock.passcode.enter(code)
    lock.passcode.submit()
    lock.passcode.await(CONFIRM_PASSCODE)
    lock.passcode.enter(code)
    lock.passcode.submit()
    lock.passcode.confirmAlert("Self-destruct passcode enabled!")
    poll(what = "self-destruct stored as on with its passcode") { storedSetting(SELF_DESTRUCT) == "true" && decoded(SD_PASSCODE) == code && exists(Ui.Lock.ChangeSelfDestruct) }
  }

  private fun UiScope.entered(): String? = textOf(Ui.Passcode.Entered)

  private fun UiScope.press(key: Key) {
    val root = ui.onRoot()
    ui.runOnUiThread { root.performKeyInput { pressKey(key) } }
    ui.waitForIdle()
  }

  private fun clearLockNoticeShownViaApi() = appPrefs.laNoticeShown.set(false)

  private fun UiScope.decoded(key: String): String? = storedSetting(key)?.let { String(Base64.getDecoder().decode(it)) }

  private fun coreUserNames(): List<String> = runBlocking { chatController.listUsers(null) }.map { it.user.displayName }

  private companion object {
    const val CODE = "1234"
    const val MISMATCH = "1243"
    const val WRONG = "9999"
    const val NEW_CODE = "5678"
    const val SD_CODE = "4321"
    const val WIPED_NAME = "wiped"
    const val BEFORE_WIPE = "before the wipe"
    const val NOTES = "Private notes"
    const val NOTE = "written after unlocking"
    const val CURRENT_PASSCODE = "Current Passcode"
    const val SET_PASSCODE = "Set passcode"
    const val PERFORM_LA = "PerformLA"
    const val LOCK_DELAY = "LocalAuthenticationLockDelay"
    const val APP_PASSCODE = "EncryptedAppPassphrase"
    const val SD_PASSCODE = "EncryptedSelfDestructPassphrase"
    const val SELF_DESTRUCT = "LocalAuthenticationSelfDestruct"
    const val SD_NAME = "LocalAuthenticationSelfDestructDisplayName"
    const val SD_IV = "InitializationVectorSelfDestructPassphrase"
    const val LA_NOTICE_SHOWN = "LANoticeShown"
    const val NEW_SD_CODE = "8765"
    const val WORK = "work"
    const val LOG_IN_REASON = "Log in using your credential"
    const val MIGRATE_TITLE = "Migrate device"
    const val CHANGE_PROFILES = "Change chat profiles"
    const val LOCK_NOTICE_TEXT = "To protect your information, turn on SimpleX Lock."
    val CONFIRM = Ui.Alert.Confirm(Ui.AlertSource.Shared)
    val SD_INFO = listOf(
      "If you enter your self-destruct passcode while opening the app:",
      "All app data is deleted.",
      "App passcode is replaced with self-destruct passcode.",
      "An empty chat profile with the provided name is created, and the app opens as usual.",
    )
    val DELAYS = listOf(10 to "10 seconds", 30 to "30 seconds", 60 to "1 minutes", 180 to "3 minutes", 600 to "10 minutes", 0 to "Immediately")
  }
}
