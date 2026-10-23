package chat.simplex.app.ui.android

import android.app.Notification
import android.app.NotificationManager
import android.content.Context
import android.os.ParcelFileDescriptor
import android.os.PowerManager
import androidx.test.platform.app.InstrumentationRegistry
import chat.simplex.app.SimplexService
import chat.simplex.app.ui.harness.Bridge
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.UiTestApp
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.configureServers
import chat.simplex.app.ui.harness.onboarding
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.common.model.AppPreferences
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.NotificationsMode
import chat.simplex.common.platform.androidAppContext
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.onboarding.OnboardingStage
import chat.simplex.common.views.usersettings.changeNotificationsMode
import org.junit.Test

class OnboardingAndroidTest : UiTest() {
  @Test fun androidOnboardingWelcomeShowsGetStarted() = uiTest {
    app(null)
    poll(what = "the welcome screen") { onboarding().stage == OnboardingStage.Step1_SimpleXInfo && exists(Ui.Onboarding.GetStarted) && exists(Ui.Onboarding.WhyBuilt) }
    check(chatModel.currentUser.value == null) { "a user exists on the welcome screen" }
    check("Get started" in textsOf(Ui.Onboarding.GetStarted)) { "Get started: ${textsOf(Ui.Onboarding.GetStarted)}" }
    check("Why SimpleX is built." in textsOf(Ui.Onboarding.WhyBuilt)) { "Why SimpleX is built.: ${textsOf(Ui.Onboarding.WhyBuilt)}" }
    check(!exists(Ui.Onboarding.LinkAMobile) && !seesText("Link a mobile") && !seesText("Create your profile")) { "the welcome screen shows desktop actions: ${texts().take(40)}" }
  }

  @Test fun onboardingNotifications_continueAppliesInstant() = uiTest {
    app(null)
    useFreshInstallNotificationPrefs()
    withBackgroundServiceAllowed {
      val o = onboarding()
      o.createProfile("alice")
      waitFor(Ui.Onboarding.SetupNotifications)
      check(storedSetting(MODE_KEY) == null && serviceNotification() == null) { "notifications applied before Continue: mode ${storedSetting(MODE_KEY)}, service notification ${serviceNotification() != null}" }
      click(Ui.Onboarding.SetupNotifications)
      poll(what = "Private notifications with the three modes") {
        seesText("Private notifications") && seesText("How it affects battery") && MODES.all { exists(Ui.Onboarding.NotificationsModeCard(it.name)) }
      }
      check(MODE_TITLES.all { (mode, title) -> title in textsOf(Ui.Onboarding.NotificationsModeCard(mode.name)) }) { "mode cards: ${MODES.map { textsOf(Ui.Onboarding.NotificationsModeCard(it.name)) }}" }
      click(Ui.Onboarding.NotificationsOk)
      poll(what = "Your network after OK") { !exists(Ui.Onboarding.NotificationsOk) && exists(Ui.Onboarding.Continue) }
      check(!AlertManager.shared.hasAlertsShown() && storedSetting(MODE_KEY) == null) { "before Continue: alert ${AlertManager.shared.hasAlertsShown()}, mode ${storedSetting(MODE_KEY)}" }
      click(Ui.Onboarding.Continue)
      poll(what = "the Instant notifications alert over Network commitments") {
        o.stage == OnboardingStage.Step4_NetworkCommitments && seesText("Instant notifications") && seesText("instead of using push notifications", substring = true)
      }
      check(storedSetting(MODE_KEY) == NotificationsMode.SERVICE.name && storedSetting(NOTICE_SHOWN_KEY) == "true") {
        "after Continue: mode ${storedSetting(MODE_KEY)}, notice shown ${storedSetting(NOTICE_SHOWN_KEY)}"
      }
      click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
      poll(what = "Network commitments with the alert closed") { !AlertManager.shared.hasAlertsShown() && exists(Ui.Onboarding.Accept) }
      poll(what = "the SimpleX Chat service notification") {
        serviceNotification()?.extras?.getCharSequence(Notification.EXTRA_TITLE)?.toString() == "SimpleX Chat service"
      }
    }
  }

  @Test fun onboardingThenConnectToCliPeerViaLocalServers() = uiTest {
    val alice = app(null)
    useFreshInstallNotificationPrefs()
    val o = onboarding()
    o.createProfile("alice")
    check(o.stage == OnboardingStage.Step3_ChooseServerOperators) { "the step after Create profile is ${o.stage}, not Your network" }
    check("Setup notifications" in textsOf(Ui.Onboarding.SetupNotifications)) { "notifications button: ${textsOf(Ui.Onboarding.SetupNotifications)}" }
    click(Ui.Onboarding.SetupNotifications)
    click(Ui.Onboarding.NotificationsModeCard(NotificationsMode.OFF.name))
    click(Ui.Onboarding.NotificationsOk)
    poll(what = "Your network after OK") { !exists(Ui.Onboarding.NotificationsOk) && exists(Ui.Onboarding.Continue) }
    check(storedSetting(MODE_KEY) == null) { "the notifications mode was stored before Continue: ${storedSetting(MODE_KEY)}" }
    click(Ui.Onboarding.Continue)
    poll(what = "Network commitments") { o.stage == OnboardingStage.Step4_NetworkCommitments && exists(Ui.Onboarding.Accept) }
    check(storedSetting(MODE_KEY) == NotificationsMode.OFF.name && !AlertManager.shared.hasAlertsShown()) {
      "after Continue with When app is running: mode ${storedSetting(MODE_KEY)}, alert shown ${AlertManager.shared.hasAlertsShown()}: ${texts().take(20)}"
    }
    click(Ui.Onboarding.Accept)
    o.waitForChatList()
    poll(what = "the onboarding cards in the chat list") { seesText("Talk to someone") }
    check(chatModel.currentUser.value?.displayName == "alice") { "current user: ${chatModel.currentUser.value?.displayName}" }
    quiesce()
    val (smp, xftp) = Bridge.servers()
    configureServers(smp, xftp)
    val link = alice.newChat().createOneTimeLink()
    val bob = cli("bob")
    bob sends "/c $link"
    bob expects "confirmation sent!"
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "contact bob ready, its chat open in place of the invitation") {
      val chat = chatModel.chats.value.firstOrNull { (it.chatInfo as? ChatInfo.Direct)?.contact?.let { c -> c.displayName == "bob" && c.ready } == true }
      chat != null && chatModel.chatId.value == chat.id
    }
    backToChatList()
    alice.chatList.row("bob")
  }

  private fun useFreshInstallNotificationPrefs() {
    check(androidAppContext.getSharedPreferences(AppPreferences.SHARED_PREFS_ID, Context.MODE_PRIVATE).edit().remove(MODE_KEY).commit()) { "could not remove $MODE_KEY" }
    appPrefs.backgroundServiceNoticeShown.set(false)
  }

  private fun UiScope.withBackgroundServiceAllowed(body: () -> Unit) {
    val pkg = androidAppContext.packageName
    val allowListed = androidAppContext.getSystemService(PowerManager::class.java).isIgnoringBatteryOptimizations(pkg)
    if (!allowListed) shell("dumpsys deviceidle whitelist +$pkg")
    val result = runCatching {
      poll(what = "the app allowed to run in background (battery allow-list, no background restriction)") { SimplexService.isBackgroundAllowed() }
      body()
    }
    val stopped = runCatching {
      UiTestApp.onMain { changeNotificationsMode(NotificationsMode.OFF, chatModel) }
      poll(what = "the SimpleX Chat service to stop and its notification to go") { !SimplexService.isServiceStarted && !SimplexService.isServiceStarting && serviceNotification() == null }
    }
    if (!allowListed) shell("dumpsys deviceidle whitelist -$pkg")
    result.getOrThrow()
    stopped.getOrThrow()
  }

  private fun shell(cmd: String): String =
    ParcelFileDescriptor.AutoCloseInputStream(InstrumentationRegistry.getInstrumentation().uiAutomation.executeShellCommand(cmd)).bufferedReader().use { it.readText() }

  private fun serviceNotification(): Notification? =
    androidAppContext.getSystemService(NotificationManager::class.java).activeNotifications.firstOrNull { it.id == SimplexService.SIMPLEX_SERVICE_ID }?.notification

  private companion object {
    const val MODE_KEY = "NotificationsMode"
    const val NOTICE_SHOWN_KEY = "BackgroundServiceNoticeShown"
    val MODE_TITLES = listOf(NotificationsMode.SERVICE to "Instant", NotificationsMode.PERIODIC to "Periodic", NotificationsMode.OFF to "When app is running")
    val MODES = MODE_TITLES.map { it.first }
  }
}
