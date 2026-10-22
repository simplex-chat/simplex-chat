package chat.simplex.app.ui.android

import android.content.Intent
import androidx.test.espresso.intent.Intents
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.chatRunningInCore
import chat.simplex.app.ui.harness.described
import chat.simplex.app.ui.harness.onboarding
import chat.simplex.app.ui.harness.openProfileMenu
import chat.simplex.app.ui.harness.userIdOf
import chat.simplex.app.BuildConfig
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.views.onboarding.OnboardingStage
import org.junit.Test

class SettingsAndroidTest : UiTest() {
  @Test fun settingsAndroid_rootListsAndroidItems() = uiTest {
    val alice = app("alice")
    alice.settings()
    waitFor(Ui.Settings.Version)
    val version = "${BuildConfig.VERSION_NAME} (${BuildConfig.VERSION_CODE})"
    poll(what = "Your settings with the Android rows and the Android version") { ROOT_ITEMS.all { seesText(it) } && version in textsOf(Ui.Settings.Version) }
    val texts = texts()
    val advanced = ADVANCED_ORDER.map { texts.indexOf(it) }
    check(advanced.zipWithNext().all { (a, b) -> a in 0 until b } && texts.indexOf(version) > advanced.last()) {
      "Advanced settings order ${ADVANCED_ORDER.zip(advanced)}, version at ${texts.indexOf(version)}"
    }

    click(described("Shutdown"))
    poll(what = "the Shutdown? alert with Cancel, Restart and Shutdown") {
      AlertManager.shared.hasAlertsShown() && seesText("Shutdown?") && seesText(SHUTDOWN_DESC) && seesText("Restart") &&
        "Cancel" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the alert closed over Your settings") { !AlertManager.shared.hasAlertsShown() && exists(Ui.Settings.Version) }
    check(chatModel.chatRunning.value == true && chatRunningInCore()) { "Cancel on the Shutdown? alert stopped the chat" }

    click(Ui.Settings.Version)
    poll(what = "App version with the Android version, build and core version") {
      seesText("App version: v${BuildConfig.VERSION_NAME}") && seesText("App build: ${BuildConfig.VERSION_CODE}") &&
        seesText("Core version: v", substring = true) && seesText("Developer tools")
    }
  }

  @Test fun helpAndroid_rateTheAppOpensMarket() = uiTest {
    val alice = app("alice")
    alice.settings()
    click(Ui.Settings.Help)
    waitFor(RATE)
    check(seesText("Rate the app") && seesText("Contribute") && seesText("Star on GitHub")) { "Support the project rows: ${texts().take(40)}" }
    check(viewIntents().isEmpty()) { "an ACTION_VIEW intent was fired before Rate the app: ${viewIntents()}" }
    click(RATE)
    poll(what = "an ACTION_VIEW intent for SimpleX Chat in the app store") { viewIntents().isNotEmpty() }
    check(viewIntents() == listOf(MARKET_URI)) { "Rate the app opened ${viewIntents()}" }
  }

  @Test fun profilesAndroid_deleteLastVisibleReturnsToOnboarding() = uiTest {
    val alice = app("alice")
    val aliceId = userIdOf("alice")
    alice.userPicker().profiles()
    waitFor(Ui.Profiles.User(aliceId))
    check(chatModel.users.size == 1 && chatRunningInCore()) { "users before the deletion: ${chatModel.users.map { it.user.displayName }}" }
    openProfileMenu(aliceId, "delete")
    click(Ui.Profiles.UserMenu(aliceId, "delete"))
    val withConnections = Ui.Alert.Option(Ui.AlertSource.Shared, "deleteWithConnections")
    poll(what = "the `Delete chat profile?` alert with `Profile and server connections`") { seesText("Delete chat profile?") && exists(withConnections) }
    click(withConnections)
    poll(what = "the welcome screen with Get started in place of the profiles list") {
      exists(Ui.Onboarding.GetStarted) && onboarding().stage == OnboardingStage.Step1_SimpleXInfo && !ModalManager.start.hasModalsOpen() && !exists(Ui.Profiles.Add)
    }
    check(chatModel.currentUser.value == null && chatModel.users.isEmpty() && !chatRunningInCore()) {
      "after deleting the last profile: current ${chatModel.currentUser.value?.displayName}, users ${chatModel.users.size}, chat running ${chatRunningInCore()}"
    }
  }

  private fun viewIntents(): List<String> =
    Intents.getIntents().filter { it.action == Intent.ACTION_VIEW }.map { it.data.toString() }

  private companion object {
    val RATE = described("Google Play")
    const val MARKET_URI = "market://details?id=chat.simplex.app"
    const val SHUTDOWN_DESC = "Notifications will stop working until you re-launch the app"
    val ROOT_ITEMS = listOf(
      "Your settings", "Appearance", "Your privacy", "Help & support", "Chat data", "Migrate to another device", "Supporter perks",
      "Network & servers", "Notifications", "Audio & video calls", "Shutdown", "Crowdfunding on Wefunder"
    )
    val ADVANCED_ORDER = listOf("Network & servers", "Notifications", "Audio & video calls", "Shutdown")
  }
}
