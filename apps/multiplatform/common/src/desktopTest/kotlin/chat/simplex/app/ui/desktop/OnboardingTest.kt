package chat.simplex.app.ui.desktop

import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.test.performTextInput
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.TestServers
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.onboarding
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiKey
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.DBMigrationResult
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.views.migration.MigrationToState
import chat.simplex.common.views.onboarding.OnboardingSharedState
import chat.simplex.common.views.onboarding.OnboardingStage
import org.junit.Test

class OnboardingTest : UiTest() {
  @Test fun onboardingWelcomeShowsDesktopActions() = uiTest {
    app(null)
    poll(what = "the welcome screen") { onboarding().stage == OnboardingStage.Step1_SimpleXInfo && exists(Ui.Onboarding.GetStarted) }
    check(chatModel.currentUser.value == null) { "a user exists on the welcome screen" }
    check("Link a mobile" in textsOf(Ui.Onboarding.LinkAMobile)) { "Link a mobile: ${textsOf(Ui.Onboarding.LinkAMobile)}" }
    check("Create your profile" in textsOf(Ui.Onboarding.GetStarted)) { "Create your profile: ${textsOf(Ui.Onboarding.GetStarted)}" }
    check("Why SimpleX is built." in textsOf(Ui.Onboarding.WhyBuilt)) { "Why SimpleX is built.: ${textsOf(Ui.Onboarding.WhyBuilt)}" }
  }

  @Test fun onboardingWhySimplexIsBuiltOpensHowItWorks() = uiTest {
    app(null)
    waitFor(Ui.Onboarding.WhyBuilt)
    check(!seesText(WHY_BUILT_HEADING)) { "the welcome screen shows the HowItWorks heading" }
    click(Ui.Onboarding.WhyBuilt)
    poll(what = "HowItWorks over the welcome screen") { ModalManager.fullscreen.hasModalsOpen() && seesText(WHY_BUILT_HEADING) }
  }

  @Test fun onboardingCreateProfileValidName() = uiTest {
    app(null)
    onboarding().createProfile("alice")
    poll(what = "the passphrase step") {
      onboarding().stage == OnboardingStage.Step2_5_SetupDatabasePassphrase && seesText("Setup database passphrase") && exists(Ui.Onboarding.SetPassphrase)
    }
  }

  @Test fun onboardingCreateProfileButtonDisabledForEmptyName() = uiTest {
    app(null)
    click(Ui.Onboarding.GetStarted)
    waitFor(Ui.Onboarding.CreateProfile)
    settle()
    check(!isEnabled(Ui.Onboarding.CreateProfile)) { "Create profile is enabled with an empty name" }
    typeInto(Ui.Onboarding.ProfileName, "   ")
    settle()
    check(!isEnabled(Ui.Onboarding.CreateProfile)) { "Create profile is enabled with a blank name" }
    check(chatModel.currentUser.value == null) { "a profile was created" }
  }

  @Test fun onboardingCreateProfileInvalidNameShowsCorrectionAlert() = uiTest {
    app(null)
    click(Ui.Onboarding.GetStarted)
    typeInto(Ui.Onboarding.ProfileName, "alice_1")
    waitFor(Ui.Onboarding.ProfileName.part("invalid"))
    waitFor(Ui.Onboarding.CreateProfile)
    check(!isEnabled(Ui.Onboarding.CreateProfile)) { "Create profile is enabled for an invalid name" }
    click(Ui.Onboarding.ProfileName.part("invalid"))
    poll(what = "the correction alert") { seesText("Invalid name!") && seesText("Correct name to alice-1?") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the corrected name") {
      editableText(Ui.Onboarding.ProfileName) == "alice-1" && isEnabled(Ui.Onboarding.CreateProfile) && !exists(Ui.Onboarding.ProfileName.part("invalid"))
    }
  }

  @Test fun onboardingCreateProfileBackReturnsToWelcome() = uiTest {
    app(null)
    click(Ui.Onboarding.GetStarted)
    waitFor(Ui.Onboarding.ProfileName)
    escape()
    poll(what = "the welcome screen") {
      onboarding().stage == OnboardingStage.Step1_SimpleXInfo && exists(Ui.Onboarding.LinkAMobile) && !exists(Ui.Onboarding.ProfileName)
    }
    check(chatModel.currentUser.value == null) { "a profile was created" }
  }

  @Test fun onboardingMigrateFromAnotherDeviceOpensMigrationScreen() = uiTest {
    app(null)
    click(Ui.Onboarding.GetStarted)
    check("Migrate from another device" in textsOf(Ui.Onboarding.Migrate)) { "migrate button: ${textsOf(Ui.Onboarding.Migrate)}" }
    click(Ui.Onboarding.Migrate)
    poll(what = "Migrate here in the paste-link state") {
      chatModel.migrationState.value is MigrationToState.PasteOrScanLink && seesText("Migrate here") && seesText("Paste archive link")
    }
  }

  @Test fun onboardingSetDatabasePassphrase() = uiTest {
    app(null)
    onboarding().createProfile("alice")
    typeSecret(Ui.Onboarding.NewPassphrase, PASSPHRASE)
    typeSecret(Ui.Onboarding.ConfirmPassphrase, PASSPHRASE)
    click(Ui.Onboarding.SetPassphrase)
    poll(what = "the Encrypt database? alert") { seesText("Encrypt database?") && seesText("Database will be encrypted.", substring = true) }
    check("Encrypt" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))) { "confirm: ${textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))}" }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(timeoutMs = 30_000, what = "Your network and the Database encrypted! alert") {
      onboarding().stage == OnboardingStage.Step3_ChooseServerOperators && seesText("Database encrypted!")
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "Your network") { !AlertManager.shared.hasAlertsShown() && exists(Ui.Onboarding.Continue) }
    check(storedSetting("StoreDBPassphrase") == "false") { "StoreDBPassphrase: ${storedSetting("StoreDBPassphrase")}" }
  }

  @Test fun onboardingSetPassphraseMismatchDisablesButton() = uiTest {
    app(null)
    onboarding().createProfile("alice")
    typeSecret(Ui.Onboarding.NewPassphrase, PASSPHRASE)
    typeSecret(Ui.Onboarding.ConfirmPassphrase, PASSPHRASE.dropLast(1))
    settle()
    waitFor(Ui.Onboarding.SetPassphrase)
    check(!isEnabled(Ui.Onboarding.SetPassphrase)) { "Set database passphrase is enabled for mismatched passphrases" }
    typeSecret(Ui.Onboarding.ConfirmPassphrase, PASSPHRASE.takeLast(1))
    poll(what = "Set database passphrase to be enabled for matching passphrases") { isEnabled(Ui.Onboarding.SetPassphrase) }
  }

  @Test fun onboardingUseRandomPassphraseAlert() = uiTest {
    app(null)
    onboarding().createProfile("alice")
    click(Ui.Onboarding.RandomPassphrase)
    poll(what = "the Use random passphrase alert") {
      AlertManager.shared.hasAlertsShown() && seesText("Random passphrase is stored in settings as plaintext.", substring = true)
    }
    check("OK" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))) { "confirm: ${textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))}" }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(timeoutMs = 30_000, what = "Your network") { onboarding().stage == OnboardingStage.Step3_ChooseServerOperators && exists(Ui.Onboarding.Continue) }
    check(appPrefs.storeDBPassphrase.get() && storedSetting("EncryptedDBPassphrase") != null) { "the random passphrase is not stored in settings" }
  }

  @Test fun onboardingPassphrasePersistsAcrossRestart() = uiTest {
    app(null)
    onboarding().createProfile("alice")
    typeSecret(Ui.Onboarding.NewPassphrase, PASSPHRASE)
    typeSecret(Ui.Onboarding.ConfirmPassphrase, PASSPHRASE)
    click(Ui.Onboarding.SetPassphrase)
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(timeoutMs = 30_000, what = "the Database encrypted! alert") { seesText("Database encrypted!") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    check(storedSetting("StoreDBPassphrase") == "false" && storedSetting("EncryptedDBPassphrase") == null) { "a passphrase is stored in settings" }
    restart()
    poll(timeoutMs = 30_000, what = "the passphrase prompt") {
      chatModel.chatDbStatus.value is DBMigrationResult.ErrorNotADatabase && seesText("Encrypted database") && seesText("Database passphrase is required to open chat.")
    }
  }

  @Test fun onboardingYourNetworkShowsSetupRouters() = uiTest {
    app(null)
    onboarding().toYourNetwork("alice")
    poll(what = "the Your network title and subtitle") { seesText("Your network") && seesText("Network routers cannot know", substring = true) }
    check("Setup routers" in textsOf(Ui.Onboarding.SetupRouters)) { "routers button: ${textsOf(Ui.Onboarding.SetupRouters)}" }
    onboarding().openOperators()
    poll(what = "Server operators with a card per operator") {
      seesText("Server operators") && chatModel.conditions.value.serverOperators.all { exists(Ui.Onboarding.Operator(it.operatorId)) }
    }
  }

  @Test fun onboardingContinueGoesToConditions() = uiTest {
    app(null)
    onboarding().toYourNetwork("alice")
    click(Ui.Onboarding.Continue)
    poll(what = "Network commitments") {
      onboarding().stage == OnboardingStage.Step4_NetworkCommitments && seesText("Network commitments") && exists(Ui.Onboarding.ConditionsLink) && isEnabled(Ui.Onboarding.Accept)
    }
  }

  @Test fun onboardingChooseOperatorsToggle() = uiTest {
    app(null)
    val o = onboarding()
    o.toYourNetwork("alice")
    o.openOperators()
    val id = chatModel.conditions.value.serverOperators.first().operatorId
    val before = id in OnboardingSharedState.selectedOperatorIds
    click(Ui.Onboarding.Operator(id))
    poll(what = "operator $id to toggle") { (id in OnboardingSharedState.selectedOperatorIds) == !before }
    click(Ui.Onboarding.Operator(id))
    poll(what = "operator $id to toggle back") { (id in OnboardingSharedState.selectedOperatorIds) == before }
  }

  @Test fun onboardingChooseOperatorsCannotDeselectAll() = uiTest {
    app(null)
    val o = onboarding()
    o.toYourNetwork("alice")
    o.openOperators()
    val enabled = o.enabledOperatorIds()
    check(enabled.isNotEmpty()) { "no operator is enabled in a new database" }
    for (id in enabled) click(Ui.Onboarding.Operator(id))
    poll(what = "no operator selected and OK disabled") { OnboardingSharedState.selectedOperatorIds.isEmpty() && exists(Ui.Onboarding.OperatorsOk) && !isEnabled(Ui.Onboarding.OperatorsOk) }
    escape()
    settle()
    check(exists(Ui.Onboarding.OperatorsOk) && ModalManager.fullscreen.hasModalsOpen()) { "Server operators closed with no operator selected" }
    click(Ui.Onboarding.Operator(enabled.first()))
    click(Ui.Onboarding.OperatorsOk)
    poll(what = "Server operators to close") { !exists(Ui.Onboarding.OperatorsOk) && OnboardingSharedState.selectedOperatorIds == setOf(enabled.first()) }
  }

  @Test fun onboardingHowItHelpsPrivacyInfo() = uiTest {
    app(null)
    val o = onboarding()
    o.toYourNetwork("alice")
    o.openOperators()
    check(!seesText("Network operators")) { "Network operators is shown before the info button" }
    val modals = ModalManager.fullscreen.openModalCount()
    click(Ui.Onboarding.HowItHelps)
    poll(what = "the Network operators info screen") { ModalManager.fullscreen.openModalCount() == modals + 1 && seesText("Network operators") }
  }

  @Test fun onboardingAcceptConditionsCompletesOnboarding() = uiTest {
    app(null)
    val o = onboarding()
    o.toConditions("alice")
    val selected = OnboardingSharedState.selectedOperatorIds
    click(Ui.Onboarding.Accept)
    o.waitForChatList()
    poll(what = "the connect onboarding view and the selected operators enabled") { seesText("Talk to someone") && o.enabledOperatorIds() == selected }
  }

  @Test fun onboardingConditionsLinkOpensConditionsText() = uiTest {
    app(null)
    val o = onboarding()
    o.toConditions("alice")
    click(Ui.Onboarding.ConditionsLink)
    poll(what = "Conditions of use") { seesText("Conditions of use") && exists(Ui.Onboarding.ConditionsAccept) }
    click(Ui.Onboarding.ConditionsAccept)
    o.waitForChatList()
    poll(what = "the connect onboarding view") { seesText("Talk to someone") }
  }

  @Test fun onboardingNoNotificationsStepOnDesktop() = uiTest {
    app(null)
    onboarding().toYourNetwork("alice")
    waitFor(Ui.Onboarding.SetupRouters)
    settle()
    check(!exists(Ui.Onboarding.SetupNotifications) && !seesText("Setup notifications")) { "Setup notifications is shown on desktop" }
  }

  @Test fun onboardingThenConnectToCliPeerViaLocalServers() = uiTest {
    val alice = app(null)
    onboarding().complete("alice")
    quiesce()
    TestServers.configure()
    val link = alice.newChat().createOneTimeLink()
    val bob = cli("bob")
    bob sends "/c $link"
    bob expects "confirmation sent!"
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "contact bob ready in the model") {
      chatModel.chats.value.any { (it.chatInfo as? ChatInfo.Direct)?.contact?.let { c -> c.displayName == "bob" && c.ready } == true }
    }
  }

  @Test fun onboardingLinkAMobileShowsQrAndDeviceName() = uiTest {
    app(null)
    appPrefs.developerTools.set(true)
    appPrefs.offerRemoteMulticast.set(false)
    click(Ui.Onboarding.LinkAMobile)
    onboarding().useRandomPassphrase()
    poll(timeoutMs = 30_000, what = "the Link a mobile screen with a QR code") { onboarding().stage == OnboardingStage.LinkAMobile && seesDescription("QR Code") }
    check(seesText("This device name") && seesText("Discoverable via local network")) { "device name section: ${texts().take(40)}" }
    check(editableText(Ui.Remote.DeviceName) == appPrefs.deviceNameForRemoteAccess.get()) { "device name field: ${editableText(Ui.Remote.DeviceName)}" }
    click(Ui.Remote.ShareLink)
    poll(what = "the invitation on the clipboard") { clipboardText()?.startsWith("xrcp:/") == true }
  }

  @Test fun onboardingLinkAMobileBackReturnsToWelcome() = uiTest {
    app(null)
    appPrefs.offerRemoteMulticast.set(false)
    click(Ui.Onboarding.LinkAMobile)
    onboarding().useRandomPassphrase()
    poll(timeoutMs = 30_000, what = "the Link a mobile screen") { onboarding().stage == OnboardingStage.LinkAMobile && exists(Ui.Remote.DeviceName) }
    escape()
    poll(what = "the welcome screen") {
      onboarding().stage == OnboardingStage.Step1_SimpleXInfo && exists(Ui.Onboarding.LinkAMobile) && !exists(Ui.Remote.DeviceName)
    }
    check(chatModel.currentUser.value == null) { "a profile was created" }
  }

  private fun UiScope.typeSecret(key: UiKey, text: String) {
    waitFor(key)
    val before = secretLength(key)
    val node = ui.onNode(matcher(key))
    ui.runOnUiThread { node.performTextInput(text) }
    poll(what = "field ${key.tag} to hold ${before + text.length} characters") { secretLength(key) == before + text.length }
  }

  private fun UiScope.secretLength(key: UiKey): Int =
    ui.onNode(matcher(key)).fetchSemanticsNode().config.getOrElseNullable(SemanticsProperties.EditableText) { null }?.length ?: 0

  private companion object {
    const val WHY_BUILT_HEADING = "You were born without an account."
    const val PASSPHRASE = "Uitest-passphrase-7"
  }
}
