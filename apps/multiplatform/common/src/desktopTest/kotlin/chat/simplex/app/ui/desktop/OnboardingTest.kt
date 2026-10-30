package chat.simplex.app.ui.desktop

import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.test.performTextInput
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.TestServers
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.goBack
import chat.simplex.app.ui.harness.onboarding
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.OperatorTag
import chat.simplex.common.model.ServerOperator
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiKey
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.DBMigrationResult
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.views.migration.MigrationToState
import chat.simplex.common.views.onboarding.OnboardingSharedState
import chat.simplex.common.views.onboarding.OnboardingStage
import kotlinx.coroutines.runBlocking
import org.junit.Test

class OnboardingTest : UiTest() {
  @Test fun onboardingWhySimplexIsBuiltOpensHowItWorks() = uiTest {
    app(null)
    poll(what = "the welcome screen") { onboarding().stage == OnboardingStage.Step1_SimpleXInfo && exists(Ui.Onboarding.GetStarted) }
    check(chatModel.currentUser.value == null) { "a user exists on the welcome screen" }
    check("Link a mobile" in textsOf(Ui.Onboarding.LinkAMobile)) { "Link a mobile: ${textsOf(Ui.Onboarding.LinkAMobile)}" }
    check("Create your profile" in textsOf(Ui.Onboarding.GetStarted)) { "Create your profile: ${textsOf(Ui.Onboarding.GetStarted)}" }
    check("Why SimpleX is built." in textsOf(Ui.Onboarding.WhyBuilt)) { "Why SimpleX is built.: ${textsOf(Ui.Onboarding.WhyBuilt)}" }
    check(!seesText(WHY_BUILT_HEADING)) { "the welcome screen shows the HowItWorks heading" }
    click(Ui.Onboarding.WhyBuilt)
    poll(what = "HowItWorks over the welcome screen") { ModalManager.fullscreen.hasModalsOpen() && seesText(WHY_BUILT_HEADING) }
    goBack()
    poll(what = "the welcome screen after Back") {
      !ModalManager.fullscreen.hasModalsOpen() && !seesText(WHY_BUILT_HEADING) && exists(Ui.Onboarding.WhyBuilt)
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
    val o = onboarding()
    o.createProfile("alice")
    click(Ui.Onboarding.RandomPassphrase)
    poll(what = "the Use random passphrase alert") {
      AlertManager.shared.hasAlertsShown() && seesText("Random passphrase is stored in settings as plaintext.", substring = true)
    }
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the passphrase step after Cancel") {
      !AlertManager.shared.hasAlertsShown() && o.stage == OnboardingStage.Step2_5_SetupDatabasePassphrase && exists(Ui.Onboarding.SetPassphrase)
    }
    click(Ui.Onboarding.RandomPassphrase)
    check("OK" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))) { "confirm: ${textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))}" }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(timeoutMs = 30_000, what = "Your network") { o.stage == OnboardingStage.Step3_ChooseServerOperators && exists(Ui.Onboarding.Continue) }
    restart()
    poll(timeoutMs = 30_000, what = "Your network after a restart, with the database open") {
      chatModel.chatDbStatus.value == DBMigrationResult.OK && o.stage == OnboardingStage.Step3_ChooseServerOperators && exists(Ui.Onboarding.Continue)
    }
    check(!seesText("Encrypted database")) { "the passphrase prompt is shown after a restart" }
  }

  @KnownAppIssue("15.5 item 3", "failed to list users", intermittent = true)
  @Test fun onboardingPassphrasePersistsAcrossRestart() = uiTest {
    app(null)
    val o = onboarding()
    o.createProfile("alice")
    poll(what = "the passphrase step") {
      o.stage == OnboardingStage.Step2_5_SetupDatabasePassphrase && seesText("Setup database passphrase") && exists(Ui.Onboarding.SetPassphrase)
    }
    check(storedSetting("StoreDBPassphrase") == "true" && storedSetting("EncryptedDBPassphrase") != null) { "no passphrase is stored in settings before encryption" }
    typeSecret(Ui.Onboarding.NewPassphrase, PASSPHRASE)
    typeSecret(Ui.Onboarding.ConfirmPassphrase, PASSPHRASE)
    click(Ui.Onboarding.SetPassphrase)
    poll(what = "the Encrypt database? alert") { seesText("Encrypt database?") && seesText("Database will be encrypted.", substring = true) }
    check("Encrypt" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))) { "confirm: ${textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))}" }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(timeoutMs = 30_000, what = "Your network and the Database encrypted! alert") {
      o.stage == OnboardingStage.Step3_ChooseServerOperators && seesText("Database encrypted!")
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "Your network") { !AlertManager.shared.hasAlertsShown() && exists(Ui.Onboarding.Continue) }
    check(storedSetting("StoreDBPassphrase") == "false" && storedSetting("EncryptedDBPassphrase") == null) {
      "a passphrase is stored in settings: StoreDBPassphrase=${storedSetting("StoreDBPassphrase")}"
    }
    click(Ui.Onboarding.Continue)
    click(Ui.Onboarding.Accept)
    o.waitForChatList()
    restart()
    poll(timeoutMs = 30_000, what = "the passphrase prompt") {
      chatModel.chatDbStatus.value is DBMigrationResult.ErrorNotADatabase && seesText("Encrypted database") && seesText("Database passphrase is required to open chat.")
    }
    typeSecret(Ui.DbError.Passphrase, PASSPHRASE.dropLast(1))
    click(Ui.DbError.Open)
    poll(timeoutMs = 30_000, what = "the Wrong passphrase! alert") { seesText("Wrong passphrase!") && chatModel.chatDbStatus.value is DBMigrationResult.ErrorNotADatabase }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    typeSecret(Ui.DbError.Passphrase, PASSPHRASE.takeLast(1))
    click(Ui.DbError.Open)
    onboarding().waitForChatList()
    check(chatModel.currentUser.value?.displayName == "alice" && storedSetting("EncryptedDBPassphrase") == null) { "after unlock: user ${chatModel.currentUser.value?.displayName}, stored passphrase ${storedSetting("EncryptedDBPassphrase") != null}" }
  }

  @Test fun onboardingYourNetworkShowsSetupRouters() = uiTest {
    app(null)
    onboarding().toYourNetwork("alice")
    poll(what = "the Your network title and subtitle") { seesText("Your network") && seesText("Network routers cannot know", substring = true) }
    check("Setup routers" in textsOf(Ui.Onboarding.SetupRouters)) { "routers button: ${textsOf(Ui.Onboarding.SetupRouters)}" }
    check(!exists(Ui.Onboarding.SetupNotifications) && !seesText("Setup notifications")) { "Setup notifications is shown on desktop" }
    val operators = chatModel.conditions.value.serverOperators
    check(operators.isNotEmpty()) { "no server operators in a new database" }
    check(!seesText("Server operators") && operators.none { exists(Ui.Onboarding.Operator(it.operatorId)) }) { "operator cards are shown before Setup routers" }
    onboarding().openOperators()
    poll(what = "Server operators with a card per operator") {
      seesText("Server operators") && operators.all { exists(Ui.Onboarding.Operator(it.operatorId)) }
    }
  }

  @Test fun onboardingChooseOperatorsCannotDeselectAll() = uiTest {
    app(null)
    val o = onboarding()
    o.toYourNetwork("alice")
    val enabled = coreOperators().filter { it.enabled }
    check(enabled.size > 1) { "fewer than two operators are enabled in a new database: $enabled" }
    val simplex = enabled.single { it.operatorTag == OperatorTag.SimpleX }.operatorId
    o.openOperators()
    for (op in enabled) click(Ui.Onboarding.Operator(op.operatorId))
    poll(what = "no operator selected and OK disabled") { OnboardingSharedState.selectedOperatorIds.isEmpty() && exists(Ui.Onboarding.OperatorsOk) && !isEnabled(Ui.Onboarding.OperatorsOk) }
    escape()
    settle()
    check(exists(Ui.Onboarding.OperatorsOk) && ModalManager.fullscreen.hasModalsOpen()) { "Server operators closed with no operator selected" }
    click(Ui.Onboarding.Operator(simplex))
    click(Ui.Onboarding.OperatorsOk)
    poll(what = "Server operators to close") { !exists(Ui.Onboarding.OperatorsOk) && OnboardingSharedState.selectedOperatorIds == setOf(simplex) }
    click(Ui.Onboarding.Continue)
    click(Ui.Onboarding.Accept)
    o.waitForChatList()
    val after = coreOperators()
    check(after.filter { it.enabled }.map { it.operatorId } == listOf(simplex)) { "enabled operators in the core: $after" }
    check(after.single { it.operatorId == simplex }.conditionsAcceptance.conditionsAccepted) { "conditions not accepted for the selected operator: $after" }
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
    o.toYourNetwork("alice")
    val before = coreOperators()
    val simplex = before.single { it.operatorTag == OperatorTag.SimpleX }
    val flux = before.single { it.operatorTag == OperatorTag.Flux }
    check(simplex.enabled && flux.enabled && !flux.smpRoles.storage && before.none { it.conditionsAcceptance.conditionsAccepted }) { "operators in a new database: $before" }
    o.openOperators()
    click(Ui.Onboarding.Operator(simplex.operatorId))
    poll(what = "only Flux to be selected") { OnboardingSharedState.selectedOperatorIds == setOf(flux.operatorId) }
    click(Ui.Onboarding.OperatorsOk)
    poll(what = "Server operators to close") { !exists(Ui.Onboarding.OperatorsOk) }
    click(Ui.Onboarding.Continue)
    poll(what = "Network commitments") {
      o.stage == OnboardingStage.Step4_NetworkCommitments && seesText("Network commitments") && exists(Ui.Onboarding.ConditionsLink)
    }
    click(Ui.Onboarding.Accept)
    o.waitForChatList()
    poll(what = "the connect onboarding view") { seesText("Talk to someone") }
    val after = coreOperators()
    val fluxAfter = after.single { it.operatorId == flux.operatorId }
    check(after.filter { it.enabled } == listOf(fluxAfter)) { "enabled operators in the core: $after" }
    check(fluxAfter.conditionsAcceptance.conditionsAccepted && !after.single { it.operatorId == simplex.operatorId }.conditionsAcceptance.conditionsAccepted) {
      "conditions acceptance in the core: $after"
    }
    check(fluxAfter.smpRoles.storage) { "Flux was enabled alone without the SMP storage role: $fluxAfter" }
  }

  @Test fun onboardingConditionsLinkOpensConditionsText() = uiTest {
    app(null)
    val o = onboarding()
    o.toConditions("alice")
    click(Ui.Onboarding.ConditionsLink)
    poll(what = "Conditions of use with the conditions text") {
      seesText("Conditions of use") && seesText("Privacy Policy and Conditions of Use", substring = true) && exists(Ui.Onboarding.ConditionsAccept)
    }
    click(Ui.Onboarding.ConditionsAccept)
    o.waitForChatList()
    poll(what = "the connect onboarding view") { seesText("Talk to someone") }
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
    alice.chatList.row("bob")
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

  private fun coreOperators(): List<ServerOperator> =
    runBlocking { chatController.getServerOperators(null) }?.serverOperators ?: error("getServerOperators failed")

  private fun UiScope.secretLength(key: UiKey): Int =
    ui.onNode(matcher(key)).fetchSemanticsNode().config.getOrElseNullable(SemanticsProperties.EditableText) { null }?.length ?: 0

  private companion object {
    const val WHY_BUILT_HEADING = "You were born without an account."
    const val PASSPHRASE = "Uitest-passphrase-7"
  }
}
