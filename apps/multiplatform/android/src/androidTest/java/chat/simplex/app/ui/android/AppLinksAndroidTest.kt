package chat.simplex.app.ui.android

import android.content.Intent
import android.net.Uri
import chat.simplex.app.MainActivity
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.CliPeer
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.createOneTimeLink
import chat.simplex.app.ui.harness.planConfirm
import chat.simplex.app.ui.harness.planProfileName
import chat.simplex.common.platform.androidAppContext
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.AlertManager
import org.junit.Assert.assertEquals
import org.junit.Test

class AppLinksAndroidTest : UiTest() {
  @Test fun appLink_oneTimeLink_opensPlan() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = bobsOneTimeLink(bob)
    alice.chatList.row(NOTES)
    check(chatModel.chats.value.none { it.chatInfo.chatViewName == "bob" } && !exists(planConfirm)) { "bob's chat or a plan alert exists before the link is opened" }
    deliverToApp(viewIntent(link))
    awaitPlanForBob()
    click(planConfirm)
    poll(what = "bob's prepared chat open with Connect") { preparedBobOpen() && exists(Ui.Chat.Connect) }
    click(Ui.Chat.Connect)
    assertEquals("alice: contact is connected", bob.nextLine(CONNECT_TIMEOUT_MS))
  }

  @Test fun appLink_noData_showsInvalidLinkAlert() = uiTest {
    app("alice").chatList.row(NOTES)
    check(!AlertManager.shared.hasAlertsShown() && !seesText(INVALID_LINK)) { "an alert is shown before the link is opened" }
    deliverToApp(viewIntent(null))
    poll(what = "the `$INVALID_LINK` alert") { seesText(INVALID_LINK) && seesText("Please check that SimpleX link is correct.") }
    check(!exists(planConfirm) && !AlertManager.privacySensitive.hasAlertsShown()) { "a link without data opened a connection plan" }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the alert closed over the chat list") { !AlertManager.shared.hasAlertsShown() && exists(Ui.ChatList.Search) }
  }

  @Test fun appLink_whileChatStopped_openedAfterStart() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = bobsOneTimeLink(bob)
    stopChatFromChatData(alice)
    deliverToApp(viewIntent(link))
    poll(what = "the link queued by the app") { chatModel.appOpenUrl.value?.second == link }
    settle()
    check(chatModel.appOpenUrl.value?.second == link && !exists(planConfirm) && !AlertManager.privacySensitive.hasAlertsShown()) {
      "the link was handled while the chat was stopped: queued ${chatModel.appOpenUrl.value}, texts ${texts().take(20)}"
    }
    click(Ui.Database.RunChat)
    awaitPlanForBob()
    check(chatModel.chatRunning.value == true && chatModel.appOpenUrl.value == null) { "the plan opened with the chat running ${chatModel.chatRunning.value}, queued ${chatModel.appOpenUrl.value}" }
    click(planConfirm)
    poll(what = "bob's prepared chat open in place of Chat data") { preparedBobOpen() && !exists(Ui.Database.RunChat) }
  }

  @Test fun appLink_intentFilter_matchesOnlyConnectionPaths() = uiTest {
    val notOpening = OPENS.filterNot(::opensMainActivity)
    val wronglyOpening = IGNORED.filter(::opensMainActivity)
    check(notOpening.isEmpty() && wronglyOpening.isEmpty()) {
      "links that must open the app but do not: $notOpening; links that open the app but must not: $wronglyOpening"
    }
  }

  private fun bobsOneTimeLink(bob: CliPeer): String =
    bob.createOneTimeLink().also { bob.eventually("and ask them to connect: /c <invitation_link_above>") }

  private fun UiScope.awaitPlanForBob() =
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the plan alert for bob with `Open new chat`") { exists(planConfirm) && "Open new chat" in textsOf(planConfirm) && textOf(planProfileName) == "bob" }

  private fun preparedBobOpen(): Boolean =
    chatModel.chats.value.firstOrNull { it.chatInfo.chatViewName == "bob" && it.chatInfo.nextConnectPrepared }?.id?.let { it == chatModel.chatId.value } == true

  private fun opensMainActivity(link: String): Boolean =
    androidAppContext.packageManager.queryIntentActivities(Intent(Intent.ACTION_VIEW, Uri.parse(link)).setPackage(androidAppContext.packageName), 0)
      .any { it.activityInfo.name == MainActivity::class.java.name }

  private companion object {
    const val NOTES = "Private notes"
    const val INVALID_LINK = "Invalid link"
    val OPENS = listOf(
      "simplex:/contact#/?v=2-7&smp=x",
      "simplexchat:/open",
      "https://simplex.chat/contact#/?v=2-7&smp=x",
      "https://simplex.chat/invitation#/?v=2-7&smp=x",
      "https://simplex.chat/a#key",
      "https://simplex.chat/c#key",
      "https://simplex.chat/g#key",
      "https://simplex.chat/i#key",
      "https://simplex.chat/contact/#/?v=2-7&smp=x",
      "https://simplex.chat/i/#key",
      "https://smp4.simplex.im/a#key",
      "https://smp19.simplex.im/i#key",
      "https://smp1.simplexonflux.com/c#key",
      "https://smp6.simplexonflux.com/g#key",
    )
    val IGNORED = listOf(
      "https://simplex.chat/",
      "https://simplex.chat/crowdfunding/",
      "https://simplex.chat/blog/",
      "https://simplex.chat/contacts",
      "https://simplex.chat/about",
      "https://localhost/a#key",
      "https://example.com/i#key",
    )
  }
}
