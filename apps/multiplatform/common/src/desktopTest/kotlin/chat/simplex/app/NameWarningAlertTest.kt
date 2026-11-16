package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.newchat.*
import kotlinx.datetime.Instant
import java.util.Locale
import kotlin.test.AfterTest
import kotlin.test.BeforeTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertIs
import kotlin.test.assertNotNull
import kotlin.test.assertNull

class NameWarningAlertTest {
  private val locale = Locale.getDefault()
  private val expiredAt by lazy { badgeDateText(Instant.parse("2026-01-01T12:00:00Z")) }
  private val graceUntil by lazy { badgeDateText(Instant.parse("2099-01-01T12:00:00Z")) }
  private val price = "\$20 for 2 years"

  @BeforeTest
  fun setEnglishLocale() {
    Locale.setDefault(Locale.US)
  }

  @AfterTest
  fun restoreLocale() {
    Locale.setDefault(locale)
  }

  private fun fixture(name: String): API =
    json.decodeFromString(API.serializer(), javaClass.getResource("/names/$name.json")!!.readText())

  private fun plan(name: String): CR.CRConnectionPlan =
    assertIs<CR.CRConnectionPlan>(assertIs<API.Result>(fixture(name)).res)

  private fun planAlert(name: String): NameWarningAlert {
    val plan = plan(name)
    return assertNotNull(planNameWarningAlert(plan.planSimplexName, plan.connectionPlan))
  }

  private fun errorAlert(name: String): NameWarningAlert {
    val error = assertIs<ChatErrorType.SimplexDomainNotReady>(assertIs<ChatError.ChatErrorChat>(assertIs<API.Error>(fixture(name)).err).errorType)
    return nameWarningAlert(error.simplexDomain, assertIs<SimplexDomainError.NameWarning>(error.simplexDomainError).nameWarning, own = false, chat = null)
  }

  private fun assertAlert(alert: NameWarningAlert, title: String, text: String, action: NameWarningAction?, chat: String?) {
    assertEquals(title, alert.title)
    assertEquals(text, alert.text)
    assertEquals(action, alert.action)
    assertEquals(chat, alert.chat?.localDisplayName)
  }

  @Test
  fun knownChatNameExpired() {
    assertAlert(planAlert("plan-known-expired"), "Name expired", "alice.simplex expired on $expiredAt. Its owner can renew it until $graceUntil.", null, "alice")
  }

  @Test
  fun ownNameExpired() {
    assertAlert(planAlert("plan-own-expired"), "Your name has expired", "Your name alice.simplex expired on $expiredAt. Renew it before $graceUntil.", NameWarningAction.Renew, null)
  }

  @Test
  fun knownChatNameAvailable() {
    assertAlert(planAlert("plan-known-available"), "Name no longer registered", "alice.simplex is available for registration for $price.", NameWarningAction.Register, "alice")
  }

  @Test
  fun ownNameAvailable() {
    assertAlert(planAlert("plan-own-available"), "Your name has expired", "Your name alice.simplex is no longer registered. It is available for registration for $price.", NameWarningAction.ReRegister, null)
  }

  @Test
  fun knownChatNameReserved() {
    assertAlert(planAlert("plan-known-reserved"), "Name not registered", "alice.simplex is reserved for community. Please contact SimpleX Chat if you are interested in using it.", NameWarningAction.ConnectToTeam, "alice")
  }

  @Test
  fun knownChatNameMoved() {
    val plan = plan("plan-known-moved")
    assertNull(planNameWarningAlert(plan.planSimplexName, plan.connectionPlan))
    assertEquals(listOf("alice"), plan.connectionPlan.localChats.map { it.localDisplayName })
  }

  @Test
  fun nameAvailable() {
    assertAlert(errorAlert("error-available"), "Name not registered", "nobody.simplex is available for registration for $price.", NameWarningAction.Register, null)
  }

  @Test
  fun nameNotRegistered() {
    assertAlert(errorAlert("error-not-registered"), "Name not registered", "This SimpleX name is not registered. Please check the name.", null, null)
  }
}
