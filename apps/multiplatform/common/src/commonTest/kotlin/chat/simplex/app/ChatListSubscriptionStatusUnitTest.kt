package chat.simplex.app

import chat.simplex.common.model.SMPServerSubs
import chat.simplex.common.views.chatlist.AppSubscriptionStatus
import chat.simplex.common.views.chatlist.SubscriptionColorType
import chat.simplex.common.views.chatlist.subscriptionStatusColorAndPercentage
import kotlin.test.Test
import kotlin.test.assertEquals

class ChatListSubscriptionStatusUnitTest {
  private val socksProxy = "localhost:9050"

  private fun status(online: Boolean = true, socksProxy: String? = null, active: Int, pending: Int, hasSess: Boolean): AppSubscriptionStatus =
    subscriptionStatusColorAndPercentage(online = online, socksProxy = socksProxy, subs = SMPServerSubs(ssActive = active, ssPending = pending), hasSess = hasSess)

  @Test
  fun offlineIsDisconnectedWithFullIconAndZeroPercent() {
    assertEquals(AppSubscriptionStatus(SubscriptionColorType.DISCONNECTED, 1f, 0f), status(online = false, active = 5, pending = 0, hasSess = true))
  }

  @Test
  fun offlineWithProxyIsStillDisconnected() {
    assertEquals(AppSubscriptionStatus(SubscriptionColorType.DISCONNECTED, 1f, 0f), status(online = false, socksProxy = socksProxy, active = 5, pending = 0, hasSess = true))
  }

  @Test
  fun noSubscriptionsAndNoSessionIsActiveWithEmptyIcon() {
    assertEquals(AppSubscriptionStatus(SubscriptionColorType.ACTIVE, 0f, 0f), status(active = 0, pending = 0, hasSess = false))
  }

  @Test
  fun noSubscriptionsWithSessionIsActiveWithEmptyIcon() {
    assertEquals(AppSubscriptionStatus(SubscriptionColorType.ACTIVE, 0f, 0f), status(active = 0, pending = 0, hasSess = true))
  }

  @Test
  fun onlyPendingWithSessionIsActiveAtZero() {
    assertEquals(AppSubscriptionStatus(SubscriptionColorType.ACTIVE, 0f, 0f), status(active = 0, pending = 4, hasSess = true))
  }

  @Test
  fun onlyPendingWithoutSessionIsDisconnected() {
    assertEquals(AppSubscriptionStatus(SubscriptionColorType.DISCONNECTED, 1f, 0f), status(active = 0, pending = 4, hasSess = false))
  }

  @Test
  fun allActiveWithSessionIsFull() {
    assertEquals(AppSubscriptionStatus(SubscriptionColorType.ACTIVE, 1f, 1f), status(active = 4, pending = 0, hasSess = true))
  }

  @Test
  fun activeWithoutSessionIsActiveDisconnected() {
    assertEquals(AppSubscriptionStatus(SubscriptionColorType.ACTIVE_DISCONNECTED, 1f, 1f), status(active = 4, pending = 0, hasSess = false))
  }

  @Test
  fun socksProxyUsesProxyColor() {
    assertEquals(AppSubscriptionStatus(SubscriptionColorType.ACTIVE_SOCKS_PROXY, 1f, 1f), status(socksProxy = socksProxy, active = 4, pending = 0, hasSess = true))
    assertEquals(AppSubscriptionStatus(SubscriptionColorType.ACTIVE_SOCKS_PROXY, 0f, 0f), status(socksProxy = socksProxy, active = 0, pending = 0, hasSess = false))
  }

  @Test
  fun iconFillIsRoundedToQuarterAndPercentIsExact() {
    assertEquals(AppSubscriptionStatus(SubscriptionColorType.ACTIVE, 0.5f, 0.6f), status(active = 3, pending = 2, hasSess = true))
    assertEquals(AppSubscriptionStatus(SubscriptionColorType.ACTIVE, 0.25f, 0.2f), status(active = 1, pending = 4, hasSess = true))
    assertEquals(AppSubscriptionStatus(SubscriptionColorType.ACTIVE, 1f, 0.875f), status(active = 7, pending = 1, hasSess = true))
  }
}
