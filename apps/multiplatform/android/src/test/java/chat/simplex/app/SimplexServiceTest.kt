package chat.simplex.app

import org.junit.Assert.assertEquals
import org.junit.Test

class SimplexServiceTest {
  @Test
  fun destroyKeepsStartedStateWhenRestartIsAllowed() {
    assertEquals(
      SimplexService.ServiceState.STARTED,
      SimplexService.serviceStateOnDestroy(allowRestart = true, stopRequested = false),
    )
  }

  @Test
  fun destroyStoresStoppedStateWhenRestartIsNotAllowed() {
    assertEquals(
      SimplexService.ServiceState.STOPPED,
      SimplexService.serviceStateOnDestroy(allowRestart = false, stopRequested = false),
    )
  }

  @Test
  fun destroyStoresStoppedStateForExplicitStopRequests() {
    assertEquals(
      SimplexService.ServiceState.STOPPED,
      SimplexService.serviceStateOnDestroy(allowRestart = true, stopRequested = true),
    )
  }
}
