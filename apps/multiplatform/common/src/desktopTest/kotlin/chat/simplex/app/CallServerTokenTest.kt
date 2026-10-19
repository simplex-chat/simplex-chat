package chat.simplex.app

import chat.simplex.common.views.call.hasValidCallServerToken
import chat.simplex.common.views.call.newCallServerToken
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class CallServerTokenTest {
  @Test
  fun testCallServerTokenRequiresExactTokenParameter() {
    val token = "secret"

    assertTrue(hasValidCallServerToken(mapOf("token" to listOf(token)), token))
    assertFalse(hasValidCallServerToken(mapOf("token" to listOf("wrong")), token))
    assertFalse(hasValidCallServerToken(mapOf("x-token" to listOf(token)), token))
    assertFalse(hasValidCallServerToken(mapOf("token" to listOf(token)), ""))
  }

  @Test
  fun testCallServerTokenIsUrlSafe() {
    val token = newCallServerToken()

    assertTrue(token.length >= 40)
    assertFalse(token.contains("+"))
    assertFalse(token.contains("/"))
    assertFalse(token.contains("="))
  }
}
