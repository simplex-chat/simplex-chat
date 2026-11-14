package chat.simplex.app

import chat.simplex.common.model.ContactStatus
import chat.simplex.common.model.json
import kotlin.test.Test
import kotlin.test.assertEquals

class ContactStatusTest {
  @Test
  fun testContactStatusesOfCore() {
    listOf("active", "deleted", "deletedByUser", "rejected").forEach { status ->
      val decoded = json.decodeFromString(ContactStatus.serializer(), "\"$status\"")
      assertEquals("\"$status\"", json.encodeToString(ContactStatus.serializer(), decoded))
    }
  }
}
