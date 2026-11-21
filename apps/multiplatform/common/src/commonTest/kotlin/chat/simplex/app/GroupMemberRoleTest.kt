package chat.simplex.app

import chat.simplex.common.model.*
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.decodeFromJsonElement
import kotlinx.serialization.json.encodeToJsonElement
import kotlinx.serialization.json.jsonObject
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class GroupMemberRoleTest {
  @Test
  fun testKnownRoles() {
    val roles = listOf("relay", "observer", "author", "member", "moderator", "admin", "owner")
    val decoded = roles.map { json.decodeFromString<GroupMemberRole>("\"$it\"") }
    assertEquals(roles, decoded.map { it.memberRole })
    assertEquals(roles.map { "\"$it\"" }, decoded.map { json.encodeToString(it) })
    assertEquals(decoded, decoded.reversed().sorted())
  }

  @Test
  fun testUnknownRole() {
    val role = json.decodeFromString<GroupMemberRole>("\"vip\"")
    assertEquals("vip", role.memberRole)
    assertEquals("vip", role.text(isChannel = false))
    assertEquals("\"vip\"", json.encodeToString(role))
    assertTrue(role < GroupMemberRole.Relay)
  }

  @Test
  fun testMemberWithUnknownRole() {
    val member = json.encodeToJsonElement(GroupMember.sampleData).jsonObject
    val decoded = json.decodeFromJsonElement<GroupMember>(JsonObject(member + ("memberRole" to JsonPrimitive("vip"))))
    assertEquals("vip", decoded.memberRole.memberRole)
  }
}
