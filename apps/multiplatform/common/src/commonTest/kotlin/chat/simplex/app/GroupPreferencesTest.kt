package chat.simplex.app

import chat.simplex.common.model.*
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.encodeToString
import kotlin.test.Test
import kotlin.test.assertEquals

class GroupPreferencesTest {
  @Test
  fun testRoundTripKeepsPreferencesWithoutUI() {
    val full = json.decodeFromString<FullGroupPreferences>(
      """
      {"timedMessages":{"enable":"off"},"directMessages":{"enable":"off"},"fullDelete":{"enable":"off"},"reactions":{"enable":"on"},
      "voice":{"enable":"on"},"files":{"enable":"on"},"simplexLinks":{"enable":"on"},"reports":{"enable":"on"},"history":{"enable":"on"},
      "support":{"enable":"on"},"signMessages":{"enable":"off"},"sessions":{"enable":"on","role":"admin"},
      "comments":{"enable":"on","duration":3600},"statusMessages":{"enable":"on","role":"owner"},"commands":[]}
      """
    )
    val prefs = json.decodeFromString<GroupPreferences>(json.encodeToString(full.toGroupPreferences()))
    assertEquals(RoleGroupPreference(GroupFeatureEnabled.ON, GroupMemberRole.Admin), prefs.sessions)
    assertEquals(CommentsGroupPreference(GroupFeatureEnabled.ON, 3600), prefs.comments)
    assertEquals(RoleGroupPreference(GroupFeatureEnabled.ON, GroupMemberRole.Owner), prefs.statusMessages)
  }
}
