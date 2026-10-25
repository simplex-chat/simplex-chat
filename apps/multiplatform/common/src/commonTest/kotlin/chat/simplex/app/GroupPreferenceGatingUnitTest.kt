package chat.simplex.app

import chat.simplex.common.model.*
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class GroupPreferenceGatingUnitTest {

  private fun member(role: GroupMemberRole): GroupMember = GroupMember.sampleData.copy(memberRole = role)

  private fun group(userRole: GroupMemberRole, prefs: FullGroupPreferences): GroupInfo =
    GroupInfo.sampleData.copy(fullGroupPreferences = prefs, membership = member(userRole))

  private val prefs = FullGroupPreferences.sampleData

  @Test
  fun rolePreferenceOnRequiresMinimumRole() {
    val ownersOnly = RoleGroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Owner)
    assertFalse(ownersOnly.on(member(GroupMemberRole.Member)))
    assertFalse(ownersOnly.on(member(GroupMemberRole.Admin)))
    assertTrue(ownersOnly.on(member(GroupMemberRole.Owner)))
  }

  @Test
  fun rolePreferenceWithoutRoleIsOnForObservers() {
    assertTrue(RoleGroupPreference(GroupFeatureEnabled.ON, role = null).on(member(GroupMemberRole.Observer)))
  }

  @Test
  fun rolePreferenceOffIsOffForOwners() {
    assertFalse(RoleGroupPreference(GroupFeatureEnabled.OFF, role = null).on(member(GroupMemberRole.Owner)))
    assertFalse(RoleGroupPreference(GroupFeatureEnabled.OFF, role = GroupMemberRole.Member).on(member(GroupMemberRole.Owner)))
  }

  @Test
  fun directMessagesForOwnersAreOffForMembersAndOnForOwners() {
    val p = prefs.copy(directMessages = RoleGroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Owner))
    assertFalse(group(GroupMemberRole.Member, p).groupFeatureEnabled(GroupFeature.DirectMessages))
    assertTrue(group(GroupMemberRole.Owner, p).groupFeatureEnabled(GroupFeature.DirectMessages))
  }

  @Test
  fun voiceForAdminsIsOffForMembersInComposer() {
    val p = prefs.copy(voice = RoleGroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Admin))
    assertFalse(group(GroupMemberRole.Member, p).groupFeatureEnabled(GroupFeature.Voice))
    assertFalse(group(GroupMemberRole.Member, p).featureEnabled(ChatFeature.Voice))
    assertTrue(group(GroupMemberRole.Admin, p).featureEnabled(ChatFeature.Voice))
  }

  @Test
  fun filesWithoutRoleAreOnForObservers() {
    val p = prefs.copy(files = RoleGroupPreference(GroupFeatureEnabled.ON, role = null))
    assertTrue(group(GroupMemberRole.Observer, p).groupFeatureEnabled(GroupFeature.Files))
    assertTrue(group(GroupMemberRole.Observer, p).featureEnabled(ChatFeature.Files))
  }

  @Test
  fun simplexLinksOffAreOffEvenForOwners() {
    val p = prefs.copy(simplexLinks = RoleGroupPreference(GroupFeatureEnabled.OFF, role = null))
    assertFalse(group(GroupMemberRole.Owner, p).groupFeatureEnabled(GroupFeature.SimplexLinks))
  }

  @Test
  fun simplexLinksForAdminsAreOffForMembers() {
    val p = prefs.copy(simplexLinks = RoleGroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Admin))
    assertFalse(group(GroupMemberRole.Member, p).groupFeatureEnabled(GroupFeature.SimplexLinks))
    assertTrue(group(GroupMemberRole.Owner, p).groupFeatureEnabled(GroupFeature.SimplexLinks))
  }

  @Test
  fun featuresWithoutRoleIgnoreTheUserRole() {
    val on = prefs.copy(
      timedMessages = TimedMessagesGroupPreference(GroupFeatureEnabled.ON, ttl = 86400),
      fullDelete = GroupPreference(GroupFeatureEnabled.ON),
      reactions = GroupPreference(GroupFeatureEnabled.ON),
      reports = GroupPreference(GroupFeatureEnabled.ON),
      history = GroupPreference(GroupFeatureEnabled.ON),
      support = GroupPreference(GroupFeatureEnabled.ON),
      signMessages = GroupPreference(GroupFeatureEnabled.ON),
    )
    val observer = group(GroupMemberRole.Observer, on)
    listOf(
      GroupFeature.TimedMessages, GroupFeature.FullDelete, GroupFeature.Reactions,
      GroupFeature.Reports, GroupFeature.History, GroupFeature.Support, GroupFeature.SignMessages
    ).forEach { assertTrue(observer.groupFeatureEnabled(it), it.name) }
  }

  @Test
  fun reactionsOffDisableTheReactionsFeature() {
    val p = prefs.copy(reactions = GroupPreference(GroupFeatureEnabled.OFF))
    assertFalse(group(GroupMemberRole.Owner, p).featureEnabled(ChatFeature.Reactions))
    assertFalse(group(GroupMemberRole.Owner, p).groupFeatureEnabled(GroupFeature.Reactions))
  }

  @Test
  fun callsAreNeverEnabledInGroups() {
    assertFalse(group(GroupMemberRole.Owner, prefs).featureEnabled(ChatFeature.Calls))
  }

  @Test
  fun timedMessagesTtlIsReportedOnlyWhenEnabled() {
    val on = prefs.copy(timedMessages = TimedMessagesGroupPreference(GroupFeatureEnabled.ON, ttl = 3600))
    val off = prefs.copy(timedMessages = TimedMessagesGroupPreference(GroupFeatureEnabled.OFF, ttl = 3600))
    assertEquals(3600, group(GroupMemberRole.Member, on).timedMessagesTTL)
    assertEquals(null, group(GroupMemberRole.Member, off).timedMessagesTTL)
  }

  @Test
  fun toGroupPreferencesKeepsEveryPreferenceWithRoleAndTtl() {
    val full = prefs.copy(
      timedMessages = TimedMessagesGroupPreference(GroupFeatureEnabled.ON, ttl = 604800),
      directMessages = RoleGroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Owner),
      voice = RoleGroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Admin),
      files = RoleGroupPreference(GroupFeatureEnabled.OFF, role = null),
      simplexLinks = RoleGroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Moderator),
      fullDelete = GroupPreference(GroupFeatureEnabled.ON),
      reactions = GroupPreference(GroupFeatureEnabled.OFF),
      reports = GroupPreference(GroupFeatureEnabled.OFF),
      history = GroupPreference(GroupFeatureEnabled.OFF),
      support = GroupPreference(GroupFeatureEnabled.OFF),
      signMessages = GroupPreference(GroupFeatureEnabled.ON),
    )
    assertEquals(
      GroupPreferences(
        timedMessages = TimedMessagesGroupPreference(GroupFeatureEnabled.ON, ttl = 604800),
        directMessages = RoleGroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Owner),
        fullDelete = GroupPreference(GroupFeatureEnabled.ON),
        reactions = GroupPreference(GroupFeatureEnabled.OFF),
        voice = RoleGroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Admin),
        files = RoleGroupPreference(GroupFeatureEnabled.OFF, role = null),
        simplexLinks = RoleGroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Moderator),
        reports = GroupPreference(GroupFeatureEnabled.OFF),
        history = GroupPreference(GroupFeatureEnabled.OFF),
        support = GroupPreference(GroupFeatureEnabled.OFF),
        signMessages = GroupPreference(GroupFeatureEnabled.ON),
        commands = emptyList(),
      ),
      full.toGroupPreferences()
    )
  }

  @Test
  fun groupPreferenceEnabledForMemberAppliesTheRole() {
    val pref = GroupPreference(GroupFeatureEnabled.ON)
    assertEquals(GroupFeatureEnabled.OFF, pref.enabled(GroupMemberRole.Admin, member(GroupMemberRole.Member)))
    assertEquals(GroupFeatureEnabled.ON, pref.enabled(GroupMemberRole.Admin, member(GroupMemberRole.Admin)))
    assertEquals(GroupFeatureEnabled.ON, pref.enabled(null, member(GroupMemberRole.Observer)))
    assertEquals(GroupFeatureEnabled.ON, pref.enabled(GroupMemberRole.Admin, null))
    assertEquals(GroupFeatureEnabled.OFF, GroupPreference(GroupFeatureEnabled.OFF).enabled(null, member(GroupMemberRole.Owner)))
  }
}
