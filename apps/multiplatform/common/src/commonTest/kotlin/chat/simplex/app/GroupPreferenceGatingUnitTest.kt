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
  fun filesForAdminsAreOffForMembersInComposer() {
    val p = prefs.copy(files = RoleGroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Admin))
    assertFalse(group(GroupMemberRole.Member, p).groupFeatureEnabled(GroupFeature.Files))
    assertFalse(group(GroupMemberRole.Member, p).featureEnabled(ChatFeature.Files))
    assertTrue(group(GroupMemberRole.Admin, p).groupFeatureEnabled(GroupFeature.Files))
    assertTrue(group(GroupMemberRole.Admin, p).featureEnabled(ChatFeature.Files))
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
    val off = GroupPreference(GroupFeatureEnabled.OFF)
    val on = GroupPreference(GroupFeatureEnabled.ON)
    val allOff = prefs.copy(
      timedMessages = TimedMessagesGroupPreference(GroupFeatureEnabled.OFF, ttl = 86400),
      fullDelete = off,
      reactions = off,
      reports = off,
      history = off,
      support = off,
      signMessages = off,
    )
    val onlyOneOn = mapOf(
      GroupFeature.TimedMessages to allOff.copy(timedMessages = TimedMessagesGroupPreference(GroupFeatureEnabled.ON, ttl = 86400)),
      GroupFeature.FullDelete to allOff.copy(fullDelete = on),
      GroupFeature.Reactions to allOff.copy(reactions = on),
      GroupFeature.Reports to allOff.copy(reports = on),
      GroupFeature.History to allOff.copy(history = on),
      GroupFeature.Support to allOff.copy(support = on),
      GroupFeature.SignMessages to allOff.copy(signMessages = on),
    )
    onlyOneOn.forEach { (onFeature, p) ->
      val observer = group(GroupMemberRole.Observer, p)
      onlyOneOn.keys.forEach { assertEquals(it == onFeature, observer.groupFeatureEnabled(it), "${onFeature.name} on, ${it.name}") }
    }
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
    val commands = listOf(ChatBotCommand.Command("help", "Help", null), ChatBotCommand.Command("join", "Join", "name"))
    val full = prefs.copy(
      timedMessages = TimedMessagesGroupPreference(GroupFeatureEnabled.ON, ttl = 604800),
      directMessages = RoleGroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Owner),
      voice = RoleGroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Admin),
      files = RoleGroupPreference(GroupFeatureEnabled.OFF, role = null),
      simplexLinks = RoleGroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Moderator),
      fullDelete = GroupPreference(GroupFeatureEnabled.ON),
      reactions = GroupPreference(GroupFeatureEnabled.OFF),
      reports = GroupPreference(GroupFeatureEnabled.OFF, role = GroupMemberRole.Observer),
      history = GroupPreference(GroupFeatureEnabled.OFF, role = GroupMemberRole.Member),
      support = GroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Admin),
      signMessages = GroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Owner),
      commands = commands,
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
        reports = GroupPreference(GroupFeatureEnabled.OFF, role = GroupMemberRole.Observer),
        history = GroupPreference(GroupFeatureEnabled.OFF, role = GroupMemberRole.Member),
        support = GroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Admin),
        signMessages = GroupPreference(GroupFeatureEnabled.ON, role = GroupMemberRole.Owner),
        commands = commands,
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
