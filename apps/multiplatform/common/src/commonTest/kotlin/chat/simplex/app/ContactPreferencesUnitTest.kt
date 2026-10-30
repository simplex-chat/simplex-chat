package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.chat.SendReceipts
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull
import kotlin.test.assertSame

class ContactPreferencesUnitTest {

  private fun enabled(asymmetric: Boolean, user: FeatureAllowed, contact: FeatureAllowed): FeatureEnabled =
    FeatureEnabled.enabled(asymmetric, user = SimpleChatPreference(user), contact = SimpleChatPreference(contact))

  private fun pref(user: ContactUserPref, contact: FeatureAllowed = FeatureAllowed.YES): ContactUserPreference =
    ContactUserPreference(
      enabled = FeatureEnabled(forUser = true, forContact = true),
      userPreference = user,
      contactPreference = SimpleChatPreference(contact)
    )

  private fun timedPref(user: ContactUserPrefTimed): ContactUserPreferenceTimed =
    ContactUserPreferenceTimed(
      enabled = FeatureEnabled(forUser = false, forContact = false),
      userPreference = user,
      contactPreference = TimedMessagesPreference(FeatureAllowed.YES)
    )

  private fun userPrefs(
    timed: ContactUserPrefTimed = ContactUserPrefTimed.User(TimedMessagesPreference(FeatureAllowed.NO)),
    fullDelete: ContactUserPref = ContactUserPref.User(SimpleChatPreference(FeatureAllowed.NO)),
    reactions: ContactUserPref = ContactUserPref.User(SimpleChatPreference(FeatureAllowed.YES)),
    voice: ContactUserPref = ContactUserPref.User(SimpleChatPreference(FeatureAllowed.YES)),
    files: ContactUserPref = ContactUserPref.User(SimpleChatPreference(FeatureAllowed.YES)),
    calls: ContactUserPref = ContactUserPref.User(SimpleChatPreference(FeatureAllowed.YES)),
    commands: List<ChatBotCommand>? = null,
  ): ContactUserPreferences =
    ContactUserPreferences(
      timedMessages = timedPref(timed),
      fullDelete = pref(fullDelete),
      reactions = pref(reactions),
      voice = pref(voice),
      files = pref(files),
      calls = pref(calls),
      commands = commands,
    )

  private fun allowed(
    timedAllowed: Boolean = false,
    ttl: Int? = null,
    fullDelete: ContactFeatureAllowed = ContactFeatureAllowed.UserDefault(FeatureAllowed.NO),
    reactions: ContactFeatureAllowed = ContactFeatureAllowed.UserDefault(FeatureAllowed.YES),
    voice: ContactFeatureAllowed = ContactFeatureAllowed.UserDefault(FeatureAllowed.YES),
    files: ContactFeatureAllowed = ContactFeatureAllowed.UserDefault(FeatureAllowed.YES),
    calls: ContactFeatureAllowed = ContactFeatureAllowed.UserDefault(FeatureAllowed.YES),
    commands: List<ChatBotCommand>? = null,
  ): ContactFeaturesAllowed =
    ContactFeaturesAllowed(
      timedMessagesAllowed = timedAllowed,
      timedMessagesTTL = ttl,
      fullDelete = fullDelete,
      reactions = reactions,
      voice = voice,
      files = files,
      calls = calls,
      commands = commands,
    )

  @Test
  fun userAlwaysAndContactNoEnablesOnlyForContactWhenAsymmetric() {
    assertEquals(FeatureEnabled(forUser = false, forContact = true), enabled(true, FeatureAllowed.ALWAYS, FeatureAllowed.NO))
    assertEquals(FeatureEnabled(forUser = false, forContact = false), enabled(false, FeatureAllowed.ALWAYS, FeatureAllowed.NO))
  }

  @Test
  fun userNoAndContactAlwaysEnablesOnlyForUserWhenAsymmetric() {
    assertEquals(FeatureEnabled(forUser = true, forContact = false), enabled(true, FeatureAllowed.NO, FeatureAllowed.ALWAYS))
    assertEquals(FeatureEnabled(forUser = false, forContact = false), enabled(false, FeatureAllowed.NO, FeatureAllowed.ALWAYS))
  }

  @Test
  fun featureIsOffForBothWhenEitherSideSaysNoWithoutAlways() {
    for (asymmetric in listOf(true, false)) {
      val off = FeatureEnabled(forUser = false, forContact = false)
      assertEquals(off, enabled(asymmetric, FeatureAllowed.YES, FeatureAllowed.NO), "yes/no asymmetric=$asymmetric")
      assertEquals(off, enabled(asymmetric, FeatureAllowed.NO, FeatureAllowed.YES), "no/yes asymmetric=$asymmetric")
      assertEquals(off, enabled(asymmetric, FeatureAllowed.NO, FeatureAllowed.NO), "no/no asymmetric=$asymmetric")
    }
  }

  @Test
  fun featureIsOnForBothWhenNeitherSideSaysNo() {
    val on = FeatureEnabled(forUser = true, forContact = true)
    for (asymmetric in listOf(true, false)) {
      for (user in listOf(FeatureAllowed.YES, FeatureAllowed.ALWAYS)) {
        for (contact in listOf(FeatureAllowed.YES, FeatureAllowed.ALWAYS)) {
          assertEquals(on, enabled(asymmetric, user, contact), "$user/$contact asymmetric=$asymmetric")
        }
      }
    }
  }

  @Test
  fun onlyDisappearingMessagesAreSymmetric() {
    assertEquals(listOf(ChatFeature.TimedMessages), ChatFeature.entries.filter { !it.asymmetric })
  }

  @Test
  fun userDefaultPreferenceBecomesUserDefaultChoice() {
    for (a in FeatureAllowed.entries) {
      assertEquals(ContactFeatureAllowed.UserDefault(a), contactUserPrefToFeatureAllowed(pref(ContactUserPref.User(SimpleChatPreference(a)))))
    }
  }

  @Test
  fun contactOverrideBecomesExplicitChoice() {
    assertSame(ContactFeatureAllowed.Always, contactUserPrefToFeatureAllowed(pref(ContactUserPref.Contact(SimpleChatPreference(FeatureAllowed.ALWAYS)))))
    assertSame(ContactFeatureAllowed.Yes, contactUserPrefToFeatureAllowed(pref(ContactUserPref.Contact(SimpleChatPreference(FeatureAllowed.YES)))))
    assertSame(ContactFeatureAllowed.No, contactUserPrefToFeatureAllowed(pref(ContactUserPref.Contact(SimpleChatPreference(FeatureAllowed.NO)))))
  }

  @Test
  fun contactUserPreferencesMapToFeaturesAllowedPerFeature() {
    val commands = listOf<ChatBotCommand>(ChatBotCommand.Command(keyword = "help", label = "Help", params = null))
    val prefs = userPrefs(
      timed = ContactUserPrefTimed.Contact(TimedMessagesPreference(FeatureAllowed.YES, ttl = 86400)),
      fullDelete = ContactUserPref.Contact(SimpleChatPreference(FeatureAllowed.ALWAYS)),
      reactions = ContactUserPref.Contact(SimpleChatPreference(FeatureAllowed.NO)),
      voice = ContactUserPref.User(SimpleChatPreference(FeatureAllowed.NO)),
      files = ContactUserPref.Contact(SimpleChatPreference(FeatureAllowed.YES)),
      calls = ContactUserPref.User(SimpleChatPreference(FeatureAllowed.YES)),
      commands = commands,
    )
    val result = contactUserPrefsToFeaturesAllowed(prefs)
    assertEquals(
      allowed(
        timedAllowed = true,
        ttl = 86400,
        fullDelete = ContactFeatureAllowed.Always,
        reactions = ContactFeatureAllowed.No,
        voice = ContactFeatureAllowed.UserDefault(FeatureAllowed.NO),
        files = ContactFeatureAllowed.Yes,
        calls = ContactFeatureAllowed.UserDefault(FeatureAllowed.YES),
        commands = commands,
      ),
      result
    )
    assertSame(commands, result.commands)
  }

  @Test
  fun disappearingMessagesAllowedForYesAndAlwaysKeepingTtl() {
    assertEquals(true to 3600, contactUserPrefsToFeaturesAllowed(userPrefs(timed = ContactUserPrefTimed.User(TimedMessagesPreference(FeatureAllowed.YES, ttl = 3600)))).let { it.timedMessagesAllowed to it.timedMessagesTTL })
    assertEquals(true to 600, contactUserPrefsToFeaturesAllowed(userPrefs(timed = ContactUserPrefTimed.Contact(TimedMessagesPreference(FeatureAllowed.ALWAYS, ttl = 600)))).let { it.timedMessagesAllowed to it.timedMessagesTTL })
    assertEquals(false to 604800, contactUserPrefsToFeaturesAllowed(userPrefs(timed = ContactUserPrefTimed.Contact(TimedMessagesPreference(FeatureAllowed.NO, ttl = 604800)))).let { it.timedMessagesAllowed to it.timedMessagesTTL })
    assertEquals(false to null, contactUserPrefsToFeaturesAllowed(userPrefs(timed = ContactUserPrefTimed.User(TimedMessagesPreference(FeatureAllowed.NO)))).let { it.timedMessagesAllowed to it.timedMessagesTTL })
  }

  @Test
  fun userDefaultChoiceSavesNoOverride() {
    for (a in FeatureAllowed.entries) {
      assertNull(contactFeatureAllowedToPref(ContactFeatureAllowed.UserDefault(a)))
    }
  }

  @Test
  fun explicitChoiceSavesMatchingOverride() {
    assertEquals(SimpleChatPreference(FeatureAllowed.ALWAYS), contactFeatureAllowedToPref(ContactFeatureAllowed.Always))
    assertEquals(SimpleChatPreference(FeatureAllowed.YES), contactFeatureAllowedToPref(ContactFeatureAllowed.Yes))
    assertEquals(SimpleChatPreference(FeatureAllowed.NO), contactFeatureAllowedToPref(ContactFeatureAllowed.No))
  }

  @Test
  fun featuresAllowedMapToChatPreferences() {
    val commands = listOf<ChatBotCommand>(ChatBotCommand.Menu(label = "Menu", commands = emptyList()))
    val result = contactFeaturesAllowedToPrefs(
      allowed(
        timedAllowed = true,
        ttl = 600,
        fullDelete = ContactFeatureAllowed.Yes,
        reactions = ContactFeatureAllowed.UserDefault(FeatureAllowed.YES),
        voice = ContactFeatureAllowed.No,
        files = ContactFeatureAllowed.Always,
        calls = ContactFeatureAllowed.UserDefault(FeatureAllowed.NO),
        commands = commands,
      )
    )
    assertEquals(
      ChatPreferences(
        timedMessages = TimedMessagesPreference(FeatureAllowed.YES, ttl = 600),
        fullDelete = SimpleChatPreference(FeatureAllowed.YES),
        reactions = null,
        voice = SimpleChatPreference(FeatureAllowed.NO),
        files = SimpleChatPreference(FeatureAllowed.ALWAYS),
        calls = null,
        commands = commands,
      ),
      result
    )
    assertSame(commands, result.commands)
  }

  @Test
  fun disappearingMessagesNotAllowedSaveNoWithTtl() {
    assertEquals(TimedMessagesPreference(FeatureAllowed.NO, ttl = 3600), contactFeaturesAllowedToPrefs(allowed(timedAllowed = false, ttl = 3600)).timedMessages)
    assertEquals(TimedMessagesPreference(FeatureAllowed.NO, ttl = null), contactFeaturesAllowedToPrefs(allowed(timedAllowed = false, ttl = null)).timedMessages)
  }

  @Test
  fun userDefaultsRoundTripToNoContactOverrides() {
    val prefs = contactFeaturesAllowedToPrefs(contactUserPrefsToFeaturesAllowed(userPrefs()))
    assertEquals(
      ChatPreferences(
        timedMessages = TimedMessagesPreference(FeatureAllowed.NO, ttl = null),
        fullDelete = null,
        reactions = null,
        voice = null,
        files = null,
        calls = null,
        commands = null,
      ),
      prefs
    )
  }

  @Test
  fun contactOverridesRoundTripUnchanged() {
    val prefs = contactFeaturesAllowedToPrefs(
      contactUserPrefsToFeaturesAllowed(
        userPrefs(
          fullDelete = ContactUserPref.Contact(SimpleChatPreference(FeatureAllowed.ALWAYS)),
          reactions = ContactUserPref.Contact(SimpleChatPreference(FeatureAllowed.NO)),
          voice = ContactUserPref.Contact(SimpleChatPreference(FeatureAllowed.YES)),
          files = ContactUserPref.Contact(SimpleChatPreference(FeatureAllowed.NO)),
          calls = ContactUserPref.Contact(SimpleChatPreference(FeatureAllowed.ALWAYS)),
        )
      )
    )
    assertEquals(SimpleChatPreference(FeatureAllowed.ALWAYS), prefs.fullDelete)
    assertEquals(SimpleChatPreference(FeatureAllowed.NO), prefs.reactions)
    assertEquals(SimpleChatPreference(FeatureAllowed.YES), prefs.voice)
    assertEquals(SimpleChatPreference(FeatureAllowed.NO), prefs.files)
    assertEquals(SimpleChatPreference(FeatureAllowed.ALWAYS), prefs.calls)
  }

  @Test
  fun contactFeatureChoicesListUserDefaultFirst() {
    assertEquals(
      listOf(ContactFeatureAllowed.UserDefault(FeatureAllowed.NO), ContactFeatureAllowed.Always, ContactFeatureAllowed.Yes, ContactFeatureAllowed.No),
      ContactFeatureAllowed.values(FeatureAllowed.NO)
    )
  }

  @Test
  fun contactFeatureChoiceResolvesToAllowedValue() {
    for (a in FeatureAllowed.entries) {
      assertEquals(a, ContactFeatureAllowed.UserDefault(a).allowed)
    }
    assertEquals(FeatureAllowed.ALWAYS, ContactFeatureAllowed.Always.allowed)
    assertEquals(FeatureAllowed.YES, ContactFeatureAllowed.Yes.allowed)
    assertEquals(FeatureAllowed.NO, ContactFeatureAllowed.No.allowed)
  }

  @Test
  fun sendReceiptsWithoutContactSettingFollowUserDefault() {
    assertEquals(SendReceipts.UserDefault(true), SendReceipts.fromBool(null, userDefault = true))
    assertEquals(SendReceipts.UserDefault(false), SendReceipts.fromBool(null, userDefault = false))
  }

  @Test
  fun sendReceiptsContactSettingOverridesUserDefault() {
    assertSame(SendReceipts.Yes, SendReceipts.fromBool(true, userDefault = false))
    assertSame(SendReceipts.No, SendReceipts.fromBool(false, userDefault = true))
  }

  @Test
  fun sendReceiptsSaveNullForUserDefault() {
    assertEquals(true, SendReceipts.Yes.bool)
    assertEquals(false, SendReceipts.No.bool)
    assertNull(SendReceipts.UserDefault(true).bool)
    assertNull(SendReceipts.UserDefault(false).bool)
  }
}
