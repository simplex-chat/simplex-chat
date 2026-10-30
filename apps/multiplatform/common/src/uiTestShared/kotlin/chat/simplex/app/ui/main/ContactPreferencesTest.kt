package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowText
import chat.simplex.app.ui.harness.closeContactInfo
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.contactOf
import chat.simplex.app.ui.harness.contactPrefs
import chat.simplex.app.ui.harness.described
import chat.simplex.app.ui.harness.savedUserPrefs
import chat.simplex.app.ui.harness.userPrefs
import chat.simplex.common.model.ChatFeature
import chat.simplex.common.model.ContactUserPref
import chat.simplex.common.model.FeatureAllowed
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertEquals
import org.junit.Test

class ContactPreferencesTest : UiTest() {
  @Test fun contactPrefsSaveNotifiesContactAndEnforces() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice one"
    awaitRowText(alice, "bob", "one")
    val chat = alice.chatList.open("bob")
    val one = chat.itemId("one")
    bob sends "\\@alice one"
    bob.eventually("message marked deleted")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`one` only marked deleted while full deletion is off") { chat.item(one)?.meta?.itemDeleted != null && exists(Ui.Chat.Item(one)) }
    chat.send("hi")
    bob.expectsMessage("alice> hi", CONNECT_TIMEOUT_MS)
    bob sends "+1 @alice hi"
    bob expects "added 👍"
    hideKeyboard()
    chat.info()
    var prefs = contactPrefs()
    poll(what = "the default preferences: everything allowed except full deletion, with `Delete after`") {
      seesText(TIMED_BOTH) && seesText(DELETE_OFF) && seesText(REACTIONS_BOTH) && seesText(VOICE_BOTH) && seesText(CALLS_BOTH) &&
        prefs.ttlShown() && "default (no)" in prefs.allowTexts(ChatFeature.FullDelete)
    }
    prefs.toggleTimed()
    prefs.allow(ChatFeature.FullDelete, "ALWAYS")
    prefs.allow(ChatFeature.Reactions, "NO")
    prefs.allow(ChatFeature.Voice, "NO")
    prefs.allow(ChatFeature.Calls, "NO")
    poll(what = "every section showing the unsaved change") {
      seesText(TIMED_OFF) && seesText(DELETE_BY_CONTACT) && seesText(REACTIONS_OFF) && seesText(VOICE_OFF) && seesText(CALLS_OFF) &&
        !seesText(TIMED_BOTH) && !seesText(VOICE_BOTH) && !prefs.ttlShown() && "always" in prefs.allowTexts(ChatFeature.FullDelete)
    }
    prefs.save()
    assertEquals("alice updated preferences for you:", bob.nextLine(CONNECT_TIMEOUT_MS))
    bob expects "Disappearing messages: off (you allow: default (yes), contact allows: no)"
    bob expects "Full deletion: enabled for you (you allow: default (no), contact allows: always)"
    bob expects "Message reactions: off (you allow: default (yes), contact allows: no)"
    bob expects "Voice messages: off (you allow: default (yes), contact allows: no)"
    bob expects "Audio/video calls: off (you allow: default (yes), contact allows: no)"
    poll(what = "the saved preferences in bob's contact") { contactOf("bob").mergedPreferences.calls.userPreference.pref.allow == FeatureAllowed.NO }
    prefs.backClosing()
    closeContactInfo()
    bob sends "@alice two"
    val two = chat.itemId("two")
    bob sends "\\@alice two"
    bob expects "message deleted"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`two` removed from alice's chat") { chat.item(two) == null && !exists(Ui.Chat.Item(two)) }
    bob sends "+^ @alice hi"
    bob expects "bad chat command: feature not allowed Message reactions"
    click(described("Record voice message"))
    poll(what = "the `Allow voice messages?` alert with `Allow`") {
      seesText("Allow voice messages?") && seesText("You need to allow your contact to send voice messages to be able to send them.") &&
        "Allow" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    assertEquals("alice updated preferences for you:", bob.nextLine(CONNECT_TIMEOUT_MS))
    bob expects "Voice messages: enabled (you allow: default (yes), contact allows: yes)"
    chat.info()
    prefs = contactPrefs()
    poll(what = "voice allowed and disappearing messages still off") { seesText(VOICE_BOTH) && seesText(TIMED_OFF) && !prefs.ttlShown() }
    prefs.toggleTimed()
    poll(what = "`Delete after` shown with `1 day`") { prefs.ttlShown() && "1 day" in prefs.ttlTexts() && seesText(TIMED_BOTH) }
    prefs.save()
    assertEquals("alice updated preferences for you:", bob.nextLine(CONNECT_TIMEOUT_MS))
    bob expects "Disappearing messages: enabled (you allow: yes (1 day), contact allows: yes (1 day))"
  }

  @Test fun contactPrefsResetAndUnsavedAlert() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.open("bob").info()
    var prefs = contactPrefs()
    poll(what = "voice messages allowed by both") { seesText(VOICE_BOTH) && !seesText(VOICE_OFF) }
    bob sends "/set voice @alice no"
    bob expects "you updated preferences for alice:"
    bob expects "Voice messages: off (you allow: no, contact allows: yes)"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's `no` shown under `Voice messages`") {
      seesText(VOICE_OFF) && !seesText(VOICE_BOTH) && contactOf("bob").mergedPreferences.voice.contactPreference.allow == FeatureAllowed.NO
    }
    prefs.allow(ChatFeature.Reactions, "NO")
    poll(what = "reactions prohibited after the change") { seesText(REACTIONS_OFF) && !seesText(REACTIONS_BOTH) }
    prefs.reset()
    poll(what = "reactions back to `default (yes)` after `Reset`") {
      seesText(REACTIONS_BOTH) && !seesText(REACTIONS_OFF) && "default (yes)" in prefs.allowTexts(ChatFeature.Reactions)
    }
    prefs.backClosing()
    prefs = contactPrefs()
    prefs.allow(ChatFeature.Reactions, "NO")
    poll(what = "reactions prohibited after the change") { seesText(REACTIONS_OFF) }
    prefs.backExpectingAlert()
    check("Save and notify contact" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) && "Exit without saving" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared))) {
      "the `Save preferences?` alert has no `Save and notify contact` / `Exit without saving`"
    }
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "contact preferences to close after `Exit without saving`") { !exists(Ui.ContactPrefs.Save) && !seesText("Save preferences?") }
    check(contactOf("bob").mergedPreferences.reactions.userPreference is ContactUserPref.User) { "`Exit without saving` saved the reactions change" }
    prefs = contactPrefs()
    poll(what = "reactions still `default (yes)` after `Exit without saving`") {
      seesText(REACTIONS_BOTH) && "default (yes)" in prefs.allowTexts(ChatFeature.Reactions)
    }
    prefs.allow(ChatFeature.Reactions, "NO")
    poll(what = "reactions prohibited after the change") { seesText(REACTIONS_OFF) }
    prefs.backExpectingAlert()
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "contact preferences to close with reactions saved off") {
      !exists(Ui.ContactPrefs.Save) && contactOf("bob").mergedPreferences.reactions.userPreference.pref.allow == FeatureAllowed.NO
    }
    assertEquals("alice updated preferences for you:", bob.nextLine(CONNECT_TIMEOUT_MS))
    bob expects "Message reactions: off (you allow: default (yes), contact allows: no)"
  }

  @Test fun userPrefsFullDeleteReactionsVoiceCalls() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob)
    connect(cath)
    alice.chatList.row("cath")
    val prefs = userPrefs(alice)
    poll(what = "the default preferences: full deletion only marked, the rest allowed if the contact allows") {
      seesText(U_TIMED_YES, substring = true) && seesText(U_DELETE_NO) && seesText(U_REACTIONS_YES) && seesText(U_VOICE_YES) && seesText(U_CALLS_YES)
    }
    prefs.toggleTimed()
    prefs.allow(ChatFeature.FullDelete, FeatureAllowed.ALWAYS)
    prefs.allow(ChatFeature.Reactions, FeatureAllowed.NO)
    prefs.allow(ChatFeature.Voice, FeatureAllowed.NO)
    prefs.allow(ChatFeature.Calls, FeatureAllowed.NO)
    poll(what = "every footer showing the unsaved change") {
      seesText(U_TIMED_NO) && seesText(U_DELETE_ALWAYS) && seesText(U_REACTIONS_NO) && seesText(U_VOICE_NO) && seesText(U_CALLS_NO) &&
        !seesText(U_TIMED_YES, substring = true) && !seesText(U_DELETE_NO) && !seesText(U_CALLS_YES)
    }
    check(savedUserPrefs().calls.allow == FeatureAllowed.YES) { "the calls change was saved before `Save and notify contacts`" }
    prefs.save()
    for (peer in listOf(bob, cath)) {
      assertEquals("alice updated preferences for you:", peer.nextLine(CONNECT_TIMEOUT_MS))
      peer expects "Disappearing messages: off (you allow: default (yes), contact allows: no)"
      peer expects "Full deletion: enabled for you (you allow: default (no), contact allows: always)"
      peer expects "Message reactions: off (you allow: default (yes), contact allows: no)"
      peer expects "Voice messages: off (you allow: default (yes), contact allows: no)"
      peer expects "Audio/video calls: off (you allow: default (yes), contact allows: no)"
    }
    poll(what = "the saved preferences in alice's profile") {
      savedUserPrefs().let { it.calls.allow == FeatureAllowed.NO && it.fullDelete.allow == FeatureAllowed.ALWAYS && it.timedMessages.allow == FeatureAllowed.NO }
    }
    prefs.backClosing()
  }

  @Test fun userPrefsResetAndUnsavedAlert() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    var prefs = userPrefs(alice)
    poll(what = "reactions allowed by default") { seesText(U_REACTIONS_YES) && "yes" in prefs.allowTexts(ChatFeature.Reactions) }
    prefs.allow(ChatFeature.Reactions, FeatureAllowed.NO)
    poll(what = "reactions prohibited after the change") { seesText(U_REACTIONS_NO) && "no" in prefs.allowTexts(ChatFeature.Reactions) }
    prefs.reset()
    poll(what = "reactions back to `yes` after `Reset`") { seesText(U_REACTIONS_YES) && "yes" in prefs.allowTexts(ChatFeature.Reactions) }
    prefs.backClosing()
    prefs = userPrefs(alice)
    prefs.allow(ChatFeature.Reactions, FeatureAllowed.NO)
    poll(what = "reactions prohibited after the change") { seesText(U_REACTIONS_NO) }
    prefs.backExpectingAlert()
    check("Save and notify contacts" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) && "Exit without saving" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared))) {
      "the `Save preferences?` alert has no `Save and notify contacts` / `Exit without saving`"
    }
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "your preferences to close after `Exit without saving`") { !exists(Ui.UserPrefs.Save) && !seesText("Save preferences?") }
    check(savedUserPrefs().reactions.allow == FeatureAllowed.YES) { "`Exit without saving` saved the reactions change" }
    prefs = userPrefs(alice)
    poll(what = "reactions still `yes` after `Exit without saving`") { seesText(U_REACTIONS_YES) && "yes" in prefs.allowTexts(ChatFeature.Reactions) }
    prefs.allow(ChatFeature.Reactions, FeatureAllowed.NO)
    poll(what = "reactions prohibited after the change") { seesText(U_REACTIONS_NO) }
    prefs.backExpectingAlert()
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "your preferences to close with reactions saved off") {
      !exists(Ui.UserPrefs.Save) && savedUserPrefs().reactions.allow == FeatureAllowed.NO
    }
    assertEquals("alice updated preferences for you:", bob.nextLine(CONNECT_TIMEOUT_MS))
    bob expects "Message reactions: off (you allow: default (yes), contact allows: no)"
  }

  private companion object {
    const val TIMED_BOTH = "Both you and your contact can send disappearing messages."
    const val TIMED_OFF = "Disappearing messages are prohibited in this chat."
    const val DELETE_OFF = "Irreversible message deletion is prohibited in this chat."
    const val DELETE_BY_CONTACT = "Only your contact can irreversibly delete messages (you can mark them for deletion). (24 hours)"
    const val REACTIONS_BOTH = "Both you and your contact can add message reactions."
    const val REACTIONS_OFF = "Message reactions are prohibited in this chat."
    const val VOICE_BOTH = "Both you and your contact can send voice messages."
    const val VOICE_OFF = "Voice messages are prohibited in this chat."
    const val CALLS_BOTH = "Both you and your contact can make calls."
    const val CALLS_OFF = "Audio/video calls are prohibited."
    const val U_TIMED_YES = "Allow disappearing messages only if your contact allows them."
    const val U_TIMED_NO = "Prohibit sending disappearing messages."
    const val U_DELETE_NO = "Contacts can mark messages for deletion; you will be able to view them."
    const val U_DELETE_ALWAYS = "Allow your contacts to irreversibly delete sent messages. (24 hours)"
    const val U_REACTIONS_YES = "Allow message reactions only if your contact allows them."
    const val U_REACTIONS_NO = "Prohibit message reactions."
    const val U_VOICE_YES = "Allow voice messages only if your contact allows them."
    const val U_VOICE_NO = "Prohibit sending voice messages."
    const val U_CALLS_YES = "Allow calls only if your contact allows them."
    const val U_CALLS_NO = "Prohibit audio/video calls."
  }
}
