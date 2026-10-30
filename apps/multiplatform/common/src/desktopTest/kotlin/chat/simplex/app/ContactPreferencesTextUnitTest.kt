package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.chat.SendReceipts
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.*
import dev.icerock.moko.resources.StringResource
import kotlin.test.Test
import kotlin.test.assertEquals

class ContactPreferencesTextUnitTest {

  private val both = FeatureEnabled(forUser = true, forContact = true)
  private val forUser = FeatureEnabled(forUser = true, forContact = false)
  private val forContact = FeatureEnabled(forUser = false, forContact = true)
  private val off = FeatureEnabled(forUser = false, forContact = false)

  private fun defaultText(value: StringResource): String =
    String.format(generalGetString(MR.strings.chat_preferences_default), generalGetString(value))

  private fun assertAllowDescriptions(feature: ChatFeature, always: StringResource, yes: StringResource, no: StringResource) {
    assertEquals(generalGetString(always), feature.allowDescription(FeatureAllowed.ALWAYS), "$feature always")
    assertEquals(generalGetString(yes), feature.allowDescription(FeatureAllowed.YES), "$feature yes")
    assertEquals(generalGetString(no), feature.allowDescription(FeatureAllowed.NO), "$feature no")
  }

  private fun assertEnabledDescriptions(feature: ChatFeature, bothText: StringResource, userText: StringResource, contactText: StringResource, offText: StringResource) {
    assertEquals(generalGetString(bothText), feature.enabledDescription(both), "$feature both")
    assertEquals(generalGetString(userText), feature.enabledDescription(forUser), "$feature for user")
    assertEquals(generalGetString(contactText), feature.enabledDescription(forContact), "$feature for contact")
    assertEquals(generalGetString(offText), feature.enabledDescription(off), "$feature off")
  }

  @Test
  fun disappearingMessagesAllowDescriptions() {
    assertAllowDescriptions(ChatFeature.TimedMessages, MR.strings.allow_your_contacts_to_send_disappearing_messages, MR.strings.allow_disappearing_messages_only_if, MR.strings.prohibit_sending_disappearing_messages)
  }

  @Test
  fun fullDeleteAllowDescriptions() {
    assertAllowDescriptions(ChatFeature.FullDelete, MR.strings.allow_your_contacts_irreversibly_delete, MR.strings.allow_irreversible_message_deletion_only_if, MR.strings.contacts_can_mark_messages_for_deletion)
  }

  @Test
  fun reactionsAllowDescriptions() {
    assertAllowDescriptions(ChatFeature.Reactions, MR.strings.allow_your_contacts_adding_message_reactions, MR.strings.allow_message_reactions_only_if, MR.strings.prohibit_message_reactions)
  }

  @Test
  fun voiceAllowDescriptions() {
    assertAllowDescriptions(ChatFeature.Voice, MR.strings.allow_your_contacts_to_send_voice_messages, MR.strings.allow_voice_messages_only_if, MR.strings.prohibit_sending_voice_messages)
  }

  @Test
  fun filesAllowDescriptions() {
    assertAllowDescriptions(ChatFeature.Files, MR.strings.allow_your_contacts_to_send_files_and_media, MR.strings.allow_files_and_media_only_if, MR.strings.prohibit_sending_files_and_media)
  }

  @Test
  fun callsAllowDescriptions() {
    assertAllowDescriptions(ChatFeature.Calls, MR.strings.allow_your_contacts_to_call, MR.strings.allow_calls_only_if, MR.strings.prohibit_calls)
  }

  @Test
  fun disappearingMessagesEnabledDescriptions() {
    assertEnabledDescriptions(ChatFeature.TimedMessages, MR.strings.both_you_and_your_contact_can_send_disappearing, MR.strings.only_you_can_send_disappearing, MR.strings.only_your_contact_can_send_disappearing, MR.strings.disappearing_prohibited_in_this_chat)
  }

  @Test
  fun fullDeleteEnabledDescriptions() {
    assertEnabledDescriptions(ChatFeature.FullDelete, MR.strings.both_you_and_your_contacts_can_delete, MR.strings.only_you_can_delete_messages, MR.strings.only_your_contact_can_delete, MR.strings.message_deletion_prohibited)
  }

  @Test
  fun reactionsEnabledDescriptions() {
    assertEnabledDescriptions(ChatFeature.Reactions, MR.strings.both_you_and_your_contact_can_add_message_reactions, MR.strings.only_you_can_add_message_reactions, MR.strings.only_your_contact_can_add_message_reactions, MR.strings.message_reactions_prohibited_in_this_chat)
  }

  @Test
  fun voiceEnabledDescriptions() {
    assertEnabledDescriptions(ChatFeature.Voice, MR.strings.both_you_and_your_contact_can_send_voice, MR.strings.only_you_can_send_voice, MR.strings.only_your_contact_can_send_voice, MR.strings.voice_prohibited_in_this_chat)
  }

  @Test
  fun filesEnabledDescriptions() {
    assertEnabledDescriptions(ChatFeature.Files, MR.strings.both_you_and_your_contact_can_send_files, MR.strings.only_you_can_send_files, MR.strings.only_your_contact_can_send_files, MR.strings.files_prohibited_in_this_chat)
  }

  @Test
  fun callsEnabledDescriptions() {
    assertEnabledDescriptions(ChatFeature.Calls, MR.strings.both_you_and_your_contact_can_make_calls, MR.strings.only_you_can_make_calls, MR.strings.only_your_contact_can_make_calls, MR.strings.calls_prohibited_with_this_contact)
  }

  @Test
  fun featureEnabledStateTexts() {
    assertEquals(generalGetString(MR.strings.feature_enabled), both.text)
    assertEquals(generalGetString(MR.strings.feature_enabled_for_you), forUser.text)
    assertEquals(generalGetString(MR.strings.feature_enabled_for_contact), forContact.text)
    assertEquals(generalGetString(MR.strings.feature_off), off.text)
  }

  @Test
  fun contactFeatureChoiceTexts() {
    assertEquals(defaultText(MR.strings.chat_preferences_yes), ContactFeatureAllowed.UserDefault(FeatureAllowed.YES).text)
    assertEquals(defaultText(MR.strings.chat_preferences_no), ContactFeatureAllowed.UserDefault(FeatureAllowed.NO).text)
    assertEquals(defaultText(MR.strings.chat_preferences_always), ContactFeatureAllowed.UserDefault(FeatureAllowed.ALWAYS).text)
    assertEquals(generalGetString(MR.strings.chat_preferences_always), ContactFeatureAllowed.Always.text)
    assertEquals(generalGetString(MR.strings.chat_preferences_yes), ContactFeatureAllowed.Yes.text)
    assertEquals(generalGetString(MR.strings.chat_preferences_no), ContactFeatureAllowed.No.text)
  }

  @Test
  fun sendReceiptsTexts() {
    assertEquals(defaultText(MR.strings.chat_preferences_yes), SendReceipts.UserDefault(true).text)
    assertEquals(defaultText(MR.strings.chat_preferences_no), SendReceipts.UserDefault(false).text)
    assertEquals(generalGetString(MR.strings.chat_preferences_yes), SendReceipts.Yes.text)
    assertEquals(generalGetString(MR.strings.chat_preferences_no), SendReceipts.No.text)
  }
}
