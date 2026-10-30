package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.chat.SendReceipts
import chat.simplex.common.views.chat.item.moderateMessageQuestionText
import chat.simplex.common.views.chat.subscriberCountStr
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.*
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class GroupLabelsUnitTest {

  private val yes = generalGetString(MR.strings.chat_preferences_yes)
  private val no = generalGetString(MR.strings.chat_preferences_no)

  @Test
  fun sendReceiptsUnsetFollowsUserDefault() {
    assertEquals(SendReceipts.UserDefault(true), SendReceipts.fromBool(null, userDefault = true))
    assertEquals(SendReceipts.UserDefault(false), SendReceipts.fromBool(null, userDefault = false))
    assertEquals(String.format(generalGetString(MR.strings.chat_preferences_default), yes), SendReceipts.fromBool(null, userDefault = true).text)
    assertEquals(String.format(generalGetString(MR.strings.chat_preferences_default), no), SendReceipts.fromBool(null, userDefault = false).text)
  }

  @Test
  fun sendReceiptsExplicitValueIgnoresUserDefault() {
    assertEquals(SendReceipts.Yes, SendReceipts.fromBool(true, userDefault = false))
    assertEquals(SendReceipts.No, SendReceipts.fromBool(false, userDefault = true))
    assertEquals(yes, SendReceipts.Yes.text)
    assertEquals(no, SendReceipts.No.text)
  }

  @Test
  fun sendReceiptsBoolRoundTrips() {
    assertEquals(true, SendReceipts.fromBool(true, userDefault = false).bool)
    assertEquals(false, SendReceipts.fromBool(false, userDefault = true).bool)
    assertNull(SendReceipts.fromBool(null, userDefault = true).bool)
  }

  @Test
  fun subscriberCountIsSingularOnlyForOne() {
    assertEquals(String.format(generalGetString(MR.strings.channel_subscriber_count_singular), 1L), subscriberCountStr(1))
    assertEquals(String.format(generalGetString(MR.strings.channel_subscriber_count_plural), 0L), subscriberCountStr(0))
    assertEquals(String.format(generalGetString(MR.strings.channel_subscriber_count_plural), 2L), subscriberCountStr(2))
    assertEquals(String.format(generalGetString(MR.strings.channel_subscriber_count_plural), 1000L), subscriberCountStr(1000))
  }

  @Test
  fun moderateQuestionSaysDeletedWhenFullDeleteAllowed() {
    assertEquals(generalGetString(MR.strings.moderate_message_will_be_deleted_warning), moderateMessageQuestionText(fullDeleteAllowed = true, count = 1))
    assertEquals(generalGetString(MR.strings.moderate_messages_will_be_deleted_warning), moderateMessageQuestionText(fullDeleteAllowed = true, count = 2))
  }

  @Test
  fun moderateQuestionSaysMarkedWhenFullDeleteProhibited() {
    assertEquals(generalGetString(MR.strings.moderate_message_will_be_marked_warning), moderateMessageQuestionText(fullDeleteAllowed = false, count = 1))
    assertEquals(generalGetString(MR.strings.moderate_messages_will_be_marked_warning), moderateMessageQuestionText(fullDeleteAllowed = false, count = 2))
  }

  @Test
  fun editablePreferenceDescriptionsOfferAllowOrProhibit() {
    assertEquals(generalGetString(MR.strings.allow_message_reactions), GroupFeature.Reactions.enableDescription(GroupFeatureEnabled.ON, canEdit = true))
    assertEquals(generalGetString(MR.strings.prohibit_message_reactions_group), GroupFeature.Reactions.enableDescription(GroupFeatureEnabled.OFF, canEdit = true))
    assertEquals(generalGetString(MR.strings.allow_direct_messages), GroupFeature.DirectMessages.enableDescription(GroupFeatureEnabled.ON, canEdit = true))
    assertEquals(generalGetString(MR.strings.allow_direct_messages_channel), GroupFeature.DirectMessages.enableDescription(GroupFeatureEnabled.ON, canEdit = true, isChannel = true))
  }

  @Test
  fun readOnlyPreferenceDescriptionsStateTheRule() {
    assertEquals(generalGetString(MR.strings.group_members_can_add_message_reactions), GroupFeature.Reactions.enableDescription(GroupFeatureEnabled.ON, canEdit = false))
    assertEquals(generalGetString(MR.strings.message_reactions_are_prohibited), GroupFeature.Reactions.enableDescription(GroupFeatureEnabled.OFF, canEdit = false))
    assertEquals(generalGetString(MR.strings.group_members_can_add_message_reactions_channel), GroupFeature.Reactions.enableDescription(GroupFeatureEnabled.ON, canEdit = false, isChannel = true))
    assertEquals(generalGetString(MR.strings.simplex_links_are_prohibited_in_group), GroupFeature.SimplexLinks.enableDescription(GroupFeatureEnabled.OFF, canEdit = false))
    assertEquals(generalGetString(MR.strings.direct_messages_are_prohibited), GroupFeature.DirectMessages.enableDescription(GroupFeatureEnabled.OFF, canEdit = false))
  }
}
