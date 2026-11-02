package chat.simplex.app

import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.test.*
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.unit.DpSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.WindowState
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.simplexWindowState
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.chat.ContactPreferencesView
import chat.simplex.common.views.chat.group.GroupPreferencesView
import chat.simplex.common.views.chat.group.MemberAdmissionView
import chat.simplex.common.views.helpers.LocalAppBarHandler
import chat.simplex.common.views.helpers.rememberAppBarHandler
import org.junit.Rule
import kotlin.test.Test
import kotlin.time.Duration.Companion.seconds

class UnsavedPreferenceEditsTest {
  @get:Rule
  val rule = createComposeRule()

  private val group = GroupInfo.sampleData.copy(membership = GroupMember.sampleData.copy(memberRole = GroupMemberRole.Owner))
  private val contact = Contact.sampleData

  private fun show(chatInfo: ChatInfo, content: @Composable () -> Unit) {
    simplexWindowState.windowState = WindowState(size = DpSize(400.dp, 800.dp))
    chatModel.chatsContext.chats.replaceAll(listOf(Chat(remoteHostId = null, chatInfo = chatInfo, chatItems = listOf())))
    rule.setContent {
      SimpleXTheme {
        CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler(), content = content)
      }
    }
  }

  private fun showGroupPreferences() =
    show(ChatInfo.Group(group, groupChatScope = null)) { GroupPreferencesView(chatModel, null, group.id, close = {}) }

  private fun showMemberAdmission() =
    show(ChatInfo.Group(group, groupChatScope = null)) { MemberAdmissionView(chatModel, null, group.id, close = {}) }

  private fun showContactPreferences(ct: Contact = contact) =
    show(ChatInfo.Direct(ct)) { ContactPreferencesView(chatModel, User.sampleData, null, ct.contactId, close = {}) }

  private fun replaceChatInfo(chatInfo: ChatInfo) = rule.runOnIdle { chatModel.chatsContext.updateChatInfo(null, chatInfo) }

  private fun replaceGroupInfo(g: GroupInfo) = replaceChatInfo(ChatInfo.Group(g, groupChatScope = null))

  private fun botWithCommands() = contact.copy(mergedPreferences = contact.mergedPreferences.copy(commands = listOf(ChatBotCommand.Command("help", "Help", null))))

  private fun timedMessagesToggle() = rule.onAllNodes(isToggleable()).onFirst()

  private fun selectAdmissionReview(criteria: String) {
    rule.onNodeWithText("off").performClick()
    rule.onNodeWithText(criteria).performClick()
  }

  @Test
  fun groupPreferencesEditIsKeptWhenAMessageArrives() {
    showGroupPreferences()
    timedMessagesToggle().performClick()
    replaceGroupInfo(group.copy(chatTs = group.chatTs!! + 1.seconds))
    timedMessagesToggle().assertIsOn()
    rule.onNodeWithText("Save and notify group members").assertHasClickAction()
  }

  @Test
  fun groupPreferencesAreReloadedWhenSavedPreferencesChange() {
    showGroupPreferences()
    timedMessagesToggle().performClick()
    val saved = group.fullGroupPreferences.copy(reactions = GroupPreference(GroupFeatureEnabled.OFF))
    replaceGroupInfo(group.copy(fullGroupPreferences = saved))
    timedMessagesToggle().assertIsOff()
    rule.onNodeWithText("Save and notify group members").assertHasNoClickAction()
  }

  @Test
  fun groupPreferencesAreReloadedWhenUserIsNoLongerOwner() {
    showGroupPreferences()
    timedMessagesToggle().performClick()
    replaceGroupInfo(group.copy(membership = group.membership.copy(memberRole = GroupMemberRole.Admin)))
    rule.onNodeWithText("Delete after").assertDoesNotExist()
  }

  @Test
  fun memberAdmissionEditIsKeptWhenAMessageArrives() {
    showMemberAdmission()
    selectAdmissionReview("all")
    replaceGroupInfo(group.copy(chatTs = group.chatTs!! + 1.seconds))
    rule.onNodeWithText("all").assertExists()
    rule.onNodeWithText("Save and notify group members").assertHasClickAction()
  }

  @Test
  fun memberAdmissionIsReloadedWhenSavedAdmissionChanges() {
    showMemberAdmission()
    selectAdmissionReview("all")
    val saved = group.groupProfile.copy(memberAdmission = GroupMemberAdmission(review = null))
    replaceGroupInfo(group.copy(groupProfile = saved))
    rule.onNodeWithText("off").assertExists()
    rule.onNodeWithText("Save and notify group members").assertHasNoClickAction()
  }

  @Test
  fun memberAdmissionIsReloadedWhenUserIsNoLongerOwner() {
    showMemberAdmission()
    selectAdmissionReview("all")
    replaceGroupInfo(group.copy(membership = group.membership.copy(memberRole = GroupMemberRole.Admin)))
    rule.onNodeWithText("off").assertExists()
  }

  @Test
  fun contactPreferencesEditIsKeptWhenAMessageArrives() {
    showContactPreferences()
    timedMessagesToggle().performClick()
    replaceChatInfo(ChatInfo.Direct(contact.copy(chatTs = contact.chatTs!! + 1.seconds)))
    timedMessagesToggle().assertIsOn()
    rule.onNodeWithText("Save and notify contact").assertHasClickAction()
  }

  @Test
  fun botContactPreferencesEditIsKeptWhenAMessageArrives() {
    val bot = botWithCommands()
    showContactPreferences(bot)
    timedMessagesToggle().performClick()
    replaceChatInfo(ChatInfo.Direct(botWithCommands().copy(chatTs = bot.chatTs!! + 1.seconds)))
    timedMessagesToggle().assertIsOn()
    rule.onNodeWithText("Save and notify contact").assertHasClickAction()
  }

  @Test
  fun contactPreferencesAreReloadedWhenSavedPreferencesChange() {
    showContactPreferences()
    timedMessagesToggle().performClick()
    val reactions = contact.mergedPreferences.reactions.copy(contactPreference = SimpleChatPreference(allow = FeatureAllowed.NO))
    replaceChatInfo(ChatInfo.Direct(contact.copy(mergedPreferences = contact.mergedPreferences.copy(reactions = reactions))))
    timedMessagesToggle().assertIsOff()
    rule.onNodeWithText("Save and notify contact").assertHasNoClickAction()
  }
}
