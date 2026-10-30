package chat.simplex.app

import chat.simplex.common.model.*
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

class ChatListMuteModeUnitTest {
  private fun groupWith(enableNtfs: MsgFilter): ChatInfo.Group =
    ChatInfo.Group(GroupInfo.sampleData.copy(chatSettings = ChatSettings(enableNtfs = enableNtfs, sendRcpts = null, favorite = false)), groupChatScope = null)

  private fun directWith(enableNtfs: MsgFilter): ChatInfo.Direct =
    ChatInfo.Direct(Contact.sampleData.copy(chatSettings = ChatSettings(enableNtfs = enableNtfs, sendRcpts = null, favorite = false)))

  @Test
  fun allGoesToMentionsWhenMentionsAreSupported() {
    assertEquals(MsgFilter.Mentions, MsgFilter.All.nextMode(mentions = true))
  }

  @Test
  fun allGoesToNoneWithoutMentions() {
    assertEquals(MsgFilter.None, MsgFilter.All.nextMode(mentions = false))
  }

  @Test
  fun mentionsGoesToNone() {
    assertEquals(MsgFilter.None, MsgFilter.Mentions.nextMode(mentions = true))
    assertEquals(MsgFilter.None, MsgFilter.Mentions.nextMode(mentions = false))
  }

  @Test
  fun noneGoesBackToAll() {
    assertEquals(MsgFilter.All, MsgFilter.None.nextMode(mentions = true))
    assertEquals(MsgFilter.All, MsgFilter.None.nextMode(mentions = false))
  }

  @Test
  fun groupCyclesAllMentionsNoneAll() {
    assertEquals(MsgFilter.Mentions, groupWith(MsgFilter.All).nextNtfMode)
    assertEquals(MsgFilter.None, groupWith(MsgFilter.Mentions).nextNtfMode)
    assertEquals(MsgFilter.All, groupWith(MsgFilter.None).nextNtfMode)
  }

  @Test
  fun contactTogglesBetweenAllAndNone() {
    assertEquals(MsgFilter.None, directWith(MsgFilter.All).nextNtfMode)
    assertEquals(MsgFilter.All, directWith(MsgFilter.None).nextNtfMode)
  }

  @Test
  fun noteFolderHasNoNextMode() {
    assertNull(ChatInfo.Local(NoteFolder.sampleData).nextNtfMode)
  }

  @Test
  fun ntfsEnabledFollowsTheMode() {
    assertTrue(groupWith(MsgFilter.All).ntfsEnabled(userMention = false))
    assertFalse(groupWith(MsgFilter.Mentions).ntfsEnabled(userMention = false))
    assertTrue(groupWith(MsgFilter.Mentions).ntfsEnabled(userMention = true))
    assertFalse(groupWith(MsgFilter.None).ntfsEnabled(userMention = true))
  }
}
