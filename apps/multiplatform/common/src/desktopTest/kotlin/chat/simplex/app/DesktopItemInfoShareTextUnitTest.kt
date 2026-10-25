package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.chat.itemInfoShareText
import kotlinx.datetime.Instant
import java.util.Locale
import kotlin.test.AfterTest
import kotlin.test.BeforeTest
import kotlin.test.Test
import kotlin.test.assertEquals

class DesktopItemInfoShareTextUnitTest {
  private lateinit var savedLocale: Locale
  private val sentTs = Instant.fromEpochSeconds(1_760_000_000)
  private val rcvTs = Instant.fromEpochSeconds(1_760_000_042)
  private val quoteTs = Instant.fromEpochSeconds(1_759_990_000)
  private val deletedTs = Instant.fromEpochSeconds(1_760_000_500)
  private val deleteAt = Instant.fromEpochSeconds(1_760_086_400)
  private val v0Ts = Instant.fromEpochSeconds(1_760_000_100)
  private val v1Ts = Instant.fromEpochSeconds(1_760_000_000)

  @BeforeTest
  fun useEnglish() {
    savedLocale = Locale.getDefault()
    Locale.setDefault(Locale.US)
  }

  @AfterTest
  fun restoreLocale() {
    Locale.setDefault(savedLocale)
  }

  private fun info(versions: List<ChatItemVersion> = emptyList()) =
    ChatItemInfo(itemVersions = versions, memberDeliveryStatuses = null, forwardedFromChatItem = null)

  private fun version(id: Long, text: String, ts: Instant) =
    ChatItemVersion(chatItemVersionId = id, msgContent = MsgContent.MCText(text), formattedText = null, itemVersionTs = ts, createdAt = ts)

  private fun item(
    dir: CIDirection = CIDirection.DirectSnd(),
    text: String = "hello",
    quotedItem: CIQuote? = null,
    itemDeleted: CIDeleted? = null,
    itemTimed: CITimed? = null
  ) = ChatItem.getSampleData(5, dir = dir, ts = sentTs, text = text, quotedItem = quotedItem, itemDeleted = itemDeleted, itemTimed = itemTimed)

  private fun share(ci: ChatItem, chatItemInfo: ChatItemInfo = info(), devTools: Boolean = false) =
    itemInfoShareText(chatModel, ci, chatItemInfo, devTools)

  private fun lines(vararg l: String) = l.joinToString("\n")

  @Test
  fun sentItemHasTitleAndSentAt() {
    assertEquals(
      lines("# Sent message", "", "Sent at: ${localTimestamp(sentTs)}"),
      share(item())
    )
  }

  @Test
  fun receivedItemAddsReceivedAt() {
    val base = item(dir = CIDirection.DirectRcv())
    val ci = base.copy(meta = base.meta.copy(createdAt = rcvTs))
    assertEquals(
      lines("# Received message", "", "Sent at: ${localTimestamp(sentTs)}", "Received at: ${localTimestamp(rcvTs)}"),
      share(ci)
    )
  }

  @Test
  fun localNoteIsSavedMessageWithCreatedAt() {
    assertEquals(
      lines("# Saved message", "", "Created at: ${localTimestamp(sentTs)}"),
      share(item(dir = CIDirection.LocalSnd()))
    )
  }

  @Test
  fun replyToOwnMessageShowsYouAndQuotedText() {
    val ci = item(quotedItem = CIQuote.getSample(1, quoteTs, "original", CIDirection.DirectSnd()))
    assertEquals(
      lines(
        "# Sent message", "", "Sent at: ${localTimestamp(sentTs)}",
        "", "## In reply to", "", "you at ${localTimestamp(quoteTs)}", "original"
      ),
      share(ci)
    )
  }

  @Test
  fun replyToContactMessageWithoutTextShowsTimestampAndNoText() {
    val ci = item(quotedItem = CIQuote.getSample(1, quoteTs, "", CIDirection.DirectRcv()))
    assertEquals(
      lines(
        "# Sent message", "", "Sent at: ${localTimestamp(sentTs)}",
        "", "## In reply to", "", localTimestamp(quoteTs), "no text"
      ),
      share(ci)
    )
  }

  @Test
  fun historyMarksFirstVersionCurrent() {
    val ci = item(text = "edited")
    val versions = listOf(version(2, "edited", v0Ts), version(1, "", v1Ts))
    assertEquals(
      lines(
        "# Sent message", "", "Sent at: ${localTimestamp(sentTs)}",
        "", "## History",
        "", "${localTimestamp(v0Ts)} (current)", "edited",
        "", localTimestamp(v1Ts), "no text"
      ),
      share(ci, info(versions))
    )
  }

  @Test
  fun deletedItemShowsDeletedAtAndNoCurrentVersion() {
    val ci = item(itemDeleted = CIDeleted.Deleted(deletedTs))
    val versions = listOf(version(2, "edited", v0Ts))
    assertEquals(
      lines(
        "# Sent message", "", "Sent at: ${localTimestamp(sentTs)}", "Deleted at: ${localTimestamp(deletedTs)}",
        "", "## History",
        "", localTimestamp(v0Ts), "edited"
      ),
      share(ci, info(versions))
    )
  }

  @Test
  fun deletedItemWithoutTimestampHasNoDeletedAt() {
    assertEquals(
      lines("# Sent message", "", "Sent at: ${localTimestamp(sentTs)}"),
      share(item(itemDeleted = CIDeleted.Deleted(null)))
    )
  }

  @Test
  fun moderatedItemShowsModeratedAt() {
    val ci = item(dir = CIDirection.GroupRcv(GroupMember.sampleData), itemDeleted = CIDeleted.Moderated(deletedTs, GroupMember.sampleData))
    assertEquals(
      lines(
        "# Received message", "", "Sent at: ${localTimestamp(sentTs)}", "Received at: ${localTimestamp(sentTs)}",
        "Moderated at: ${localTimestamp(deletedTs)}"
      ),
      share(ci)
    )
  }

  @Test
  fun timedItemShowsDisappearsAt() {
    val ci = item(itemTimed = CITimed(ttl = 86400, deleteAt = deleteAt))
    assertEquals(
      lines("# Sent message", "", "Sent at: ${localTimestamp(sentTs)}", "Disappears at: ${localTimestamp(deleteAt)}"),
      share(ci)
    )
  }

  private fun verified(ci: ChatItem, msgVerified: MsgVerified) = ci.copy(meta = ci.meta.copy(msgVerified = msgVerified))

  @Test
  fun sentVerifiedItemIsSigned() {
    assertEquals(
      lines("# Sent message", "", "Sent at: ${localTimestamp(sentTs)}", "Signed"),
      share(verified(item(), MsgVerified.Signed(MsgSigStatus.Verified)))
    )
  }

  @Test
  fun receivedVerifiedItemIsSignedAndVerified() {
    assertEquals(
      lines("# Received message", "", "Sent at: ${localTimestamp(sentTs)}", "Received at: ${localTimestamp(sentTs)}", "Signed & verified"),
      share(verified(item(dir = CIDirection.DirectRcv()), MsgVerified.Signed(MsgSigStatus.Verified)))
    )
  }

  @Test
  fun itemWithMissingSignatureSaysSignatureMissing() {
    assertEquals(
      lines("# Received message", "", "Sent at: ${localTimestamp(sentTs)}", "Received at: ${localTimestamp(sentTs)}", "Signature missing"),
      share(verified(item(dir = CIDirection.DirectRcv()), MsgVerified.SigMissing))
    )
  }

  @Test
  fun devToolsAddDatabaseIdUpdatedAtAndStatus() {
    assertEquals(
      lines(
        "# Sent message", "", "Sent at: ${localTimestamp(sentTs)}",
        "Database ID: 5", "Record updated at: $sentTs", "Message status: {\"type\":\"sndNew\"}"
      ),
      share(item(), devTools = true)
    )
  }
}
