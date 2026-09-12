package chat.simplex.app

import android.app.Application
import android.app.PendingIntent
import android.content.Intent
import android.os.Bundle
import androidx.core.app.NotificationCompat
import androidx.core.app.RemoteInput
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.RuntimeEnvironment
import org.robolectric.annotation.Config

/**
 * Robolectric (JVM) contract test for the notification quick-reply RemoteInput mechanism.
 *
 * NtfManager itself can't be exercised here: it is an `object` whose init reads SimplexApp.context,
 * which is only set after SimplexApp.onCreate -> initHaskell() loads the native core. So this test
 * reproduces the reply-action construction the impl uses (NtfManager.android.kt: replyAction) and
 * asserts the contract the receiver relies on — a mutable PendingIntent plus a RemoteInput keyed by
 * the reply key whose typed result round-trips via getResultsFromIntent. The full displayNotification
 * / NtfActionReceiver path is verified by the on-device runbook.
 */
@RunWith(RobolectricTestRunner::class)
@Config(application = Application::class, sdk = [34])
class NtfReplyRemoteInputTest {
  // must match NtfManager.ReplyTextKey
  private val replyKey = "chat.simplex.app.NTF_REPLY_TEXT"

  @Test
  fun replyActionExposesRemoteInputAndRoundTripsTypedText() {
    val ctx = RuntimeEnvironment.getApplication()
    val intent = Intent(ctx, Application::class.java)
      .setAction("chat.simplex.app.NTF_REPLY")
      .putExtra("userId", 7L)
      .putExtra("chatId", "@123")
    val pendingIntent = PendingIntent.getBroadcast(
      ctx, "@123".hashCode(), intent,
      PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_MUTABLE
    )
    val remoteInput = RemoteInput.Builder(replyKey).setLabel("Message").build()
    val action = NotificationCompat.Action.Builder(0, "Reply", pendingIntent)
      .addRemoteInput(remoteInput)
      .setSemanticAction(NotificationCompat.Action.SEMANTIC_ACTION_REPLY)
      .setAllowGeneratedReplies(true)
      .build()

    // the action exposes exactly one RemoteInput, keyed by the reply key, tagged as a reply (Wear/Auto)
    assertEquals(1, action.remoteInputs?.size)
    assertEquals(replyKey, action.remoteInputs!![0].resultKey)
    assertEquals(NotificationCompat.Action.SEMANTIC_ACTION_REPLY, action.semanticAction)

    // the system writes the typed reply into the action's intent; the receiver reads it back by key
    val results = Bundle().apply { putCharSequence(replyKey, "hello there") }
    RemoteInput.addResultsToIntent(arrayOf(remoteInput), intent, results)
    val extracted = RemoteInput.getResultsFromIntent(intent)
    assertNotNull(extracted)
    assertEquals("hello there", extracted!!.getCharSequence(replyKey)?.toString())
    assertEquals("@123", intent.getStringExtra("chatId"))
    assertEquals(7L, intent.getLongExtra("userId", -1L))
  }
}
