package chat.simplex.app.ui.harness

import chat.simplex.common.ui.Ui
import kotlinx.datetime.LocalDateTime
import kotlinx.datetime.TimeZone
import kotlinx.datetime.toInstant

private const val PEER_RECHECK_MS = 1_000L
private val SENT_AT = Regex("sent at: (\\S+ \\S+)")
private val RECEIVED_AT = Regex("received at: .+")
private val DELETE_AT = Regex("to be deleted at: (\\S+ \\S+)")
private val SERVERS_SUMMARY = Regex("agent servers summary: .*")
private val CONN_DELETED = Regex("\"_connDeleted\":[1-9]")

fun UiScope.openTimedMenu() {
  rightClick(Ui.Chat.Send)
  click(Ui.Chat.SendTimed)
  poll(what = "the `Send disappearing message` menu with `30 seconds` and `Custom time`") {
    seesText("Send disappearing message") && exists(Ui.Chat.SendTimed.option(30)) && exists(Ui.Chat.SendTimed.option("custom"))
  }
}

fun CliPeer.receivedTtlSeconds(contact: String, text: String): Long {
  sends("/item info @$contact $text")
  val sentAt = eventuallyMatching(SENT_AT).groupValues[1]
  eventuallyMatching(RECEIVED_AT)
  val deleteAt = eventuallyMatching(DELETE_AT).groupValues[1]
  return (localTs(deleteAt) - localTs(sentAt)).inWholeSeconds
}

fun UiScope.awaitGoneFromPeer(peer: CliPeer, contact: String, line: String) {
  var askedAt = 0L
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`$line` gone from the end of ${peer.name}'s chat with $contact") {
    if (System.currentTimeMillis() - askedAt < PEER_RECHECK_MS) return@poll false
    askedAt = System.currentTimeMillis()
    peer sends "/tail @$contact 1"
    !peer.nextLine().endsWith(line)
  }
}

fun UiScope.awaitConnectionDeletedOnServer(peer: CliPeer) {
  var askedAt = 0L
  var deleted = false
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the server to confirm ${peer.name}'s deleted connection") {
    if (System.currentTimeMillis() - askedAt >= PEER_RECHECK_MS) {
      askedAt = System.currentTimeMillis()
      peer sends "/get servers summary 1"
      deleted = CONN_DELETED.containsMatchIn(peer.eventuallyMatching(SERVERS_SUMMARY).value)
    }
    deleted
  }
}

private fun localTs(s: String) = LocalDateTime.parse(s.replace(' ', 'T')).toInstant(TimeZone.UTC)
