package chat.simplex.app.ui.harness

import chat.simplex.common.ui.Ui

fun CliPeer.expectsScopedMessage(from: String, text: String, timeoutMs: Long = CONNECT_TIMEOUT_MS) {
  val got = nextLine(timeoutMs)
  val expected = Regex("(\\d{2}:\\d{2} )?" + Regex.escape(from) + "!?> " + Regex.escape(text))
  check(expected.matches(got)) { "$name: expected message `$from> $text` (attention marker optional), got `$got`; next: ${pending()}" }
}

fun UiScope.memberOfBobsTeamWithCath(app: App, bob: CliPeer, cath: CliPeer): ChatScreen {
  connect(bob)
  connectPeers(bob, cath)
  hostGroup(bob, "team", "alice", "cath")
  joinGroupFromList(app, bob, "team")
  cath sends "/j team"
  cath.eventually("#team: you joined the group")
  bob.eventually("#team: cath joined the group")
  cath.eventually("#team: member alice is connected", CONNECT_TIMEOUT_MS)
  quiesce()
  backToChatList()
  return app.chatList.open("team")
}

fun UiScope.ownTeamWithSupportRequests(app: App, bob: CliPeer, cath: CliPeer): ChatScreen {
  connect(bob)
  connect(cath)
  val chat = ownGroup(app, "team", bob, cath)
  bob.eventuallyMatching(Regex("#team: (new )?member cath is connected"), CONNECT_TIMEOUT_MS)
  cath.eventuallyMatching(Regex("#team: (new )?member bob is connected"), CONNECT_TIMEOUT_MS)
  poll(what = "team's chat open with no support banner") { exists(Ui.Chat.Message) && !exists(Ui.Chat.SupportChats) }
  bob sends "#team (support) bob needs help"
  cath sends "#team (support) cath needs help"
  return chat
}

fun UiScope.supportBanner(): List<String> = if (exists(Ui.Chat.SupportChats)) textsOf(Ui.Chat.SupportChats) else emptyList()

fun UiScope.supportRowBadge(name: String): String? {
  val row = Ui.MemberSupport.Row(groupMemberId(name))
  return if (exists(row)) unreadBadge(row) else null
}

fun UiScope.openSupportRowMenu(name: String): Long {
  val id = groupMemberId(name)
  openContextMenu(Ui.MemberSupport.Row(id))
  waitFor(Ui.MemberSupport.RowMenu(id, "delete"))
  return id
}
