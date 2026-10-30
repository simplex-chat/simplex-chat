package chat.simplex.app.ui.harness

import chat.simplex.common.ui.Ui

private const val BOT_COMMANDS =
  """[{"type":"command","keyword":"help","label":"Help"},{"type":"command","keyword":"order","label":"Order","params":"<number>"},""" +
    """{"type":"menu","label":"More","commands":[{"type":"command","keyword":"status","label":"Status"},{"type":"command","keyword":"cost","label":"Cost"}]}]"""
private val BOT_COMMANDS_FOOTER = Regex("use /set bot commands .* or /delete bot commands")

const val PREVIEW_IMAGE_DATA =
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII="

fun CliPeer.becomeBotWithCommands() {
  this sends """/_profile 1 {"displayName":"$name","fullName":"","peerType":"bot","preferences":{"commands":$BOT_COMMANDS}}"""
  this sends "/u"
  eventually("user profile: $name (bot)")
  this expects "use /p <name> [<bio>] to change it"
  this expects "Bot commands:"
  eventuallyMatching(BOT_COMMANDS_FOOTER)
}

fun UiScope.openItemInfo(id: Long, title: String): ItemInfoScreen {
  itemMenu(id, "info")
  poll(what = "the item info titled `$title`") { seesText(title) }
  return ItemInfoScreen(this)
}

fun UiScope.commandShown(keyword: String, label: String): Boolean =
  exists(Ui.Chat.Command(keyword)) && textsOf(Ui.Chat.Command(keyword)).let { label in it && "/$keyword" in it }
