package chat.simplex.common.views.chat

import SectionBottomSpacer
import SectionDividerSpaced
import SectionTextFooter
import SectionView
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.chat.item.markedDeletedText
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.Job
import kotlinx.datetime.Clock
import kotlinx.datetime.Instant
import java.io.IOException
import java.io.Writer
import java.net.URI
import java.time.LocalDate
import java.time.Month
import java.time.ZoneId
import java.time.format.TextStyle
import java.util.Locale

@Composable
fun ExportChatButton(chat: Chat) {
  SettingsActionItem(
    painterResource(MR.images.ic_ios_share),
    stringResource(MR.strings.export_chat),
    click = { ModalManager.end.showModal(cardScreen = true) { ExportChatView(chat.remoteHostId, chat.chatInfo) } }
  )
}

@Composable
fun ExportChatView(rhId: Long?, chatInfo: ChatInfo) {
  val fromDate = remember { mutableStateOf<LocalDate?>(null) }
  val toDate = remember { mutableStateOf<LocalDate?>(null) }
  val imagePreviews = remember { mutableStateOf(true) }
  val written = remember { mutableStateOf<Int?>(null) }
  val export = remember { mutableStateOf<Job?>(null) }
  DisposableEffect(Unit) { onDispose { export.value?.cancel() } }
  val saveLauncher = rememberFileChooserLauncher(false) { file: URI? ->
    if (file != null) {
      written.value = 0
      export.value = withLongRunningApi {
        try {
          exportChatToFile(rhId, chatInfo, fromDate.value, toDate.value, imagePreviews.value, file) { written.value = it }
        } finally {
          written.value = null
        }
      }
    }
  }
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.export_chat))
    val count = written.value
    SectionView {
      ExportDateRow(stringResource(MR.strings.export_chat_from), fromDate, enabled = count == null)
      ExportDateRow(stringResource(MR.strings.export_chat_to), toDate, enabled = count == null)
      PreferenceToggle(stringResource(MR.strings.export_chat_image_previews), disabled = count != null, checked = imagePreviews.value) { imagePreviews.value = it }
    }
    SectionDividerSpaced()
    SectionView {
      if (count == null) {
        val from = fromDate.value
        val to = toDate.value
        val invalidRange = from != null && to != null && from.isAfter(to)
        SettingsActionItem(
          painterResource(MR.images.ic_ios_share),
          stringResource(MR.strings.export_verb),
          click = { withLongRunningApi { saveLauncher.launch(exportFileName(chatInfo)) } },
          textColor = MaterialTheme.colors.primary,
          iconColor = MaterialTheme.colors.primary,
          disabled = invalidRange
        )
      } else {
        SettingsActionItemWithContent(
          painterResource(MR.images.ic_close),
          stringResource(MR.strings.cancel_verb),
          click = { export.value?.cancel() },
          iconColor = MaterialTheme.colors.primary,
          textColor = MaterialTheme.colors.primary
        ) {
          Text(stringResource(MR.strings.export_chat_messages_written).format(count), color = MaterialTheme.colors.secondary)
        }
      }
    }
    SectionTextFooter(stringResource(MR.strings.export_chat_file_not_encrypted))
    SectionBottomSpacer()
  }
}

@Composable
private fun ExportDateRow(title: String, date: MutableState<LocalDate?>, enabled: Boolean) {
  SettingsActionItemWithContent(null, title, disabled = !enabled) {
    val d = date.value
    if (d != null) {
      val dropDownsEnabled = rememberUpdatedState(enabled)
      val year = LocalDate.now().year
      ExposedDropDownSetting((1..d.lengthOfMonth()).map { it to "$it" }, rememberUpdatedState(d.dayOfMonth), enabled = dropDownsEnabled) { date.value = d.withDayOfMonth(it) }
      ExposedDropDownSetting((1..12).map { it to Month.of(it).getDisplayName(TextStyle.SHORT, Locale.getDefault()) }, rememberUpdatedState(d.monthValue), enabled = dropDownsEnabled) { date.value = d.withMonth(it) }
      ExposedDropDownSetting((year - 9..year).map { it to "$it" }, rememberUpdatedState(d.year), enabled = dropDownsEnabled) { date.value = d.withYear(it) }
    }
    DefaultSwitch(checked = d != null, onCheckedChange = { date.value = if (it) LocalDate.now() else null }, enabled = enabled)
  }
}

private suspend fun exportChatToFile(rhId: Long?, chatInfo: ChatInfo, from: LocalDate?, to: LocalDate?, imagePreviews: Boolean, file: URI, onWritten: (Int) -> Unit) {
  var saved = false
  try {
    saved = file.outputStream().bufferedWriter().use { writeChatMarkdown(rhId, chatInfo, from, to, imagePreviews, it, onWritten) }
  } catch (e: IOException) {
    AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_saving_file), e.toString())
  } finally {
    if (saved) showToast(generalGetString(MR.strings.file_saved)) else file.toFile().delete()
  }
}

private suspend fun writeChatMarkdown(rhId: Long?, chatInfo: ChatInfo, from: LocalDate?, to: LocalDate?, imagePreviews: Boolean, out: Writer, onWritten: (Int) -> Unit): Boolean {
  out.write(exportHeaderMarkdown(exportChatName(chatInfo), from, to, Clock.System.now()))
  val group = chatInfo.chatType == ChatType.Group
  val pageSize = if (group) 200 else 500
  val end = to?.plusDays(1)?.let(::startOfDay)
  var pagination: ChatPagination = ChatPagination.From(from?.let(::startOfDay) ?: Instant.fromEpochSeconds(0), pageSize)
  var day: String? = null
  var written = 0
  while (true) {
    val items = chatModel.controller.apiGetChat(rhId, chatInfo.chatType, chatInfo.apiId, null, pagination = pagination, log = false)?.first?.chatItems ?: return false
    for (ci in items) {
      if (ci.id <= 0) continue
      val orderTs = if (group) ci.meta.itemTs else ci.meta.createdAt
      if (end != null && orderTs >= end) return true
      val md = chatItemMarkdown(ci, chatInfo, imagePreviews) ?: continue
      val itemDay = localDate(ci.meta.itemTs)
      if (itemDay != day) {
        out.write("## $itemDay\n\n")
        day = itemDay
      }
      out.write(md)
      written++
    }
    onWritten(written)
    val cursor = items.lastOrNull { it.id > 0 }
    if (items.size < pageSize || cursor == null) return true
    pagination = ChatPagination.After(cursor.id, pageSize)
  }
}

private fun startOfDay(date: LocalDate): Instant =
  Instant.fromEpochSeconds(date.atStartOfDay(ZoneId.systemDefault()).toEpochSecond())

private fun exportFileName(chatInfo: ChatInfo): String =
  "${exportChatName(chatInfo).replace(Regex("""[\\/:*?"<>|\p{Cntrl}]"""), "")} ${LocalDate.now()}.md"

private fun exportChatName(chatInfo: ChatInfo): String = when (chatInfo) {
  is ChatInfo.Direct -> chatInfo.contact.profile.displayName
  is ChatInfo.Group -> chatInfo.groupInfo.groupProfile.displayName
  else -> chatInfo.displayName
}

private fun exportSenderName(chatDir: CIDirection, asGroup: Boolean, chatInfo: ChatInfo): String = when (chatDir) {
  is CIDirection.DirectSnd, is CIDirection.LocalSnd -> generalGetString(MR.strings.export_chat_you)
  is CIDirection.DirectRcv, is CIDirection.LocalRcv, is CIDirection.ChannelRcv -> exportChatName(chatInfo)
  is CIDirection.GroupSnd -> if (asGroup) exportChatName(chatInfo) else generalGetString(MR.strings.export_chat_you)
  is CIDirection.GroupRcv -> chatDir.groupMember.memberProfile.displayName
}

private fun exportHeaderMarkdown(chatName: String, from: LocalDate?, to: LocalDate?, now: Instant): String {
  val exported = localTimestamp(now)
  val summary = when {
    from != null -> generalGetString(MR.strings.export_chat_header_from_to).format(exported, localDate(startOfDay(from)), localDate(to?.let(::startOfDay) ?: now))
    to != null -> generalGetString(MR.strings.export_chat_header_until).format(exported, localDate(startOfDay(to)))
    else -> generalGetString(MR.strings.export_chat_header_all).format(exported)
  }
  return "# ${safeMarkdown(chatName)}\n\n$summary\n\n"
}

private fun chatItemMarkdown(ci: ChatItem, chatInfo: ChatInfo, imagePreviews: Boolean): String? {
  val mc = ci.content.msgContent
  return when {
    ci.meta.itemTimed != null || mc is MsgContent.MCReport -> null
    ci.meta.itemDeleted != null || ci.isDeletedContent ->
      messageMarkdown(ci, chatInfo, listOf(italicMarkdown(markedDeletedText(ci, chatInfo) { it.memberProfile.displayName })))
    mc != null ->
      messageMarkdown(ci, chatInfo, listOfNotNull(
        ci.quotedItem?.let { quoteMarkdown(it, chatInfo) },
        mc.text.ifBlank { null }?.let(::safeMarkdown),
        if (imagePreviews) imagePreviewMarkdown(mc) else null,
        attachmentMarkdown(mc, ci.file)
      ))
    else -> ci.content.text(chatInfo.isChannel).ifBlank { null }?.let { eventMarkdown(ci, it) }
  }
}

private fun messageMarkdown(ci: ChatItem, chatInfo: ChatInfo, parts: List<String>): String {
  val sender = exportSenderName(ci.chatDir, asGroup = ci.meta.showGroupAsSender, chatInfo)
  return (listOf("**${safeMarkdown(sender)}**, ${ci.timestampText}") + parts).joinToString("\n\n", postfix = "\n\n")
}

private fun eventMarkdown(ci: ChatItem, text: String): String {
  val dir = ci.chatDir
  val event = if (dir is CIDirection.GroupRcv && ci.memberDisplayName != null) "${dir.groupMember.memberProfile.displayName} $text" else text
  return italicMarkdown(event) + "\n\n"
}

private fun quoteMarkdown(quote: CIQuote, chatInfo: ChatInfo): String {
  val sender = quote.chatDir?.let { exportSenderName(it, asGroup = false, chatInfo) }
  val text = if (sender == null) quote.text else "$sender: ${quote.text}"
  return safeMarkdown(text).lines().joinToString("\n") { "> $it" }
}

private fun imagePreviewMarkdown(mc: MsgContent): String? {
  val image = when (mc) {
    is MsgContent.MCImage -> mc.image
    is MsgContent.MCVideo -> mc.image
    else -> null
  }
  return image?.ifEmpty { null }?.let { "![image]($it)" }
}

private fun attachmentMarkdown(mc: MsgContent, file: CIFile?): String? = when {
  mc is MsgContent.MCVoice -> "[${generalGetString(MR.strings.export_chat_voice_message).format(durationText(mc.duration))}]"
  file != null -> "[${generalGetString(MR.strings.export_chat_file).format(safeMarkdown(file.fileName))}]"
  else -> null
}

private fun italicMarkdown(text: String): String = "_${safeMarkdown(text.trim())}_"

private val htmlStart = Regex("""(\\*)<(?=[\p{L}/!?])""")
private val codeFence = Regex("""^ {0,3}(`{3,}|~{3,})(.*)$""")

internal fun safeMarkdown(text: String): String {
  val escaped = text
    .replace("![", "!\\[")
    .replace(htmlStart) { if (it.groupValues[1].length % 2 == 0) "${it.groupValues[1]}\\<" else it.value }
  return escaped + closingFence(escaped)
}

private fun closingFence(text: String): String {
  var open: String? = null
  for (line in text.lines()) {
    val (fence, info) = codeFence.matchEntire(line)?.destructured ?: continue
    if (open == null) {
      if (fence[0] == '~' || '`' !in info) open = fence
    } else if (fence[0] == open[0] && fence.length >= open.length && info.all { it == ' ' || it == '\t' }) {
      open = null
    }
  }
  return if (open == null) "" else "\n$open"
}
