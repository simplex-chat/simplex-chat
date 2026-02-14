@file:UseSerializers(UriSerializer::class, ComposeMessageSerializer::class)
package chat.simplex.common.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.Saver
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.text.TextRange
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.style.TextDecoration
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.util.*
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.model.ChatModel.filesToDelete
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.item.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.serialization.*
import kotlinx.serialization.descriptors.*
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import java.io.File
import java.net.URI
import java.nio.file.Files
import kotlin.math.min

const val MAX_NUMBER_OF_MENTIONS = 3

@Serializable
sealed class ComposePreview {
  @Serializable object NoPreview: ComposePreview()
  @Serializable class CLinkPreview(val linkPreview: LinkPreview?): ComposePreview()
  @Serializable class MediaPreview(val images: List<String>, val content: List<UploadContent>): ComposePreview()
  @Serializable data class VoicePreview(val voice: String, val durationMs: Int, val finished: Boolean): ComposePreview()
  @Serializable class FilePreview(val fileName: String, val uri: URI): ComposePreview()
}

@Serializable
sealed class ComposeContextItem {
  @Serializable object NoContextItem: ComposeContextItem()
  @Serializable class QuotedItem(val chatItem: ChatItem): ComposeContextItem()
  @Serializable class EditingItem(val chatItem: ChatItem): ComposeContextItem()
  @Serializable class ForwardingItems(val chatItems: List<ChatItem>, val fromChatInfo: ChatInfo): ComposeContextItem()
  @Serializable class ReportedItem(val chatItem: ChatItem, val reason: ReportReason): ComposeContextItem()
}

@Serializable
data class LiveMessage(
  val chatItem: ChatItem,
  val typedMsg: String,
  val sentMsg: String,
  val sent: Boolean
)

typealias MentionedMembers = Map<String, CIMention>

@Serializable
data class ComposeMessage(
  val text: String = "",
  val selection: TextRange = TextRange.Zero
) {
  constructor(text: String): this(text, TextRange(text.length))
}

@Serializer(forClass = TextRange::class)
object ComposeMessageSerializer : KSerializer<TextRange> {
  override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("TextRange", PrimitiveKind.LONG)
  override fun serialize(encoder: Encoder, value: TextRange) =
    encoder.encodeLong(packInts(value.start, value.end))
  override fun deserialize(decoder: Decoder): TextRange =
    decoder.decodeLong().let { value ->  TextRange(unpackInt1(value), unpackInt2(value)) }
}

@Serializable
data class ComposeState(
  val message: ComposeMessage = ComposeMessage(),
  val parsedMessage: List<FormattedText> = emptyList(),
  val liveMessage: LiveMessage? = null,
  val preview: ComposePreview = ComposePreview.NoPreview,
  val contextItem: ComposeContextItem = ComposeContextItem.NoContextItem,
  val inProgress: Boolean = false,
  val progressByTimeout: Boolean = false,
  val useLinkPreviews: Boolean,
  val mentions: MentionedMembers = emptyMap()
) {
  constructor(editingItem: ChatItem, liveMessage: LiveMessage? = null, useLinkPreviews: Boolean): this(
    ComposeMessage(editingItem.content.text),
    editingItem.formattedText ?: FormattedText.plain(editingItem.content.text),
    liveMessage,
    chatItemPreview(editingItem),
    ComposeContextItem.EditingItem(editingItem),
    useLinkPreviews = useLinkPreviews,
    mentions = editingItem.mentions ?: emptyMap()
  )

  val memberMentions: Map<String, Long>
    get() = this.mentions.mapNotNull {
      val memberRef = it.value.memberRef

      if (memberRef != null) {
        it.key to memberRef.groupMemberId
      } else {
        null
      }
    }.toMap()

  val editing: Boolean
    get() =
      when (contextItem) {
        is ComposeContextItem.EditingItem -> true
        else -> false
      }
  val forwarding: Boolean
    get() = when (contextItem) {
      is ComposeContextItem.ForwardingItems -> true
      else -> false
    }
  val reporting: Boolean
    get() = when (contextItem) {
      is ComposeContextItem.ReportedItem -> true
      else -> false
    }
  val submittingValidReport: Boolean
    get() = when (contextItem) {
      is ComposeContextItem.ReportedItem -> {
        when (contextItem.reason) {
          is ReportReason.Other -> message.text.isNotEmpty()
          else -> true
        }
      }
      else -> false
    }
  val sendEnabled: () -> Boolean
    get() = {
      val hasContent = when (preview) {
        is ComposePreview.MediaPreview -> true
        is ComposePreview.VoicePreview -> true
        is ComposePreview.FilePreview -> true
        else -> !whitespaceOnly || forwarding || liveMessage != null || submittingValidReport
      }
      hasContent && !inProgress
    }
  val endLiveDisabled: Boolean
    get() = liveMessage != null && message.text.isEmpty() && preview is ComposePreview.NoPreview && contextItem is ComposeContextItem.NoContextItem

  val linkPreviewAllowed: Boolean
    get() =
      when (preview) {
        is ComposePreview.MediaPreview -> false
        is ComposePreview.VoicePreview -> false
        is ComposePreview.FilePreview -> false
        else -> useLinkPreviews
      }
  val linkPreview: LinkPreview?
    get() =
      when (preview) {
        is ComposePreview.CLinkPreview -> preview.linkPreview
        else -> null
      }

  val attachmentDisabled: Boolean
    get() {
      if (editing || forwarding || liveMessage != null || inProgress || reporting) return true
      return when (preview) {
        ComposePreview.NoPreview -> false
        is ComposePreview.CLinkPreview -> false
        else -> true
      }
    }

  val attachmentPreview: Boolean
    get() = when (preview) {
      ComposePreview.NoPreview -> false
      is ComposePreview.CLinkPreview -> false
      is ComposePreview.MediaPreview -> preview.content.isNotEmpty()
      is ComposePreview.VoicePreview -> false
      is ComposePreview.FilePreview -> true
    }

  val placeholder: String
    get() = when (contextItem) {
      is ComposeContextItem.ReportedItem -> contextItem.reason.text
      else -> generalGetString(MR.strings.compose_message_placeholder)
    }

  val empty: Boolean
    get() = whitespaceOnly && preview is ComposePreview.NoPreview && contextItem is ComposeContextItem.NoContextItem

  val whitespaceOnly: Boolean
    get() = message.text.all { it.isWhitespace() }

  companion object {
    fun saver(): Saver<MutableState<ComposeState>, *> = Saver(
      save = { json.encodeToString(serializer(), it.value) },
      restore = {
        mutableStateOf(json.decodeFromString(it))
      }
    )
  }

  fun mentionMemberName(name: String): String {
    var n = 0
    var tryName = name

    while (mentions.containsKey(tryName)) {
      n++
      tryName = "${name}_$n"
    }

    return tryName
  }
}

private val maxFileSize = getMaxFileSize(FileProtocol.XFTP)

sealed class RecordingState {
  object NotStarted: RecordingState()
  class Started(val filePath: String, val progressMs: Int = 0): RecordingState()
  class Finished(val filePath: String, val durationMs: Int): RecordingState()

  val filePathNullable: String?
    get() = (this as? Started)?.filePath
}

fun chatItemPreview(chatItem: ChatItem): ComposePreview {
  val fileName = chatItem.file?.fileName ?: ""
  return when (val mc = chatItem.content.msgContent) {
    is MsgContent.MCText -> ComposePreview.NoPreview
    is MsgContent.MCLink -> ComposePreview.CLinkPreview(linkPreview = mc.preview)
    // TODO: include correct type
    is MsgContent.MCImage -> ComposePreview.MediaPreview(images = listOf(mc.image), listOf(UploadContent.SimpleImage(getAppFileUri(fileName))))
    is MsgContent.MCVideo -> ComposePreview.MediaPreview(images = listOf(mc.image), listOf(UploadContent.SimpleImage(getAppFileUri(fileName))))
    is MsgContent.MCVoice -> ComposePreview.VoicePreview(voice = fileName, mc.duration / 1000, true)
    is MsgContent.MCFile -> ComposePreview.FilePreview(fileName, getAppFileUri(fileName))
    is MsgContent.MCReport -> ComposePreview.NoPreview
    is MsgContent.MCChat -> ComposePreview.NoPreview
    is MsgContent.MCUnknown, null -> ComposePreview.NoPreview
  }
}

@Composable
expect fun AttachmentSelection(
  composeState: MutableState<ComposeState>,
  attachmentOption: MutableState<AttachmentOption?>,
  processPickedFile: (URI?, String?) -> Unit,
  processPickedMedia: (List<URI>, String?) -> Unit
)

fun MutableState<ComposeState>.onFilesAttached(uris: List<URI>) {
  val groups =  uris.groupBy { isImage(it) }
  val images = groups[true] ?: emptyList()
  val files = groups[false] ?: emptyList()
  if (images.isNotEmpty()) {
    CoroutineScope(Dispatchers.IO).launch { processPickedMedia(images, null) }
  } else if (files.isNotEmpty()) {
    processPickedFile(uris.first(), null)
  }
}

fun MutableState<ComposeState>.processPickedFile(uri: URI?, text: String?) {
  if (uri != null) {
    val fileSize = getFileSize(uri)
    if (fileSize != null && fileSize <= maxFileSize) {
      val fileName = getFileName(uri)
      if (fileName != null) {
        value = value.copy(message = if (text != null) ComposeMessage(text) else value.message, preview = ComposePreview.FilePreview(fileName, uri))
      }
    } else if (fileSize != null) {
      AlertManager.shared.showAlertMsg(
        generalGetString(MR.strings.large_file),
        String.format(generalGetString(MR.strings.maximum_supported_file_size), formatBytes(maxFileSize))
      )
    } else {
      showWrongUriAlert()
    }
  }
}

suspend fun MutableState<ComposeState>.processPickedMedia(uris: List<URI>, text: String?) {
  val content = ArrayList<UploadContent>()
  val imagesPreview = ArrayList<String>()
  uris.forEach { uri ->
    var bitmap: ImageBitmap?
    when {
      isImage(uri) -> {
        // Image
        val drawable = getDrawableFromUri(uri)
        // Do not show alert in case it's already shown from the function above
        bitmap = getBitmapFromUri(uri, withAlertOnException = !AlertManager.shared.hasAlertsShown())
        if (isAnimImage(uri, drawable)) {
          // It's a gif or webp
          val fileSize = getFileSize(uri)
          if (fileSize != null && fileSize <= maxFileSize) {
            content.add(UploadContent.AnimatedImage(uri))
          } else {
            bitmap = null
            AlertManager.shared.showAlertMsg(
              generalGetString(MR.strings.large_file),
              String.format(generalGetString(MR.strings.maximum_supported_file_size), formatBytes(maxFileSize))
            )
          }
        } else if (bitmap != null) {
          content.add(UploadContent.SimpleImage(uri))
        }
      }
      else -> {
        // Video
        val res = getBitmapFromVideo(uri, withAlertOnException = true)
        bitmap = res.preview
        val durationMs = res.duration
        content.add(UploadContent.Video(uri, durationMs?.div(1000)?.toInt() ?: 0))
      }
    }
    if (bitmap != null) {
      imagesPreview.add(resizeImageToStrSize(bitmap, maxDataSize = 14000))
    }
  }
  if (imagesPreview.isNotEmpty()) {
    value = value.copy(message = if (text != null) ComposeMessage(text) else value.message, preview = ComposePreview.MediaPreview(imagesPreview, content))
  }
}

@Composable
fun ComposeView(
  rhId: Long?,
  chatModel: ChatModel,
  chatsCtx: ChatModel.ChatsContext,
  chat: Chat,
  composeState: MutableState<ComposeState>,
  showCommandsMenu: MutableState<Boolean>,
  attachmentOption: MutableState<AttachmentOption?>,
  showChooseAttachment: () -> Unit,
  focusRequester: FocusRequester?,
) {
  val cancelledLinks = rememberSaveable { mutableSetOf<String>() }
  fun isSimplexLink(link: String): Boolean =
    link.startsWith("https://simplex.chat", true) || link.startsWith("http://simplex.chat", true)

  fun getMessageLinks(parsedMsg: List<FormattedText>?): Pair<String?, Boolean> {
    if (parsedMsg == null) return null to false
    val simplexLink = parsedMsg.any { ft -> ft.format is Format.SimplexLink }
    for (ft in parsedMsg) {
      val link = ft.linkUri
      if (link != null && !cancelledLinks.contains(link) && !isSimplexLink(link)) {
        return link to simplexLink
      }
    }
    return null to simplexLink
  }

  val linkUrl = rememberSaveable { mutableStateOf<String?>(null) }
  // default value parsed because of draft
  val hasSimplexLink = rememberSaveable { mutableStateOf(getMessageLinks(parseToMarkdown(composeState.value.message.text)).second) }
  val prevLinkUrl = rememberSaveable { mutableStateOf<String?>(null) }
  val pendingLinkUrl = rememberSaveable { mutableStateOf<String?>(null) }
  val useLinkPreviews = true
  val saveLastDraft = chatModel.controller.appPrefs.privacySaveLastDraft.get()
  val smallFont = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  val textStyle = remember(MaterialTheme.colors.isLight) { mutableStateOf(smallFont) }
  val recState: MutableState<RecordingState> = remember { mutableStateOf(RecordingState.NotStarted) }
  AttachmentSelection(composeState, attachmentOption, composeState::processPickedFile) { uris, text -> CoroutineScope(Dispatchers.IO).launch { composeState.processPickedMedia(uris, text) } }

  fun loadLinkPreview(url: String, wait: Long? = null) {
    if (pendingLinkUrl.value == url) {
      composeState.value = composeState.value.copy(preview = ComposePreview.CLinkPreview(null))
      withLongRunningApi(slow = 60_000) {
        if (wait != null) delay(wait)
        val lp = getLinkPreview(url)
        if (lp != null && pendingLinkUrl.value == url) {
          chatModel.controller.appPrefs.privacyLinkPreviewsShowAlert.set(false) // to avoid showing alert to current users, show alert in v6.5
          composeState.value = composeState.value.copy(preview = ComposePreview.CLinkPreview(lp))
          pendingLinkUrl.value = null
        } else if (pendingLinkUrl.value == url) {
          composeState.value = composeState.value.copy(preview = ComposePreview.NoPreview)
          pendingLinkUrl.value = null
        }
      }
    }
  }

  fun showLinkPreview(parsedMessage: List<FormattedText>?) {
    prevLinkUrl.value = linkUrl.value
    val linkParsed = getMessageLinks(parsedMessage)
    linkUrl.value = linkParsed.first
    hasSimplexLink.value = linkParsed.second
    val url = linkUrl.value
    if (url != null) {
      if (url != composeState.value.linkPreview?.uri && url != pendingLinkUrl.value) {
        pendingLinkUrl.value = url
        loadLinkPreview(url, wait = if (prevLinkUrl.value == url) null else 1500L)
      }
    } else {
      composeState.value = composeState.value.copy(preview = ComposePreview.NoPreview)
    }
  }

  fun resetLinkPreview() {
    linkUrl.value = null
    prevLinkUrl.value = null
    pendingLinkUrl.value = null
    cancelledLinks.clear()
  }

  fun clearPrevDraft(prevChatId: String?) {
    if (chatModel.draftChatId.value == prevChatId) {
      chatModel.draft.value = null
      chatModel.draftChatId.value = null
    }
  }

  fun clearCurrentDraft() {
    if (chatModel.draftChatId.value == chat.id) {
      chatModel.draft.value = null
      chatModel.draftChatId.value = null
    }
  }

  fun clearState(live: Boolean = false) {
    if (live) {
      composeState.value = composeState.value.copy(inProgress = false, progressByTimeout = false)
    } else {
      composeState.value = ComposeState(useLinkPreviews = useLinkPreviews)
      resetLinkPreview()
    }
    recState.value = RecordingState.NotStarted
    textStyle.value = smallFont
    chatModel.removeLiveDummy()
  }

  fun deleteUnusedFiles() {
    val shared = chatModel.sharedContent.value
    if (shared == null) {
      chatModel.filesToDelete.forEach { it.delete() }
      chatModel.filesToDelete.clear()
    } else {
      val sharedPaths = when (shared) {
        is SharedContent.Media -> shared.uris.map { it.toString() }
        is SharedContent.File -> listOf(shared.uri.toString())
        is SharedContent.Text -> emptyList()
        is SharedContent.Forward -> emptyList()
      }
      // When sharing a file and pasting it in SimpleX itself, the file shouldn't be deleted before sending or before leaving the chat after sharing
      chatModel.filesToDelete.removeAll { file ->
        if (sharedPaths.any { it.endsWith(file.name) }) {
          false
        } else {
          file.delete()
          true
        }
      }
    }
  }

  suspend fun send(chat: Chat, mc: MsgContent, quoted: Long?, file: CryptoFile? = null, live: Boolean = false, ttl: Int?, mentions: Map<String, Long>): ChatItem? {
    val cInfo = chat.chatInfo
    val chatItems = if (chat.chatInfo.chatType == ChatType.Local)
      chatModel.controller.apiCreateChatItems(
        rh = chat.remoteHostId,
        noteFolderId = chat.chatInfo.apiId,
        composedMessages = listOf(ComposedMessage(file, null, mc, mentions))
      )
    else
      chatModel.controller.apiSendMessages(
        rh = chat.remoteHostId,
        type = cInfo.chatType,
        id = cInfo.apiId,
        scope = cInfo.groupChatScope(),
        live = live,
        ttl = ttl,
        composedMessages = listOf(ComposedMessage(file, quoted, mc, mentions))
      )
    if (!chatItems.isNullOrEmpty()) {
      chatItems.forEach { aChatItem ->
        withContext(Dispatchers.Main) {
          chatsCtx.addChatItem(chat.remoteHostId, aChatItem.chatInfo, aChatItem.chatItem)
        }
      }
      return chatItems.first().chatItem
    }
    if (file != null) removeFile(file.filePath)
    return null
  }

  // TODO [short links] connectCheckLinkPreview
  fun checkLinkPreview(): MsgContent {
    val msgText = composeState.value.message.text
    return when (val composePreview = composeState.value.preview) {
      is ComposePreview.CLinkPreview -> {
        val parsedMsg = parseToMarkdown(msgText)
        val url = getMessageLinks(parsedMsg).first
        val lp = composePreview.linkPreview
        if (lp != null && url == lp.uri) {
          MsgContent.MCLink(msgText, preview = lp)
        } else {
          MsgContent.MCText(msgText)
        }
      }

      else -> MsgContent.MCText(msgText)
    }
  }

  fun sending() {
    composeState.value = composeState.value.copy(inProgress = true)
  }

  suspend fun sendMemberContactInvitation() {
    val mc = checkLinkPreview()
    sending()
    val contact = chatModel.controller.apiSendMemberContactInvitation(chat.remoteHostId, chat.chatInfo.apiId, mc)
    if (contact != null) {
      withContext(Dispatchers.Main) {
        chatsCtx.updateContact(chat.remoteHostId, contact)
        clearState()
        chatModel.setContactNetworkStatus(contact, NetworkStatus.Connected())
      }
    } else {
      composeState.value = composeState.value.copy(inProgress = false)
    }
  }

  suspend fun sendConnectPreparedContact() {
    val mc = checkLinkPreview()
    sending()
    val incognito = if (chat.chatInfo.profileChangeProhibited) chat.chatInfo.incognito else chatModel.controller.appPrefs.incognito.get()
    val contact = chatModel.controller.apiConnectPreparedContact(
      rh = chat.remoteHostId,
      contactId = chat.chatInfo.apiId,
      incognito = incognito,
      msg = mc
    )
    if (contact != null) {
      withContext(Dispatchers.Main) {
        chatsCtx.updateContact(chat.remoteHostId, contact)
        clearState()
        chatModel.setContactNetworkStatus(contact, NetworkStatus.Connected())
      }
    } else {
      composeState.value = composeState.value.copy(inProgress = false)
    }
  }

  fun showSendConnectPreparedContactAlert() {
    val empty = composeState.value.whitespaceOnly
    AlertManager.shared.showAlertDialogStacked(
      title = generalGetString(MR.strings.compose_view_send_contact_request_alert_question),
      text = generalGetString(MR.strings.compose_view_send_contact_request_alert_text),
      confirmText = (
          if (empty)
            generalGetString(MR.strings.compose_view_send_request_without_message)
          else
            generalGetString(MR.strings.compose_view_send_request)
          ),
      onConfirm = { withApi { sendConnectPreparedContact() } },
      dismissText = (
          if (empty)
            generalGetString(MR.strings.compose_view_add_message)
          else
            generalGetString(MR.strings.cancel_verb)
          )
    )
  }

  suspend fun connectPreparedGroup() {
    val mc = checkLinkPreview()
    sending()
    val incognito = if (chat.chatInfo.profileChangeProhibited) chat.chatInfo.incognito else chatModel.controller.appPrefs.incognito.get()
    val groupInfo = chatModel.controller.apiConnectPreparedGroup(
      rh = chat.remoteHostId,
      groupId = chat.chatInfo.apiId,
      incognito = incognito,
      msg = mc
    )
    if (groupInfo != null) {
      withContext(Dispatchers.Main) {
        chatsCtx.updateGroup(chat.remoteHostId, groupInfo)
        clearState()
      }
    } else {
      composeState.value = composeState.value.copy(inProgress = false)
    }
  }

  suspend fun sendMessageAsync(text: String?, live: Boolean, ttl: Int?): List<ChatItem>? {
    val cs = composeState.value
    var sent: List<ChatItem>?
    var lastMessageFailedToSend: ComposeState? = null
    val msgText = text ?: cs.message.text

    suspend fun forwardItem(rhId: Long?, forwardedItem: List<ChatItem>, fromChatInfo: ChatInfo, ttl: Int?): List<ChatItem>? {
      val chatItems = controller.apiForwardChatItems(
        rh = rhId,
        toChatType = chat.chatInfo.chatType,
        toChatId = chat.chatInfo.apiId,
        toScope = chat.chatInfo.groupChatScope(),
        fromChatType = fromChatInfo.chatType,
        fromChatId = fromChatInfo.apiId,
        fromScope = fromChatInfo.groupChatScope(),
        itemIds = forwardedItem.map { it.id },
        ttl = ttl
      )

      withContext(Dispatchers.Main) {
        chatItems?.forEach { chatItem ->
          chatsCtx.addChatItem(rhId, chat.chatInfo, chatItem)
        }
      }

      if (chatItems != null && chatItems.count() < forwardedItem.count()) {
        AlertManager.shared.showAlertMsg(
          title = String.format(generalGetString(MR.strings.forward_files_messages_deleted_after_selection_title), forwardedItem.count() - chatItems.count()),
          text = generalGetString(MR.strings.forward_files_messages_deleted_after_selection_desc)
        )
      }

      return chatItems
    }

    fun constructFailedMessage(cs: ComposeState): ComposeState {
      val preview = when (cs.preview) {
        is ComposePreview.MediaPreview -> {
          ComposePreview.MediaPreview(
            if (cs.preview.images.isNotEmpty()) listOf(cs.preview.images.last()) else emptyList(),
            if (cs.preview.content.isNotEmpty()) listOf(cs.preview.content.last()) else emptyList()
          )
        }
        else -> cs.preview
      }
      return cs.copy(inProgress = false, preview = preview)
    }

    fun updateMsgContent(msgContent: MsgContent): MsgContent {
      return when (msgContent) {
        is MsgContent.MCText -> checkLinkPreview()
        is MsgContent.MCLink -> checkLinkPreview()
        is MsgContent.MCImage -> MsgContent.MCImage(msgText, image = msgContent.image)
        is MsgContent.MCVideo -> MsgContent.MCVideo(msgText, image = msgContent.image, duration = msgContent.duration)
        is MsgContent.MCVoice -> MsgContent.MCVoice(msgText, duration = msgContent.duration)
        is MsgContent.MCFile -> MsgContent.MCFile(msgText)
        is MsgContent.MCReport -> MsgContent.MCReport(msgText, reason = msgContent.reason)
        // TODO [short links] update chat link
        is MsgContent.MCChat -> MsgContent.MCChat(msgText, chatLink = msgContent.chatLink)
        is MsgContent.MCUnknown -> MsgContent.MCUnknown(type = msgContent.type, text = msgText, json = msgContent.json)
      }
    }

    fun showReportsInSupportChatAlert() {
      AlertManager.shared.showAlertDialog(
        title = generalGetString(MR.strings.report_sent_alert_title),
        text = generalGetString(MR.strings.report_sent_alert_msg_view_in_support_chat),
        confirmText = generalGetString(MR.strings.ok),
        dismissText = generalGetString(MR.strings.dont_show_again),
        onDismiss = {
          chatModel.controller.appPrefs.showReportsInSupportChatAlert.set(false)
        },
      )
    }

    suspend fun sendReport(reportReason: ReportReason, chatItemId: Long): List<ChatItem>? {
      val cItems = chatModel.controller.apiReportMessage(chat.remoteHostId, chat.chatInfo.apiId, chatItemId, reportReason, msgText)
      if (chatModel.controller.appPrefs.showReportsInSupportChatAlert.get()) showReportsInSupportChatAlert()
      return cItems?.map { it.chatItem }
    }

    suspend fun updateMessage(ei: ChatItem, chat: Chat, live: Boolean): ChatItem? {
      val cInfo = chat.chatInfo
      val oldMsgContent = ei.content.msgContent
      if (oldMsgContent != null) {
        val updatedItem = chatModel.controller.apiUpdateChatItem(
          rh = chat.remoteHostId,
          type = cInfo.chatType,
          id = cInfo.apiId,
          scope = cInfo.groupChatScope(),
          itemId = ei.meta.itemId,
          updatedMessage = UpdatedMessage(updateMsgContent(oldMsgContent), cs.memberMentions),
          live = live
        )
        if (updatedItem != null) {
          withContext(Dispatchers.Main) {
            chatsCtx.upsertChatItem(chat.remoteHostId, cInfo, updatedItem.chatItem)
          }
        }
        return updatedItem?.chatItem
      }
      return null
    }

    val liveMessage = cs.liveMessage
    if (!live) {
      if (liveMessage != null) composeState.value = cs.copy(liveMessage = null)
      sending()
    }
    if (!cs.forwarding || chatModel.draft.value?.forwarding == true) {
      clearCurrentDraft()
    }

    if (cs.contextItem is ComposeContextItem.ForwardingItems) {
      sent = forwardItem(chat.remoteHostId, cs.contextItem.chatItems, cs.contextItem.fromChatInfo, ttl = ttl)
      if (sent == null) {
        lastMessageFailedToSend = constructFailedMessage(cs)
      }
      if (cs.message.text.isNotEmpty()) {
        sent?.mapIndexed { index, message ->
          if (index == sent!!.lastIndex) {
            send(chat, checkLinkPreview(), quoted = message.id, live = false, ttl = ttl, mentions = cs.memberMentions)
          } else {
            message
          }
        }
      }
    }
    else if (cs.contextItem is ComposeContextItem.EditingItem) {
      val ei = cs.contextItem.chatItem
      val updatedMessage = updateMessage(ei, chat, live)
      sent = if (updatedMessage != null) listOf(updatedMessage) else null
      lastMessageFailedToSend = if (updatedMessage == null) constructFailedMessage(cs) else null
    } else if (liveMessage != null && liveMessage.sent) {
      val updatedMessage = updateMessage(liveMessage.chatItem, chat, live)
      sent = if (updatedMessage != null) listOf(updatedMessage) else null
    } else if (cs.contextItem is ComposeContextItem.ReportedItem) {
      sent = sendReport(cs.contextItem.reason, cs.contextItem.chatItem.id)
    } else {
      val msgs: ArrayList<MsgContent> = ArrayList()
      val files: ArrayList<CryptoFile> = ArrayList()
      val remoteHost = chatModel.currentRemoteHost.value
      when (val preview = cs.preview) {
        ComposePreview.NoPreview -> msgs.add(MsgContent.MCText(msgText))
        is ComposePreview.CLinkPreview -> msgs.add(checkLinkPreview())
        is ComposePreview.MediaPreview -> {
          // TODO batch send: batch media previews
          preview.content.forEachIndexed { index, it ->
            val file = when (it) {
              is UploadContent.SimpleImage ->
                if (remoteHost == null) saveImage(it.uri)
                else desktopSaveImageInTmp(it.uri)
              is UploadContent.AnimatedImage ->
                if (remoteHost == null) saveAnimImage(it.uri)
                else CryptoFile.desktopPlain(it.uri)
              is UploadContent.Video ->
                if (remoteHost == null) saveFileFromUri(it.uri, hiddenFileNamePrefix = "video")
                else CryptoFile.desktopPlain(it.uri)
            }
            if (file != null) {
              files.add(file)
              if (it is UploadContent.Video) {
                msgs.add(MsgContent.MCVideo(if (preview.content.lastIndex == index) msgText else "", preview.images[index], it.duration))
              } else {
                msgs.add(MsgContent.MCImage(if (preview.content.lastIndex == index) msgText else "", preview.images[index]))
              }
            }
          }
        }
        is ComposePreview.VoicePreview -> {
          val tmpFile = File(preview.voice)
          AudioPlayer.stop(tmpFile.absolutePath)
          if (remoteHost == null) {
            val actualFile = File(getAppFilePath(tmpFile.name.replaceAfter(RecorderInterface.extension, "")))
            val file = withContext(Dispatchers.IO) {
              if (chatController.appPrefs.privacyEncryptLocalFiles.get()) {
                val args = try {
                  encryptCryptoFile(tmpFile.absolutePath, actualFile.absolutePath)
                } catch (e: Exception) {
                  Log.e(TAG, "Unable to encrypt plain file: " + e.stackTraceToString())
                  AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.error), text = e.stackTraceToString())
                  return@withContext null
                } finally {
                  tmpFile.delete()
                }
                CryptoFile(actualFile.name, args)
              } else {
                Files.move(tmpFile.toPath(), actualFile.toPath())
                CryptoFile.plain(actualFile.name)
              }
            }
            if (file != null) {
              files.add(file)
              msgs.add(MsgContent.MCVoice(if (msgs.isEmpty()) msgText else "", preview.durationMs / 1000))
            }
            deleteUnusedFiles()
          } else {
            files.add(CryptoFile.plain(tmpFile.absolutePath))
            // It will be deleted on JVM shutdown or next start (if the app crashes unexpectedly)
            filesToDelete.remove(tmpFile)
            msgs.add(MsgContent.MCVoice(if (msgs.isEmpty()) msgText else "", preview.durationMs / 1000))
          }
        }
        is ComposePreview.FilePreview -> {
          val file = if (remoteHost == null) {
            saveFileFromUri(preview.uri)
          } else {
            CryptoFile.desktopPlain(preview.uri)
          }
          if (file != null) {
            files.add((file))
            msgs.add(MsgContent.MCFile(if (msgs.isEmpty()) msgText else ""))
          }
        }
      }
      val quotedItemId: Long? = when (cs.contextItem) {
        is ComposeContextItem.QuotedItem -> cs.contextItem.chatItem.id
        else -> null
      }
      sent = null
      msgs.forEachIndexed { index, content ->
        if (index > 0) delay(100)
        var file = files.getOrNull(index)
        if (remoteHost != null && file != null) {
          file = controller.storeRemoteFile(
            rhId = remoteHost.remoteHostId,
            storeEncrypted = if (content is MsgContent.MCVideo) false else null,
            localPath = file.filePath
          )
        }
        val sendResult = send(chat, content, if (index == 0) quotedItemId else null, file,
          live = if (content !is MsgContent.MCVoice && index == msgs.lastIndex) live else false,
          ttl = ttl,
          mentions = cs.memberMentions
        )
        sent = if (sendResult != null) listOf(sendResult) else null
        if (sent == null && index == msgs.lastIndex && cs.liveMessage == null) {
          constructFailedMessage(cs)
          // it's the last message in the series so if it fails, restore it in ComposeView for editing
          lastMessageFailedToSend = constructFailedMessage(cs)
        }
      }
    }
    val wasForwarding = cs.forwarding
    val forwardingFromChatId = (cs.contextItem as? ComposeContextItem.ForwardingItems)?.fromChatInfo?.id
    val lastFailed = lastMessageFailedToSend
    if (lastFailed == null) {
      clearState(live)
    } else {
      composeState.value = lastFailed
    }
    val draft = chatModel.draft.value
    if (wasForwarding && chatModel.draftChatId.value == chat.chatInfo.id && forwardingFromChatId != chat.chatInfo.id && draft != null) {
      composeState.value = draft
    } else {
      clearCurrentDraft()
    }
    return sent
  }

  fun sendMessage(ttl: Int?) {
    withLongRunningApi(slow = 120_000) {
      sendMessageAsync(null, false, ttl)
    }
  }

  fun sanitizeMessage(parsedMsg: List<FormattedText>): Triple<String, List<FormattedText>, Int?> {
    var pos = 0
    var updatedMsg = ""
    var sanitizedPos: Int? = null
    val updatedParsedMsg = parsedMsg.map { ft ->
      var updated = ft
      when(ft.format) {
        is Format.Uri -> {
          val sanitized = parseSanitizeUri(ft.text, safe = true)?.uriInfo?.sanitized
          if (sanitized != null) {
            updated = FormattedText(text = sanitized, format = Format.Uri())
            pos += updated.text.count()
            sanitizedPos = pos
          }
        }
        is Format.HyperLink -> {
          val sanitized = parseSanitizeUri(ft.format.linkUri, safe = true)?.uriInfo?.sanitized
          if (sanitized != null) {
            val updatedText = if (ft.format.showText == null) sanitized else "[${ft.format.showText}]($sanitized)"
            updated = FormattedText(text = updatedText, format = Format.HyperLink(showText = ft.format.showText, linkUri = sanitized))
            pos += updated.text.count()
            sanitizedPos = pos
          }
        }
        else ->
          pos += ft.text.count()
      }
      updatedMsg += updated.text
      updated
    }
    return Triple(updatedMsg, updatedParsedMsg, sanitizedPos)
  }

  fun onMessageChange(s: ComposeMessage) {
    var parsedMessage = parseToMarkdown(s.text)
    if (chatModel.controller.appPrefs.privacySanitizeLinks.get() && parsedMessage != null) {
      val (updatedMsg, updatedParsedMsg, sanitizedPos) = sanitizeMessage(parsedMessage)
      if (sanitizedPos == null) {
        composeState.value = composeState.value.copy(message = s, parsedMessage = parsedMessage)
      } else {
        val pos = min(updatedMsg.count(), if (sanitizedPos < s.selection.start) s.selection.start else sanitizedPos)
        val message = s.copy(text = updatedMsg, selection = TextRange(pos))
        composeState.value = composeState.value.copy(message = message, parsedMessage = updatedParsedMsg)
        parsedMessage = updatedParsedMsg
      }
    } else {
      composeState.value = composeState.value.copy(message = s, parsedMessage = parsedMessage ?: FormattedText.plain(s.text))
    }
    if (isShortEmoji(s.text)) {
      textStyle.value = if (s.text.codePoints().count() < 4) largeEmojiFont else mediumEmojiFont
    } else {
      textStyle.value = smallFont
      if (composeState.value.linkPreviewAllowed && chatModel.controller.appPrefs.privacyLinkPreviews.get()) {
        if (s.text.isNotEmpty()) {
          showLinkPreview(parsedMessage)
        } else {
          resetLinkPreview()
          hasSimplexLink.value = false
          composeState.value = composeState.value.copy(preview = ComposePreview.NoPreview)
        }
      } else {
        resetLinkPreview()
        hasSimplexLink.value = s.text.isNotEmpty() && !chat.groupFeatureEnabled(GroupFeature.SimplexLinks) && getMessageLinks(parsedMessage).second
        if (composeState.value.linkPreviewAllowed) composeState.value = composeState.value.copy(preview = ComposePreview.NoPreview)
      }
    }
  }

  fun onAudioAdded(filePath: String, durationMs: Int, finished: Boolean) {
    val file = File(filePath)
    chatModel.filesToDelete.add(file)
    composeState.value = composeState.value.copy(preview = ComposePreview.VoicePreview(filePath, durationMs, finished))
  }

  fun allowVoiceToContact() {
    val contact = (chat.chatInfo as ChatInfo.Direct?)?.contact ?: return
    withBGApi {
      chatModel.controller.allowFeatureToContact(chat.remoteHostId, contact, ChatFeature.Voice)
    }
  }

  fun cancelLinkPreview() {
    val pendingLink = pendingLinkUrl.value
    if (pendingLink != null) {
      cancelledLinks.add(pendingLink)
    }
    val uri = composeState.value.linkPreview?.uri
    if (uri != null) {
      cancelledLinks.add(uri)
    }
    pendingLinkUrl.value = null
    composeState.value = composeState.value.copy(preview = ComposePreview.NoPreview)
  }

  fun cancelImages() {
    composeState.value = composeState.value.copy(preview = ComposePreview.NoPreview)
  }

  fun cancelVoice() {
    val filePath = recState.value.filePathNullable
    recState.value = RecordingState.NotStarted
    composeState.value = composeState.value.copy(preview = ComposePreview.NoPreview)
    withBGApi {
      RecorderInterface.stopRecording?.invoke()
      AudioPlayer.stop(filePath)
      filePath?.let { File(it).delete() }
    }
  }

  fun cancelFile() {
    composeState.value = composeState.value.copy(preview = ComposePreview.NoPreview)
  }

  fun truncateToWords(s: String): String {
    var acc = ""
    val word = StringBuilder()
    for (c in s) {
      if (c.isLetter() || c.isDigit()) {
        word.append(c)
      } else {
        acc = acc + word.toString() + c
        word.clear()
      }
    }
    return acc
  }

  suspend fun sendLiveMessage() {
    val cs = composeState.value
    val typedMsg = cs.message.text
    if ((cs.sendEnabled() || cs.contextItem is ComposeContextItem.QuotedItem) && (cs.liveMessage == null || !cs.liveMessage.sent)) {
      val ci = sendMessageAsync(typedMsg, live = true, ttl = null)
      if (!ci.isNullOrEmpty()) {
        composeState.value = composeState.value.copy(liveMessage = LiveMessage(ci.last(), typedMsg = typedMsg, sentMsg = typedMsg, sent = true))
      }
    } else if (cs.liveMessage == null) {
      val cItem = chatModel.addLiveDummy(chat.chatInfo)
      composeState.value = composeState.value.copy(liveMessage = LiveMessage(cItem, typedMsg = typedMsg, sentMsg = typedMsg, sent = false))
    }
  }

  fun liveMessageToSend(lm: LiveMessage, t: String): String? {
    val s = if (t != lm.typedMsg) truncateToWords(t) else t
    return if (s != lm.sentMsg) s else null
  }

  suspend fun updateLiveMessage() {
    val typedMsg = composeState.value.message
    val liveMessage = composeState.value.liveMessage
    if (liveMessage != null) {
      val sentMsg = liveMessageToSend(liveMessage, typedMsg.text)
      if (sentMsg != null) {
        val ci = sendMessageAsync(sentMsg, live = true, ttl = null)
        if (!ci.isNullOrEmpty()) {
          composeState.value = composeState.value.copy(liveMessage = LiveMessage(ci.last(), typedMsg = typedMsg.text, sentMsg = sentMsg, sent = true))
        }
      } else if (liveMessage.typedMsg != typedMsg.text) {
        composeState.value = composeState.value.copy(liveMessage = liveMessage.copy(typedMsg = typedMsg.text))
      }
    }
  }

  fun editPrevMessage() {
    if (composeState.value.contextItem != ComposeContextItem.NoContextItem || composeState.value.preview != ComposePreview.NoPreview) return
    val lastEditable = chatsCtx.chatItems.value.findLast { it.meta.editable }
    if (lastEditable != null) {
      composeState.value = ComposeState(editingItem = lastEditable, useLinkPreviews = useLinkPreviews)
    }
  }

  @Composable
  fun previewView() {
    when (val preview = composeState.value.preview) {
      ComposePreview.NoPreview -> {}
      is ComposePreview.CLinkPreview -> ComposeLinkView(
        preview.linkPreview,
        ::cancelLinkPreview,
        cancelEnabled = !composeState.value.inProgress
      )
      is ComposePreview.MediaPreview -> ComposeImageView(
        preview,
        ::cancelImages,
        cancelEnabled = !composeState.value.editing && !composeState.value.inProgress
      )
      is ComposePreview.VoicePreview -> ComposeVoiceView(
        preview.voice,
        preview.durationMs,
        preview.finished,
        cancelEnabled = !composeState.value.editing && !composeState.value.inProgress,
        ::cancelVoice
      )
      is ComposePreview.FilePreview -> ComposeFileView(
        preview.fileName,
        ::cancelFile,
        cancelEnabled = !composeState.value.editing && !composeState.value.inProgress
      )
    }
  }

  @Composable
  fun MsgNotAllowedView(reason: String, icon: Painter) {
    val color = MaterialTheme.appColors.receivedQuote
    Row(Modifier.fillMaxWidth().background(color).padding(horizontal = DEFAULT_PADDING_HALF, vertical = DEFAULT_PADDING_HALF * 1.5f), verticalAlignment = Alignment.CenterVertically) {
      Icon(icon, null, tint = MaterialTheme.colors.secondary)
      Spacer(Modifier.width(DEFAULT_PADDING_HALF))
      Text(reason, fontStyle = FontStyle.Italic)
    }
  }

  @Composable
  fun ReportReasonView(reason: ReportReason) {
    val reportText = when (reason) {
      is ReportReason.Spam -> generalGetString(MR.strings.report_compose_reason_header_spam)
      is ReportReason.Illegal -> generalGetString(MR.strings.report_compose_reason_header_illegal)
      is ReportReason.Profile -> generalGetString(MR.strings.report_compose_reason_header_profile)
      is ReportReason.Community -> generalGetString(MR.strings.report_compose_reason_header_community)
      is ReportReason.Other -> generalGetString(MR.strings.report_compose_reason_header_other)
      is ReportReason.Unknown -> null // should never happen
    }

    if (reportText != null) {
      val color = MaterialTheme.appColors.receivedQuote
      Row(Modifier.fillMaxWidth().background(color).padding(horizontal = DEFAULT_PADDING_HALF, vertical = DEFAULT_PADDING_HALF * 1.5f), verticalAlignment = Alignment.CenterVertically) {
        Text(reportText, fontStyle = FontStyle.Italic, fontSize = 12.sp)
      }
    }
  }

  @Composable
  fun contextItemView() {
    when (val contextItem = composeState.value.contextItem) {
      ComposeContextItem.NoContextItem -> {}
      is ComposeContextItem.QuotedItem -> ContextItemView(listOf(contextItem.chatItem), painterResource(MR.images.ic_reply), chatInfo = chat.chatInfo) {
        composeState.value = composeState.value.copy(contextItem = ComposeContextItem.NoContextItem)
      }
      is ComposeContextItem.EditingItem -> ContextItemView(listOf(contextItem.chatItem), painterResource(MR.images.ic_edit_filled),  chatInfo = chat.chatInfo) {
        clearState()
      }
      is ComposeContextItem.ForwardingItems -> ContextItemView(contextItem.chatItems, painterResource(MR.images.ic_forward), showSender = false,  chatInfo = chat.chatInfo) {
        composeState.value = composeState.value.copy(contextItem = ComposeContextItem.NoContextItem)
      }
      is ComposeContextItem.ReportedItem -> ContextItemView(listOf(contextItem.chatItem), painterResource(MR.images.ic_flag), chatInfo = chat.chatInfo, contextIconColor = Color.Red) {
        composeState.value = composeState.value.copy(contextItem = ComposeContextItem.NoContextItem)
      }
    }
  }

  val sendMsgEnabled = rememberUpdatedState(chat.chatInfo.sendMsgEnabled)
  val userCantSendReason = rememberUpdatedState(chat.chatInfo.userCantSendReason)
  val nextSendGrpInv = rememberUpdatedState(chat.nextSendGrpInv)

  @Composable
  fun CommandsButton() {
    val commandsEnabled = chat.chatInfo.sendMsgEnabled && chat.chatInfo.menuCommands.isNotEmpty()
    IconButton(
      onClick = { showCommandsMenu.value = !showCommandsMenu.value },
      enabled = commandsEnabled
    ) {
      Box(
        modifier = Modifier.size(28.dp).clip(CircleShape),
        contentAlignment = Alignment.Center
      ) {
        Text("//", style = MaterialTheme.typography.h3.copy(fontStyle = FontStyle.Italic, color = if (commandsEnabled) MaterialTheme.colors.primary else MaterialTheme.colors.secondary))
      }
    }
  }

  @Composable
  fun AttachmentButton() {
    val isGroupAndProhibitedFiles =
      chatsCtx.secondaryContextFilter == null
          && chat.chatInfo is ChatInfo.Group
          && !chat.chatInfo.groupInfo.fullGroupPreferences.files.on(chat.chatInfo.groupInfo.membership)
    val attachmentClicked = if (isGroupAndProhibitedFiles) {
      {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.files_and_media_prohibited),
          text = generalGetString(MR.strings.only_owners_can_enable_files_and_media)
        )
      }
    } else {
      showChooseAttachment
    }
    val attachmentEnabled =
      !composeState.value.attachmentDisabled
          && sendMsgEnabled.value
          && !isGroupAndProhibitedFiles
          && !nextSendGrpInv.value
    IconButton(
      attachmentClicked,
      enabled = attachmentEnabled
    ) {
      Icon(
        painterResource(MR.images.ic_attach_file_filled_500),
        contentDescription = stringResource(MR.strings.attach),
        tint = if (attachmentEnabled) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
        modifier = Modifier
          .size(28.dp)
          .clip(CircleShape)
      )
    }
  }

  @Composable
  fun AttachmentAndCommandsButtons() {
    val cInfo = chat.chatInfo
    Row(
      Modifier.padding(start = 3.dp, end = 1.dp, bottom = if (appPlatform.isAndroid) 2.sp.toDp() else 5.sp.toDp() * fontSizeSqrtMultiplier),
      horizontalArrangement = Arrangement.spacedBy((-8).dp)
    ) {
      val msg = composeState.value.message.text.trim()
      val showAttachment = cInfo !is ChatInfo.Direct || cInfo.contact.profile.peerType != ChatPeerType.Bot || cInfo.featureEnabled(ChatFeature.Files)
      if (cInfo.useCommands && (!showAttachment || msg.isEmpty() || msg.startsWith("/"))) {
        CommandsButton()
      }
      if (showAttachment) {
        AttachmentButton()
      }
    }
  }

  val allowedVoiceByPrefs = remember(chat.chatInfo) { chat.chatInfo.featureEnabled(ChatFeature.Voice) }
  LaunchedEffect(allowedVoiceByPrefs) {
    if (!allowedVoiceByPrefs && composeState.value.preview is ComposePreview.VoicePreview) {
      // Voice was disabled right when this user records it, just cancel it
      cancelVoice()
    }
  }
  val needToAllowVoiceToContact = remember(chat.chatInfo) {
    chat.chatInfo is ChatInfo.Direct && with(chat.chatInfo.contact.mergedPreferences.voice) {
      ((userPreference as? ContactUserPref.User)?.preference?.allow == FeatureAllowed.NO || (userPreference as? ContactUserPref.Contact)?.preference?.allow == FeatureAllowed.NO) &&
          contactPreference.allow == FeatureAllowed.YES
    }
  }
  LaunchedEffect(Unit) {
    snapshotFlow { recState.value }
      .distinctUntilChanged()
      .collect {
        when (it) {
          is RecordingState.Started -> onAudioAdded(it.filePath, it.progressMs, false)
          is RecordingState.Finished -> if (it.durationMs > 300) {
            onAudioAdded(it.filePath, it.durationMs, true)
          } else {
            cancelVoice()
          }
          is RecordingState.NotStarted -> {}
        }
      }
  }

  LaunchedEffect(rememberUpdatedState(chat.chatInfo.sendMsgEnabled).value) {
    if (!chat.chatInfo.sendMsgEnabled) {
      clearCurrentDraft()
      clearState()
    }
  }

  KeyChangeEffect(chatModel.chatId.value) { prevChatId ->
    val cs = composeState.value
    if (cs.liveMessage != null && (cs.message.text.isNotEmpty() || cs.liveMessage.sent)) {
      sendMessage(null)
      resetLinkPreview()
      clearPrevDraft(prevChatId)
      deleteUnusedFiles()
    } else if (cs.inProgress) {
      clearPrevDraft(prevChatId)
      composeState.value = cs.copy(inProgress = false, progressByTimeout = false)
    } else if (!cs.empty) {
      if (cs.preview is ComposePreview.VoicePreview && !cs.preview.finished) {
        composeState.value = cs.copy(preview = cs.preview.copy(finished = true))
      }
      if (saveLastDraft) {
        chatModel.draft.value = composeState.value
        chatModel.draftChatId.value = prevChatId
      }
      composeState.value = ComposeState(useLinkPreviews = useLinkPreviews)
    } else if (chatModel.draftChatId.value == chatModel.chatId.value && chatModel.draft.value != null) {
      composeState.value = chatModel.draft.value ?: ComposeState(useLinkPreviews = useLinkPreviews)
    } else {
      clearPrevDraft(prevChatId)
      deleteUnusedFiles()
    }
    chatModel.removeLiveDummy()
    CIFile.cachedRemoteFileRequests.clear()
  }
  if (appPlatform.isDesktop) {
    // Don't enable this on Android, it breaks it, This method only works on desktop. For Android there is a `KeyChangeEffect(chatModel.chatId.value)`
    DisposableEffect(Unit) {
      onDispose {
        if (chatModel.sharedContent.value is SharedContent.Forward && saveLastDraft && !composeState.value.empty) {
          chatModel.draft.value = composeState.value
          chatModel.draftChatId.value = chat.id
        }
      }
    }
  }

  @Composable
  fun SendMsgView_(
    disableSendButton: Boolean,
    placeholder: String? = null,
    sendToConnect: (() -> Unit)? = null
  ) {
    val timedMessageAllowed = remember(chat.chatInfo) { chat.chatInfo.featureEnabled(ChatFeature.TimedMessages) }
    val sendButtonColor =
      if (chat.chatInfo.incognito)
        if (isInDarkTheme()) Indigo else Indigo.copy(alpha = 0.7F)
      else MaterialTheme.colors.primary
    SendMsgView(
      composeState,
      showVoiceRecordIcon = true,
      recState,
      chat.chatInfo is ChatInfo.Direct,
      liveMessageAlertShown = chatModel.controller.appPrefs.liveMessageAlertShown,
      sendMsgEnabled = sendMsgEnabled.value,
      userCantSendReason = userCantSendReason.value,
      sendButtonEnabled = sendMsgEnabled.value && !disableSendButton,
      sendToConnect = sendToConnect,
      hideSendButton = chat.chatInfo.nextConnect && !nextSendGrpInv.value && composeState.value.whitespaceOnly,
      nextConnect = chat.chatInfo.nextConnect,
      needToAllowVoiceToContact = needToAllowVoiceToContact,
      allowedVoiceByPrefs = allowedVoiceByPrefs,
      allowVoiceToContact = ::allowVoiceToContact,
      sendButtonColor = sendButtonColor,
      timedMessageAllowed = timedMessageAllowed,
      customDisappearingMessageTimePref = chatModel.controller.appPrefs.customDisappearingMessageTime,
      placeholder = placeholder ?: composeState.value.placeholder,
      sendMessage = { ttl ->
        sendMessage(ttl)
        resetLinkPreview()
      },
      sendLiveMessage = if (chat.chatInfo.chatType != ChatType.Local) ::sendLiveMessage else null,
      updateLiveMessage = ::updateLiveMessage,
      cancelLiveMessage = {
        composeState.value = composeState.value.copy(liveMessage = null)
        chatModel.removeLiveDummy()
      },
      editPrevMessage = ::editPrevMessage,
      onEscape = {
        when (composeState.value.contextItem) {
          is ComposeContextItem.EditingItem -> clearState()
          ComposeContextItem.NoContextItem -> {}
          else -> composeState.value = composeState.value.copy(contextItem = ComposeContextItem.NoContextItem)
        }
      },
      onFilesPasted = { composeState.onFilesAttached(it) },
      onMessageChange = ::onMessageChange,
      textStyle = textStyle,
      focusRequester = focusRequester,
    )
  }

  @Composable
  fun SendContactRequestView(
    disableSendButton: Boolean,
    icon: ImageResource,
    sendRequest: () -> Unit
  ) {
    Row(
      Modifier.padding(horizontal = DEFAULT_PADDING_HALF),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Box(
        Modifier.weight(1f)
      ) {
        SendMsgView_(
          disableSendButton = disableSendButton,
          placeholder = generalGetString(MR.strings.compose_view_add_message),
          sendToConnect = sendRequest
        )
      }
      if (composeState.value.whitespaceOnly) {
        SimpleButtonIconEnded(
          text = stringResource(MR.strings.compose_view_connect),
          icon = painterResource(icon),
          style = MaterialTheme.typography.body2,
          color = if (composeState.value.inProgress) MaterialTheme.colors.secondary else MaterialTheme.colors.primary,
          disabled = composeState.value.inProgress,
          click = { withApi { sendRequest() } }
        )
      }
    }
  }

  @Composable
  fun ConnectButtonView(
    text: String,
    icon: ImageResource,
    connect: () -> Unit
  ) {
    var modifier = Modifier.height(60.dp).fillMaxWidth()
    modifier = if (composeState.value.inProgress) modifier else modifier.clickable(onClick = { connect() })
    Box(
      modifier,
      contentAlignment = Alignment.Center
    ) {
      Row(
        horizontalArrangement = Arrangement.spacedBy(8.dp),
        verticalAlignment = Alignment.CenterVertically
      ) {
        Icon(
          painterResource(icon),
          contentDescription = null,
          tint = if (composeState.value.inProgress) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
        )
        Text(
          text,
          style = MaterialTheme.typography.body2,
          color = if (composeState.value.inProgress) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
        )
      }
      if (composeState.value.progressByTimeout) {
        Box(
          Modifier.fillMaxWidth().padding(end = DEFAULT_PADDING_HALF),
          contentAlignment = Alignment.CenterEnd
        ) {
          ComposeProgressIndicator()
        }
      }
    }
  }

  @Composable
  fun ContextSendMessageToConnect(s: String) {
    Row(
      Modifier
        .height(60.dp)
        .fillMaxWidth()
        .padding(horizontal = DEFAULT_PADDING),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Icon(
        painterResource(MR.images.ic_chat),
        contentDescription = null,
        modifier = Modifier
          .padding(end = 8.dp)
          .height(20.dp)
          .width(20.dp),
        tint = MaterialTheme.colors.secondary
      )
      Text(s)
    }
  }

  // In case a user sent something, state is in progress, the user rotates a screen to different orientation.
  // Without clearing the state the user will be unable to send anything until re-enters ChatView
  LaunchedEffect(Unit) {
    if (composeState.value.inProgress) {
      clearState()
    }
  }

  LaunchedEffect(chatModel.sharedContent.value) {
    // Important. If it's null, don't do anything, chat is not closed yet but will be after a moment
    if (chatModel.chatId.value == null) return@LaunchedEffect

    when (val shared = chatModel.sharedContent.value) {
      is SharedContent.Text -> onMessageChange(ComposeMessage(shared.text))
      is SharedContent.Media -> composeState.processPickedMedia(shared.uris, shared.text)
      is SharedContent.File -> composeState.processPickedFile(shared.uri, shared.text)
      is SharedContent.Forward -> composeState.value = composeState.value.copy(
        contextItem = ComposeContextItem.ForwardingItems(shared.chatItems, shared.fromChatInfo),
        preview = if (composeState.value.preview is ComposePreview.CLinkPreview) composeState.value.preview else ComposePreview.NoPreview
      )
      null -> {}
    }
    chatModel.sharedContent.value = null
  }

  LaunchedEffect(composeState.value.inProgress) {
    val newProgressByTimeout = if (composeState.value.inProgress) {
      delay(1000)
      composeState.value.inProgress
    } else {
      false
    }
    composeState.value = composeState.value.copy(progressByTimeout = newProgressByTimeout)
  }

  Column {
    val currentUser = chatModel.currentUser.value
    if (chat.chatInfo.nextConnectPrepared && currentUser != null) {
      ComposeContextProfilePickerView(
        rhId = rhId,
        chat = chat,
        currentUser = currentUser
      )
    }

    if (
      chat.chatInfo is ChatInfo.Group
      && chatsCtx.secondaryContextFilter is SecondaryContextFilter.GroupChatScopeContext
      && chatsCtx.secondaryContextFilter.groupScopeInfo is GroupChatScopeInfo.MemberSupport
      && chatsCtx.secondaryContextFilter.groupScopeInfo.groupMember_ != null
      && chatsCtx.secondaryContextFilter.groupScopeInfo.groupMember_.memberPending
      && composeState.value.contextItem == ComposeContextItem.NoContextItem
      && composeState.value.preview == ComposePreview.NoPreview
    ) {
      ComposeContextPendingMemberActionsView(
        rhId = rhId,
        groupInfo = chat.chatInfo.groupInfo,
        member = chatsCtx.secondaryContextFilter.groupScopeInfo.groupMember_
      )
    }

    val ctx = composeState.value.contextItem
    if (ctx is ComposeContextItem.ReportedItem) {
      ReportReasonView(ctx.reason)
    }

    val simplexLinkProhibited = chatsCtx.secondaryContextFilter == null && hasSimplexLink.value && !chat.groupFeatureEnabled(GroupFeature.SimplexLinks)
    val fileProhibited = chatsCtx.secondaryContextFilter == null && composeState.value.attachmentPreview && !chat.groupFeatureEnabled(GroupFeature.Files)
    val voiceProhibited = composeState.value.preview is ComposePreview.VoicePreview && !chat.chatInfo.featureEnabled(ChatFeature.Voice)
    val disableSendButton = simplexLinkProhibited || fileProhibited || voiceProhibited
    if (composeState.value.preview !is ComposePreview.VoicePreview || composeState.value.editing) {
      if (simplexLinkProhibited) {
        MsgNotAllowedView(generalGetString(MR.strings.simplex_links_not_allowed), icon = painterResource(MR.images.ic_link))
      } else if (fileProhibited) {
        MsgNotAllowedView(generalGetString(MR.strings.files_and_media_not_allowed), icon = painterResource(MR.images.ic_draft))
      } else if (voiceProhibited) {
        MsgNotAllowedView(generalGetString(MR.strings.voice_messages_not_allowed), icon = painterResource(MR.images.ic_mic))
      }
      contextItemView()
      when {
        composeState.value.editing && composeState.value.preview is ComposePreview.VoicePreview -> {}
        composeState.value.editing && composeState.value.preview is ComposePreview.FilePreview -> {}
        else -> previewView()
      }
    } else {
      Box {
        Box(Modifier.align(Alignment.TopStart).padding(bottom = 69.dp)) {
          contextItemView()
        }
        Box(Modifier.align(Alignment.BottomStart)) {
          previewView()
        }
      }
    }

    Surface(color = MaterialTheme.colors.background, contentColor = MaterialTheme.colors.onBackground) {
      Divider()
      if (chat.chatInfo is ChatInfo.Group && chat.chatInfo.groupInfo.nextConnectPrepared) {
        if (chat.chatInfo.groupInfo.businessChat == null) {
          ConnectButtonView(
            text = stringResource(MR.strings.compose_view_join_group),
            icon = MR.images.ic_group_filled,
            connect = { withApi { connectPreparedGroup() } }
          )
        } else {
          SendContactRequestView(
            disableSendButton = disableSendButton,
            icon = MR.images.ic_work_filled,
            sendRequest = { withApi { connectPreparedGroup() } }
          )
        }
      } else if (nextSendGrpInv.value) {
        Column {
          ContextSendMessageToConnect(generalGetString(MR.strings.compose_send_direct_message_to_connect))
          Divider()
          Row(Modifier.padding(end = 8.dp), verticalAlignment = Alignment.Bottom) {
            AttachmentAndCommandsButtons()
            SendMsgView_(
              disableSendButton = disableSendButton,
              sendToConnect = { withApi { sendMemberContactInvitation() } }
            )
          }
        }
      } else if (
        chat.chatInfo is ChatInfo.Direct
        && chat.chatInfo.contact.nextConnectPrepared
        && chat.chatInfo.contact.preparedContact != null
      ) {
        when (chat.chatInfo.contact.preparedContact.uiConnLinkType) {
          ConnectionMode.Inv ->
            ConnectButtonView(
              text = stringResource(MR.strings.compose_view_connect),
              icon = MR.images.ic_person_add_filled,
              connect = { withApi { sendConnectPreparedContact() } }
            )
          ConnectionMode.Con ->
            if (chat.chatInfo.contact.isBot) {
              ConnectButtonView(
                text = stringResource(MR.strings.compose_view_connect),
                icon = MR.images.ic_bolt_filled,
                connect = { withApi { sendConnectPreparedContact() } }
              )
            } else {
              SendContactRequestView(
                disableSendButton = disableSendButton,
                icon = MR.images.ic_person_add_filled,
                sendRequest = { showSendConnectPreparedContactAlert() }
              )
            }
        }
      } else if (
        chat.chatInfo is ChatInfo.Direct
        && chat.chatInfo.contact.nextAcceptContactRequest
        && chat.chatInfo.contact.contactRequestId != null
      ) {
        ComposeContextContactRequestActionsView(
          rhId = rhId,
          contactRequestId = chat.chatInfo.contact.contactRequestId
        )
      } else if (
        chat.chatInfo is ChatInfo.Direct
        && chat.chatInfo.contact.nextAcceptContactRequest
        && chat.chatInfo.contact.groupDirectInv != null
      ) {
        ComposeContextMemberContactActionsView(
          rhId = rhId,
          contact = chat.chatInfo.contact,
          groupDirectInv = chat.chatInfo.contact.groupDirectInv
        )
      } else {
        Row(Modifier.padding(end = 8.dp), verticalAlignment = Alignment.Bottom) {
          AttachmentAndCommandsButtons()
          SendMsgView_(disableSendButton = disableSendButton)
        }
      }
    }
  }
}
