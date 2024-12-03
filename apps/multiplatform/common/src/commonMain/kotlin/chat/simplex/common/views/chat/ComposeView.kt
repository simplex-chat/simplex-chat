@file:UseSerializers(UriSerializer::class)
package chat.simplex.common.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.Saver
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.text.font.FontStyle
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.model.ChatModel.filesToDelete
import chat.simplex.common.model.ChatModel.withChats
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.item.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.serialization.*
import java.io.File
import java.net.URI
import java.nio.file.Files

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
}

@Serializable
data class LiveMessage(
  val chatItem: ChatItem,
  val typedMsg: String,
  val sentMsg: String,
  val sent: Boolean
)

@Serializable
data class ComposeState(
  val message: String = "",
  val liveMessage: LiveMessage? = null,
  val preview: ComposePreview = ComposePreview.NoPreview,
  val contextItem: ComposeContextItem = ComposeContextItem.NoContextItem,
  val inProgress: Boolean = false,
  val useLinkPreviews: Boolean
) {
  constructor(editingItem: ChatItem, liveMessage: LiveMessage? = null, useLinkPreviews: Boolean): this(
    editingItem.content.text,
    liveMessage,
    chatItemPreview(editingItem),
    ComposeContextItem.EditingItem(editingItem),
    useLinkPreviews = useLinkPreviews
  )

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
  val sendEnabled: () -> Boolean
    get() = {
      val hasContent = when (preview) {
        is ComposePreview.MediaPreview -> true
        is ComposePreview.VoicePreview -> true
        is ComposePreview.FilePreview -> true
        else -> message.isNotEmpty() || forwarding || liveMessage != null
      }
      hasContent && !inProgress
    }
  val endLiveDisabled: Boolean
    get() = liveMessage != null && message.isEmpty() && preview is ComposePreview.NoPreview && contextItem is ComposeContextItem.NoContextItem

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
      if (editing || forwarding || liveMessage != null || inProgress) return true
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

  val empty: Boolean
    get() = message.isEmpty() && preview is ComposePreview.NoPreview && contextItem is ComposeContextItem.NoContextItem

  companion object {
    fun saver(): Saver<MutableState<ComposeState>, *> = Saver(
      save = { json.encodeToString(serializer(), it.value) },
      restore = {
        mutableStateOf(json.decodeFromString(it))
      }
    )
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
        value = value.copy(message = text ?: value.message, preview = ComposePreview.FilePreview(fileName, uri))
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
    value = value.copy(message = text ?: value.message, preview = ComposePreview.MediaPreview(imagesPreview, content))
  }
}

@Composable
fun ComposeView(
  chatModel: ChatModel,
  chat: Chat,
  composeState: MutableState<ComposeState>,
  attachmentOption: MutableState<AttachmentOption?>,
  showChooseAttachment: () -> Unit
) {
  val cancelledLinks = rememberSaveable { mutableSetOf<String>() }
  fun isSimplexLink(link: String): Boolean =
    link.startsWith("https://simplex.chat", true) || link.startsWith("http://simplex.chat", true)

  fun parseMessage(msg: String): Pair<String?, Boolean> {
    if (msg.isBlank()) return null to false
    val parsedMsg = parseToMarkdown(msg) ?: return null to false
    val link = parsedMsg.firstOrNull { ft -> ft.format is Format.Uri && !cancelledLinks.contains(ft.text) && !isSimplexLink(ft.text) }
    val simplexLink = parsedMsg.any { ft -> ft.format is Format.SimplexLink }
    return link?.text to simplexLink
  }

  val linkUrl = rememberSaveable { mutableStateOf<String?>(null) }
  // default value parsed because of draft
  val hasSimplexLink = rememberSaveable { mutableStateOf(parseMessage(composeState.value.message).second) }
  val prevLinkUrl = rememberSaveable { mutableStateOf<String?>(null) }
  val pendingLinkUrl = rememberSaveable { mutableStateOf<String?>(null) }
  val useLinkPreviews = chatModel.controller.appPrefs.privacyLinkPreviews.get()
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
          composeState.value = composeState.value.copy(preview = ComposePreview.CLinkPreview(lp))
          pendingLinkUrl.value = null
        } else if (pendingLinkUrl.value == url) {
          composeState.value = composeState.value.copy(preview = ComposePreview.NoPreview)
          pendingLinkUrl.value = null
        }
      }
    }
  }

  fun showLinkPreview(s: String) {
    prevLinkUrl.value = linkUrl.value
    val parsed = parseMessage(s)
    linkUrl.value = parsed.first
    hasSimplexLink.value = parsed.second
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
      composeState.value = composeState.value.copy(inProgress = false)
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

  suspend fun send(chat: Chat, mc: MsgContent, quoted: Long?, file: CryptoFile? = null, live: Boolean = false, ttl: Int?): ChatItem? {
    val cInfo = chat.chatInfo
    val chatItems = if (chat.chatInfo.chatType == ChatType.Local)
      chatModel.controller.apiCreateChatItems(
        rh = chat.remoteHostId,
        noteFolderId = chat.chatInfo.apiId,
        composedMessages = listOf(ComposedMessage(file, null, mc))
      )
    else
      chatModel.controller.apiSendMessages(
        rh = chat.remoteHostId,
        type = cInfo.chatType,
        id = cInfo.apiId,
        live = live,
        ttl = ttl,
        composedMessages = listOf(ComposedMessage(file, quoted, mc))
      )
    if (!chatItems.isNullOrEmpty()) {
      chatItems.forEach { aChatItem ->
        withChats {
          addChatItem(chat.remoteHostId, cInfo, aChatItem.chatItem)
        }
      }
      return chatItems.first().chatItem
    }
    if (file != null) removeFile(file.filePath)
    return null
  }

  suspend fun sendMessageAsync(text: String?, live: Boolean, ttl: Int?): List<ChatItem>? {
    val cInfo = chat.chatInfo
    val cs = composeState.value
    var sent: List<ChatItem>?
    val msgText = text ?: cs.message

    fun sending() {
      composeState.value = composeState.value.copy(inProgress = true)
    }

    suspend fun forwardItem(rhId: Long?, forwardedItem: List<ChatItem>, fromChatInfo: ChatInfo, ttl: Int?): List<ChatItem>? {
      val chatItems = controller.apiForwardChatItems(
        rh = rhId,
        toChatType = chat.chatInfo.chatType,
        toChatId = chat.chatInfo.apiId,
        fromChatType = fromChatInfo.chatType,
        fromChatId = fromChatInfo.apiId,
        itemIds = forwardedItem.map { it.id },
        ttl = ttl
      )

      chatItems?.forEach { chatItem ->
        withChats {
          addChatItem(rhId, chat.chatInfo, chatItem)
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

    fun checkLinkPreview(): MsgContent {
      return when (val composePreview = cs.preview) {
        is ComposePreview.CLinkPreview -> {
          val url = parseMessage(msgText).first
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

    fun updateMsgContent(msgContent: MsgContent): MsgContent {
      return when (msgContent) {
        is MsgContent.MCText -> checkLinkPreview()
        is MsgContent.MCLink -> checkLinkPreview()
        is MsgContent.MCImage -> MsgContent.MCImage(msgText, image = msgContent.image)
        is MsgContent.MCVideo -> MsgContent.MCVideo(msgText, image = msgContent.image, duration = msgContent.duration)
        is MsgContent.MCVoice -> MsgContent.MCVoice(msgText, duration = msgContent.duration)
        is MsgContent.MCFile -> MsgContent.MCFile(msgText)
        is MsgContent.MCUnknown -> MsgContent.MCUnknown(type = msgContent.type, text = msgText, json = msgContent.json)
      }
    }

    suspend fun sendMemberContactInvitation() {
      val mc = checkLinkPreview()
      val contact = chatModel.controller.apiSendMemberContactInvitation(chat.remoteHostId, chat.chatInfo.apiId, mc)
      if (contact != null) {
        withChats {
          updateContact(chat.remoteHostId, contact)
        }
      }
    }

    suspend fun updateMessage(ei: ChatItem, chat: Chat, live: Boolean): ChatItem? {
      val cInfo = chat.chatInfo
      val oldMsgContent = ei.content.msgContent
      if (oldMsgContent != null) {
        val updatedItem = chatModel.controller.apiUpdateChatItem(
          rh = chat.remoteHostId,
          type = cInfo.chatType,
          id = cInfo.apiId,
          itemId = ei.meta.itemId,
          mc = updateMsgContent(oldMsgContent),
          live = live
        )
        if (updatedItem != null) withChats {
          upsertChatItem(chat.remoteHostId, cInfo, updatedItem.chatItem)
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

    if (chat.nextSendGrpInv) {
      sendMemberContactInvitation()
      sent = null
    } else if (cs.contextItem is ComposeContextItem.ForwardingItems) {
      sent = forwardItem(chat.remoteHostId, cs.contextItem.chatItems, cs.contextItem.fromChatInfo, ttl = ttl)
      if (cs.message.isNotEmpty()) {
        sent?.mapIndexed { index, message ->
          if (index == sent!!.lastIndex) {
            send(chat, checkLinkPreview(), quoted = message.id, live = false, ttl = ttl)
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
    } else if (liveMessage != null && liveMessage.sent) {
      val updatedMessage = updateMessage(liveMessage.chatItem, chat, live)
      sent = if (updatedMessage != null) listOf(updatedMessage) else null
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
                if (remoteHost == null) saveFileFromUri(it.uri)
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
          ttl = ttl
        )
        sent = if (sendResult != null) listOf(sendResult) else null
      }
      if (sent == null &&
        (cs.preview is ComposePreview.MediaPreview ||
            cs.preview is ComposePreview.FilePreview ||
            cs.preview is ComposePreview.VoicePreview)
      ) {
        val sendResult = send(chat, MsgContent.MCText(msgText), quotedItemId, null, live, ttl)
        sent = if (sendResult != null) listOf(sendResult) else null
      }
    }
    val wasForwarding = cs.forwarding
    val forwardingFromChatId = (cs.contextItem as? ComposeContextItem.ForwardingItems)?.fromChatInfo?.id
    clearState(live)
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

  fun onMessageChange(s: String) {
    composeState.value = composeState.value.copy(message = s)
    if (isShortEmoji(s)) {
      textStyle.value = if (s.codePoints().count() < 4) largeEmojiFont else mediumEmojiFont
    } else {
      textStyle.value = smallFont
      if (composeState.value.linkPreviewAllowed) {
        if (s.isNotEmpty()) {
          showLinkPreview(s)
        } else {
          resetLinkPreview()
          hasSimplexLink.value = false
        }
      } else if (s.isNotEmpty() && !chat.groupFeatureEnabled(GroupFeature.SimplexLinks)) {
        hasSimplexLink.value = parseMessage(s).second
      } else {
        hasSimplexLink.value = false
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
    val typedMsg = cs.message
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
      val sentMsg = liveMessageToSend(liveMessage, typedMsg)
      if (sentMsg != null) {
        val ci = sendMessageAsync(sentMsg, live = true, ttl = null)
        if (!ci.isNullOrEmpty()) {
          composeState.value = composeState.value.copy(liveMessage = LiveMessage(ci.last(), typedMsg = typedMsg, sentMsg = sentMsg, sent = true))
        }
      } else if (liveMessage.typedMsg != typedMsg) {
        composeState.value = composeState.value.copy(liveMessage = liveMessage.copy(typedMsg = typedMsg))
      }
    }
  }

  fun editPrevMessage() {
    if (composeState.value.contextItem != ComposeContextItem.NoContextItem || composeState.value.preview != ComposePreview.NoPreview) return
    val lastEditable = chatModel.chatItems.value.findLast { it.meta.editable }
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
    val color = MaterialTheme.appColors.receivedMessage
    Row(Modifier.padding(top = 5.dp).fillMaxWidth().background(color).padding(horizontal = DEFAULT_PADDING_HALF, vertical = DEFAULT_PADDING_HALF * 1.5f), verticalAlignment = Alignment.CenterVertically) {
      Icon(icon, null, tint = MaterialTheme.colors.secondary)
      Spacer(Modifier.width(DEFAULT_PADDING_HALF))
      Text(reason, fontStyle = FontStyle.Italic)
    }
  }

  @Composable
  fun contextItemView() {
    when (val contextItem = composeState.value.contextItem) {
      ComposeContextItem.NoContextItem -> {}
      is ComposeContextItem.QuotedItem -> ContextItemView(listOf(contextItem.chatItem), painterResource(MR.images.ic_reply), chatType = chat.chatInfo.chatType) {
        composeState.value = composeState.value.copy(contextItem = ComposeContextItem.NoContextItem)
      }
      is ComposeContextItem.EditingItem -> ContextItemView(listOf(contextItem.chatItem), painterResource(MR.images.ic_edit_filled),  chatType = chat.chatInfo.chatType) {
        clearState()
      }
      is ComposeContextItem.ForwardingItems -> ContextItemView(contextItem.chatItems, painterResource(MR.images.ic_forward), showSender = false,  chatType = chat.chatInfo.chatType) {
        composeState.value = composeState.value.copy(contextItem = ComposeContextItem.NoContextItem)
      }
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
      is SharedContent.Text -> onMessageChange(shared.text)
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

  val userCanSend = rememberUpdatedState(chat.chatInfo.userCanSend)
  val sendMsgEnabled = rememberUpdatedState(chat.chatInfo.sendMsgEnabled)
  val userIsObserver = rememberUpdatedState(chat.userIsObserver)
  val nextSendGrpInv = rememberUpdatedState(chat.nextSendGrpInv)

  Column {
    if (nextSendGrpInv.value) {
      ComposeContextInvitingContactMemberView()
    }
    val simplexLinkProhibited = hasSimplexLink.value && !chat.groupFeatureEnabled(GroupFeature.SimplexLinks)
    val fileProhibited = composeState.value.attachmentPreview && !chat.groupFeatureEnabled(GroupFeature.Files)
    val voiceProhibited = composeState.value.preview is ComposePreview.VoicePreview && !chat.chatInfo.featureEnabled(ChatFeature.Voice)
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
    Column(Modifier.background(MaterialTheme.colors.background)) {
    Divider()
    Row(Modifier.padding(end = 8.dp), verticalAlignment = Alignment.Bottom) {
      val isGroupAndProhibitedFiles = chat.chatInfo is ChatInfo.Group && !chat.chatInfo.groupInfo.fullGroupPreferences.files.on(chat.chatInfo.groupInfo.membership)
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
            && userCanSend.value
            && !isGroupAndProhibitedFiles
            && !nextSendGrpInv.value
      IconButton(
        attachmentClicked,
        Modifier.padding(bottom = if (appPlatform.isAndroid) 2.sp.toDp() else 5.sp.toDp() * fontSizeSqrtMultiplier),
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

      LaunchedEffect(rememberUpdatedState(chat.chatInfo.userCanSend).value) {
        if (!chat.chatInfo.userCanSend) {
          clearCurrentDraft()
          clearState()
        }
      }

      KeyChangeEffect(chatModel.chatId.value) { prevChatId ->
        val cs = composeState.value
        if (cs.liveMessage != null && (cs.message.isNotEmpty() || cs.liveMessage.sent)) {
          sendMessage(null)
          resetLinkPreview()
          clearPrevDraft(prevChatId)
          deleteUnusedFiles()
        } else if (cs.inProgress) {
          clearPrevDraft(prevChatId)
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
        sendButtonEnabled = sendMsgEnabled.value && !(simplexLinkProhibited || fileProhibited || voiceProhibited),
        nextSendGrpInv = nextSendGrpInv.value,
        needToAllowVoiceToContact,
        allowedVoiceByPrefs,
        allowVoiceToContact = ::allowVoiceToContact,
        userIsObserver = userIsObserver.value,
        userCanSend = userCanSend.value,
        sendButtonColor = sendButtonColor,
        timedMessageAllowed = timedMessageAllowed,
        customDisappearingMessageTimePref = chatModel.controller.appPrefs.customDisappearingMessageTime,
        placeholder = stringResource(MR.strings.compose_message_placeholder),
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
        onFilesPasted = { composeState.onFilesAttached(it) },
        onMessageChange = ::onMessageChange,
        textStyle = textStyle
      )
    }
  }
  }
}
