package chat.simplex.app.views.chat

import ComposeImageView
import android.content.Context
import android.graphics.Bitmap
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.AttachFile
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.views.chat.item.*
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.*
import java.io.File
import java.io.FileOutputStream

sealed class ComposePreview {
  object NoPreview: ComposePreview()
  class CLinkPreview(val linkPreview: LinkPreview): ComposePreview()
  class ImagePreview(val image: String): ComposePreview()
}

sealed class ComposeContextItem {
  object NoContextItem: ComposeContextItem()
  class QuotedItem(val chatItem: ChatItem): ComposeContextItem()
  class EditingItem(val chatItem: ChatItem): ComposeContextItem()
}

data class ComposeState(
  val message: String = "",
  val preview: ComposePreview = ComposePreview.NoPreview,
  val contextItem: ComposeContextItem = ComposeContextItem.NoContextItem,
  val inProgress: Boolean = false
) {
  constructor(editingItem: ChatItem): this(
    editingItem.content.text,
    chatItemPreview(editingItem),
    ComposeContextItem.EditingItem(editingItem)
  )

  val editing: Boolean
    get() =
      when (contextItem) {
        is ComposeContextItem.EditingItem -> true
        else -> false
      }
  val sendEnabled: Boolean
    get() =
      when (preview) {
        is ComposePreview.ImagePreview -> true
        else -> message.isNotEmpty()
      }
  val linkPreviewAllowed: Boolean
    get() =
      when (preview) {
        is ComposePreview.ImagePreview -> false
        else -> true
      }
  val linkPreview: LinkPreview?
    get() =
      when (preview) {
        is ComposePreview.CLinkPreview -> preview.linkPreview
        else -> null
      }
}

fun chatItemPreview(chatItem: ChatItem): ComposePreview {
  return when (val mc = chatItem.content.msgContent) {
    is MsgContent.MCLink -> ComposePreview.CLinkPreview(linkPreview = mc.preview)
    is MsgContent.MCImage -> ComposePreview.ImagePreview(image = mc.image)
    else -> ComposePreview.NoPreview
  }
}

@Composable
fun ComposeView(
  chatModel: ChatModel,
  chat: Chat,
  composeState: MutableState<ComposeState>,
  chosenImage: MutableState<Bitmap?>,
  showAttachmentBottomSheet: () -> Unit
) {
  val context = LocalContext.current
  val linkUrl = remember { mutableStateOf<String?>(null) }
  val prevLinkUrl = remember { mutableStateOf<String?>(null) }
  val pendingLinkUrl = remember { mutableStateOf<String?>(null) }
  val cancelledLinks = remember { mutableSetOf<String>() }
  val smallFont = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  val textStyle = remember { mutableStateOf(smallFont) }

  fun isSimplexLink(link: String): Boolean =
    link.startsWith("https://simplex.chat", true) || link.startsWith("http://simplex.chat", true)

  fun parseMessage(msg: String): String? {
    val parsedMsg = runBlocking { chatModel.controller.apiParseMarkdown(msg) }
    val link = parsedMsg?.firstOrNull { ft -> ft.format is Format.Uri && !cancelledLinks.contains(ft.text) && !isSimplexLink(ft.text) }
    return link?.text
  }

  fun loadLinkPreview(url: String, wait: Long? = null) {
    if (pendingLinkUrl.value == url) {
      withApi {
        if (wait != null) delay(wait)
        val lp = getLinkPreview(url)
        if (lp != null && pendingLinkUrl.value == url) {
          composeState.value = composeState.value.copy(preview = ComposePreview.CLinkPreview(lp))
          pendingLinkUrl.value = null
        }
      }
    }
  }

  fun showLinkPreview(s: String) {
    prevLinkUrl.value = linkUrl.value
    linkUrl.value = parseMessage(s)
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

  fun checkLinkPreview(): MsgContent {
    val cs = composeState.value
    return when (val composePreview = cs.preview) {
      is ComposePreview.CLinkPreview -> {
        val url = parseMessage(cs.message)
        val lp = composePreview.linkPreview
        if (url == lp.uri) {
          MsgContent.MCLink(cs.message, preview = lp)
        } else {
          MsgContent.MCText(cs.message)
        }
      }
      else -> MsgContent.MCText(cs.message)
    }
  }

  fun updateMsgContent(msgContent: MsgContent): MsgContent {
    val cs = composeState.value
    return when (msgContent) {
      is MsgContent.MCText -> checkLinkPreview()
      is MsgContent.MCLink -> checkLinkPreview()
      is MsgContent.MCImage -> MsgContent.MCImage(cs.message, image = msgContent.image)
      is MsgContent.MCFile -> MsgContent.MCFile(cs.message)
      is MsgContent.MCUnknown -> MsgContent.MCUnknown(type = msgContent.type, text = cs.message, json = msgContent.json)
    }
  }

  fun saveImage(context: Context, image: Bitmap): String? {
    return try {
      val dataResized = resizeImageToDataSize(image, maxDataSize = MAX_IMAGE_SIZE)
      val fileToSave = "image_${System.currentTimeMillis()}.jpg"
      val file = File(getAppFilesDirectory(context) + "/" + fileToSave)
      val output = FileOutputStream(file)
      dataResized.writeTo(output)
      output.flush()
      output.close()
      fileToSave
    } catch (e: Exception) {
      null
    }
  }

  fun sendMessage() {
    withApi {
      // show "in progress"
      val cInfo = chat.chatInfo
      val cs = composeState.value
      when (val contextItem = cs.contextItem) {
        is ComposeContextItem.EditingItem -> {
          val ei = contextItem.chatItem
          val oldMsgContent = ei.content.msgContent
          if (oldMsgContent != null) {
            val updatedItem = chatModel.controller.apiUpdateChatItem(
              type = cInfo.chatType,
              id = cInfo.apiId,
              itemId = ei.meta.itemId,
              mc = updateMsgContent(oldMsgContent)
            )
            if (updatedItem != null) chatModel.upsertChatItem(cInfo, updatedItem.chatItem)
          }
        }
        else -> {
          var mc: MsgContent? = null
          var file: String? = null
          when (val preview = cs.preview) {
            ComposePreview.NoPreview -> mc = MsgContent.MCText(cs.message)
            is ComposePreview.CLinkPreview -> mc = checkLinkPreview()
            is ComposePreview.ImagePreview -> {
              val chosenImageVal = chosenImage.value
              if (chosenImageVal != null) {
                file = saveImage(context, chosenImageVal)
                if (file != null) {
                  mc = MsgContent.MCImage(cs.message, preview.image)
                }
              }
            }
          }
          val quotedItemId: Long? = when (contextItem) {
            is ComposeContextItem.QuotedItem -> contextItem.chatItem.id
            else -> null
          }

          if (mc != null) {
            val aChatItem = chatModel.controller.apiSendMessage(
              type = cInfo.chatType,
              id = cInfo.apiId,
              file = file,
              quotedItemId = quotedItemId,
              mc = mc
            )
            if (aChatItem != null) chatModel.addChatItem(cInfo, aChatItem.chatItem)
          }
        }
      }
      // hide "in progress"
      composeState.value = ComposeState()
    }
  }

  fun onMessageChange(s: String) {
    composeState.value = composeState.value.copy(message = s)
    if (isShortEmoji(s)) {
      textStyle.value = if (s.codePoints().count() < 4) largeEmojiFont else mediumEmojiFont
    } else {
      textStyle.value = smallFont
      if (composeState.value.linkPreviewAllowed) {
        if (s.isNotEmpty()) showLinkPreview(s)
        else resetLinkPreview()
      }
    }
  }

  fun cancelLinkPreview() {
    val uri = composeState.value.linkPreview?.uri
    if (uri != null) {
      cancelledLinks.add(uri)
    }
    composeState.value = composeState.value.copy(preview = ComposePreview.NoPreview)
  }

  fun cancelImage() {
    chosenImage.value = null
    composeState.value = composeState.value.copy(preview = ComposePreview.NoPreview)
  }

  @Composable
  fun previewView() {
    when (val preview = composeState.value.preview) {
      ComposePreview.NoPreview -> {}
      is ComposePreview.CLinkPreview -> ComposeLinkView(preview.linkPreview, ::cancelLinkPreview)
      is ComposePreview.ImagePreview -> ComposeImageView(
        preview.image,
        ::cancelImage,
        cancelEnabled = !composeState.value.editing
      )
    }
  }

  @Composable
  fun contextItemView() {
    when (val contextItem = composeState.value.contextItem) {
      ComposeContextItem.NoContextItem -> {}
      is ComposeContextItem.QuotedItem -> ContextItemView(contextItem.chatItem) { composeState.value = composeState.value.copy(contextItem = ComposeContextItem.NoContextItem) }
      is ComposeContextItem.EditingItem -> ContextItemView(contextItem.chatItem) { composeState.value = ComposeState() }
    }
  }

  Column {
    contextItemView()
    previewView()
    Row(
      modifier = Modifier.padding(start = 4.dp, end = 8.dp),
      verticalAlignment = Alignment.Bottom,
      horizontalArrangement = Arrangement.spacedBy(2.dp)
    ) {
      val attachEnabled = !composeState.value.editing
      Box(Modifier.padding(bottom = 12.dp)) {
        Icon(
          Icons.Filled.AttachFile,
          contentDescription = stringResource(R.string.attach),
          tint = if (attachEnabled) MaterialTheme.colors.primary else Color.Gray,
          modifier = Modifier
            .size(28.dp)
            .clip(CircleShape)
            .clickable {
              if (attachEnabled) {
                showAttachmentBottomSheet()
              }
            }
        )
      }
      SendMsgView(
        composeState,
        sendMessage = {
          sendMessage()
          resetLinkPreview()
        },
        ::onMessageChange,
        textStyle
      )
    }
  }
}
