package chat.simplex.app.views.chat

import ComposeFileView
import ComposeImageView
import android.Manifest
import android.app.Activity
import android.content.Context
import android.content.Intent
import android.content.pm.PackageManager
import android.graphics.Bitmap
import android.graphics.ImageDecoder
import android.net.Uri
import android.provider.MediaStore
import android.util.Log
import android.widget.Toast
import androidx.activity.compose.ManagedActivityResultLauncher
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContract
import androidx.activity.result.contract.ActivityResultContracts
import androidx.annotation.CallSuper
import androidx.camera.core.ImageCapture
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.AttachFile
import androidx.compose.material.icons.filled.Edit
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.focus.onFocusChanged
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.core.content.ContextCompat
import androidx.core.content.FileProvider
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.views.chat.item.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.ActionButton
import kotlinx.coroutines.*
import java.io.File

sealed class ComposePreview {
  object NoPreview: ComposePreview()
  class CLinkPreview(val linkPreview: LinkPreview): ComposePreview()
  class ImagePreview(val image: String): ComposePreview()
  class FilePreview(val fileName: String): ComposePreview()
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
  val sendEnabled: () -> Boolean
    get() = {
      val hasContent = when (preview) {
        is ComposePreview.ImagePreview -> true
        is ComposePreview.FilePreview -> true
        else -> message.isNotEmpty()
      }
      hasContent && !inProgress
    }
  val linkPreviewAllowed: Boolean
    get() =
      when (preview) {
        is ComposePreview.ImagePreview -> false
        is ComposePreview.FilePreview -> false
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
    is MsgContent.MCText -> ComposePreview.NoPreview
    is MsgContent.MCLink -> ComposePreview.CLinkPreview(linkPreview = mc.preview)
    is MsgContent.MCImage -> ComposePreview.ImagePreview(image = mc.image)
    is MsgContent.MCFile -> {
      val fileName = chatItem.file?.fileName ?: ""
      ComposePreview.FilePreview(fileName)
    }
    else -> ComposePreview.NoPreview
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
  val context = LocalContext.current
  val linkUrl = remember { mutableStateOf<String?>(null) }
  val prevLinkUrl = remember { mutableStateOf<String?>(null) }
  val pendingLinkUrl = remember { mutableStateOf<String?>(null) }
  val cancelledLinks = remember { mutableSetOf<String>() }
  val smallFont = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  val textStyle = remember { mutableStateOf(smallFont) }
  // attachments
  val chosenImage = remember { mutableStateOf<Bitmap?>(null) }
  val chosenFile = remember { mutableStateOf<Uri?>(null) }
//  val cameraLauncher = rememberCameraLauncher { bitmap: Bitmap? ->
//    if (bitmap != null) {
//      chosenImage.value = bitmap
//      val imagePreview = resizeImageToStrSize(bitmap, maxDataSize = 14000)
//      composeState.value = composeState.value.copy(preview = ComposePreview.ImagePreview(imagePreview))
//    }
//  }
  val photoUri = remember { mutableStateOf<Uri?>(null) }
  val photoTmpFile = remember { mutableStateOf<File?>(null) }

  class CustomTakePicturePreview2: ActivityResultContract<Void?, Bitmap?>() {
//    lateinit var externalContext: Context

    @CallSuper
    override fun createIntent(context: Context, input: Void?): Intent {
//      externalContext = context
      photoTmpFile.value = File.createTempFile("image", ".bmp", SimplexApp.context.filesDir)
      photoUri.value = FileProvider.getUriForFile(context, "${BuildConfig.APPLICATION_ID}.provider", photoTmpFile.value!!)
//      Log.e(TAG, "####################################### in createIntent uri: $uri")
      return Intent(MediaStore.ACTION_IMAGE_CAPTURE)
        .putExtra(MediaStore.EXTRA_OUTPUT, photoUri.value)
    }

    override fun getSynchronousResult(
      context: Context,
      input: Void?
    ): SynchronousResult<Bitmap?>? = null

    override fun parseResult(resultCode: Int, intent: Intent?): Bitmap? {
      val photoUriVal = photoUri.value
      val photoTmpFileVal = photoTmpFile.value
      Log.e(TAG, "####################################### in parseResult resultCode: $resultCode, uri: $photoUriVal, tmpFile: $photoTmpFileVal")
      return if (resultCode == Activity.RESULT_OK && photoUriVal != null && photoTmpFileVal != null) {
        Log.e(TAG, "####################################### 1")
        val source = ImageDecoder.createSource(SimplexApp.context.contentResolver, photoUriVal)
        Log.e(TAG, "####################################### 2")
        val bitmap = ImageDecoder.decodeBitmap(source)
        Log.e(TAG, "####################################### 3")
        photoTmpFileVal.delete()
        Log.e(TAG, "####################################### 4")
        bitmap
      } else {
        Log.e(TAG, "Getting image from camera cancelled or failed.")
        photoTmpFile.value?.delete()
        null
      }
    }
  }

  val cameraLauncher = rememberLauncherForActivityResult(contract = CustomTakePicturePreview2()) { bitmap: Bitmap? ->
    if (bitmap != null) {
      chosenImage.value = bitmap
      val imagePreview = resizeImageToStrSize(bitmap, maxDataSize = 14000)
      composeState.value = composeState.value.copy(preview = ComposePreview.ImagePreview(imagePreview))
    }
  }

//  val cameraLauncher = rememberLauncherForActivityResult(
//    contract = ActivityResultContracts.TakePicturePreview()
//  ) { bitmap: Bitmap? ->
//    if (bitmap != null) {
//      chosenImage.value = bitmap
//      val imagePreview = resizeImageToStrSize(bitmap, maxDataSize = 14000)
//      composeState.value = composeState.value.copy(preview = ComposePreview.ImagePreview(imagePreview))
//    }
//  }
  val cameraPermissionLauncher = rememberPermissionLauncher { isGranted: Boolean ->
    if (isGranted) {
      cameraLauncher.launch(null)
    } else {
      Toast.makeText(context, generalGetString(R.string.toast_permission_denied), Toast.LENGTH_SHORT).show()
    }
  }
  val galleryLauncher = rememberGetContentLauncher { uri: Uri? ->
    if (uri != null) {
      val source = ImageDecoder.createSource(context.contentResolver, uri)
      val bitmap = ImageDecoder.decodeBitmap(source)
      chosenImage.value = bitmap
      val imagePreview = resizeImageToStrSize(bitmap, maxDataSize = 14000)
      composeState.value = composeState.value.copy(preview = ComposePreview.ImagePreview(imagePreview))
    }
  }
  val filesLauncher = rememberGetContentLauncher { uri: Uri? ->
    if (uri != null) {
      val fileSize = getFileSize(context, uri)
      if (fileSize != null && fileSize <= MAX_FILE_SIZE) {
        val fileName = getFileName(SimplexApp.context, uri)
        if (fileName != null) {
          chosenFile.value = uri
          composeState.value = composeState.value.copy(preview = ComposePreview.FilePreview(fileName))
        }
      } else {
        AlertManager.shared.showAlertMsg(
          generalGetString(R.string.large_file),
          String.format(generalGetString(R.string.maximum_supported_file_size), formatBytes(MAX_FILE_SIZE))
        )
      }
    }
  }

  LaunchedEffect(attachmentOption.value) {
    when (attachmentOption.value) {
      AttachmentOption.TakePhoto -> {
        when (PackageManager.PERMISSION_GRANTED) {
          ContextCompat.checkSelfPermission(context, Manifest.permission.CAMERA) -> {
            cameraLauncher.launch(null)
          }
          else -> {
            cameraPermissionLauncher.launch(Manifest.permission.CAMERA)
          }
        }
        attachmentOption.value = null
      }
      AttachmentOption.PickImage -> {
        galleryLauncher.launch("image/*")
        attachmentOption.value = null
      }
      AttachmentOption.PickFile -> {
        filesLauncher.launch("*/*")
        attachmentOption.value = null
      }
      else -> {}
    }
  }

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

  fun clearState() {
    composeState.value = ComposeState()
    textStyle.value = smallFont
    chosenImage.value = null
    chosenFile.value = null
    linkUrl.value = null
    prevLinkUrl.value = null
    pendingLinkUrl.value = null
    cancelledLinks.clear()
  }

  fun sendMessage() {
    withApi {
      composeState.value = composeState.value.copy(inProgress = true)
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
            is ComposePreview.FilePreview -> {
              val chosenFileVal = chosenFile.value
              if (chosenFileVal != null) {
                file = saveFileFromUri(context, chosenFileVal)
                if (file != null) {
                  mc = MsgContent.MCFile(cs.message)
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
      clearState()
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
    composeState.value = composeState.value.copy(preview = ComposePreview.NoPreview)
    chosenImage.value = null
  }

  fun cancelFile() {
    composeState.value = composeState.value.copy(preview = ComposePreview.NoPreview)
    chosenFile.value = null
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
      is ComposePreview.FilePreview -> ComposeFileView(
        preview.fileName,
        ::cancelFile,
        cancelEnabled = !composeState.value.editing
      )
    }
  }

  @Composable
  fun contextItemView() {
    when (val contextItem = composeState.value.contextItem) {
      ComposeContextItem.NoContextItem -> {}
      is ComposeContextItem.QuotedItem -> ContextItemView(contextItem.chatItem, Icons.Outlined.Reply) {
        composeState.value = composeState.value.copy(contextItem = ComposeContextItem.NoContextItem)
      }
      is ComposeContextItem.EditingItem -> ContextItemView(contextItem.chatItem, Icons.Filled.Edit) {
        clearState()
      }
    }
  }

  Column {
    contextItemView()
    when {
      composeState.value.editing && composeState.value.preview is ComposePreview.FilePreview -> {}
      else -> previewView()
    }
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
                showChooseAttachment()
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
