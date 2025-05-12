package chat.simplex.common.views.chat.item

import SectionItemView
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CornerSize
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import java.io.File
import java.net.URI

@Composable
fun CIFileView(
  file: CIFile?,
  edited: Boolean,
  showMenu: MutableState<Boolean>,
  smallView: Boolean = false,
  receiveFile: (Long) -> Unit
) {
  val saveFileLauncher = rememberSaveFileLauncher(ciFile = file)
  val sizeMultiplier = 1f
  val progressSizeMultiplier = if (smallView) 0.7f else 1f
  @Composable
  fun fileIcon(
    innerIcon: Painter? = null,
    color: Color = if (isInDarkTheme()) FileDark else FileLight,
    topPadding: Dp = 12.sp.toDp()
  ) {
    Box(
      contentAlignment = Alignment.Center
    ) {
      Icon(
        painterResource(MR.images.ic_draft_filled),
        stringResource(MR.strings.icon_descr_file),
        Modifier.fillMaxSize(),
        tint = color
      )
      if (innerIcon != null) {
        Icon(
          innerIcon,
          stringResource(MR.strings.icon_descr_file),
          Modifier
            .padding(top = topPadding * sizeMultiplier)
            .height(20.sp.toDp() * sizeMultiplier)
            .width(32.sp.toDp() * sizeMultiplier),
          tint = Color.White
        )
      }
    }
  }

  fun fileAction() {
    if (file != null) {
      when {
        file.fileStatus is CIFileStatus.RcvInvitation || file.fileStatus is CIFileStatus.RcvAborted -> {
          if (fileSizeValid(file)) {
            receiveFile(file.fileId)
          } else {
            AlertManager.shared.showAlertMsg(
              generalGetString(MR.strings.large_file),
              String.format(generalGetString(MR.strings.contact_sent_large_file), formatBytes(getMaxFileSize(file.fileProtocol)))
            )
          }
        }
        file.fileStatus is CIFileStatus.RcvAccepted ->
          when (file.fileProtocol) {
            FileProtocol.XFTP ->
              AlertManager.shared.showAlertMsg(
                generalGetString(MR.strings.waiting_for_file),
                generalGetString(MR.strings.file_will_be_received_when_contact_completes_uploading)
              )
            FileProtocol.SMP ->
              AlertManager.shared.showAlertMsg(
                generalGetString(MR.strings.waiting_for_file),
                generalGetString(MR.strings.file_will_be_received_when_contact_is_online)
              )
            FileProtocol.LOCAL -> {}
          }
        file.fileStatus is CIFileStatus.RcvError ->
          showFileErrorAlert(file.fileStatus.rcvFileError)
        file.fileStatus is CIFileStatus.RcvWarning ->
          showFileErrorAlert(file.fileStatus.rcvFileError, temporary = true)
        file.fileStatus is CIFileStatus.SndError ->
          showFileErrorAlert(file.fileStatus.sndFileError)
        file.fileStatus is CIFileStatus.SndWarning ->
          showFileErrorAlert(file.fileStatus.sndFileError, temporary = true)
        file.forwardingAllowed() -> {
          withLongRunningApi(slow = 600_000) {
            var filePath = getLoadedFilePath(file)
            if (chatModel.connectedToRemote() && filePath == null) {
              file.loadRemoteFile(true)
              filePath = getLoadedFilePath(file)
            }
            if (filePath != null) {
              withLongRunningApi {
                saveFileLauncher.launch(file.fileName)
              }
            } else {
              showToast(generalGetString(MR.strings.file_not_found))
            }
          }
        }
        else -> {}
      }
    }
  }

  @Composable
  fun fileIndicator() {
    Box(
      Modifier
        .size(42.sp.toDp() * sizeMultiplier)
        .clip(RoundedCornerShape(4.sp.toDp() * sizeMultiplier)),
      contentAlignment = Alignment.Center
    ) {
      if (file != null) {
        when (file.fileStatus) {
          is CIFileStatus.SndStored ->
            when (file.fileProtocol) {
              FileProtocol.XFTP -> CIFileViewScope.progressIndicator(progressSizeMultiplier)
              FileProtocol.SMP -> fileIcon()
              FileProtocol.LOCAL -> fileIcon()
            }
          is CIFileStatus.SndTransfer ->
            when (file.fileProtocol) {
              FileProtocol.XFTP -> CIFileViewScope.progressCircle(file.fileStatus.sndProgress, file.fileStatus.sndTotal, progressSizeMultiplier)
              FileProtocol.SMP -> CIFileViewScope.progressIndicator(progressSizeMultiplier)
              FileProtocol.LOCAL -> {}
            }
          is CIFileStatus.SndComplete -> fileIcon(innerIcon = if (!smallView) painterResource(MR.images.ic_check_filled) else null)
          is CIFileStatus.SndCancelled -> fileIcon(innerIcon = painterResource(MR.images.ic_close))
          is CIFileStatus.SndError -> fileIcon(innerIcon = painterResource(MR.images.ic_close))
          is CIFileStatus.SndWarning -> fileIcon(innerIcon = painterResource(MR.images.ic_warning_filled))
          is CIFileStatus.RcvInvitation ->
            if (fileSizeValid(file))
              fileIcon(innerIcon = painterResource(MR.images.ic_arrow_downward), color = MaterialTheme.colors.primary, topPadding = 10.sp.toDp())
            else
              fileIcon(innerIcon = painterResource(MR.images.ic_priority_high), color = WarningOrange)
          is CIFileStatus.RcvAccepted -> fileIcon(innerIcon = painterResource(MR.images.ic_more_horiz))
          is CIFileStatus.RcvTransfer ->
            if (file.fileProtocol == FileProtocol.XFTP && file.fileStatus.rcvProgress < file.fileStatus.rcvTotal) {
              CIFileViewScope.progressCircle(file.fileStatus.rcvProgress, file.fileStatus.rcvTotal, progressSizeMultiplier)
            } else {
              CIFileViewScope.progressIndicator(progressSizeMultiplier)
            }
          is CIFileStatus.RcvAborted ->
            fileIcon(innerIcon = painterResource(MR.images.ic_sync_problem), color = MaterialTheme.colors.primary)
          is CIFileStatus.RcvComplete -> fileIcon()
          is CIFileStatus.RcvCancelled -> fileIcon(innerIcon = painterResource(MR.images.ic_close))
          is CIFileStatus.RcvError -> fileIcon(innerIcon = painterResource(MR.images.ic_close))
          is CIFileStatus.RcvWarning -> fileIcon(innerIcon = painterResource(MR.images.ic_warning_filled))
          is CIFileStatus.Invalid -> fileIcon(innerIcon = painterResource(MR.images.ic_question_mark))
        }
      } else {
        fileIcon()
      }
    }
  }

  val showOpenSaveMenu = rememberSaveable(file?.fileId) { mutableStateOf(false) }
  val ext = file?.fileSource?.filePath?.substringAfterLast(".")?.takeIf { it.isNotBlank() }
  val loadedFilePath = if (appPlatform.isAndroid && file?.fileSource != null) getLoadedFilePath(file) else null
  if (loadedFilePath != null && file?.fileSource != null) {
    val encrypted = file.fileSource.cryptoArgs != null
    SaveOrOpenFileMenu(showOpenSaveMenu, encrypted, ext, File(loadedFilePath).toURI(), file.fileSource, saveFile = { fileAction() })
  }
  Row(
    Modifier
      .combinedClickable(
        onClick = {
          if (appPlatform.isAndroid && loadedFilePath != null) {
            showOpenSaveMenu.value = true
          } else {
            fileAction()
          }
        },
        onLongClick = { showMenu.value = true }
      )
      .padding(if (smallView) PaddingValues() else PaddingValues(top = 4.sp.toDp(), bottom = 6.sp.toDp(), start = 6.sp.toDp(), end = 12.sp.toDp())),
    verticalAlignment = Alignment.Bottom,
    horizontalArrangement = Arrangement.spacedBy(2.sp.toDp())
  ) {
    fileIndicator()
    if (!smallView) {
      val metaReserve = if (edited)
        "                       "
      else
        "                   "
      if (file != null) {
        Column {
          Text(
            file.fileName,
            maxLines = 1
          )
          Text(
            formatBytes(file.fileSize) + metaReserve,
            color = MaterialTheme.colors.secondary,
            fontSize = 14.sp,
            maxLines = 1
          )
        }
      } else {
        Text(metaReserve)
      }
    }
  }
}

fun fileSizeValid(file: CIFile): Boolean = file.fileSize <= getMaxFileSize(file.fileProtocol)

fun showFileErrorAlert(err: FileError, temporary: Boolean = false) {
  val title: String = generalGetString(if (temporary) MR.strings.temporary_file_error else MR.strings.file_error)
  val btn = err.moreInfoButton
  if (btn != null) {
    showContentBlockedAlert(title, err.errorInfo)
  } else {
    AlertManager.shared.showAlertMsg(title, err.errorInfo)
  }
}

val contentModerationPostLink = "https://simplex.chat/blog/20250114-simplex-network-large-groups-privacy-preserving-content-moderation.html#preventing-server-abuse-without-compromising-e2e-encryption"

fun showContentBlockedAlert(title: String, message: String) {
  AlertManager.shared.showAlertDialogButtonsColumn(title, text = message, buttons = {
    val uriHandler = LocalUriHandler.current
    Column {
      SectionItemView({
        AlertManager.shared.hideAlert()
        uriHandler.openUriCatching(contentModerationPostLink)
      }) {
        Text(generalGetString(MR.strings.how_it_works), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
      }
      SectionItemView({
        AlertManager.shared.hideAlert()
      }) {
        Text(generalGetString(MR.strings.ok), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
      }
    }
  })
}

@Composable
expect fun SaveOrOpenFileMenu(
  showMenu: MutableState<Boolean>,
  encrypted: Boolean,
  ext: String?,
  encryptedUri: URI,
  fileSource: CryptoFile,
  saveFile: () -> Unit
)

@Composable
fun rememberSaveFileLauncher(ciFile: CIFile?): FileChooserLauncher =
  rememberFileChooserLauncher(false, ciFile) { to: URI? ->
    val filePath = getLoadedFilePath(ciFile)
    if (filePath != null && to != null) {
      if (ciFile?.fileSource?.cryptoArgs != null) {
        createTmpFileAndDelete { tmpFile ->
          try {
            decryptCryptoFile(filePath, ciFile.fileSource.cryptoArgs, tmpFile.absolutePath)
          } catch (e: Exception) {
            Log.e(TAG, "Unable to decrypt crypto file: " + e.stackTraceToString())
            AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.error), text = e.stackTraceToString())
            tmpFile.delete()
            return@createTmpFileAndDelete
          }
          copyFileToFile(tmpFile, to) {}
          tmpFile.delete()
        }
      } else {
        copyFileToFile(File(filePath), to) {}
      }
    }
  }

object CIFileViewScope {
  @Composable
  fun progressIndicator(sizeMultiplier: Float = 1f) {
    CircularProgressIndicator(
      Modifier.size(32.sp.toDp() * sizeMultiplier),
      color = if (isInDarkTheme()) FileDark else FileLight,
      strokeWidth = 3.sp.toDp() * sizeMultiplier
    )
  }

  @Composable
  fun progressCircle(progress: Long, total: Long, sizeMultiplier: Float = 1f) {
    val angle = 360f * (progress.toDouble() / total.toDouble()).toFloat()
    val strokeWidth = with(LocalDensity.current) { 3.sp.toPx() }
    val strokeColor = if (isInDarkTheme()) FileDark else FileLight
    Surface(
      Modifier.drawRingModifier(angle, strokeColor, strokeWidth),
      color = Color.Transparent,
      shape = MaterialTheme.shapes.small.copy(CornerSize(percent = 50)),
      contentColor = LocalContentColor.current
    ) {
      Box(Modifier.size(32.sp.toDp() * sizeMultiplier))
    }
  }
}

/*
class ChatItemProvider: PreviewParameterProvider<ChatItem> {
  private val sentFile = ChatItem(
    chatDir = CIDirection.DirectSnd(),
    meta = CIMeta.getSample(1, Clock.System.now(), "", CIStatus.SndSent(), itemEdited = true),
    content = CIContent.SndMsgContent(msgContent = MsgContent.MCFile("")),
    quotedItem = null,
    reactions = listOf(),
    file = CIFile.getSample(fileStatus = CIFileStatus.SndComplete)
  )
  private val fileChatItemWtFile = ChatItem(
    chatDir = CIDirection.DirectRcv(),
    meta = CIMeta.getSample(1, Clock.System.now(), "", CIStatus.RcvRead(), ),
    content = CIContent.RcvMsgContent(msgContent = MsgContent.MCFile("")),
    quotedItem = null,
    reactions = listOf(),
    file = null
  )
  override val values = listOf(
    sentFile,
    ChatItem.getFileMsgContentSample(),
    ChatItem.getFileMsgContentSample(fileName = "some_long_file_name_here", fileStatus = CIFileStatus.RcvInvitation),
    ChatItem.getFileMsgContentSample(fileStatus = CIFileStatus.RcvAccepted),
    ChatItem.getFileMsgContentSample(fileStatus = CIFileStatus.RcvTransfer(rcvProgress = 7, rcvTotal = 10)),
    ChatItem.getFileMsgContentSample(fileStatus = CIFileStatus.RcvCancelled),
    ChatItem.getFileMsgContentSample(fileSize = 1_000_000_000, fileStatus = CIFileStatus.RcvInvitation),
    ChatItem.getFileMsgContentSample(text = "Hello there", fileStatus = CIFileStatus.RcvInvitation),
    ChatItem.getFileMsgContentSample(text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.", fileStatus = CIFileStatus.RcvInvitation),
    fileChatItemWtFile
  ).asSequence()
}

@Preview
@Composable
fun PreviewCIFileFramedItemView(@PreviewParameter(ChatItemProvider::class) chatItem: ChatItem) {
  val showMenu = remember { mutableStateOf(false) }
  SimpleXTheme {
    FramedItemView(ChatInfo.Direct.sampleData, chatItem, linkMode = SimplexLinkMode.DESCRIPTION, showMenu = showMenu, receiveFile = {})
  }
}*/
