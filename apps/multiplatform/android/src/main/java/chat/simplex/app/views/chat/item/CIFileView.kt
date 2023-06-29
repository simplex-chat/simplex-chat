package chat.simplex.app.views.chat.item

import android.widget.Toast
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CornerSize
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import kotlinx.datetime.Clock

@Composable
fun CIFileView(
  file: CIFile?,
  edited: Boolean,
  receiveFile: (Long) -> Unit
) {
  val context = LocalContext.current
  val saveFileLauncher = rememberSaveFileLauncher(cxt = context, ciFile = file)

  @Composable
  fun fileIcon(
    innerIcon: Painter? = null,
    color: Color = if (isInDarkTheme()) FileDark else FileLight
  ) {
    Box(
      contentAlignment = Alignment.Center
    ) {
      Icon(
        painterResource(R.drawable.ic_draft_filled),
        stringResource(R.string.icon_descr_file),
        Modifier.fillMaxSize(),
        tint = color
      )
      if (innerIcon != null) {
        Icon(
          innerIcon,
          stringResource(R.string.icon_descr_file),
          Modifier
            .size(32.dp)
            .padding(top = 12.dp),
          tint = Color.White
        )
      }
    }
  }

  fun fileSizeValid(): Boolean {
    if (file != null) {
      return file.fileSize <= getMaxFileSize(file.fileProtocol)
    }
    return false
  }

  fun fileAction() {
    if (file != null) {
      when (file.fileStatus) {
        is CIFileStatus.RcvInvitation -> {
          if (fileSizeValid()) {
            receiveFile(file.fileId)
          } else {
            AlertManager.shared.showAlertMsg(
              generalGetString(R.string.large_file),
              String.format(generalGetString(R.string.contact_sent_large_file), formatBytes(getMaxFileSize(file.fileProtocol)))
            )
          }
        }
        is CIFileStatus.RcvAccepted ->
          when (file.fileProtocol) {
            FileProtocol.XFTP ->
              AlertManager.shared.showAlertMsg(
                generalGetString(R.string.waiting_for_file),
                generalGetString(R.string.file_will_be_received_when_contact_completes_uploading)
              )
            FileProtocol.SMP ->
              AlertManager.shared.showAlertMsg(
                generalGetString(R.string.waiting_for_file),
                generalGetString(R.string.file_will_be_received_when_contact_is_online)
              )
          }
        is CIFileStatus.RcvComplete -> {
          val filePath = getLoadedFilePath(context, file)
          if (filePath != null) {
            saveFileLauncher.launch(file.fileName)
          } else {
            Toast.makeText(context, generalGetString(R.string.file_not_found), Toast.LENGTH_SHORT).show()
          }
        }
        else -> {}
      }
    }
  }

  @Composable
  fun progressIndicator() {
    CircularProgressIndicator(
      Modifier.size(32.dp),
      color = if (isInDarkTheme()) FileDark else FileLight,
      strokeWidth = 3.dp
    )
  }

  @Composable
  fun progressCircle(progress: Long, total: Long) {
    val angle = 360f * (progress.toDouble() / total.toDouble()).toFloat()
    val strokeWidth = with(LocalDensity.current) { 3.dp.toPx() }
    val strokeColor = if (isInDarkTheme()) FileDark else FileLight
    Surface(
      Modifier.drawRingModifier(angle, strokeColor, strokeWidth),
      color = Color.Transparent,
      shape = MaterialTheme.shapes.small.copy(CornerSize(percent = 50))
    ) {
      Box(Modifier.size(32.dp))
    }
  }

  @Composable
  fun fileIndicator() {
    Box(
      Modifier
        .size(42.dp)
        .clip(RoundedCornerShape(4.dp))
        .clickable(onClick = { fileAction() }),
      contentAlignment = Alignment.Center
    ) {
      if (file != null) {
        when (file.fileStatus) {
          is CIFileStatus.SndStored ->
            when (file.fileProtocol) {
              FileProtocol.XFTP -> progressIndicator()
              FileProtocol.SMP -> fileIcon()
            }
          is CIFileStatus.SndTransfer ->
            when (file.fileProtocol) {
              FileProtocol.XFTP -> progressCircle(file.fileStatus.sndProgress, file.fileStatus.sndTotal)
              FileProtocol.SMP -> progressIndicator()
            }
          is CIFileStatus.SndComplete -> fileIcon(innerIcon = painterResource(R.drawable.ic_check_filled))
          is CIFileStatus.SndCancelled -> fileIcon(innerIcon = painterResource(R.drawable.ic_close))
          is CIFileStatus.SndError -> fileIcon(innerIcon = painterResource(R.drawable.ic_close))
          is CIFileStatus.RcvInvitation ->
            if (fileSizeValid())
              fileIcon(innerIcon = painterResource(R.drawable.ic_arrow_downward), color = MaterialTheme.colors.primary)
            else
              fileIcon(innerIcon = painterResource(R.drawable.ic_priority_high), color = WarningOrange)
          is CIFileStatus.RcvAccepted -> fileIcon(innerIcon = painterResource(R.drawable.ic_more_horiz))
          is CIFileStatus.RcvTransfer ->
            if (file.fileProtocol == FileProtocol.XFTP && file.fileStatus.rcvProgress < file.fileStatus.rcvTotal) {
              progressCircle(file.fileStatus.rcvProgress, file.fileStatus.rcvTotal)
            } else {
              progressIndicator()
            }
          is CIFileStatus.RcvComplete -> fileIcon()
          is CIFileStatus.RcvCancelled -> fileIcon(innerIcon = painterResource(R.drawable.ic_close))
          is CIFileStatus.RcvError -> fileIcon(innerIcon = painterResource(R.drawable.ic_close))
        }
      } else {
        fileIcon()
      }
    }
  }

  Row(
    Modifier.padding(top = 4.dp, bottom = 6.dp, start = 6.dp, end = 12.dp),
    verticalAlignment = Alignment.Bottom,
    horizontalArrangement = Arrangement.spacedBy(2.dp)
  ) {
    fileIndicator()
    val metaReserve = if (edited)
      "                     "
    else
      "                 "
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
}
