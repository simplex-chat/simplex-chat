package chat.simplex.common.views.chat.item

import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.size
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.ui.Modifier
import androidx.compose.foundation.layout.padding
import androidx.compose.ui.platform.ClipboardManager
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.EmojiFont
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import java.io.File

@Composable
actual fun ReactionIcon(text: String, fontSize: TextUnit) {
  if (desktopPlatform.isMac() && isHeartEmoji(text)) {
    val sp = with(LocalDensity.current) { (fontSize.value + 8).sp.toDp() }
    Image(painterResource(MR.images.ic_heart), null, Modifier.size(sp).padding(top = 4.dp, bottom = 2.dp))
  } else {
    Text(text, fontSize = fontSize, fontFamily = EmojiFont)
  }
}

@Composable
actual fun SaveContentItemAction(cItem: ChatItem, saveFileLauncher: FileChooserLauncher, showMenu: MutableState<Boolean>) {
  ItemAction(stringResource(MR.strings.save_verb), painterResource(if (cItem.file?.fileSource?.cryptoArgs == null) MR.images.ic_download else MR.images.ic_lock_open_right), onClick = {
    val saveIfExists = {
      when (cItem.content.msgContent) {
        is MsgContent.MCImage, is MsgContent.MCFile, is MsgContent.MCVoice, is MsgContent.MCVideo -> withLongRunningApi { saveFileLauncher.launch(cItem.file?.fileName ?: "") }
        else -> {}
      }
      showMenu.value = false
    }
    var fileSource = getLoadedFileSource(cItem.file)
    if (chatModel.connectedToRemote() && fileSource == null) {
      withLongRunningApi(slow = 600_000) {
        cItem.file?.loadRemoteFile(true)
        fileSource = getLoadedFileSource(cItem.file)
        saveIfExists()
      }
    } else saveIfExists()
  })
}

actual fun copyItemToClipboard(cItem: ChatItem, clipboard: ClipboardManager) = withLongRunningApi(slow = 600_000) {
  var fileSource = getLoadedFileSource(cItem.file)
  if (chatModel.connectedToRemote() && fileSource == null) {
    cItem.file?.loadRemoteFile(true)
    fileSource = getLoadedFileSource(cItem.file)
  }

  if (fileSource != null) {
    val filePath: String = if (fileSource.cryptoArgs != null) {
      val tmpFile = File(tmpDir, fileSource.filePath)
      tmpFile.deleteOnExit()
      try {
        decryptCryptoFile(getAppFilePath(fileSource.filePath), fileSource.cryptoArgs ?: return@withLongRunningApi, tmpFile.absolutePath)
      } catch (e: Exception) {
        Log.e(TAG, "Unable to decrypt crypto file: " + e.stackTraceToString())
        return@withLongRunningApi
      }
      tmpFile.absolutePath
    } else {
      getAppFilePath(fileSource.filePath)
    }
    when {
      desktopPlatform.isWindows() -> clipboard.setText(AnnotatedString("\"${File(filePath).absolutePath}\""))
      else -> clipboard.setText(AnnotatedString(filePath))
    }
  } else {
    clipboard.setText(AnnotatedString(cItem.content.text))
  }
  showToast(MR.strings.copied.localized())
}.run {}
