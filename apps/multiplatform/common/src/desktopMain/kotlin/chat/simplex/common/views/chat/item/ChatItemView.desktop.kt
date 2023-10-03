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
import java.util.*

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
    when (cItem.content.msgContent) {
      is MsgContent.MCImage, is MsgContent.MCFile, is MsgContent.MCVoice, is MsgContent.MCVideo -> withApi { saveFileLauncher.launch(cItem.file?.fileName ?: "") }
      else -> {}
    }
    showMenu.value = false
  })
}

actual fun copyItemToClipboard(cItem: ChatItem, clipboard: ClipboardManager) {
  val fileSource = getLoadedFileSource(cItem.file)
  if (fileSource != null) {
    val filePath: String = if (fileSource.cryptoArgs != null) {
      val tmpFile = File(tmpDir, fileSource.filePath)
      tmpFile.deleteOnExit()
      decryptCryptoFile(getAppFilePath(fileSource.filePath), fileSource.cryptoArgs, tmpFile.absolutePath)
      tmpFile.absolutePath
    } else {
      getAppFilePath(fileSource.filePath)
    }
    when  {
      desktopPlatform.isWindows() -> clipboard.setText(AnnotatedString("\"${File(filePath).absolutePath}\""))
      else -> clipboard.setText(AnnotatedString(filePath))
    }
  } else {
    clipboard.setText(AnnotatedString(cItem.content.text))
  }
}
