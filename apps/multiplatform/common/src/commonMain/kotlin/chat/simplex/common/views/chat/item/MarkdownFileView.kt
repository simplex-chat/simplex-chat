package chat.simplex.common.views.chat.item

import SectionBottomSpacer
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyListScope
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.painter.BitmapPainter
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.text.withStyle
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.chat.topPaddingToContent
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.networkAndServers.internalUriHandler
import chat.simplex.res.*
import com.mikepenz.markdown.compose.MarkdownElement
import com.mikepenz.markdown.m2.Markdown
import com.mikepenz.markdown.m2.markdownColor
import com.mikepenz.markdown.m2.markdownTypography
import com.mikepenz.markdown.model.ImageData
import com.mikepenz.markdown.model.ImageTransformer
import com.mikepenz.markdown.model.markdownAnnotator
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import org.intellij.markdown.MarkdownElementTypes
import org.intellij.markdown.ast.ASTNode
import org.intellij.markdown.ast.getTextInNode
import java.io.ByteArrayInputStream
import java.io.File
import java.net.URI
import java.nio.ByteBuffer
import java.nio.charset.CharacterCodingException

private const val DATA_IMAGE = "data:image/"

fun isMarkdownFile(file: CIFile): Boolean =
  file.fileSize <= MAX_MARKDOWN_VIEW_SIZE && file.fileName.substringAfterLast('.', "").lowercase() in listOf("md", "markdown")

fun showMarkdownFile(file: CIFile, saveFileLauncher: FileChooserLauncher) {
  withLongRunningApi(slow = 600_000) {
    var filePath = getLoadedFilePath(file)
    if (chatModel.connectedToRemote() && filePath == null) {
      file.loadRemoteFile(true)
      filePath = getLoadedFilePath(file)
    }
    if (filePath == null) {
      AlertManager.shared.showAlertMsg(MR.strings.file_not_found)
      return@withLongRunningApi
    }
    val bytes = try {
      val cryptoArgs = file.fileSource?.cryptoArgs
      if (cryptoArgs != null) readCryptoFile(filePath, cryptoArgs) else File(filePath).readBytes()
    } catch (e: Exception) {
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error), e.message)
      return@withLongRunningApi
    }
    val text = textFileContent(bytes)
    if (text == null) {
      AlertManager.shared.showAlertDialog(
        generalGetString(MR.strings.not_a_text_file),
        confirmText = generalGetString(MR.strings.save_verb),
        onConfirm = { withLongRunningApi { saveFileLauncher.launch(file.fileName) } }
      )
      return@withLongRunningApi
    }
    ModalManager.fullscreen.showModal(endButtons = { MarkdownFileButtons(file, bytes) }) {
      MarkdownFileView(file.fileName, text)
    }
  }
}

private fun textFileContent(bytes: ByteArray): String? =
  try {
    Charsets.UTF_8.newDecoder().decode(ByteBuffer.wrap(bytes)).toString()
      .takeIf { text -> text.none { it < ' ' && it != '\t' && it != '\n' && it != '\r' } }
  } catch (e: CharacterCodingException) {
    null
  }

@Composable
private fun MarkdownFileButtons(file: CIFile, bytes: ByteArray) {
  val textView = remember { appPrefs.markdownFileTextView.state }
  IconButton({ appPrefs.markdownFileTextView.set(!textView.value) }) {
    Icon(
      painterResource(if (textView.value) MR.images.ic_article else MR.images.ic_code),
      stringResource(if (textView.value) MR.strings.markdown_formatted_view else MR.strings.markdown_text_view),
      tint = MaterialTheme.colors.primary
    )
  }
  val saveFileLauncher = rememberFileChooserLauncher(false, bytes) { to: URI? ->
    if (to != null) copyBytesToFile(ByteArrayInputStream(bytes), to) {}
  }
  IconButton({ withLongRunningApi { saveFileLauncher.launch(file.fileName) } }) {
    Icon(painterResource(MR.images.ic_download), stringResource(MR.strings.save_verb), tint = MaterialTheme.colors.primary)
  }
}

@Composable
private fun MarkdownFileView(fileName: String, text: String) {
  val textView = remember { appPrefs.markdownFileTextView.state }
  if (textView.value) {
    val lines = remember(text) { text.lines() }
    SelectionContainer {
      MarkdownFileColumn(fileName) {
        items(lines) { line ->
          Text(line, Modifier.padding(horizontal = DEFAULT_PADDING), fontFamily = FontFamily.Monospace)
        }
      }
    }
  } else {
    FormattedMarkdown(fileName, text)
  }
}

@Composable
private fun FormattedMarkdown(fileName: String, text: String) {
  val colors = markdownColor()
  val typography = markdownTypography(
    link = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.primary, textDecoration = TextDecoration.Underline)
  )
  val codeSpanStyle = typography.inlineCode.toSpanStyle().copy(background = colors.inlineCodeBackground)
  val annotator = remember(codeSpanStyle) {
    markdownAnnotator { content, child ->
      when (child.type) {
        MarkdownElementTypes.CODE_SPAN -> {
          withStyle(codeSpanStyle) { append(" ${child.getTextInNode(content).trim('`')} ") }
          true
        }
        MarkdownElementTypes.IMAGE ->
          if (imageLink(content, child)?.startsWith(DATA_IMAGE) == true) {
            false
          } else {
            append(child.getTextInNode(content).toString())
            true
          }
        else -> false
      }
    }
  }
  val parentUriHandler = LocalUriHandler.current
  CompositionLocalProvider(LocalUriHandler provides remember { internalUriHandler(parentUriHandler) }) {
    Markdown(
      text,
      colors = colors,
      typography = typography,
      imageTransformer = DataImageTransformer,
      annotator = annotator,
      success = { state, components, _ ->
        MarkdownFileColumn(fileName) {
          items(state.node.children) { node ->
            Box(Modifier.padding(horizontal = DEFAULT_PADDING)) {
              MarkdownElement(node, components, state.content, skipLinkDefinition = state.linksLookedUp)
            }
          }
        }
      }
    )
  }
}

@Composable
private fun MarkdownFileColumn(fileName: String, content: LazyListScope.() -> Unit) {
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  LazyColumnWithScrollBar(
    contentPadding = PaddingValues(
      top = if (oneHandUI.value) WindowInsets.statusBars.asPaddingValues().calculateTopPadding() + DEFAULT_PADDING + 5.dp else topPaddingToContent(false)
    )
  ) {
    item { AppBarTitle(fileName) }
    content()
    item { SectionBottomSpacer() }
  }
}

private fun imageLink(content: String, image: ASTNode): String? =
  image.children.firstOrNull { it.type == MarkdownElementTypes.INLINE_LINK }
    ?.children?.firstOrNull { it.type == MarkdownElementTypes.LINK_DESTINATION }
    ?.getTextInNode(content)?.toString()

private object DataImageTransformer : ImageTransformer {
  @Composable
  override fun transform(link: String): ImageData? =
    remember(link) {
      if (link.startsWith(DATA_IMAGE)) ImageData(BitmapPainter(base64ToBitmap(link.substringAfter("base64,")))) else null
    }
}
