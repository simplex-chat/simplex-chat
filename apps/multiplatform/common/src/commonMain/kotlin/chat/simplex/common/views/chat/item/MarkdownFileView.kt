package chat.simplex.common.views.chat.item

import SectionBottomSpacer
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyListScope
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.painter.BitmapPainter
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.buildAnnotatedString
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.withStyle
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.WarningOrange
import chat.simplex.common.views.chat.topPaddingToContent
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.networkAndServers.documentMarkdownStyle
import chat.simplex.res.*
import com.mikepenz.markdown.compose.MarkdownElement
import com.mikepenz.markdown.m2.Markdown
import com.mikepenz.markdown.model.ImageData
import com.mikepenz.markdown.model.ImageTransformer
import com.mikepenz.markdown.model.State
import com.mikepenz.markdown.model.markdownAnnotator
import com.mikepenz.markdown.model.parseMarkdownFlow
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.withContext
import org.intellij.markdown.MarkdownElementTypes
import org.intellij.markdown.ast.ASTNode
import org.intellij.markdown.ast.getTextInNode
import org.intellij.markdown.flavours.gfm.GFMElementTypes
import java.io.ByteArrayInputStream
import java.io.File
import java.net.URI
import java.nio.ByteBuffer
import java.nio.charset.CodingErrorAction

private const val DATA_IMAGE = "data:image/"
private const val MAX_MARKDOWN_VIEW_SIZE: Long = 1_048_576
private const val MAX_MARKDOWN_LINE_LENGTH = 15_000
private const val MAX_MARKDOWN_DEPTH = 32
private const val MAX_MARKDOWN_TABLE_ROWS = 200
private const val MAX_MARKDOWN_IMAGE_PIXELS = 2160L * 4320

private val controlChar = Regex("[\\x00-\\x08\\x0B\\x0C\\x0E-\\x1F]")

private val bidiControlChar = Regex("[\\u061C\\u200E\\u200F\\u202A-\\u202E\\u2066-\\u2069]")

private val invisibleCharMarkerStyle = SpanStyle(color = WarningOrange)

private class MarkdownDocument(val lines: List<String>, val formatted: State.Success?, val hasBidiControls: Boolean)

fun isMarkdownFile(file: CIFile): Boolean =
  file.fileSize <= MAX_MARKDOWN_VIEW_SIZE && file.fileName.substringAfterLast('.', "").lowercase() in listOf("md", "markdown")

fun showMarkdownFile(file: CIFile) {
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
    val text = decodedText(bytes)
    val lines = text.lines()
    val formatted = if (lines.all { it.length <= MAX_MARKDOWN_LINE_LENGTH }) formattedWithinLimits(text) else null
    val document = MarkdownDocument(
      lines.flatMap { line -> line.chunked(MAX_MARKDOWN_LINE_LENGTH).ifEmpty { listOf("") } },
      formatted,
      bidiControlChar.containsMatchIn(text)
    )
    withContext(Dispatchers.Main) {
      if (appPlatform.isDesktop && ModalManager.center.isTopModalOpen(ModalViewId.MARKDOWN_FILE)) ModalManager.center.closeModal()
      ModalManager.center.showModal(id = ModalViewId.MARKDOWN_FILE, endButtons = {
        if (formatted != null) MarkdownViewSwitch()
        MarkdownFileSaveButton(file, bytes)
      }) {
        key(this) { MarkdownFileView(file.fileName, document) }
      }
    }
  }
}

private fun decodedText(bytes: ByteArray): String =
  Charsets.UTF_8.newDecoder()
    .onMalformedInput(CodingErrorAction.REPLACE)
    .onUnmappableCharacter(CodingErrorAction.REPLACE)
    .replaceWith("?")
    .decode(ByteBuffer.wrap(bytes))
    .toString()
    .replace(controlChar, "?")

private suspend fun formattedWithinLimits(text: String): State.Success? =
  (parseMarkdownFlow(text).first { it !is State.Loading } as? State.Success)?.takeIf { treeWithinLimits(it.node) }

private fun treeWithinLimits(root: ASTNode): Boolean {
  val nodes = ArrayDeque(listOf(root to 1))
  while (nodes.isNotEmpty()) {
    val (node, depth) = nodes.removeLast()
    if (depth > MAX_MARKDOWN_DEPTH) return false
    if (node.type == GFMElementTypes.TABLE && node.children.count { it.type == GFMElementTypes.ROW } > MAX_MARKDOWN_TABLE_ROWS) return false
    node.children.forEach { nodes.addLast(it to depth + 1) }
  }
  return true
}

@Composable
private fun MarkdownViewSwitch() {
  val textView = remember { appPrefs.markdownFileTextView.state }
  IconButton({ appPrefs.markdownFileTextView.set(!textView.value) }) {
    Icon(
      painterResource(if (textView.value) MR.images.ic_article else MR.images.ic_code),
      stringResource(if (textView.value) MR.strings.markdown_formatted_view else MR.strings.markdown_text_view),
      tint = MaterialTheme.colors.primary
    )
  }
}

@Composable
private fun MarkdownFileSaveButton(file: CIFile, bytes: ByteArray) {
  val saveFileLauncher = rememberFileChooserLauncher(false, bytes) { to: URI? ->
    if (to != null) copyBytesToFile(ByteArrayInputStream(bytes), to) {}
  }
  IconButton({ withLongRunningApi { saveFileLauncher.launch(file.fileName) } }) {
    Icon(painterResource(MR.images.ic_download), stringResource(MR.strings.save_verb), tint = MaterialTheme.colors.primary)
  }
}

@Composable
private fun MarkdownFileView(fileName: String, document: MarkdownDocument) {
  val textView = remember { appPrefs.markdownFileTextView.state }
  val formatted = document.formatted
  if (textView.value || formatted == null) {
    SelectionContainer {
      MarkdownFileColumn(fileName, document.hasBidiControls) {
        items(document.lines) { line ->
          Text(remember(line) { withInvisibleCharMarkers(line) }, Modifier.padding(horizontal = DEFAULT_PADDING), fontFamily = FontFamily.Monospace)
        }
      }
    }
  } else {
    FormattedMarkdown(fileName, formatted, document.hasBidiControls)
  }
}

private fun withInvisibleCharMarkers(line: String): AnnotatedString = buildAnnotatedString {
  var start = 0
  invisibleChar.findAll(line).forEach {
    append(line.substring(start, it.range.first))
    withStyle(invisibleCharMarkerStyle) { append(unicodeMarker(it.value)) }
    start = it.range.last + 1
  }
  append(line.substring(start))
}

@Composable
private fun FormattedMarkdown(fileName: String, formatted: State.Success, hasBidiControls: Boolean) {
  val style = documentMarkdownStyle(h1 = MaterialTheme.typography.h2.copy(fontSize = 26.sp, fontWeight = FontWeight.Bold))
  val codeSpanStyle = style.typography.inlineCode.toSpanStyle().copy(background = style.colors.inlineCodeBackground)
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
  val imageTransformer = remember { DataImageTransformer() }
  val parentUriHandler = LocalUriHandler.current
  CompositionLocalProvider(LocalUriHandler provides remember { markdownFileUriHandler(parentUriHandler) }) {
    Markdown(
      formatted,
      colors = style.colors,
      typography = style.typography,
      padding = style.padding,
      imageTransformer = imageTransformer,
      annotator = annotator,
      components = style.components,
      success = { state, components, _ ->
        MarkdownFileColumn(fileName, hasBidiControls) {
          items(state.node.children) { node ->
            Column(Modifier.padding(horizontal = DEFAULT_PADDING)) {
              MarkdownElement(node, components, state.content, skipLinkDefinition = state.linksLookedUp)
            }
          }
        }
      }
    )
  }
}

@Composable
private fun MarkdownFileColumn(fileName: String, hasBidiControls: Boolean, content: LazyListScope.() -> Unit) {
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  LazyColumnWithScrollBar(
    contentPadding = PaddingValues(
      top = if (oneHandUI.value) WindowInsets.statusBars.asPaddingValues().calculateTopPadding() + DEFAULT_PADDING + 5.dp else topPaddingToContent(false)
    )
  ) {
    item { AppBarTitle(fileName) }
    if (hasBidiControls) {
      item {
        Text(stringResource(MR.strings.markdown_file_bidi_warning), Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, bottom = DEFAULT_PADDING), color = WarningOrange)
      }
    }
    content()
    item { SectionBottomSpacer() }
  }
}

private fun imageLink(content: String, image: ASTNode): String? =
  image.children.firstOrNull { it.type == MarkdownElementTypes.INLINE_LINK }
    ?.children?.firstOrNull { it.type == MarkdownElementTypes.LINK_DESTINATION }
    ?.getTextInNode(content)?.toString()

private fun markdownFileUriHandler(parentUriHandler: UriHandler): UriHandler = object: UriHandler {
  override fun openUri(uri: String) {
    val format = parseToMarkdown(uri)?.singleOrNull()?.format
    when {
      format is Format.SimplexLink -> parentUriHandler.openVerifiedSimplexUri(format.simplexUri)
      format is Format.Uri && (uri.startsWith("https://") || uri.startsWith("http://")) -> openBrowserAlert(uri, parentUriHandler)
      else -> showInvalidLinkAlert(uri)
    }
  }
}

private class DataImageTransformer : ImageTransformer {
  private var pixels = MAX_MARKDOWN_IMAGE_PIXELS
  private val images = mutableMapOf<String, ImageBitmap?>()

  @Composable
  override fun transform(link: String): ImageData? {
    val width = with(LocalDensity.current) { LocalWindowWidth().roundToPx() }
    val bitmap by produceState<ImageBitmap?>(null, link) {
      value = withContext(Dispatchers.Default) { decodedImage(link, width) }
    }
    return remember(bitmap) { bitmap?.let { ImageData(BitmapPainter(it)) } }
  }

  private fun decodedImage(link: String, width: Int): ImageBitmap? = synchronized(this) {
    if (link !in images) images[link] = imageWithinBudget(link, width)
    images[link]
  }

  private fun imageWithinBudget(link: String, width: Int): ImageBitmap? =
    link.takeIf { it.startsWith(DATA_IMAGE) }
      ?.let { decodeBase64Image(it.substringAfter("base64,"), width) }
      ?.takeIf { it.width.toLong() * it.height <= pixels }
      ?.also { pixels -= it.width.toLong() * it.height }
}
