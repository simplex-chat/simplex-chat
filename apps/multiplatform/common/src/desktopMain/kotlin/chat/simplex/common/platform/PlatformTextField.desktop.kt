package chat.simplex.common.platform

import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.input.key.*
import androidx.compose.ui.platform.*
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.input.*
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.dp
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource
import kotlinx.coroutines.delay
import java.awt.Image
import java.awt.Toolkit
import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.UnsupportedFlavorException
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.io.File
import java.net.URI
import java.util.*
import javax.imageio.ImageIO
import kotlin.collections.ArrayList
import kotlin.io.path.*
import kotlin.math.min
import kotlin.text.substring

@Composable
actual fun PlatformTextField(
  composeState: MutableState<ComposeState>,
  sendMsgEnabled: Boolean,
  sendMsgButtonDisabled: Boolean,
  textStyle: MutableState<TextStyle>,
  showDeleteTextButton: MutableState<Boolean>,
  userIsObserver: Boolean,
  placeholder: String,
  showVoiceButton: Boolean,
  onMessageChange: (String) -> Unit,
  onUpArrow: () -> Unit,
  onFilesPasted: (List<URI>) -> Unit,
  onDone: () -> Unit,
) {
  val cs = composeState.value
  val focusRequester = remember { FocusRequester() }
  val focusManager = LocalFocusManager.current
  val keyboard = LocalSoftwareKeyboardController.current
  LaunchedEffect(cs.contextItem) {
    if (cs.contextItem !is ComposeContextItem.QuotedItem) return@LaunchedEffect
    // In replying state
    focusRequester.requestFocus()
    delay(50)
    keyboard?.show()
  }
  LaunchedEffect(sendMsgEnabled) {
    if (!sendMsgEnabled) {
      focusManager.clearFocus()
      delay(50)
      keyboard?.hide()
    }
  }
  val lastTimeWasRtlByCharacters = remember { mutableStateOf(isRtl(cs.message.subSequence(0, min(50, cs.message.length)))) }
  val isRtlByCharacters = remember(cs.message) {
    if (cs.message.isNotEmpty()) isRtl(cs.message.subSequence(0, min(50, cs.message.length))) else lastTimeWasRtlByCharacters.value
  }
  LaunchedEffect(isRtlByCharacters) {
    lastTimeWasRtlByCharacters.value = isRtlByCharacters
  }
  val isLtrGlobally = LocalLayoutDirection.current == LayoutDirection.Ltr
  // Different padding here is for a text that is considered RTL with non-RTL locale set globally.
  // In this case padding from right side should be bigger
  val startEndPadding = if (cs.message.isEmpty() && showVoiceButton && isRtlByCharacters && isLtrGlobally) 95.dp else 50.dp
  val startPadding = if (isRtlByCharacters && isLtrGlobally) startEndPadding else 0.dp
  val endPadding = if (isRtlByCharacters && isLtrGlobally) 0.dp else startEndPadding
  val padding = PaddingValues(startPadding, 12.dp, endPadding, 0.dp)
  var textFieldValueState by remember { mutableStateOf(TextFieldValue(text = cs.message)) }
  val textFieldValue = textFieldValueState.copy(text = cs.message)
  val clipboard = LocalClipboardManager.current
  BasicTextField(
    value = textFieldValue,
    onValueChange = onValueChange@ {
      if (!composeState.value.inProgress && !(composeState.value.preview is ComposePreview.VoicePreview && it.text != "")) {
        val diff = textFieldValueState.selection.length + (it.text.length - textFieldValueState.text.length)
        if (diff > 1 && it.text != textFieldValueState.text && it.selection.max - diff >= 0) {
          val pasted = it.text.substring(it.selection.max - diff, it.selection.max)
          val files = parseToFiles(AnnotatedString(pasted))
          if (files.isNotEmpty()) {
            onFilesPasted(files)
            return@onValueChange
          }
        }
        textFieldValueState = it
        onMessageChange(it.text)
      }
    },
    textStyle = textStyle.value,
    maxLines = 16,
    keyboardOptions = KeyboardOptions.Default.copy(
      capitalization = KeyboardCapitalization.Sentences,
      autoCorrectEnabled = true
    ),
    modifier = Modifier
      .padding(vertical = 4.dp)
      .focusRequester(focusRequester)
      .onPreviewKeyEvent {
        if ((it.key == Key.Enter || it.key == Key.NumPadEnter) && it.type == KeyEventType.KeyDown) {
          if (it.isShiftPressed) {
            val start = if (minOf(textFieldValue.selection.min) == 0) "" else textFieldValue.text.substring(0 until textFieldValue.selection.min)
            val newText = start + "\n" +
                  textFieldValue.text.substring(textFieldValue.selection.max, textFieldValue.text.length)
            textFieldValueState = textFieldValue.copy(
              text = newText,
              selection = TextRange(textFieldValue.selection.min + 1)
            )
            onMessageChange(newText)
          } else if (!sendMsgButtonDisabled) {
            onDone()
          }
          true
        } else if (it.key == Key.DirectionUp && it.type == KeyEventType.KeyDown && cs.message.isEmpty()) {
          onUpArrow()
          true
        } else if (it.key == Key.V &&
            it.type == KeyEventType.KeyDown &&
            ((it.isCtrlPressed && !desktopPlatform.isMac()) || (it.isMetaPressed && desktopPlatform.isMac()))) {
            if (parseToFiles(clipboard.getText()).isNotEmpty()) {
              onFilesPasted(parseToFiles(clipboard.getText()))
              true
            } else {
              // It's much faster to getData instead of getting transferable first
              val image = try {
                Toolkit.getDefaultToolkit().systemClipboard.getData(DataFlavor.imageFlavor) as Image
              } catch (e: UnsupportedFlavorException) {
                null
              }
              if (image != null) {
                try {
                  // create BufferedImage from Image
                  val bi = BufferedImage(image.getWidth(null), image.getHeight(null), BufferedImage.TYPE_INT_ARGB)
                  val bgr = bi.createGraphics()
                  bgr.drawImage(image, 0, 0, null)
                  bgr.dispose()
                  // create byte array from BufferedImage
                  val baos = ByteArrayOutputStream()
                  ImageIO.write(bi, "png", baos)
                  val bytes = baos.toByteArray()
                  withBGApi {
                    val tempFile = File(tmpDir, "${UUID.randomUUID()}.png")
                    chatModel.filesToDelete.add(tempFile)

                    tempFile.writeBytes(bytes)
                    composeState.processPickedMedia(listOf(tempFile.toURI()), composeState.value.message)
                  }
                } catch (e: Exception) {
                  Log.e(TAG, "Pasting image exception: ${e.stackTraceToString()}")
                }
                true
              } else {
                false
              }
            }
          }
        else false
      },
    cursorBrush = SolidColor(MaterialTheme.colors.secondary),
    decorationBox = { innerTextField ->
      Row(verticalAlignment = Alignment.Bottom) {
        CompositionLocalProvider(
          LocalLayoutDirection provides if (isRtlByCharacters) LayoutDirection.Rtl else LocalLayoutDirection.current
        ) {
          Column(Modifier.weight(1f).padding(start = startPadding, end = endPadding)) {
            Spacer(Modifier.height(8.dp))
            TextFieldDefaults.TextFieldDecorationBox(
              value = textFieldValue.text,
              innerTextField = innerTextField,
              placeholder = { Text(placeholder, style = textStyle.value.copy(color = MaterialTheme.colors.secondary)) },
              singleLine = false,
              enabled = true,
              isError = false,
              trailingIcon = null,
              interactionSource = remember { MutableInteractionSource() },
              contentPadding = PaddingValues(),
              visualTransformation = VisualTransformation.None,
              colors = TextFieldDefaults.textFieldColors(backgroundColor = Color.Unspecified)
            )
            Spacer(Modifier.height(10.dp))
          }
        }
      }
    },
  )
  showDeleteTextButton.value = cs.message.split("\n").size >= 4 && !cs.inProgress
  if (composeState.value.preview is ComposePreview.VoicePreview) {
    ComposeOverlay(MR.strings.voice_message_send_text, textStyle, padding)
  } else if (userIsObserver) {
    ComposeOverlay(MR.strings.you_are_observer, textStyle, padding)
  }
}

@Composable
private fun ComposeOverlay(textId: StringResource, textStyle: MutableState<TextStyle>, padding: PaddingValues) {
  Text(
    generalGetString(textId),
    Modifier.padding(padding),
    color = MaterialTheme.colors.secondary,
    style = textStyle.value.copy(fontStyle = FontStyle.Italic)
  )
}

private fun parseToFiles(text: AnnotatedString?): List<URI> {
  text ?: return emptyList()
  val files = ArrayList<URI>()
  text.lines().forEach {
    try {
      val uri = File(it.removePrefix("\"").removeSuffix("\"")).toURI()
      val path = uri.toPath()
      if (!path.exists() || !path.isAbsolute || path.isDirectory()) return emptyList()
      files.add(uri)
    } catch (e: Exception) {
      return emptyList()
    }
  }
  return files
}
