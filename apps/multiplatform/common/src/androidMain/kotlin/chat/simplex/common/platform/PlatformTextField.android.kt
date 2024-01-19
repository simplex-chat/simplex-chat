package chat.simplex.common.platform

import android.annotation.SuppressLint
import android.content.Context
import android.os.Build
import android.text.InputType
import android.util.Log
import android.view.OnReceiveContentListener
import android.view.ViewGroup
import android.view.inputmethod.*
import android.widget.EditText
import android.widget.TextView
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.padding
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.toArgb
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.unit.dp
import androidx.compose.ui.viewinterop.AndroidView
import androidx.core.graphics.drawable.DrawableCompat
import androidx.core.view.inputmethod.EditorInfoCompat
import androidx.core.view.inputmethod.InputConnectionCompat
import androidx.core.widget.doAfterTextChanged
import androidx.core.widget.doOnTextChanged
import chat.simplex.common.R
import chat.simplex.common.helpers.toURI
import chat.simplex.common.model.ChatModel
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.SharedContent
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource
import kotlinx.coroutines.delay
import java.lang.reflect.Field
import java.net.URI

@Composable
actual fun PlatformTextField(
  composeState: MutableState<ComposeState>,
  sendMsgEnabled: Boolean,
  sendMsgButtonDisabled: Boolean,
  textStyle: MutableState<TextStyle>,
  showDeleteTextButton: MutableState<Boolean>,
  userIsObserver: Boolean,
  onMessageChange: (String) -> Unit,
  onUpArrow: () -> Unit,
  onFilesPasted: (List<URI>) -> Unit,
  onDone: () -> Unit,
) {
  val cs = composeState.value
  val textColor = MaterialTheme.colors.onBackground
  val tintColor = MaterialTheme.colors.secondaryVariant
  val padding = PaddingValues(12.dp, 7.dp, 45.dp, 0.dp)
  val paddingStart = with(LocalDensity.current) { 12.dp.roundToPx() }
  val paddingTop = with(LocalDensity.current) { 7.dp.roundToPx() }
  val paddingEnd = with(LocalDensity.current) { 45.dp.roundToPx() }
  val paddingBottom = with(LocalDensity.current) { 7.dp.roundToPx() }
  var showKeyboard by remember { mutableStateOf(false) }
  var freeFocus by remember { mutableStateOf(false) }
  LaunchedEffect(cs.contextItem) {
    if (cs.contextItem is ComposeContextItem.QuotedItem) {
      delay(100)
      showKeyboard = true
    } else if (cs.contextItem is ComposeContextItem.EditingItem) {
      // Keyboard will not show up if we try to show it too fast
      delay(300)
      showKeyboard = true
    }
  }
  LaunchedEffect(sendMsgEnabled) {
    if (!sendMsgEnabled) {
      freeFocus = true
    }
  }

  AndroidView(modifier = Modifier, factory = {
    val editText = @SuppressLint("AppCompatCustomView") object: EditText(it) {
      override fun setOnReceiveContentListener(
        mimeTypes: Array<out String>?,
        listener: OnReceiveContentListener?
      ) {
        super.setOnReceiveContentListener(mimeTypes, listener)
      }

      override fun onCreateInputConnection(editorInfo: EditorInfo): InputConnection {
        val connection = super.onCreateInputConnection(editorInfo)
        EditorInfoCompat.setContentMimeTypes(editorInfo, arrayOf("image/*"))
        val onCommit = InputConnectionCompat.OnCommitContentListener { inputContentInfo, _, _ ->
          try {
            inputContentInfo.requestPermission()
          } catch (e: Exception) {
            return@OnCommitContentListener false
          }
          ChatModel.sharedContent.value = SharedContent.Media("", listOf(inputContentInfo.contentUri.toURI()))
          true
        }
        return InputConnectionCompat.createWrapper(connection, editorInfo, onCommit)
      }
    }
    editText.layoutParams = ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT)
    editText.maxLines = 16
    editText.inputType = InputType.TYPE_TEXT_FLAG_CAP_SENTENCES or editText.inputType
    editText.setTextColor(textColor.toArgb())
    editText.textSize = textStyle.value.fontSize.value
    val drawable = androidAppContext.getDrawable(R.drawable.send_msg_view_background)!!
    DrawableCompat.setTint(drawable, tintColor.toArgb())
    editText.background = drawable
    editText.setPadding(paddingStart, paddingTop, paddingEnd, paddingBottom)
    editText.setText(cs.message)
    if (Build.VERSION.SDK_INT >= 29) {
      editText.textCursorDrawable?.let { DrawableCompat.setTint(it, CurrentColors.value.colors.secondary.toArgb()) }
    } else {
      try {
        val f: Field = TextView::class.java.getDeclaredField("mCursorDrawableRes")
        f.isAccessible = true
        f.set(editText, R.drawable.edit_text_cursor)
      } catch (e: Exception) {
        Log.e(TAG, e.stackTraceToString())
      }
    }
    editText.doOnTextChanged { text, _, _, _ ->
      if (!composeState.value.inProgress) {
        onMessageChange(text.toString())
      } else if (text.toString() != composeState.value.message) {
        editText.setText(composeState.value.message)
      }
    }
    editText.doAfterTextChanged { text -> if (composeState.value.preview is ComposePreview.VoicePreview && text.toString() != "") editText.setText("") }
    editText
  }) {
    it.setTextColor(textColor.toArgb())
    it.textSize = textStyle.value.fontSize.value
    DrawableCompat.setTint(it.background, tintColor.toArgb())
    it.isFocusable = composeState.value.preview !is ComposePreview.VoicePreview
    it.isFocusableInTouchMode = it.isFocusable
    if (cs.message != it.text.toString()) {
      it.setText(cs.message)
      // Set cursor to the end of the text
      it.setSelection(it.text.length)
    }
    if (showKeyboard) {
      it.requestFocus()
      val imm: InputMethodManager = androidAppContext.getSystemService(Context.INPUT_METHOD_SERVICE) as InputMethodManager
      imm.showSoftInput(it, InputMethodManager.SHOW_IMPLICIT)
      showKeyboard = false
    }
    if (freeFocus) {
      it.clearFocus()
      hideKeyboard(it)
      freeFocus = false
    }
    showDeleteTextButton.value = it.lineCount >= 4 && !cs.inProgress
  }
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
