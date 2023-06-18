package chat.simplex.app.views.chat

import android.Manifest
import android.annotation.SuppressLint
import android.app.Activity
import android.content.Context
import android.content.pm.ActivityInfo
import android.content.res.Configuration
import android.os.Build
import android.text.InputType
import android.util.Log
import android.view.ViewGroup
import android.view.WindowManager
import android.view.inputmethod.*
import android.widget.EditText
import android.widget.TextView
import androidx.compose.animation.core.*
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.*
import androidx.compose.material.*
import androidx.compose.material.ripple.rememberRipple
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.*
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.*
import androidx.compose.ui.res.*
import androidx.compose.ui.semantics.Role
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.*
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.*
import androidx.compose.ui.viewinterop.AndroidView
import androidx.compose.ui.window.Dialog
import androidx.core.graphics.drawable.DrawableCompat
import androidx.core.view.inputmethod.EditorInfoCompat
import androidx.core.view.inputmethod.InputConnectionCompat
import androidx.core.widget.*
import chat.simplex.app.R
import chat.simplex.app.SimplexApp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.item.ItemAction
import chat.simplex.app.views.helpers.*
import com.google.accompanist.permissions.rememberMultiplePermissionsState
import kotlinx.coroutines.*
import java.lang.reflect.Field

@Composable
fun SendMsgView(
  composeState: MutableState<ComposeState>,
  showVoiceRecordIcon: Boolean,
  recState: MutableState<RecordingState>,
  isDirectChat: Boolean,
  liveMessageAlertShown: SharedPreference<Boolean>,
  needToAllowVoiceToContact: Boolean,
  allowedVoiceByPrefs: Boolean,
  userIsObserver: Boolean,
  userCanSend: Boolean,
  allowVoiceToContact: () -> Unit,
  timedMessageAllowed: Boolean = false,
  customDisappearingMessageTimePref: SharedPreference<Int>? = null,
  sendMessage: (Int?) -> Unit,
  sendLiveMessage: (suspend () -> Unit)? = null,
  updateLiveMessage: (suspend () -> Unit)? = null,
  cancelLiveMessage: (() -> Unit)? = null,
  onMessageChange: (String) -> Unit,
  textStyle: MutableState<TextStyle>
) {
  val showCustomDisappearingMessageDialog = remember { mutableStateOf(false) }

  if (showCustomDisappearingMessageDialog.value) {
    CustomDisappearingMessageDialog(
      sendMessage = sendMessage,
      setShowDialog = { showCustomDisappearingMessageDialog.value = it },
      customDisappearingMessageTimePref = customDisappearingMessageTimePref
    )
  }

  Box(Modifier.padding(vertical = 8.dp)) {
    val cs = composeState.value
    val showProgress = cs.inProgress && (cs.preview is ComposePreview.MediaPreview || cs.preview is ComposePreview.FilePreview)
    val showVoiceButton = cs.message.isEmpty() && showVoiceRecordIcon && !composeState.value.editing &&
        cs.liveMessage == null && (cs.preview is ComposePreview.NoPreview || recState.value is RecordingState.Started)
    val showDeleteTextButton = rememberSaveable { mutableStateOf(false) }
    NativeKeyboard(composeState, textStyle, showDeleteTextButton, userIsObserver, onMessageChange)
    // Disable clicks on text field
    if (cs.preview is ComposePreview.VoicePreview || !userCanSend || cs.inProgress) {
      Box(
        Modifier
          .matchParentSize()
          .clickable(enabled = !userCanSend, indication = null, interactionSource = remember { MutableInteractionSource() }, onClick = {
            AlertManager.shared.showAlertMsg(
              title = generalGetString(R.string.observer_cant_send_message_title),
              text = generalGetString(R.string.observer_cant_send_message_desc)
            )
          })
      )
    }
    if (showDeleteTextButton.value) {
      DeleteTextButton(composeState)
    }
    Box(Modifier.align(Alignment.BottomEnd)) {
      val sendButtonSize = remember { Animatable(36f) }
      val sendButtonAlpha = remember { Animatable(1f) }
      val permissionsState = rememberMultiplePermissionsState(listOf(Manifest.permission.RECORD_AUDIO))
      val scope = rememberCoroutineScope()
      LaunchedEffect(Unit) {
        // Making LiveMessage alive when screen orientation was changed
        if (cs.liveMessage != null && sendLiveMessage != null && updateLiveMessage != null) {
          startLiveMessage(scope, sendLiveMessage, updateLiveMessage, sendButtonSize, sendButtonAlpha, composeState, liveMessageAlertShown)
        }
      }
      when {
        showProgress -> ProgressIndicator()
        showVoiceButton -> {
          Row(verticalAlignment = Alignment.CenterVertically) {
            val stopRecOnNextClick = remember { mutableStateOf(false) }
            when {
              needToAllowVoiceToContact || !allowedVoiceByPrefs || !userCanSend -> {
                DisallowedVoiceButton(userCanSend) {
                  if (needToAllowVoiceToContact) {
                    showNeedToAllowVoiceAlert(allowVoiceToContact)
                  } else if (!allowedVoiceByPrefs) {
                    showDisabledVoiceAlert(isDirectChat)
                  }
                }
              }
              !permissionsState.allPermissionsGranted ->
                VoiceButtonWithoutPermission { permissionsState.launchMultiplePermissionRequest() }
              else ->
                RecordVoiceView(recState, stopRecOnNextClick)
            }
            if (sendLiveMessage != null
              && updateLiveMessage != null
              && (cs.preview !is ComposePreview.VoicePreview || !stopRecOnNextClick.value)
              && cs.contextItem is ComposeContextItem.NoContextItem
            ) {
              Spacer(Modifier.width(10.dp))
              StartLiveMessageButton(userCanSend) {
                if (composeState.value.preview is ComposePreview.NoPreview) {
                  startLiveMessage(scope, sendLiveMessage, updateLiveMessage, sendButtonSize, sendButtonAlpha, composeState, liveMessageAlertShown)
                }
              }
            }
          }
        }
        cs.liveMessage?.sent == false && cs.message.isEmpty() -> {
          CancelLiveMessageButton {
            cancelLiveMessage?.invoke()
          }
        }
        else -> {
          val cs = composeState.value
          val icon = if (cs.editing || cs.liveMessage != null) painterResource(R.drawable.ic_check_filled) else painterResource(R.drawable.ic_arrow_upward)
          val disabled = !cs.sendEnabled() ||
              (!allowedVoiceByPrefs && cs.preview is ComposePreview.VoicePreview) ||
              cs.endLiveDisabled
          val showDropdown = rememberSaveable { mutableStateOf(false) }

          @Composable
          fun MenuItems(): List<@Composable () -> Unit> {
            val menuItems = mutableListOf<@Composable () -> Unit>()

            if (cs.liveMessage == null && !cs.editing) {
              if (
                cs.preview !is ComposePreview.VoicePreview &&
                cs.contextItem is ComposeContextItem.NoContextItem &&
                sendLiveMessage != null && updateLiveMessage != null
              ) {
                menuItems.add {
                  ItemAction(
                    generalGetString(R.string.send_live_message),
                    BoltFilled,
                    onClick = {
                      startLiveMessage(scope, sendLiveMessage, updateLiveMessage, sendButtonSize, sendButtonAlpha, composeState, liveMessageAlertShown)
                      showDropdown.value = false
                    }
                  )
                }
              }
              if (timedMessageAllowed) {
                menuItems.add {
                  ItemAction(
                    generalGetString(R.string.disappearing_message),
                    painterResource(R.drawable.ic_timer),
                    onClick = {
                      showCustomDisappearingMessageDialog.value = true
                      showDropdown.value = false
                    }
                  )
                }
              }
            }

            return menuItems
          }

          val menuItems = MenuItems()
          if (menuItems.isNotEmpty()) {
            SendMsgButton(icon, sendButtonSize, sendButtonAlpha, !disabled, sendMessage) { showDropdown.value = true }
            DefaultDropdownMenu(showDropdown) {
              menuItems.forEach { composable -> composable() }
            }
          } else {
            SendMsgButton(icon, sendButtonSize, sendButtonAlpha, !disabled, sendMessage)
          }
        }
      }
    }
  }
}

@Composable
private fun CustomDisappearingMessageDialog(
  sendMessage: (Int?) -> Unit,
  setShowDialog: (Boolean) -> Unit,
  customDisappearingMessageTimePref: SharedPreference<Int>?
) {
  val showCustomTimePicker = remember { mutableStateOf(false) }

  if (showCustomTimePicker.value) {
    val selectedDisappearingMessageTime = remember {
      mutableStateOf(customDisappearingMessageTimePref?.get?.invoke() ?: 300)
    }
    CustomTimePickerDialog(
      selectedDisappearingMessageTime,
      title = generalGetString(R.string.delete_after),
      confirmButtonText = generalGetString(R.string.send_disappearing_message_send),
      confirmButtonAction = { ttl ->
        sendMessage(ttl)
        customDisappearingMessageTimePref?.set?.invoke(ttl)
        setShowDialog(false)
      },
      cancel = { setShowDialog(false) }
    )
  } else {
    @Composable
    fun ChoiceButton(
      text: String,
      onClick: () -> Unit
    ) {
      TextButton(onClick) {
        Text(
          text,
          fontSize = 18.sp,
          color = MaterialTheme.colors.primary
        )
      }
    }

    Dialog(onDismissRequest = { setShowDialog(false) }) {
      Surface(
        shape = RoundedCornerShape(corner = CornerSize(25.dp))
      ) {
        Box(
          contentAlignment = Alignment.Center
        ) {
          Column(
            modifier = Modifier.padding(DEFAULT_PADDING),
            verticalArrangement = Arrangement.spacedBy(6.dp),
            horizontalAlignment = Alignment.CenterHorizontally
          ) {
            Row(
              modifier = Modifier.fillMaxWidth(),
              horizontalArrangement = Arrangement.SpaceBetween,
              verticalAlignment = Alignment.CenterVertically
            ) {
              Text(" ") // centers title
              Text(
                generalGetString(R.string.send_disappearing_message),
                fontSize = 16.sp,
                color = MaterialTheme.colors.secondary
              )
              Icon(
                painterResource(R.drawable.ic_close),
                generalGetString(R.string.icon_descr_close_button),
                tint = MaterialTheme.colors.secondary,
                modifier = Modifier
                  .size(25.dp)
                  .clickable { setShowDialog(false) }
              )
            }

            ChoiceButton(generalGetString(R.string.send_disappearing_message_30_seconds)) {
              sendMessage(30)
              setShowDialog(false)
            }
            ChoiceButton(generalGetString(R.string.send_disappearing_message_1_minute)) {
              sendMessage(60)
              setShowDialog(false)
            }
            ChoiceButton(generalGetString(R.string.send_disappearing_message_5_minutes)) {
              sendMessage(300)
              setShowDialog(false)
            }
            ChoiceButton(generalGetString(R.string.send_disappearing_message_custom_time)) {
              showCustomTimePicker.value = true
            }
          }
        }
      }
    }
  }
}

@Composable
private fun NativeKeyboard(
  composeState: MutableState<ComposeState>,
  textStyle: MutableState<TextStyle>,
  showDeleteTextButton: MutableState<Boolean>,
  userIsObserver: Boolean,
  onMessageChange: (String) -> Unit
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

  AndroidView(modifier = Modifier, factory = {
    val editText = @SuppressLint("AppCompatCustomView") object: EditText(it) {
      override fun setOnReceiveContentListener(
        mimeTypes: Array<out String>?,
        listener: android.view.OnReceiveContentListener?
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
          SimplexApp.context.chatModel.sharedContent.value = SharedContent.Media("", listOf(inputContentInfo.contentUri))
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
    val drawable = it.getDrawable(R.drawable.send_msg_view_background)!!
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
        Log.e(chat.simplex.app.TAG, e.stackTraceToString())
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
    it.isFocusable = composeState.value.preview !is ComposePreview.VoicePreview && !cs.inProgress
    it.isFocusableInTouchMode = it.isFocusable
    if (cs.message != it.text.toString()) {
      it.setText(cs.message)
      // Set cursor to the end of the text
      it.setSelection(it.text.length)
    }
    if (showKeyboard) {
      it.requestFocus()
      val imm: InputMethodManager = SimplexApp.context.getSystemService(Context.INPUT_METHOD_SERVICE) as InputMethodManager
      imm.showSoftInput(it, InputMethodManager.SHOW_IMPLICIT)
      showKeyboard = false
    }
    showDeleteTextButton.value = it.lineCount >= 4 && !cs.inProgress
  }
  if (composeState.value.preview is ComposePreview.VoicePreview) {
    ComposeOverlay(R.string.voice_message_send_text, textStyle, padding)
  } else if (userIsObserver) {
    ComposeOverlay(R.string.you_are_observer, textStyle, padding)
  }
}

@Composable
private fun ComposeOverlay(textId: Int, textStyle: MutableState<TextStyle>, padding: PaddingValues) {
  Text(
    generalGetString(textId),
    Modifier.padding(padding),
    color = MaterialTheme.colors.secondary,
    style = textStyle.value.copy(fontStyle = FontStyle.Italic)
  )
}

@Composable
private fun BoxScope.DeleteTextButton(composeState: MutableState<ComposeState>) {
  IconButton(
    { composeState.value = composeState.value.copy(message = "") },
    Modifier.align(Alignment.TopEnd).size(36.dp)
  ) {
    Icon(painterResource(R.drawable.ic_close), null, Modifier.padding(7.dp).size(36.dp), tint = MaterialTheme.colors.secondary)
  }
}

@Composable
private fun RecordVoiceView(recState: MutableState<RecordingState>, stopRecOnNextClick: MutableState<Boolean>) {
  val rec: Recorder = remember { RecorderNative() }
  DisposableEffect(Unit) { onDispose { rec.stop() } }
  val stopRecordingAndAddAudio: () -> Unit = {
    recState.value.filePathNullable?.let {
      recState.value = RecordingState.Finished(it, rec.stop())
    }
  }
  if (stopRecOnNextClick.value) {
    LaunchedEffect(recState.value) {
      if (recState.value is RecordingState.NotStarted) {
        stopRecOnNextClick.value = false
      }
    }
    // Lock orientation to current orientation because screen rotation will break the recording
    LockToCurrentOrientationUntilDispose()
    StopRecordButton(stopRecordingAndAddAudio)
  } else {
    val startRecording: () -> Unit = {
      recState.value = RecordingState.Started(
        filePath = rec.start { progress: Int?, finished: Boolean ->
          val state = recState.value
          if (state is RecordingState.Started && progress != null) {
            recState.value = if (!finished)
              RecordingState.Started(state.filePath, progress)
            else
              RecordingState.Finished(state.filePath, progress)
          }
        },
      )
    }
    val interactionSource = interactionSourceWithTapDetection(
      onPress = { if (recState.value is RecordingState.NotStarted) startRecording() },
      onClick = {
        if (stopRecOnNextClick.value) {
          stopRecordingAndAddAudio()
        } else {
          // tapped and didn't hold a finger
          stopRecOnNextClick.value = true
        }
      },
      onCancel = stopRecordingAndAddAudio,
      onRelease = stopRecordingAndAddAudio
    )
    RecordVoiceButton(interactionSource)
  }
}

@Composable
private fun DisallowedVoiceButton(enabled: Boolean, onClick: () -> Unit) {
  IconButton(onClick, Modifier.size(36.dp), enabled = enabled) {
    Icon(
      painterResource(R.drawable.ic_keyboard_voice),
      stringResource(R.string.icon_descr_record_voice_message),
      tint = MaterialTheme.colors.secondary,
      modifier = Modifier
        .size(36.dp)
        .padding(4.dp)
    )
  }
}

@Composable
private fun VoiceButtonWithoutPermission(onClick: () -> Unit) {
  IconButton(onClick, Modifier.size(36.dp)) {
    Icon(
      painterResource(R.drawable.ic_keyboard_voice_filled),
      stringResource(R.string.icon_descr_record_voice_message),
      tint = MaterialTheme.colors.primary,
      modifier = Modifier
        .size(34.dp)
        .padding(4.dp)
    )
  }
}

@Composable
private fun LockToCurrentOrientationUntilDispose() {
  val context = LocalContext.current
  DisposableEffect(Unit) {
    val activity = context as Activity
    val manager = context.getSystemService(Activity.WINDOW_SERVICE) as WindowManager
    val rotation = if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.Q) manager.defaultDisplay.rotation else activity.display?.rotation
    activity.requestedOrientation = when (rotation) {
      android.view.Surface.ROTATION_90 -> ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE
      android.view.Surface.ROTATION_180 -> ActivityInfo.SCREEN_ORIENTATION_REVERSE_PORTRAIT
      android.view.Surface.ROTATION_270 -> ActivityInfo.SCREEN_ORIENTATION_REVERSE_LANDSCAPE
      else -> ActivityInfo.SCREEN_ORIENTATION_PORTRAIT
    }
    // Unlock orientation
    onDispose { activity.requestedOrientation = ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED }
  }
}

@Composable
private fun StopRecordButton(onClick: () -> Unit) {
  IconButton(onClick, Modifier.size(36.dp)) {
    Icon(
      painterResource(R.drawable.ic_stop_filled),
      stringResource(R.string.icon_descr_record_voice_message),
      tint = MaterialTheme.colors.primary,
      modifier = Modifier
        .size(36.dp)
        .padding(4.dp)
    )
  }
}

@Composable
private fun RecordVoiceButton(interactionSource: MutableInteractionSource) {
  IconButton({}, Modifier.size(36.dp), interactionSource = interactionSource) {
    Icon(
      painterResource(R.drawable.ic_keyboard_voice_filled),
      stringResource(R.string.icon_descr_record_voice_message),
      tint = MaterialTheme.colors.primary,
      modifier = Modifier
        .size(34.dp)
        .padding(4.dp)
    )
  }
}

@Composable
private fun ProgressIndicator() {
  CircularProgressIndicator(Modifier.size(36.dp).padding(4.dp), color = MaterialTheme.colors.secondary, strokeWidth = 3.dp)
}

@Composable
private fun CancelLiveMessageButton(
  onClick: () -> Unit
) {
  IconButton(onClick, Modifier.size(36.dp)) {
    Icon(
      painterResource(R.drawable.ic_close),
      stringResource(R.string.icon_descr_cancel_live_message),
      tint = MaterialTheme.colors.primary,
      modifier = Modifier
        .size(36.dp)
        .padding(4.dp)
    )
  }
}

@Composable
private fun SendMsgButton(
  icon: Painter,
  sizeDp: Animatable<Float, AnimationVector1D>,
  alpha: Animatable<Float, AnimationVector1D>,
  enabled: Boolean,
  sendMessage: (Int?) -> Unit,
  onLongClick: (() -> Unit)? = null
) {
  val interactionSource = remember { MutableInteractionSource() }
  Box(
    modifier = Modifier.requiredSize(36.dp)
      .combinedClickable(
        onClick = { sendMessage(null) },
        onLongClick = onLongClick,
        enabled = enabled,
        role = Role.Button,
        interactionSource = interactionSource,
        indication = rememberRipple(bounded = false, radius = 24.dp)
      ),
    contentAlignment = Alignment.Center
  ) {
    Icon(
      icon,
      stringResource(R.string.icon_descr_send_message),
      tint = Color.White,
      modifier = Modifier
        .size(sizeDp.value.dp)
        .padding(4.dp)
        .alpha(alpha.value)
        .clip(CircleShape)
        .background(if (enabled) MaterialTheme.colors.primary else MaterialTheme.colors.secondary)
        .padding(3.dp)
    )
  }
}

@Composable
private fun StartLiveMessageButton(enabled: Boolean, onClick: () -> Unit) {
  val interactionSource = remember { MutableInteractionSource() }
  Box(
    modifier = Modifier.requiredSize(36.dp)
      .clickable(
        onClick = onClick,
        enabled = enabled,
        role = Role.Button,
        interactionSource = interactionSource,
        indication = rememberRipple(bounded = false, radius = 24.dp)
      ),
    contentAlignment = Alignment.Center
  ) {
    Icon(
      BoltFilled,
      stringResource(R.string.icon_descr_send_message),
      tint = if (enabled) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
      modifier = Modifier
        .size(36.dp)
        .padding(4.dp)
    )
  }
}

private fun startLiveMessage(
  scope: CoroutineScope,
  send: suspend () -> Unit,
  update: suspend () -> Unit,
  sendButtonSize: Animatable<Float, AnimationVector1D>,
  sendButtonAlpha: Animatable<Float, AnimationVector1D>,
  composeState: MutableState<ComposeState>,
  liveMessageAlertShown: SharedPreference<Boolean>
) {
  fun run() {
    scope.launch {
      while (composeState.value.liveMessage != null) {
        sendButtonSize.animateTo(if (sendButtonSize.value == 36f) 32f else 36f, tween(700, 50))
      }
      sendButtonSize.snapTo(36f)
    }
    scope.launch {
      while (composeState.value.liveMessage != null) {
        sendButtonAlpha.animateTo(if (sendButtonAlpha.value == 1f) 0.75f else 1f, tween(700, 50))
      }
      sendButtonAlpha.snapTo(1f)
    }
    scope.launch {
      delay(3000)
      while (composeState.value.liveMessage != null) {
        update()
        delay(3000)
      }
    }
  }

  fun start() = withBGApi {
    if (composeState.value.liveMessage == null) {
      send()
    }
    run()
  }

  if (liveMessageAlertShown.state.value) {
    start()
  } else {
    AlertManager.shared.showAlertDialog(
      title = generalGetString(R.string.live_message),
      text = generalGetString(R.string.send_live_message_desc),
      confirmText = generalGetString(R.string.send_verb),
      onConfirm = {
        liveMessageAlertShown.set(true)
        start()
      })
  }
}

private fun showNeedToAllowVoiceAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.allow_voice_messages_question),
    text = generalGetString(R.string.you_need_to_allow_to_send_voice),
    confirmText = generalGetString(R.string.allow_verb),
    dismissText = generalGetString(R.string.cancel_verb),
    onConfirm = onConfirm,
  )
}

private fun showDisabledVoiceAlert(isDirectChat: Boolean) {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(R.string.voice_messages_prohibited),
    text = generalGetString(
      if (isDirectChat)
        R.string.ask_your_contact_to_enable_voice
      else
        R.string.only_group_owners_can_enable_voice
    )
  )
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewSendMsgView() {
  val smallFont = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  val textStyle = remember { mutableStateOf(smallFont) }
  SimpleXTheme {
    SendMsgView(
      composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = true)) },
      showVoiceRecordIcon = false,
      recState = remember { mutableStateOf(RecordingState.NotStarted) },
      isDirectChat = true,
      liveMessageAlertShown = SharedPreference(get = { true }, set = { }),
      needToAllowVoiceToContact = false,
      allowedVoiceByPrefs = true,
      userIsObserver = false,
      userCanSend = true,
      allowVoiceToContact = {},
      timedMessageAllowed = false,
      sendMessage = {},
      onMessageChange = { _ -> },
      textStyle = textStyle
    )
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewSendMsgViewEditing() {
  val smallFont = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  val textStyle = remember { mutableStateOf(smallFont) }
  val composeStateEditing = ComposeState(editingItem = ChatItem.getSampleData(), useLinkPreviews = true)
  SimpleXTheme {
    SendMsgView(
      composeState = remember { mutableStateOf(composeStateEditing) },
      showVoiceRecordIcon = false,
      recState = remember { mutableStateOf(RecordingState.NotStarted) },
      isDirectChat = true,
      liveMessageAlertShown = SharedPreference(get = { true }, set = { }),
      needToAllowVoiceToContact = false,
      allowedVoiceByPrefs = true,
      userIsObserver = false,
      userCanSend = true,
      allowVoiceToContact = {},
      timedMessageAllowed = false,
      sendMessage = {},
      onMessageChange = { _ -> },
      textStyle = textStyle
    )
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewSendMsgViewInProgress() {
  val smallFont = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  val textStyle = remember { mutableStateOf(smallFont) }
  val composeStateInProgress = ComposeState(preview = ComposePreview.FilePreview("test.txt", getAppFileUri("test.txt")), inProgress = true, useLinkPreviews = true)
  SimpleXTheme {
    SendMsgView(
      composeState = remember { mutableStateOf(composeStateInProgress) },
      showVoiceRecordIcon = false,
      recState = remember { mutableStateOf(RecordingState.NotStarted) },
      isDirectChat = true,
      liveMessageAlertShown = SharedPreference(get = { true }, set = { }),
      needToAllowVoiceToContact = false,
      allowedVoiceByPrefs = true,
      userIsObserver = false,
      userCanSend = true,
      allowVoiceToContact = {},
      timedMessageAllowed = false,
      sendMessage = {},
      onMessageChange = { _ -> },
      textStyle = textStyle
    )
  }
}
