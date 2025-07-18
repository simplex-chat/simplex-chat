package chat.simplex.common.views.chat

import androidx.compose.animation.core.*
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.*
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.draw.clip
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.semantics.Role
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatItem
import chat.simplex.common.platform.*
import chat.simplex.common.views.usersettings.showInDevelopingAlert
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource
import dev.icerock.moko.resources.compose.painterResource
import kotlinx.coroutines.*
import java.net.URI

@Composable
fun SendMsgView(
  composeState: MutableState<ComposeState>,
  showVoiceRecordIcon: Boolean,
  recState: MutableState<RecordingState>,
  isDirectChat: Boolean,
  liveMessageAlertShown: SharedPreference<Boolean>,
  sendMsgEnabled: Boolean,
  userCantSendReason: Pair<String, String?>?,
  sendButtonEnabled: Boolean,
  sendToConnect: (() -> Unit)? = null,
  hideSendButton: Boolean = false,
  nextConnect: Boolean,
  needToAllowVoiceToContact: Boolean,
  allowedVoiceByPrefs: Boolean,
  sendButtonColor: Color = MaterialTheme.colors.primary,
  allowVoiceToContact: () -> Unit,
  timedMessageAllowed: Boolean = false,
  customDisappearingMessageTimePref: SharedPreference<Int>? = null,
  placeholder: String,
  sendMessage: (Int?) -> Unit,
  sendLiveMessage: (suspend () -> Unit)? = null,
  updateLiveMessage: (suspend () -> Unit)? = null,
  cancelLiveMessage: (() -> Unit)? = null,
  editPrevMessage: () -> Unit,
  onFilesPasted: (List<URI>) -> Unit,
  onMessageChange: (ComposeMessage) -> Unit,
  textStyle: MutableState<TextStyle>,
  focusRequester: FocusRequester? = null,
  ) {
  val showCustomDisappearingMessageDialog = remember { mutableStateOf(false) }
  val padding = if (appPlatform.isAndroid) PaddingValues(vertical = 8.dp) else PaddingValues(top = 3.dp, bottom = 4.dp)
  Box(Modifier.padding(padding)) {
    val cs = composeState.value
    val showVoiceButton = !nextConnect && cs.message.text.isEmpty() && showVoiceRecordIcon && !composeState.value.editing &&
        !composeState.value.forwarding && cs.liveMessage == null && (cs.preview is ComposePreview.NoPreview || recState.value is RecordingState.Started) && (cs.contextItem !is ComposeContextItem.ReportedItem)
    val showDeleteTextButton = rememberSaveable { mutableStateOf(false) }
    val sendMsgButtonDisabled = !sendMsgEnabled || !cs.sendEnabled() ||
      (!allowedVoiceByPrefs && cs.preview is ComposePreview.VoicePreview) ||
        cs.endLiveDisabled ||
        !sendButtonEnabled
    val clicksOnTextFieldDisabled = !sendMsgEnabled || cs.preview is ComposePreview.VoicePreview || cs.inProgress
    PlatformTextField(
      composeState,
      sendMsgEnabled,
      disabledText = userCantSendReason?.first,
      sendMsgButtonDisabled,
      textStyle,
      showDeleteTextButton,
      if (clicksOnTextFieldDisabled) "" else placeholder,
      showVoiceButton,
      onMessageChange,
      editPrevMessage,
      onFilesPasted,
      focusRequester
    ) {
      if (!cs.inProgress) {
        if (sendToConnect != null) {
          sendToConnect()
        } else {
          sendMessage(null)
        }
      }
    }
    if (clicksOnTextFieldDisabled) {
      if (userCantSendReason != null) {
        Box(
          Modifier
            .matchParentSize()
            .clickable(indication = null, interactionSource = remember { MutableInteractionSource() }, onClick = {
              AlertManager.shared.showAlertMsg(
                title = generalGetString(MR.strings.cant_send_message_alert_title),
                text = userCantSendReason.second
              )
            })
        )
      } else {
        Box(
          Modifier
            .matchParentSize()
        )
      }
    }
    if (showDeleteTextButton.value) {
      DeleteTextButton(composeState)
    }
    Box(Modifier.align(Alignment.BottomEnd).padding(bottom = if (appPlatform.isAndroid) 0.dp else 5.sp.toDp() * fontSizeSqrtMultiplier)) {
      val sendButtonSize = remember { Animatable(36f) }
      val sendButtonAlpha = remember { Animatable(1f) }
      val scope = rememberCoroutineScope()
      LaunchedEffect(Unit) {
        // Making LiveMessage alive when screen orientation was changed
        if (cs.liveMessage != null && sendLiveMessage != null && updateLiveMessage != null) {
          startLiveMessage(scope, sendLiveMessage, updateLiveMessage, sendButtonSize, sendButtonAlpha, composeState, liveMessageAlertShown)
        }
      }
      when {
        cs.progressByTimeout -> ComposeProgressIndicator()
        cs.contextItem is ComposeContextItem.ReportedItem -> {
          SendMsgButton(painterResource(MR.images.ic_check_filled), sendButtonSize, sendButtonAlpha, sendButtonColor, !sendMsgButtonDisabled, sendToConnect, sendMessage)
        }
        showVoiceButton && sendMsgEnabled -> {
          Row(verticalAlignment = Alignment.CenterVertically) {
            val stopRecOnNextClick = remember { mutableStateOf(false) }
            when {
              needToAllowVoiceToContact || !allowedVoiceByPrefs -> {
                DisallowedVoiceButton {
                  if (needToAllowVoiceToContact) {
                    showNeedToAllowVoiceAlert(allowVoiceToContact)
                  } else {
                    showDisabledVoiceAlert(isDirectChat)
                  }
                }
              }
              !allowedToRecordVoiceByPlatform() ->
                VoiceButtonWithoutPermissionByPlatform()
              else ->
                RecordVoiceView(recState, stopRecOnNextClick)
            }
            if (sendLiveMessage != null
              && updateLiveMessage != null
              && (cs.preview !is ComposePreview.VoicePreview || !stopRecOnNextClick.value)
              && cs.contextItem is ComposeContextItem.NoContextItem
            ) {
              Spacer(Modifier.width(12.dp))
              StartLiveMessageButton {
                if (composeState.value.preview is ComposePreview.NoPreview) {
                  startLiveMessage(scope, sendLiveMessage, updateLiveMessage, sendButtonSize, sendButtonAlpha, composeState, liveMessageAlertShown)
                }
              }
            }
          }
        }
        cs.liveMessage?.sent == false && cs.message.text.isEmpty() -> {
          CancelLiveMessageButton {
            cancelLiveMessage?.invoke()
          }
        }
        else -> {
          val cs = composeState.value
          val icon = if (cs.editing || cs.liveMessage != null) painterResource(MR.images.ic_check_filled) else painterResource(MR.images.ic_arrow_upward)
          val showDropdown = rememberSaveable { mutableStateOf(false) }

          @Composable
          fun MenuItems(): List<@Composable () -> Unit> {
            val menuItems = mutableListOf<@Composable () -> Unit>()

            if (cs.liveMessage == null && !cs.editing && !nextConnect || sendMsgEnabled) {
              if (
                cs.preview !is ComposePreview.VoicePreview &&
                cs.contextItem is ComposeContextItem.NoContextItem &&
                sendLiveMessage != null && updateLiveMessage != null
              ) {
                menuItems.add {
                  ItemAction(
                    generalGetString(MR.strings.send_live_message),
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
                    generalGetString(MR.strings.disappearing_message),
                    painterResource(MR.images.ic_timer),
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

          if (!hideSendButton) {
            val menuItems = MenuItems()
            if (menuItems.isNotEmpty()) {
              SendMsgButton(icon, sendButtonSize, sendButtonAlpha, sendButtonColor, !sendMsgButtonDisabled, sendToConnect, sendMessage) { showDropdown.value = true }
              DefaultDropdownMenu(showDropdown) {
                menuItems.forEach { composable -> composable() }
              }
              CustomDisappearingMessageDialog(
                showCustomDisappearingMessageDialog,
                sendMessage = sendMessage,
                customDisappearingMessageTimePref = customDisappearingMessageTimePref
              )
            } else {
              SendMsgButton(icon, sendButtonSize, sendButtonAlpha, sendButtonColor, !sendMsgButtonDisabled, sendToConnect, sendMessage)
            }
          }
        }
      }
    }
  }
}

@Composable
expect fun allowedToRecordVoiceByPlatform(): Boolean

@Composable
expect fun VoiceButtonWithoutPermissionByPlatform()

@Composable
private fun CustomDisappearingMessageDialog(
  showMenu: MutableState<Boolean>,
  sendMessage: (Int?) -> Unit,
  customDisappearingMessageTimePref: SharedPreference<Int>?
) {
  DefaultDropdownMenu(showMenu) {
    Text(
      generalGetString(MR.strings.send_disappearing_message),
      Modifier.padding(vertical = DEFAULT_PADDING_HALF, horizontal = DEFAULT_PADDING * 1.5f),
      fontSize = 16.sp,
      color = MaterialTheme.colors.secondary
    )

    ItemAction(generalGetString(MR.strings.send_disappearing_message_30_seconds)) {
      sendMessage(30)
      showMenu.value = false
    }
    ItemAction(generalGetString(MR.strings.send_disappearing_message_1_minute)) {
      sendMessage(60)
      showMenu.value = false
    }
    ItemAction(generalGetString(MR.strings.send_disappearing_message_5_minutes)) {
      sendMessage(300)
      showMenu.value = false
    }
    ItemAction(generalGetString(MR.strings.send_disappearing_message_custom_time)) {
      showMenu.value = false
      val selectedDisappearingMessageTime = mutableStateOf(customDisappearingMessageTimePref?.get?.invoke() ?: 300)
      showCustomTimePickerDialog(
        selectedDisappearingMessageTime,
        title = generalGetString(MR.strings.delete_after),
        confirmButtonText = generalGetString(MR.strings.send_disappearing_message_send),
        confirmButtonAction = { ttl ->
          sendMessage(ttl)
          customDisappearingMessageTimePref?.set?.invoke(ttl)
        },
        cancel = { showMenu.value = false }
      )
    }
  }
}

@Composable
private fun BoxScope.DeleteTextButton(composeState: MutableState<ComposeState>) {
  IconButton(
    { composeState.value = composeState.value.copy(message = ComposeMessage()) },
    Modifier.align(Alignment.TopEnd).size(36.dp)
  ) {
    Icon(painterResource(MR.images.ic_close), null, Modifier.padding(7.dp).size(36.dp), tint = MaterialTheme.colors.secondary)
  }
}

@Composable
private fun RecordVoiceView(recState: MutableState<RecordingState>, stopRecOnNextClick: MutableState<Boolean>) {
  val rec: RecorderInterface = remember { RecorderNative() }
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
    val startRecording: () -> Unit = out@ {
      if (appPlatform.isDesktop) {
        return@out showInDevelopingAlert()
      }
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
private fun DisallowedVoiceButton(onClick: () -> Unit) {
  IconButton(onClick, Modifier.size(36.dp)) {
    Icon(
      painterResource(MR.images.ic_keyboard_voice),
      stringResource(MR.strings.icon_descr_record_voice_message),
      tint = MaterialTheme.colors.secondary,
      modifier = Modifier
        .size(36.dp)
        .padding(4.dp)
    )
  }
}

@Composable
fun VoiceButtonWithoutPermission(onClick: () -> Unit) {
  IconButton(onClick, Modifier.size(36.dp)) {
    Icon(
      painterResource(MR.images.ic_keyboard_voice_filled),
      stringResource(MR.strings.icon_descr_record_voice_message),
      tint = MaterialTheme.colors.primary,
      modifier = Modifier
        .size(34.dp)
        .padding(4.dp)
    )
  }
}

@Composable
private fun StopRecordButton(onClick: () -> Unit) {
  IconButton(onClick, Modifier.size(36.dp)) {
    Icon(
      painterResource(MR.images.ic_stop_filled),
      stringResource(MR.strings.icon_descr_record_voice_message),
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
      painterResource(MR.images.ic_keyboard_voice_filled),
      stringResource(MR.strings.icon_descr_record_voice_message),
      tint = MaterialTheme.colors.primary,
      modifier = Modifier
        .size(34.dp)
        .padding(4.dp)
    )
  }
}

@Composable
fun ComposeProgressIndicator() {
  CircularProgressIndicator(Modifier.size(36.dp).padding(4.dp), color = MaterialTheme.colors.secondary, strokeWidth = 2.5.dp)
}

@Composable
private fun CancelLiveMessageButton(
  onClick: () -> Unit
) {
  IconButton(onClick, Modifier.size(36.dp)) {
    Icon(
      painterResource(MR.images.ic_close),
      stringResource(MR.strings.icon_descr_cancel_live_message),
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
  sendButtonColor: Color,
  enabled: Boolean,
  sendToConnect: (() -> Unit)?,
  sendMessage: (Int?) -> Unit,
  onLongClick: (() -> Unit)? = null
) {
  val interactionSource = remember { MutableInteractionSource() }
  val ripple = remember { ripple(bounded = false, radius = 24.dp) }
  Box(
    modifier = Modifier.requiredSize(36.dp)
      .combinedClickable(
        onClick = {
          if (sendToConnect != null) {
            sendToConnect()
          } else {
            sendMessage(null)
          }
        },
        onLongClick = onLongClick,
        enabled = enabled,
        role = Role.Button,
        interactionSource = interactionSource,
        indication = ripple
      )
      .onRightClick { onLongClick?.invoke() },
    contentAlignment = Alignment.Center
  ) {
    Icon(
      icon,
      stringResource(MR.strings.icon_descr_send_message),
      tint = Color.White,
      modifier = Modifier
        .size(sizeDp.value.dp)
        .padding(4.dp)
        .alpha(alpha.value)
        .clip(CircleShape)
        .background(if (enabled) sendButtonColor else MaterialTheme.colors.secondary.copy(alpha = 0.75f))
        .padding(3.dp)
    )
  }
}

@Composable
private fun StartLiveMessageButton(onClick: () -> Unit) {
  val interactionSource = remember { MutableInteractionSource() }
  val ripple = remember { ripple(bounded = false, radius = 24.dp) }
  Box(
    modifier = Modifier.requiredSize(36.dp)
      .clickable(
        onClick = onClick,
        role = Role.Button,
        interactionSource = interactionSource,
        indication = ripple
      ),
    contentAlignment = Alignment.Center
  ) {
    Icon(
      BoltFilled,
      stringResource(MR.strings.icon_descr_send_message),
      tint = MaterialTheme.colors.primary,
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
      title = generalGetString(MR.strings.live_message),
      text = generalGetString(MR.strings.send_live_message_desc),
      confirmText = generalGetString(MR.strings.send_verb),
      onConfirm = {
        liveMessageAlertShown.set(true)
        start()
      })
  }
}

private fun showNeedToAllowVoiceAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.allow_voice_messages_question),
    text = generalGetString(MR.strings.you_need_to_allow_to_send_voice),
    confirmText = generalGetString(MR.strings.allow_verb),
    dismissText = generalGetString(MR.strings.cancel_verb),
    onConfirm = onConfirm,
  )
}

private fun showDisabledVoiceAlert(isDirectChat: Boolean) {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.voice_messages_prohibited),
    text = generalGetString(
      if (isDirectChat)
        MR.strings.ask_your_contact_to_enable_voice
      else
        MR.strings.only_group_owners_can_enable_voice
    )
  )
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
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
      sendMsgEnabled = true,
      userCantSendReason = null,
      sendButtonEnabled = true,
      nextConnect = false,
      needToAllowVoiceToContact = false,
      allowedVoiceByPrefs = true,
      allowVoiceToContact = {},
      timedMessageAllowed = false,
      placeholder = "",
      sendMessage = {},
      editPrevMessage = {},
      onMessageChange = { _ -> },
      onFilesPasted = {},
      textStyle = textStyle,
    )
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
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
      sendMsgEnabled = true,
      userCantSendReason = null,
      sendButtonEnabled = true,
      nextConnect = false,
      needToAllowVoiceToContact = false,
      allowedVoiceByPrefs = true,
      allowVoiceToContact = {},
      timedMessageAllowed = false,
      placeholder = "",
      sendMessage = {},
      editPrevMessage = {},
      onMessageChange = { _ -> },
      onFilesPasted = {},
      textStyle = textStyle,
    )
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
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
      sendMsgEnabled = true,
      userCantSendReason = null,
      sendButtonEnabled = true,
      nextConnect = false,
      needToAllowVoiceToContact = false,
      allowedVoiceByPrefs = true,
      allowVoiceToContact = {},
      timedMessageAllowed = false,
      placeholder = "",
      sendMessage = {},
      editPrevMessage = {},
      onMessageChange = { _ -> },
      onFilesPasted = {},
      textStyle = textStyle,
    )
  }
}
