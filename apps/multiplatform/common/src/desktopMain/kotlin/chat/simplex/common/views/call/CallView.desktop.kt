package chat.simplex.common.views.call

import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.snapshots.SnapshotStateList
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.drawscope.rotate
import androidx.compose.ui.graphics.drawscope.translate
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.graphics.toComposeImageBitmap
import androidx.compose.ui.semantics.Role
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.helpers.*
import chat.simplex.res.*
import dev.icerock.moko.resources.StringResource
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import dev.onvoid.webrtc.media.FourCC
import dev.onvoid.webrtc.media.audio.AudioDevice
import dev.onvoid.webrtc.media.video.VideoBufferConverter
import dev.onvoid.webrtc.media.video.VideoDevice
import dev.onvoid.webrtc.media.video.VideoFrame
import dev.onvoid.webrtc.media.video.VideoTrack
import dev.onvoid.webrtc.media.video.VideoTrackSink
import kotlinx.coroutines.isActive
import kotlinx.coroutines.flow.*
import kotlinx.datetime.Clock
import kotlinx.serialization.SerializationException
import org.jetbrains.skia.ColorAlphaType
import org.jetbrains.skia.ColorType
import org.jetbrains.skia.Image
import org.jetbrains.skia.ImageInfo
import kotlin.math.min

private val callForeground = Color(0xFFFFFFD8)
private val controlButtonSize = 56.dp
private val localVideoWidth = 240.dp

// Spec: spec/services/calls.md#ActiveCallView
@Composable
actual fun ActiveCallView() {
  val scope = rememberCoroutineScope()
  val engine = NativeCallController(chatModel.callCommand) { apiMsg ->
    Log.d(TAG, "received from NativeCallEngine: ${apiMsg.resp.javaClass.simpleName}")
    val call = chatModel.activeCall.value
    if (call != null) {
      Log.d(TAG, "has active call $call")
      val callRh = call.remoteHostId
      when (val r = apiMsg.resp) {
        is WCallResponse.Capabilities -> withBGApi {
          val callType = CallType(call.initialCallType, r.capabilities)
          chatModel.controller.apiSendCallInvitation(callRh, call.contact, callType)
          chatModel.activeCall.value = call.copy(callState = CallState.InvitationSent, localCapabilities = r.capabilities)
          CallSoundsPlayer.startConnectingCallSound(scope)
          activeCallWaitDeliveryReceipt(scope)
        }
        is WCallResponse.Offer -> withBGApi {
          chatModel.controller.apiSendCallOffer(callRh, call.contact, r.offer, r.iceCandidates, call.initialCallType, r.capabilities)
          chatModel.activeCall.value = call.copy(callState = CallState.OfferSent, localCapabilities = r.capabilities)
        }
        is WCallResponse.Answer -> withBGApi {
          chatModel.controller.apiSendCallAnswer(callRh, call.contact, r.answer, r.iceCandidates)
          chatModel.activeCall.value = call.copy(callState = CallState.Negotiated)
          CallSoundsPlayer.stop()
        }
        is WCallResponse.Ice -> withBGApi {
          chatModel.controller.apiSendCallExtraInfo(callRh, call.contact, r.session, r.iceCandidates)
        }
        is WCallResponse.Connection ->
          // the call is restored and NOT sent to the core
          // Makes sure the core DOES NOT record the duration wrong if a reconnect happens
          if (r.state.connectionState == "reconnecting") {
            if (call.callState != CallState.Reconnecting) {
              CallSoundsPlayer.startConnectingCallSound(scope)
            }
            chatModel.activeCall.value = call.copy(callState = CallState.Reconnecting)
          } else try {
            val callStatus = json.decodeFromString<WebRTCCallStatus>("\"${r.state.connectionState}\"")
            if (callStatus == WebRTCCallStatus.Connected) {
              if (call.callState == CallState.Reconnecting) {
                CallSoundsPlayer.stop()
              }
              // connectedAt is only set ONCE, this way call duration is NOT reset when the call is restored from reconnecting
              chatModel.activeCall.value = call.copy(callState = CallState.Connected, connectedAt = call.connectedAt ?: Clock.System.now())
            }
            withBGApi { chatModel.controller.apiCallStatus(callRh, call.contact, callStatus) }
          } catch (e: Throwable) {
            Log.d(TAG, "call status ${r.state.connectionState} not used")
          }
        is WCallResponse.Connected -> {
          chatModel.activeCall.value = call.copy(callState = CallState.Connected, connectionInfo = r.connectionInfo)
        }
        is WCallResponse.PeerMedia -> {
          val sources = call.peerMediaSources
          chatModel.activeCall.value = when (r.source) {
            CallMediaSource.Mic -> call.copy(peerMediaSources = sources.copy(mic = r.enabled))
            CallMediaSource.Camera -> call.copy(peerMediaSources = sources.copy(camera = r.enabled))
            CallMediaSource.ScreenAudio -> call.copy(peerMediaSources = sources.copy(screenAudio = r.enabled))
            CallMediaSource.ScreenVideo -> call.copy(peerMediaSources = sources.copy(screenVideo = r.enabled))
          }
        }
        is WCallResponse.End -> {
          withBGApi { chatModel.callManager.endCall(call) }
        }
        is WCallResponse.Ended -> {
          chatModel.activeCall.value = call.copy(callState = CallState.Ended)
          withBGApi { chatModel.callManager.endCall(call) }
          chatModel.showCallView.value = false
        }
        is WCallResponse.Ok -> when (val cmd = apiMsg.command) {
          is WCallCommand.Answer ->
            chatModel.activeCall.value = call.copy(callState = CallState.Negotiated)
          is WCallCommand.Media -> {
            val sources = call.localMediaSources
            when (cmd.source) {
              CallMediaSource.Mic -> chatModel.activeCall.value = call.copy(localMediaSources = sources.copy(mic = cmd.enable))
              CallMediaSource.Camera -> chatModel.activeCall.value = call.copy(localMediaSources = sources.copy(camera = cmd.enable))
              CallMediaSource.ScreenAudio -> chatModel.activeCall.value = call.copy(localMediaSources = sources.copy(screenAudio = cmd.enable))
              CallMediaSource.ScreenVideo -> chatModel.activeCall.value = call.copy(localMediaSources = sources.copy(screenVideo = cmd.enable))
            }
          }
          is WCallCommand.End ->
            chatModel.showCallView.value = false
          else -> {}
        }
        is WCallResponse.Error -> {
          if (apiMsg.command is WCallCommand.Capabilities) {
            AlertManager.shared.showAlertMsg(generalGetString(MR.strings.call_desktop_permission_denied_title))
            withBGApi { chatModel.callManager.endCall(call) }
          }
          Log.e(TAG, "ActiveCallView: command error ${r.message}")
        }
      }
    }
  }

  val call = chatModel.activeCall.value
  if (engine != null && call != null && !chatModel.activeCallViewIsCollapsed.value) {
    ActiveCallLayout(engine, call)
  }
  DisposableEffect(Unit) {
    chatModel.activeCallViewIsVisible.value = true
    // After the first call, End command gets added to the list which prevents making another calls
    chatModel.callCommand.removeAll { it is WCallCommand.End }
    onDispose {
      CallSoundsPlayer.stop()
      chatModel.activeCallViewIsVisible.value = false
      chatModel.callCommand.clear()
    }
  }
}

@Composable
private fun NativeCallController(callCommand: SnapshotStateList<WCallCommand>, onResponse: (WVAPIMessage) -> Unit): NativeCallEngine? {
  val engine = remember {
    try {
      NativeCallEngine(callAudioSettings(), ::showAudioDeviceNotice, onResponse)
    } catch (e: Throwable) {
      Log.e(TAG, "Unable to start call engine: ${e.stackTraceToString()}")
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.call_desktop_engine_error), e.message)
      chatModel.activeCall.value?.let { call -> withBGApi { chatModel.callManager.endCall(call) } }
      null
    }
  }
  if (engine != null) {
    DisposableEffect(engine) {
      onDispose { engine.close() }
    }
    LaunchedEffect(engine) {
      while (isActive) {
        // waiting for the first command would miss a command equal to the previous one
        snapshotFlow { callCommand.isNotEmpty() }.first { it }
        while (callCommand.isNotEmpty()) {
          val cmd = callCommand.removeFirstOrNull() ?: break
          Log.d(TAG, "NativeCallController executing ${cmd.javaClass.simpleName}")
          engine.process(cmd)
        }
      }
    }
  }
  return engine
}

@Composable
private fun ActiveCallLayout(engine: NativeCallEngine, call: Call) {
  val remoteCamera by engine.remoteCamera.collectAsState()
  val remoteScreen by engine.remoteScreen.collectAsState()
  val localCamera by engine.localCamera.collectAsState()
  val peer = call.peerMediaSources
  val mainVideo = if (peer.screenVideo) remoteScreen else if (peer.camera) remoteCamera else null
  Box(
    Modifier
      .fillMaxSize()
      .background(Color.Black)
      // consumes clicks so they don't reach the chat UI below
      .clickable(interactionSource = remember { MutableInteractionSource() }, indication = null, onClick = {})
  ) {
    if (mainVideo != null) {
      CallVideoView(engine, mainVideo, Modifier.fillMaxSize())
    } else {
      Column(Modifier.align(Alignment.Center), horizontalAlignment = Alignment.CenterHorizontally) {
        ProfileImage(size = 192.dp, image = call.contact.profile.image)
        CallInfoView(call, showName = true)
      }
    }
    Column(Modifier.align(Alignment.TopEnd).padding(DEFAULT_PADDING), horizontalAlignment = Alignment.End) {
      val camera = localCamera
      if (camera != null && call.localMediaSources.camera) {
        CallVideoView(engine, camera, Modifier.width(localVideoWidth).clip(RoundedCornerShape(8.dp)), sizeToFrame = true)
      }
      val remoteCameraBesideScreen = remoteCamera
      if (peer.screenVideo && peer.camera && remoteCameraBesideScreen != null) {
        Spacer(Modifier.height(DEFAULT_PADDING_HALF))
        CallVideoView(engine, remoteCameraBesideScreen, Modifier.width(localVideoWidth).clip(RoundedCornerShape(8.dp)), sizeToFrame = true)
      }
    }
    Column {
      CallAppBar(
        title = {
          if (mainVideo != null) {
            Text(call.contact.chatViewName, color = callForeground, style = MaterialTheme.typography.h2, overflow = TextOverflow.Ellipsis, maxLines = 1)
          }
        },
        onBack = { chatModel.activeCallViewIsCollapsed.value = true }
      )
      if (mainVideo != null) {
        Box(Modifier.padding(horizontal = DEFAULT_PADDING)) { CallInfoView(call, showName = false) }
      }
    }
    CallControls(engine, call, Modifier.align(Alignment.BottomCenter).padding(bottom = DEFAULT_PADDING * 3))
  }
}

@Composable
private fun CallInfoView(call: Call, showName: Boolean) {
  Column(horizontalAlignment = if (showName) Alignment.CenterHorizontally else Alignment.Start) {
    if (showName) Text(call.contact.chatViewName, color = callForeground, style = MaterialTheme.typography.h2)
    Text(call.callState.text, color = callForeground, style = MaterialTheme.typography.body2)
    val connInfo = call.connectionInfo
    val connInfoText = if (connInfo == null) "" else " (${connInfo.text})"
    Text(call.encryptionStatus + connInfoText, color = callForeground, style = MaterialTheme.typography.body2)
  }
}

@Composable
private fun CallControls(engine: NativeCallEngine, call: Call, modifier: Modifier) {
  val remoteAudioEnabled = remember { mutableStateOf(true) }
  // A video call starts with the camera marked on even when no camera could be opened
  val cameraOn = call.localMediaSources.camera && engine.localCamera.collectAsState().value != null
  Row(modifier, horizontalArrangement = Arrangement.spacedBy(DEFAULT_PADDING), verticalAlignment = Alignment.CenterVertically) {
    val sources = call.localMediaSources
    ControlButton(
      painterResource(if (sources.mic) MR.images.ic_mic else MR.images.ic_mic_off),
      if (sources.mic) MR.strings.icon_descr_audio_off else MR.strings.icon_descr_audio_on
    ) { chatModel.callCommand.add(WCallCommand.Media(CallMediaSource.Mic, enable = !sources.mic)) }
    ControlButton(
      painterResource(if (remoteAudioEnabled.value) MR.images.ic_volume_up else MR.images.ic_volume_off),
      if (remoteAudioEnabled.value) MR.strings.icon_descr_speaker_off else MR.strings.icon_descr_speaker_on
    ) {
      remoteAudioEnabled.value = !remoteAudioEnabled.value
      engine.setRemoteAudioEnabled(remoteAudioEnabled.value)
    }
    ControlButton(
      painterResource(if (cameraOn) MR.images.ic_videocam_filled else MR.images.ic_videocam_off),
      if (cameraOn) MR.strings.icon_descr_video_off else MR.strings.icon_descr_video_on
    ) { chatModel.callCommand.add(WCallCommand.Media(CallMediaSource.Camera, enable = !cameraOn)) }
    ScreenShareButton(engine, sources.screenVideo)
    CameraPickerButton(engine)
    AudioDevicePickerButton(engine, AudioDeviceKind.Mic, appPrefs.desktopCallMic, painterResource(MR.images.ic_keyboard_voice), MR.strings.permissions_record_audio)
    AudioDevicePickerButton(engine, AudioDeviceKind.Speaker, appPrefs.desktopCallSpeaker, painterResource(MR.images.ic_headphones), MR.strings.audio_device_speaker)
    ControlButton(painterResource(MR.images.ic_call_end_filled), MR.strings.icon_descr_hang_up, background = Color.Red) {
      withBGApi { chatModel.callManager.endCall(call) }
    }
  }
}

@Composable
private fun ScreenShareButton(engine: NativeCallEngine, sharing: Boolean) {
  val showMenu = remember { mutableStateOf(false) }
  val sources = remember { mutableStateOf(emptyList<ScreenShareSource>()) }
  Box {
    ControlButton(
      painterResource(if (sharing) MR.images.ic_stop_filled else MR.images.ic_desktop),
      if (sharing) MR.strings.icon_descr_screen_share_off else MR.strings.icon_descr_screen_share_on
    ) {
      if (sharing) {
        chatModel.callCommand.add(WCallCommand.Media(CallMediaSource.ScreenVideo, enable = false))
      } else {
        sources.value = NativeCallEngine.screenShareSources()
        showMenu.value = true
      }
    }
    DefaultDropdownMenu(showMenu) {
      var screenNumber = 0
      sources.value.forEach { source ->
        val title = if (!source.isWindow) {
          screenNumber++
          source.title.ifEmpty { String.format(generalGetString(MR.strings.call_screen_share_screen), screenNumber) }
        } else {
          source.title
        }
        ItemAction(title, painterResource(if (source.isWindow) MR.images.ic_more_horiz else MR.images.ic_desktop), onClick = {
          showMenu.value = false
          engine.screenShareSource = source
          chatModel.callCommand.add(WCallCommand.Media(CallMediaSource.ScreenVideo, enable = true))
        })
      }
    }
  }
}

@Composable
private fun CameraPickerButton(engine: NativeCallEngine) {
  val showMenu = remember { mutableStateOf(false) }
  val cameras = remember { mutableStateOf(emptyList<VideoDevice>()) }
  Box {
    ControlButton(painterResource(MR.images.ic_settings), MR.strings.permissions_camera) {
      cameras.value = NativeCallEngine.videoCaptureDevices()
      showMenu.value = true
    }
    DefaultDropdownMenu(showMenu) {
      if (cameras.value.isEmpty()) {
        Text(stringResource(MR.strings.camera_not_available), Modifier.padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF), color = MaterialTheme.colors.secondary)
      }
      cameras.value.forEach { camera ->
        ItemAction(camera.name, onClick = {
          showMenu.value = false
          engine.selectCamera(camera)
        })
      }
    }
  }
}

@Composable
private fun AudioDevicePickerButton(engine: NativeCallEngine, kind: AudioDeviceKind, pref: SharedPreference<String?>, icon: Painter, description: StringResource) {
  val showMenu = remember { mutableStateOf(false) }
  val devices = remember { mutableStateOf(emptyList<AudioDevice>()) }
  Box {
    ControlButton(icon, description) {
      devices.value = engine.audioDevices(kind)
      showMenu.value = true
    }
    DefaultDropdownMenu(showMenu) {
      val chosenId = storedAudioDevice(pref.state.value)?.id
      (listOf(null) + devices.value).forEach { device ->
        ItemAction(
          device?.name ?: stringResource(MR.strings.call_audio_device_system_default),
          { if (device?.descriptor == chosenId) Icon(painterResource(MR.images.ic_check), null, tint = MenuTextColor) },
          onClick = {
            showMenu.value = false
            pref.set(device?.let { json.encodeToString(SavedAudioDevice.serializer(), savedAudioDevice(it)) })
            engine.selectAudioDevice(kind, device)
          }
        )
      }
    }
  }
}

private fun callAudioSettings() = CallAudioSettings(
  mic = storedAudioDevice(appPrefs.desktopCallMic.get()),
  speaker = storedAudioDevice(appPrefs.desktopCallSpeaker.get()),
  fallback = appPrefs.privacyCallAudioFallback.get()
)

private fun storedAudioDevice(stored: String?): SavedAudioDevice? =
  try {
    stored?.let { json.decodeFromString<SavedAudioDevice>(it) }
  } catch (e: SerializationException) {
    Log.e(TAG, "stored audio device: ${e.message}")
    null
  }

private fun showAudioDeviceNotice(notice: AudioDeviceNotice) {
  val text = when (notice.kind) {
    AudioDeviceKind.Mic -> when (notice.state) {
      AudioDeviceState.Connected -> MR.strings.call_mic_connected
      AudioDeviceState.Fallback -> MR.strings.call_mic_fallback
      AudioDeviceState.Unavailable -> MR.strings.call_mic_unavailable
    }
    AudioDeviceKind.Speaker -> when (notice.state) {
      AudioDeviceState.Connected -> MR.strings.call_speaker_connected
      AudioDeviceState.Fallback -> MR.strings.call_speaker_fallback
      AudioDeviceState.Unavailable -> MR.strings.call_speaker_unavailable
    }
  }
  showToast(String.format(generalGetString(text), notice.deviceName))
}

@Composable
private fun ControlButton(icon: Painter, description: StringResource, background: Color = Color.White.copy(alpha = 0.2f), action: () -> Unit) {
  Box(
    Modifier
      .size(controlButtonSize)
      .background(background, CircleShape)
      .clip(CircleShape)
      .clickable(onClick = action, role = Role.Button),
    contentAlignment = Alignment.Center
  ) {
    Icon(icon, stringResource(description), Modifier.padding(controlButtonSize * 0.2f).fillMaxSize(), tint = callForeground)
  }
}

private class RenderedFrame(val image: ImageBitmap, val rotation: Int)

@Composable
private fun CallVideoView(engine: NativeCallEngine, track: VideoTrack, modifier: Modifier, sizeToFrame: Boolean = false) {
  val frame = remember(track) { mutableStateOf<RenderedFrame?>(null) }
  DisposableEffect(track) {
    val sink = VideoTrackSink { videoFrame -> frame.value = RenderedFrame(videoFrame.toImageBitmap(), videoFrame.rotation) }
    engine.addVideoSink(track, sink)
    onDispose { engine.removeVideoSink(track, sink) }
  }
  val f = frame.value ?: return
  val rotated = f.rotation % 180 != 0
  val width = if (rotated) f.image.height else f.image.width
  val height = if (rotated) f.image.width else f.image.height
  Canvas(if (sizeToFrame) modifier.aspectRatio(width.toFloat() / height) else modifier) {
    val scale = min(size.width / width, size.height / height)
    val dstSize = IntSize((f.image.width * scale).toInt(), (f.image.height * scale).toInt())
    translate(size.width / 2, size.height / 2) {
      rotate(f.rotation.toFloat(), pivot = Offset.Zero) {
        drawImage(f.image, dstOffset = IntOffset(-dstSize.width / 2, -dstSize.height / 2), dstSize = dstSize)
      }
    }
  }
}

private fun VideoFrame.toImageBitmap(): ImageBitmap {
  val i420 = buffer.toI420()
  try {
    val width = i420.width
    val height = i420.height
    val pixels = ByteArray(width * height * 4)
    // libyuv names formats by 32-bit word, so ARGB is laid out in memory as B, G, R, A
    VideoBufferConverter.convertFromI420(i420, pixels, FourCC.ARGB)
    val info = ImageInfo(width, height, ColorType.BGRA_8888, ColorAlphaType.OPAQUE)
    return Image.makeRaster(info, pixels, width * 4).toComposeImageBitmap()
  } finally {
    i420.release()
  }
}
