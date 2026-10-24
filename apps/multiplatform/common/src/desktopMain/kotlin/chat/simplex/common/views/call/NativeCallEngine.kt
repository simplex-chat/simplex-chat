package chat.simplex.common.views.call

import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import dev.onvoid.webrtc.*
import dev.onvoid.webrtc.media.MediaDevices
import dev.onvoid.webrtc.media.MediaStreamTrack
import dev.onvoid.webrtc.media.MediaType
import dev.onvoid.webrtc.media.audio.*
import dev.onvoid.webrtc.media.video.*
import dev.onvoid.webrtc.media.video.desktop.DesktopSource
import dev.onvoid.webrtc.media.video.desktop.ScreenCapturer
import dev.onvoid.webrtc.media.video.desktop.WindowCapturer
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.serialization.Serializable
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.json.Json
import java.util.concurrent.Executors
import kotlin.coroutines.resume
import kotlin.coroutines.resumeWithException

data class ScreenShareSource(val title: String, val id: Long, val isWindow: Boolean)

// Native counterpart of call.ts processCommand: consumes WCallCommand and reports WCallResponse with the same
// semantics, so it interoperates with call.ts (Android, older desktop) and iOS peers.
// All state is confined to one thread; webrtc callbacks are posted onto it.
class NativeCallEngine(audioSettings: CallAudioSettings, private val onAudioDeviceNotice: (AudioDeviceNotice) -> Unit, private val onResponse: (WVAPIMessage) -> Unit) {
  private val executor = Executors.newSingleThreadExecutor { Thread(it, "call-engine").apply { isDaemon = true } }
  private val scope = CoroutineScope(executor.asCoroutineDispatcher() + SupervisorJob())

  // Without any audio backend the call can still receive video, so audio falls back to the dummy layer
  private val audioModule = try {
    CubebAudioDeviceModule()
  } catch (e: Error) {
    Log.e(TAG, "NativeCallEngine audio devices unavailable: ${e.message}")
    AudioDeviceModule(AudioLayer.kDummyAudio)
  }
  private val cubebModule = audioModule as? CubebAudioDeviceModule
  private val audioDeviceChoices = cubebModule?.let { m ->
    listOf(
      AudioDeviceChoice(AudioDeviceKind.Mic, audioSettings.mic, m::getRecordingDevices, m::getRecordingDeviceInUse, m::setRecordingDevice, m::setDefaultRecordingDevice),
      AudioDeviceChoice(AudioDeviceKind.Speaker, audioSettings.speaker, m::getPlayoutDevices, m::getPlayoutDeviceInUse, m::setPlayoutDevice, m::setDefaultPlayoutDevice),
    )
  }.orEmpty()
  private val factory = PeerConnectionFactory(audioModule)
  private val micSource = factory.createAudioSource(AudioOptions().apply {
    echoCancellation = true
    autoGainControl = true
    noiseSuppression = true
  })
  private val micTrack = factory.createAudioTrack("mic", micSource)
  private var cameraSource: VideoDeviceSource? = null
  private var cameraTrack: VideoTrack? = null
  private var screenSource: VideoDesktopSource? = null
  private var screenTrack: VideoTrack? = null
  private val placeholderVideoSource = CustomVideoSource()
  private val placeholderTracks = ArrayList<MediaStreamTrack>()

  private var pc: RTCPeerConnection? = null
  private val slots = ArrayList<TransceiverSlot>()
  private var senderCipher: CallFrameCipher? = null
  private var receiverCipher: CallFrameCipher? = null
  private var micEnabled = true
  private var peerHasOldVersion = false
  private val peerMedia = HashMap<CallMediaSource, Boolean>()
  private val pendingCandidates = ArrayList<IceCandidateJson>()
  private val gatheredCandidates = ArrayList<IceCandidateJson>()
  private var iceGatheringComplete = CompletableDeferred<Unit>()
  private var connectionTimeout: Job? = null
  private var reconnectingTimeout: Job? = null
  private var iceRestartTimeout: Job? = null
  private var wasConnected = false
  private var statsPolling: Job? = null
  private val videoSinks = ArrayList<Pair<VideoTrack, VideoTrackSink>>()
  private var closed = false

  private val _localCamera = MutableStateFlow<VideoTrack?>(null)
  val localCamera: StateFlow<VideoTrack?> = _localCamera
  private val _remoteCamera = MutableStateFlow<VideoTrack?>(null)
  val remoteCamera: StateFlow<VideoTrack?> = _remoteCamera
  private val _remoteScreen = MutableStateFlow<VideoTrack?>(null)
  val remoteScreen: StateFlow<VideoTrack?> = _remoteScreen

  var screenShareSource: ScreenShareSource? = null
  private var remoteAudioEnabled = true

  init {
    cubebModule?.run {
      setRecordingFallbackEnabled(audioSettings.fallback)
      setPlayoutFallbackEnabled(audioSettings.fallback)
      setDevicesChangedListener { scope.launch { updateAudioDevices() } }
    }
    scope.launch { updateAudioDevices() }
  }

  fun process(command: WCallCommand) {
    scope.launch {
      val resp = try {
        handle(command)
      } catch (e: Exception) {
        Log.e(TAG, "NativeCallEngine ${command.javaClass.simpleName}: ${e.stackTraceToString()}")
        WCallResponse.Error("${commandName(command)}: ${e.message}")
      }
      onResponse(WVAPIMessage(resp = resp, command = command))
    }
  }

  fun addVideoSink(track: VideoTrack, sink: VideoTrackSink) {
    scope.launch {
      if (closed) return@launch
      track.addSink(sink)
      videoSinks.add(track to sink)
    }
  }

  fun removeVideoSink(track: VideoTrack, sink: VideoTrackSink) {
    scope.launch {
      if (videoSinks.remove(track to sink)) track.removeSink(sink)
    }
  }

  fun setRemoteAudioEnabled(enabled: Boolean) {
    scope.launch {
      remoteAudioEnabled = enabled
      slots.forEach { if (it.receiver.track is AudioTrack) it.receiver.track.isEnabled = enabled }
    }
  }

  fun selectCamera(device: VideoDevice) {
    scope.launch {
      selectedCamera = device
      if (cameraSource != null) {
        slot(CallMediaSource.Camera)?.sender?.replaceTrack(null)
        stopCamera()
        startCamera()
        slot(CallMediaSource.Camera)?.sender?.replaceTrack(cameraTrack)
      }
    }
  }

  fun audioDevices(kind: AudioDeviceKind): List<AudioDevice> = audioDeviceChoices.firstOrNull { it.kind == kind }?.devices().orEmpty()

  fun selectAudioDevice(kind: AudioDeviceKind, device: AudioDevice?) {
    scope.launch {
      try {
        audioDeviceChoices.firstOrNull { it.kind == kind }?.choose(device)
      } catch (e: Error) {
        Log.e(TAG, "NativeCallEngine select $kind: ${e.message}")
      }
    }
  }

  fun close(): Job =
    scope.launch {
      closed = true
      endCall()
      micTrack.dispose()
      micSource.dispose()
      placeholderVideoSource.dispose()
      factory.dispose()
      audioModule.dispose()
      scope.cancel()
      executor.shutdown()
    }

  private suspend fun handle(command: WCallCommand): WCallResponse = when (command) {
    is WCallCommand.Capabilities ->
      if (MediaDevices.getAudioCaptureDevices().isEmpty()) {
        WCallResponse.Error("capabilities: no permissions were granted for mic and/or camera")
      } else {
        if (command.media == CallMediaType.Video) startCamera()
        WCallResponse.Capabilities(CallCapabilities(encryption = true))
      }
    is WCallCommand.Start -> {
      if (pc != null) endCall()
      if (command.media == CallMediaType.Video && cameraSource == null) startCamera()
      val conn = createConnection(command.iceServers, command.relay, command.aesKey)
      addTransceivers(conn)
      setupCodecsAndEncryption()
      val offer = conn.awaitCreateOffer()
      conn.awaitSetLocalDescription(offer)
      setupAudioSending()
      WCallResponse.Offer(serializeSdp(offer), initialIceCandidates(), CallCapabilities(encryption = true))
    }
    is WCallCommand.Offer ->
      if (pc != null) {
        WCallResponse.Error("accept: call already started")
      } else {
        val conn = createConnection(command.iceServers, command.relay, command.aesKey)
        conn.awaitSetRemoteDescription(parseSdp(command.offer))
        conn.transceivers.forEach { slots.add(TransceiverSlot(it)) }
        slot(CallMediaSource.Mic)?.sender?.replaceTrack(micTrack)
        slot(CallMediaSource.Camera)?.sender?.replaceTrack(cameraTrack)
        slots.forEach { it.transceiver.direction = RTCRtpTransceiverDirection.SEND_RECV }
        adaptToOldVersion(slots.size <= 2)
        setupCodecsAndEncryption()
        startPeerMediaPolling(conn)
        val answer = conn.awaitCreateAnswer()
        conn.awaitSetLocalDescription(answer)
        setupAudioSending()
        addIceCandidates(conn, parseIceCandidates(command.iceCandidates) + pendingCandidates)
        pendingCandidates.clear()
        WCallResponse.Answer(serializeSdp(answer), initialIceCandidates())
      }
    is WCallCommand.Answer -> {
      val conn = pc
      when {
        conn == null -> WCallResponse.Error("answer: call not started")
        conn.localDescription == null -> WCallResponse.Error("answer: local description is not set")
        conn.remoteDescription != null -> WCallResponse.Error("answer: remote description already set")
        else -> {
          conn.awaitSetRemoteDescription(parseSdp(command.answer))
          adaptToOldVersion(slot(CallMediaSource.ScreenAudio)?.transceiver?.currentDirection == RTCRtpTransceiverDirection.SEND_ONLY)
          startPeerMediaPolling(conn)
          addIceCandidates(conn, parseIceCandidates(command.iceCandidates) + pendingCandidates)
          pendingCandidates.clear()
          WCallResponse.Ok
        }
      }
    }
    is WCallCommand.Ice -> {
      val conn = pc
      val candidates = parseIceCandidates(command.iceCandidates)
      if (conn != null && command.session != null) {
        receiveRestartSession(conn, command.session, candidates)
        WCallResponse.Ok
      } else if (conn?.remoteDescription != null) {
        addIceCandidates(conn, candidates)
        WCallResponse.Ok
      } else {
        pendingCandidates.addAll(candidates)
        WCallResponse.Error("ice: call not started yet, will add candidates later")
      }
    }
    is WCallCommand.Media -> enableMedia(command.source, command.enable)
    is WCallCommand.End -> {
      endCall()
      WCallResponse.Ok
    }
    is WCallCommand.Camera -> WCallResponse.Error("camera: switching front/back camera is not supported")
    is WCallCommand.Layout -> WCallResponse.Ok
  }

  private fun enableMedia(source: CallMediaSource, enable: Boolean): WCallResponse = when (source) {
    CallMediaSource.Mic -> {
      micEnabled = enable
      slot(CallMediaSource.Mic)?.let { setSending(it, enable) }
      WCallResponse.Ok
    }
    CallMediaSource.Camera ->
      if (enable && peerHasOldVersion && peerMedia[CallMediaSource.Camera] != true) {
        WCallResponse.Error("media: cannot enable camera since the peer has an old version")
      } else if (enable) {
        if (cameraSource == null) startCamera()
        if (cameraTrack == null) {
          WCallResponse.Error("media: cannot enable media source")
        } else {
          slot(CallMediaSource.Camera)?.sender?.replaceTrack(cameraTrack)
          WCallResponse.Ok
        }
      } else {
        slot(CallMediaSource.Camera)?.sender?.replaceTrack(null)
        stopCamera()
        WCallResponse.Ok
      }
    CallMediaSource.ScreenVideo ->
      if (enable) {
        if (screenSource == null) startScreenShare()
        val screenSlot = slot(CallMediaSource.ScreenVideo)
        if (screenTrack == null || screenSlot == null) {
          stopScreenShare()
          WCallResponse.Error("media: cannot enable media source")
        } else {
          screenSlot.sender.replaceTrack(screenTrack)
          WCallResponse.Ok
        }
      } else {
        slot(CallMediaSource.ScreenVideo)?.sender?.replaceTrack(null)
        stopScreenShare()
        WCallResponse.Ok
      }
    CallMediaSource.ScreenAudio -> WCallResponse.Error("media: sending screen audio is not supported")
  }

  private fun createConnection(iceServers: List<RTCIceServer>?, relay: Boolean?, aesKey: String?): RTCPeerConnection {
    val config = RTCConfiguration().apply {
      this.iceServers = (iceServers ?: defaultIceServers).map { s ->
        dev.onvoid.webrtc.RTCIceServer().apply {
          urls = s.urls
          username = s.username
          password = s.credential
        }
      }
      iceTransportPolicy = if (relay == true) RTCIceTransportPolicy.RELAY else RTCIceTransportPolicy.ALL
    }
    startIceGathering()
    wasConnected = false
    peerMedia.clear()
    if (aesKey != null) {
      senderCipher = CallFrameCipher(aesKey)
      receiverCipher = CallFrameCipher(aesKey)
    }
    val conn = factory.createPeerConnection(config, observer)
    pc = conn
    connectionTimeout = scope.launch {
      delay(ANSWER_TIMEOUT_MS)
      onConnectionStateChange(conn)
    }
    return conn
  }

  // Transceiver order defines the m-line order that both peers map to media sources: mic, camera, screen audio, screen video.
  private fun addTransceivers(conn: RTCPeerConnection) {
    val micCamera = RTCRtpTransceiverInit().apply { direction = RTCRtpTransceiverDirection.SEND_RECV; streamIds = listOf("micCamera") }
    val screen = RTCRtpTransceiverInit().apply { direction = RTCRtpTransceiverDirection.SEND_RECV; streamIds = listOf("screenAudioVideo") }
    // webrtc-java can only add a transceiver with a track, so empty slots get a placeholder track that is detached at once.
    listOf(
      Triple(micTrack, micCamera, micTrack),
      Triple(cameraTrack ?: placeholderVideoTrack(), micCamera, cameraTrack),
      Triple(placeholderAudioTrack(), screen, null),
      Triple(placeholderVideoTrack(), screen, null),
    ).forEach { (track, init, sending) ->
      val slot = TransceiverSlot(conn.addTransceiver(track, init))
      slots.add(slot)
      slot.sender.replaceTrack(sending)
    }
  }

  // Device-captured audio reaches every audio sender regardless of its track, so audio is stopped by deactivating the
  // encoding. The stream then stops sending RTP, which peers detect as muted, as when call.ts detaches its track.
  private fun setupAudioSending() {
    slot(CallMediaSource.Mic)?.let { setSending(it, micEnabled) }
    slot(CallMediaSource.ScreenAudio)?.let { setSending(it, false) }
  }

  private fun setSending(slot: TransceiverSlot, sending: Boolean) {
    val parameters = slot.sender.parameters
    parameters.encodings.forEach { it.active = sending }
    slot.sender.parameters = parameters
  }

  private fun setupCodecsAndEncryption() {
    // call.ts keeps codec headers in the clear assuming VP8, so VP8 is preferred for camera and screen
    val vp8First = factory.getRtpReceiverCapabilities(MediaType.VIDEO).codecs.sortedBy { if (it.name == "VP8") 0 else 1 }
    slots.forEach { s ->
      val track = s.receiver.track
      if (track is VideoTrack) s.transceiver.setCodecPreferences(vp8First)
      if (track is AudioTrack) track.isEnabled = remoteAudioEnabled
      senderCipher?.let { c -> s.sender.setTransform(c::encrypt) }
      receiverCipher?.let { c -> s.receiver.setTransform(c::decrypt) }
    }
    _remoteCamera.value = slot(CallMediaSource.Camera)?.receiver?.track as? VideoTrack
    _remoteScreen.value = slot(CallMediaSource.ScreenVideo)?.receiver?.track as? VideoTrack
  }

  private fun adaptToOldVersion(oldVersion: Boolean) {
    peerHasOldVersion = oldVersion
    if (oldVersion) {
      onMediaMuteUnmute(CallMediaSource.Mic, mute = false)
      if (_remoteCamera.value != null) onMediaMuteUnmute(CallMediaSource.Camera, mute = false)
    }
  }

  private fun updateAudioDevices() {
    for (choice in audioDeviceChoices) {
      try {
        choice.update()?.let(onAudioDeviceNotice)
      } catch (e: Error) {
        Log.e(TAG, "NativeCallEngine update ${choice.kind}: ${e.message}")
      }
    }
  }

  private fun slot(source: CallMediaSource): TransceiverSlot? = slots.getOrNull(SLOT_SOURCES.indexOf(source))

  private var selectedCamera: VideoDevice? = null

  private fun startCamera() {
    val device = selectedCamera ?: MediaDevices.getVideoCaptureDevices().firstOrNull() ?: return
    val source = VideoDeviceSource()
    try {
      source.setVideoCaptureDevice(device)
      preferredCapability(device)?.let(source::setVideoCaptureCapability)
      source.start()
    } catch (e: Exception) {
      Log.e(TAG, "NativeCallEngine camera: ${e.stackTraceToString()}")
      source.dispose()
      return
    }
    cameraSource = source
    cameraTrack = factory.createVideoTrack("camera", source).also { _localCamera.value = it }
  }

  private fun stopCamera() {
    _localCamera.value?.let(::detachSinks)
    _localCamera.value = null
    cameraTrack?.dispose()
    cameraTrack = null
    cameraSource?.stop()
    cameraSource?.dispose()
    cameraSource = null
  }

  private fun startScreenShare() {
    val selected = screenShareSource ?: screenShareSources().firstOrNull() ?: return
    val source = VideoDesktopSource()
    try {
      source.setSourceId(selected.id, selected.isWindow)
      source.setFrameRate(SCREEN_FRAME_RATE)
      source.setMaxFrameSize(SCREEN_MAX_WIDTH, SCREEN_MAX_HEIGHT)
      source.start()
    } catch (e: Exception) {
      Log.e(TAG, "NativeCallEngine screen: ${e.stackTraceToString()}")
      source.dispose()
      return
    }
    screenSource = source
    screenTrack = factory.createVideoTrack("screen", source)
  }

  private fun stopScreenShare() {
    screenTrack?.dispose()
    screenTrack = null
    screenSource?.stop()
    screenSource?.dispose()
    screenSource = null
  }

  private fun placeholderVideoTrack(): VideoTrack =
    factory.createVideoTrack("placeholder-video", placeholderVideoSource).also { placeholderTracks.add(it) }

  private fun placeholderAudioTrack(): AudioTrack =
    factory.createAudioTrack("placeholder-audio", micSource).also { placeholderTracks.add(it) }

  private fun startIceGathering() {
    iceGatheringComplete = CompletableDeferred()
    gatheredCandidates.clear()
  }

  private suspend fun initialIceCandidates(): String {
    withTimeoutOrNull(ICE_INITIAL_DELAY_MS) { iceGatheringComplete.await() }
    val candidates = takeGatheredCandidates()
    scope.launch { sendMoreIceCandidates() }
    return candidates
  }

  private suspend fun sendMoreIceCandidates() {
    withTimeoutOrNull(ICE_EXTRAS_TIMEOUT_MS) {
      while (!iceGatheringComplete.isCompleted) {
        delay(ICE_EXTRAS_INTERVAL_MS)
        sendGatheredCandidates()
      }
    }
    sendGatheredCandidates()
  }

  private fun sendGatheredCandidates() {
    if (gatheredCandidates.isEmpty() || pc == null) return
    onResponse(WVAPIMessage(resp = WCallResponse.Ice(takeGatheredCandidates())))
  }

  private fun takeGatheredCandidates(): String {
    val candidates = LZString.compressToBase64(json.encodeToString(ListSerializer(IceCandidateJson.serializer()), gatheredCandidates))
    gatheredCandidates.clear()
    return candidates
  }

  private fun addIceCandidates(conn: RTCPeerConnection, candidates: List<IceCandidateJson>) {
    for (c in candidates) {
      try {
        conn.addIceCandidate(dev.onvoid.webrtc.RTCIceCandidate(c.sdpMid ?: "", c.sdpMLineIndex ?: 0, c.candidate))
      } catch (e: Exception) {
        Log.e(TAG, "NativeCallEngine addIceCandidate: ${e.message}")
      }
    }
  }

  private fun onConnectionStateChange(conn: RTCPeerConnection, reconnectTimedOut: Boolean = false) {
    if (conn !== pc) return
    var state = conn.connectionState.jsName()
    if (wasConnected) {
      if (reconnectTimedOut) {
        reconnectingTimeout = null
        if (state == "connected") return
        state = "disconnected"
      } else if (state == "disconnected" || state == "failed") {
        if (reconnectingTimeout != null) return
        reconnectingTimeout = scope.launch {
          delay(RECONNECT_TIMEOUT_MS)
          onConnectionStateChange(conn, reconnectTimedOut = true)
        }
        if (isIceRestarter(conn)) scope.launch { restartIce(conn) }
        state = "reconnecting"
      } else if (state == "connecting" || state == "new") {
        return
      }
    }
    onResponse(WVAPIMessage(resp = WCallResponse.Connection(ConnectionState(
      connectionState = state,
      iceConnectionState = conn.iceConnectionState.jsName(),
      iceGatheringState = conn.iceGatheringState.jsName(),
      signalingState = conn.signalingState.jsName()
    ))))
    when (state) {
      "disconnected", "failed" -> {
        connectionTimeout?.cancel()
        clearReconnecting()
        endCall()
        onResponse(WVAPIMessage(resp = WCallResponse.Ended))
      }
      "connected" -> {
        connectionTimeout?.cancel()
        clearReconnecting()
        wasConnected = true
        conn.getStats { report -> scope.launch { reportConnected(conn, report) } }
      }
      else -> {}
    }
  }

  private fun clearReconnecting() {
    reconnectingTimeout?.cancel()
    reconnectingTimeout = null
    iceRestartTimeout?.cancel()
    iceRestartTimeout = null
  }

  // Only the party that sent the initial offer restarts ICE, so that restart offers never collide
  private fun isIceRestarter(conn: RTCPeerConnection): Boolean = conn.localDescription?.sdpType == RTCSdpType.OFFER

  private suspend fun restartIce(conn: RTCPeerConnection) {
    if (conn !== pc || reconnectingTimeout == null) return
    iceRestartTimeout = scope.launch {
      delay(ICE_RESTART_INTERVAL_MS)
      restartIce(conn)
    }
    // the previous restart offer is not answered yet
    if (conn.signalingState != RTCSignalingState.STABLE) return
    try {
      val offer = conn.awaitCreateOffer(RTCOfferOptions().apply { iceRestart = true })
      startIceGathering()
      conn.awaitSetLocalDescription(offer)
      onResponse(WVAPIMessage(resp = WCallResponse.Ice(initialIceCandidates(), serializeSdp(offer))))
    } catch (e: Exception) {
      Log.e(TAG, "NativeCallEngine ICE restart: ${e.message}")
    }
  }

  private suspend fun receiveRestartSession(conn: RTCPeerConnection, session: String, iceCandidates: List<IceCandidateJson>) {
    val desc = parseSdp(session)
    if (desc.sdpType == RTCSdpType.OFFER && !isIceRestarter(conn)) {
      conn.awaitSetRemoteDescription(desc)
      startIceGathering()
      val answer = conn.awaitCreateAnswer()
      conn.awaitSetLocalDescription(answer)
      addIceCandidates(conn, iceCandidates)
      onResponse(WVAPIMessage(resp = WCallResponse.Ice(initialIceCandidates(), serializeSdp(answer))))
    } else if (desc.sdpType == RTCSdpType.ANSWER && conn.signalingState == RTCSignalingState.HAVE_LOCAL_OFFER) {
      conn.awaitSetRemoteDescription(desc)
      addIceCandidates(conn, iceCandidates)
    } else {
      error("unexpected ICE restart ${desc.sdpType.jsName()} in ${conn.signalingState.jsName()} state")
    }
  }

  private suspend fun reportConnected(conn: RTCPeerConnection, report: RTCStatsReport) {
    val stats = report.stats
    val pair = stats.values.firstOrNull { it.type == RTCStatsType.CANDIDATE_PAIR && it.attributes["state"] == "succeeded" } ?: return
    fun candidate(id: Any?) = stats[id as? String]?.attributes?.let { a ->
      RTCIceCandidate(
        candidateType = RTCIceCandidateType.entries.firstOrNull { it.value == a["candidateType"] },
        protocol = a["protocol"] as? String
      )
    }
    val info = ConnectionInfo(candidate(pair.attributes["localCandidateId"]), candidate(pair.attributes["remoteCandidateId"]))
    delay(CONNECTED_REPORT_DELAY_MS)
    if (conn === pc) onResponse(WVAPIMessage(resp = WCallResponse.Connected(info)))
  }

  // The peer stops sending a source by detaching its track, so a source is treated as muted after 3 seconds without received bytes.
  private fun startPeerMediaPolling(conn: RTCPeerConnection) {
    val receivers = slots.zip(SLOT_SOURCES) { s, source -> source to s.receiver }
    val lastBytes = HashMap<CallMediaSource, Long>()
    val mutedSeconds = HashMap<CallMediaSource, Int>()
    statsPolling?.cancel()
    statsPolling = scope.launch {
      while (isActive && conn === pc) {
        delay(PEER_MEDIA_POLL_MS)
        for ((source, receiver) in receivers) {
          val bytes = receivedBytes(conn, receiver) ?: continue
          if (bytes <= (lastBytes[source] ?: 0)) {
            val muted = (mutedSeconds[source] ?: MUTED_INITIALLY_SECONDS) + 1
            mutedSeconds[source] = muted
            if (muted == MUTED_AFTER_SECONDS) onMediaMuteUnmute(source, mute = true)
          } else {
            if ((mutedSeconds[source] ?: MUTED_INITIALLY_SECONDS) >= MUTED_AFTER_SECONDS) onMediaMuteUnmute(source, mute = false)
            lastBytes[source] = bytes
            mutedSeconds[source] = 0
          }
        }
      }
    }
  }

  private suspend fun receivedBytes(conn: RTCPeerConnection, receiver: RTCRtpReceiver): Long? =
    suspendCancellableCoroutine { cont ->
      conn.getStats(receiver) { report ->
        val bytes = report.stats.values.firstOrNull { it.type == RTCStatsType.INBOUND_RTP }?.attributes?.get("bytesReceived") as? Number
        cont.resume(bytes?.toLong())
      }
    }

  private fun onMediaMuteUnmute(source: CallMediaSource, mute: Boolean) {
    if ((peerMedia[source] ?: false) != mute) return
    peerMedia[source] = !mute
    onResponse(WVAPIMessage(resp = WCallResponse.PeerMedia(source, enabled = !mute)))
  }

  private fun detachSinks(track: VideoTrack) {
    val it = videoSinks.iterator()
    while (it.hasNext()) {
      val (t, sink) = it.next()
      if (t === track) {
        t.removeSink(sink)
        it.remove()
      }
    }
  }

  private fun endCall() {
    connectionTimeout?.cancel()
    clearReconnecting()
    statsPolling?.cancel()
    videoSinks.forEach { (track, sink) -> track.removeSink(sink) }
    videoSinks.clear()
    _remoteCamera.value = null
    _remoteScreen.value = null
    val conn = pc
    pc = null
    // Senders release their tracks synchronously here, while closing the connection tears them down asynchronously
    slots.forEach { it.sender.replaceTrack(null) }
    conn?.close()
    slots.forEach(TransceiverSlot::dispose)
    slots.clear()
    stopCamera()
    stopScreenShare()
    placeholderTracks.forEach { it.dispose() }
    placeholderTracks.clear()
    pendingCandidates.clear()
    senderCipher = null
    receiverCipher = null
  }

  private val observer = object : PeerConnectionObserver {
    override fun onIceCandidate(candidate: dev.onvoid.webrtc.RTCIceCandidate) {
      scope.launch { gatheredCandidates.add(IceCandidateJson(candidate.sdp, candidate.sdpMid, candidate.sdpMLineIndex)) }
    }

    override fun onIceGatheringChange(state: RTCIceGatheringState) {
      if (state == RTCIceGatheringState.COMPLETE) scope.launch { iceGatheringComplete.complete(Unit) }
    }

    // As in call.ts, "failed" is only acted on by the answer timeout until connected: it is reported early when the peer has not answered yet.
    override fun onConnectionChange(state: RTCPeerConnectionState) {
      scope.launch { if (state != RTCPeerConnectionState.FAILED || wasConnected) pc?.let(::onConnectionStateChange) }
    }
  }

  private fun commandName(command: WCallCommand): String = command.javaClass.simpleName.replaceFirstChar { it.lowercase() }

  private fun preferredCapability(device: VideoDevice): VideoCaptureCapability? =
    MediaDevices.getVideoCaptureCapabilities(device)
      .filter { it.width <= CAMERA_MAX_WIDTH }
      .maxWithOrNull(compareBy<VideoCaptureCapability> { it.frameRate >= CAMERA_FRAME_RATE }.thenBy { it.width })

  companion object {
    // Index of each source's transceiver, equal to its mid
    private val SLOT_SOURCES = listOf(CallMediaSource.Mic, CallMediaSource.Camera, CallMediaSource.ScreenAudio, CallMediaSource.ScreenVideo)
    private const val ANSWER_TIMEOUT_MS = 30_000L
    private const val RECONNECT_TIMEOUT_MS = 30_000L
    private const val ICE_RESTART_INTERVAL_MS = 10_000L
    private const val ICE_INITIAL_DELAY_MS = 750L
    private const val ICE_EXTRAS_INTERVAL_MS = 1_500L
    private const val ICE_EXTRAS_TIMEOUT_MS = 12_000L
    private const val CONNECTED_REPORT_DELAY_MS = 500L
    private const val PEER_MEDIA_POLL_MS = 1_000L
    private const val MUTED_INITIALLY_SECONDS = 4
    private const val MUTED_AFTER_SECONDS = 3
    private const val CAMERA_MAX_WIDTH = 1280
    private const val CAMERA_FRAME_RATE = 24
    private const val SCREEN_FRAME_RATE = 24
    private const val SCREEN_MAX_WIDTH = 1920
    private const val SCREEN_MAX_HEIGHT = 1080

    private val defaultIceServers = listOf(
      RTCIceServer(urls = listOf("stuns:stun.simplex.im:443")),
      RTCIceServer(urls = listOf("turns:turn.simplex.im:443?transport=tcp"), username = "private2", credential = "Hxuq2QxUjnhj96Zq2r4HjqHRj"),
    )

    private val json = Json { ignoreUnknownKeys = true; explicitNulls = false }

    private fun Enum<*>.jsName(): String = name.lowercase().replace('_', '-')

    fun serializeSdp(d: RTCSessionDescription): String =
      LZString.compressToBase64(json.encodeToString(SessionDescriptionJson.serializer(), SessionDescriptionJson(d.sdpType.jsName(), d.sdp)))

    fun parseSdp(s: String): RTCSessionDescription {
      val d = json.decodeFromString(SessionDescriptionJson.serializer(), LZString.decompressFromBase64(s) ?: error("invalid session description"))
      return RTCSessionDescription(RTCSdpType.valueOf(d.type.uppercase()), d.sdp)
    }

    fun parseIceCandidates(s: String): List<IceCandidateJson> =
      json.decodeFromString(ListSerializer(IceCandidateJson.serializer()), LZString.decompressFromBase64(s) ?: error("invalid ice candidates"))

    fun videoCaptureDevices(): List<VideoDevice> = MediaDevices.getVideoCaptureDevices()

    fun screenShareSources(): List<ScreenShareSource> {
      val screens = ScreenCapturer().run { try { desktopSources } finally { dispose() } }
      val windows = WindowCapturer().run { try { desktopSources } finally { dispose() } }
      return screens.map { it.toShareSource(isWindow = false) } + windows.map { it.toShareSource(isWindow = true) }
    }

    private fun DesktopSource.toShareSource(isWindow: Boolean) = ScreenShareSource(title, id, isWindow)
  }
}

// getTransceivers, getSender and getReceiver each return a new wrapper holding a native reference, so they are taken once and disposed
private class TransceiverSlot(val transceiver: RTCRtpTransceiver) {
  val sender: RTCRtpSender = transceiver.sender
  val receiver: RTCRtpReceiver = transceiver.receiver

  fun dispose() {
    sender.dispose()
    receiver.dispose()
    transceiver.dispose()
  }
}

@Serializable
data class SessionDescriptionJson(val type: String, val sdp: String)

@Serializable
data class IceCandidateJson(val candidate: String, val sdpMid: String? = null, val sdpMLineIndex: Int? = null)

private suspend fun RTCPeerConnection.awaitCreateOffer(options: RTCOfferOptions = RTCOfferOptions()): RTCSessionDescription =
  suspendCancellableCoroutine { cont -> createOffer(options, sessionObserver(cont)) }

private suspend fun RTCPeerConnection.awaitCreateAnswer(): RTCSessionDescription =
  suspendCancellableCoroutine { cont -> createAnswer(RTCAnswerOptions(), sessionObserver(cont)) }

private suspend fun RTCPeerConnection.awaitSetLocalDescription(d: RTCSessionDescription) =
  suspendCancellableCoroutine { cont -> setLocalDescription(d, setObserver(cont)) }

private suspend fun RTCPeerConnection.awaitSetRemoteDescription(d: RTCSessionDescription) =
  suspendCancellableCoroutine { cont -> setRemoteDescription(d, setObserver(cont)) }

private fun sessionObserver(cont: CancellableContinuation<RTCSessionDescription>) = object : CreateSessionDescriptionObserver {
  override fun onSuccess(description: RTCSessionDescription) = cont.resume(description)
  override fun onFailure(error: String) = cont.resumeWithException(IllegalStateException(error))
}

private fun setObserver(cont: CancellableContinuation<Unit>) = object : SetSessionDescriptionObserver {
  override fun onSuccess() = cont.resume(Unit)
  override fun onFailure(error: String) = cont.resumeWithException(IllegalStateException(error))
}
