package chat.simplex.common.views.call

import androidx.compose.runtime.*
import androidx.compose.runtime.snapshots.SnapshotStateList
import androidx.compose.ui.platform.LocalUriHandler
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.*
import kotlinx.datetime.Clock
import kotlinx.serialization.encodeToString
import org.nanohttpd.protocols.http.IHTTPSession
import org.nanohttpd.protocols.http.response.Response
import org.nanohttpd.protocols.http.response.Response.newFixedLengthResponse
import org.nanohttpd.protocols.http.response.Status
import org.nanohttpd.protocols.websockets.*
import java.io.IOException
import java.net.URI

private const val SERVER_HOST = "localhost"
private const val SERVER_PORT = 50395
val connections = ArrayList<WebSocket>()

@Composable
actual fun ActiveCallView() {
  val endCall = {
    val call = chatModel.activeCall.value
    if (call != null) withBGApi { chatModel.callManager.endCall(call) }
  }
  BackHandler(onBack = endCall)
  val scope = rememberCoroutineScope()
  WebRTCController(chatModel.callCommand) { apiMsg ->
    Log.d(TAG, "received from WebRTCController: $apiMsg")
    val call = chatModel.activeCall.value
    if (call != null) {
      Log.d(TAG, "has active call $call")
      val callRh = call.remoteHostId
      when (val r = apiMsg.resp) {
        is WCallResponse.Capabilities -> withBGApi {
          val callType = CallType(call.localMedia, r.capabilities)
          chatModel.controller.apiSendCallInvitation(callRh, call.contact, callType)
          chatModel.activeCall.value = call.copy(callState = CallState.InvitationSent, localCapabilities = r.capabilities)
          CallSoundsPlayer.startConnectingCallSound(scope)
          withBGApi {
            delay(7000)
            CallSoundsPlayer.startInCallSound(scope)
          }
        }
        is WCallResponse.Offer -> withBGApi {
          chatModel.controller.apiSendCallOffer(callRh, call.contact, r.offer, r.iceCandidates, call.localMedia, r.capabilities)
          chatModel.activeCall.value = call.copy(callState = CallState.OfferSent, localCapabilities = r.capabilities)
        }
        is WCallResponse.Answer -> withBGApi {
          chatModel.controller.apiSendCallAnswer(callRh, call.contact, r.answer, r.iceCandidates)
          chatModel.activeCall.value = call.copy(callState = CallState.Negotiated)
          CallSoundsPlayer.stop()
        }
        is WCallResponse.Ice -> withBGApi {
          chatModel.controller.apiSendCallExtraInfo(callRh, call.contact, r.iceCandidates)
        }
        is WCallResponse.Connection ->
          try {
            val callStatus = json.decodeFromString<WebRTCCallStatus>("\"${r.state.connectionState}\"")
            if (callStatus == WebRTCCallStatus.Connected) {
              chatModel.activeCall.value = call.copy(callState = CallState.Connected, connectedAt = Clock.System.now())
            }
            withBGApi { chatModel.controller.apiCallStatus(callRh, call.contact, callStatus) }
          } catch (e: Throwable) {
            Log.d(TAG, "call status ${r.state.connectionState} not used")
          }
        is WCallResponse.Connected -> {
          chatModel.activeCall.value = call.copy(callState = CallState.Connected, connectionInfo = r.connectionInfo)
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
            when (cmd.media) {
              CallMediaType.Video -> chatModel.activeCall.value = call.copy(videoEnabled = cmd.enable)
              CallMediaType.Audio -> chatModel.activeCall.value = call.copy(audioEnabled = cmd.enable)
            }
          }
          is WCallCommand.Camera -> {
            chatModel.activeCall.value = call.copy(localCamera = cmd.camera)
            if (!call.audioEnabled) {
              chatModel.callCommand.add(WCallCommand.Media(CallMediaType.Audio, enable = false))
            }
          }
          is WCallCommand.End ->
            chatModel.showCallView.value = false
          else -> {}
        }
        is WCallResponse.Error -> {
          Log.e(TAG, "ActiveCallView: command error ${r.message}")
        }
      }
    }
  }

  SendStateUpdates()
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
private fun SendStateUpdates() {
  LaunchedEffect(Unit) {
    snapshotFlow { chatModel.activeCall.value }
      .distinctUntilChanged()
      .filterNotNull()
      .collect { call ->
        val state = call.callState.text
        val connInfo = call.connectionInfo
        val connInfoText = if (connInfo == null) ""  else " (${connInfo.text})"
        val description = call.encryptionStatus + connInfoText
        chatModel.callCommand.add(WCallCommand.Description(state, description))
      }
  }
}

@Composable
fun WebRTCController(callCommand: SnapshotStateList<WCallCommand>, onResponse: (WVAPIMessage) -> Unit) {
  val uriHandler = LocalUriHandler.current
  val endCall = {
    val call = chatModel.activeCall.value
    if (call != null) withBGApi { chatModel.callManager.endCall(call) }
  }
  val server = remember {
    try {
      uriHandler.openUri("http://${SERVER_HOST}:$SERVER_PORT/simplex/call/")
    } catch (e: Exception) {
      Log.e(TAG, "Unable to open browser: ${e.stackTraceToString()}")
      AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.unable_to_open_browser_title),
        text = generalGetString(MR.strings.unable_to_open_browser_desc)
      )
      endCall()
    }
    startServer(onResponse)
  }
  fun processCommand(cmd: WCallCommand) {
    val apiCall = WVAPICall(command = cmd)
    for (connection in connections.toList()) {
      try {
        connection.send(json.encodeToString(apiCall))
        break
      } catch (e: Exception) {
        Log.e(TAG, "Failed to send message to browser: ${e.stackTraceToString()}")
      }
    }
  }
  DisposableEffect(Unit) {
    onDispose {
      processCommand(WCallCommand.End)
      server.stop()
      connections.clear()
    }
  }
  LaunchedEffect(Unit) {
    snapshotFlow { callCommand.firstOrNull() }
      .distinctUntilChanged()
      .filterNotNull()
      .collect {
        while (connections.isEmpty()) {
          delay(100)
        }
        while (callCommand.isNotEmpty()) {
          val cmd = callCommand.removeFirst()
          Log.d(TAG, "WebRTCController LaunchedEffect executing $cmd")
          processCommand(cmd)
        }
      }
  }
}

fun startServer(onResponse: (WVAPIMessage) -> Unit): NanoWSD {
  val server = object: NanoWSD(SERVER_HOST, SERVER_PORT) {
    override fun openWebSocket(session: IHTTPSession): WebSocket = MyWebSocket(onResponse, session)

    fun resourcesToResponse(path: String): Response {
      val uri = Class.forName("chat.simplex.common.AppKt").getResource("/assets/www$path") ?: return resourceNotFound
      val response = newFixedLengthResponse(
        Status.OK, getMimeTypeForFile(uri.file),
        uri.openStream().readBytes()
      )
      response.setKeepAlive(true)
      response.setUseGzip(true)
      return response
    }

    val resourceNotFound = newFixedLengthResponse(Status.NOT_FOUND, "text/plain", "This page couldn't be found")

    override fun handle(session: IHTTPSession): Response {
      return when {
        session.headers["upgrade"] == "websocket" -> super.handle(session)
        session.uri.contains("/simplex/call/") -> resourcesToResponse("/desktop/call.html")
        else -> resourcesToResponse(URI.create(session.uri).path)
      }
    }
  }
  server.start(60_000_000)
  return server
}

class MyWebSocket(val onResponse: (WVAPIMessage) -> Unit, handshakeRequest: IHTTPSession) : WebSocket(handshakeRequest) {
  override fun onOpen() {
    connections.add(this)
  }

  override fun onClose(closeCode: CloseCode?, reason: String?, initiatedByRemote: Boolean) {
    onResponse(WVAPIMessage(null, WCallResponse.End))
  }

  override fun onMessage(message: WebSocketFrame) {
    Log.d(TAG, "MyWebSocket.onMessage")
    try {
      // for debugging
      // onResponse(message.textPayload)
      onResponse(json.decodeFromString(message.textPayload))
    } catch (e: Exception) {
      Log.e(TAG, "failed parsing browser message: $message")
    }
  }

  override fun onPong(pong: WebSocketFrame?) = Unit

  override fun onException(exception: IOException) {
    Log.e(TAG, "WebSocket exception: ${exception.stackTraceToString()}")
  }
}
