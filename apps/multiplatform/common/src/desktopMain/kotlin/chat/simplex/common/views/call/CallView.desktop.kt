package chat.simplex.common.views.call

import SectionItemView
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.snapshots.SnapshotStateList
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.delay
import kotlinx.serialization.decodeFromString
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
    if (call != null) withApi { chatModel.callManager.endCall(call) }
  }
  BackHandler(onBack = endCall)
  WebRTCController(chatModel.callCommand) { apiMsg ->
    Log.d(TAG, "received from WebRTCController: $apiMsg")
    val call = chatModel.activeCall.value
    if (call != null) {
      Log.d(TAG, "has active call $call")
      when (val r = apiMsg.resp) {
        is WCallResponse.Capabilities -> withApi {
          val callType = CallType(call.localMedia, r.capabilities)
          chatModel.controller.apiSendCallInvitation(call.contact, callType)
          chatModel.activeCall.value = call.copy(callState = CallState.InvitationSent, localCapabilities = r.capabilities)
        }
        is WCallResponse.Offer -> withApi {
          chatModel.controller.apiSendCallOffer(call.contact, r.offer, r.iceCandidates, call.localMedia, r.capabilities)
          chatModel.activeCall.value = call.copy(callState = CallState.OfferSent, localCapabilities = r.capabilities)
        }
        is WCallResponse.Answer -> withApi {
          chatModel.controller.apiSendCallAnswer(call.contact, r.answer, r.iceCandidates)
          chatModel.activeCall.value = call.copy(callState = CallState.Negotiated)
        }
        is WCallResponse.Ice -> withApi {
          chatModel.controller.apiSendCallExtraInfo(call.contact, r.iceCandidates)
        }
        is WCallResponse.Connection ->
          try {
            val callStatus = json.decodeFromString<WebRTCCallStatus>("\"${r.state.connectionState}\"")
            if (callStatus == WebRTCCallStatus.Connected) {
              chatModel.activeCall.value = call.copy(callState = CallState.Connected)
            }
            withApi { chatModel.controller.apiCallStatus(call.contact, callStatus) }
          } catch (e: Error) {
            Log.d(TAG, "call status ${r.state.connectionState} not used")
          }
        is WCallResponse.Connected -> {
          chatModel.activeCall.value = call.copy(callState = CallState.Connected, connectionInfo = r.connectionInfo)
        }
        is WCallResponse.End -> {
          withApi { chatModel.callManager.endCall(call) }
        }
        is WCallResponse.Ended -> {
          chatModel.activeCall.value = call.copy(callState = CallState.Ended)
          withApi { chatModel.callManager.endCall(call) }
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
  val call = chatModel.activeCall.value
  if (call?.callState == CallState.Connected) ActiveCallOverlayLayout(call, endCall)
//  if (call != null) ActiveCallOverlayLayout(call, endCall)

  ShowCallAlert(endCall)
  SendStateUpdates()
  DisposableEffect(Unit) {
    chatModel.activeCallViewIsVisible.value = true
    // After the first call, End command gets added to the list which prevents making another calls
    chatModel.callCommand.removeAll { it is WCallCommand.End }
    onDispose {
      chatModel.activeCallViewIsVisible.value = false
      chatModel.callCommand.clear()
    }
  }
}

@Composable
private fun SendStateUpdates() {
  LaunchedEffect(chatModel.activeCall.value) {
    val call = chatModel.activeCall.value
    if (call != null) {
      val state = call.callState.text
      val connInfo = call.connectionInfo
      //    val connInfoText = if (connInfo == null) ""  else " (${connInfo.text}, ${connInfo.protocolText})"
      val connInfoText = if (connInfo == null) ""  else " (${connInfo.text})"
      val description = call.encryptionStatus + connInfoText
      chatModel.callCommand.add(WCallCommand.Description(state, description))
    }
  }
}

@Composable
private fun ShowCallAlert(endCall: () -> Unit) {
  DisposableEffect(Unit) {
    AlertManager.shared.showAlertDialogButtonsColumn(
      title = chatModel.activeCall.value?.contact?.chatViewName ?: "",
      text = null,
      onDismissRequest = endCall,
      buttons = {
        val call = remember { chatModel.activeCall }.value
        Column {
          if (call != null) {
            CallInfoView(call)
          }
          SectionItemView({
            AlertManager.shared.hideAlert()
            endCall()
          }) {
            Text(generalGetString(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
          }
        }
      }
    )
    onDispose {
      AlertManager.shared.hideAlert()
    }
  }
  LaunchedEffect(chatModel.activeCall.value?.callState) {
    if (chatModel.activeCall.value?.callState == CallState.Connected) {
      AlertManager.shared.hideAlert()
    }
  }
}

@Composable
private fun ActiveCallOverlayLayout(
  call: Call,
  endCall: () -> Unit,
) {
  Column(Modifier.padding(DEFAULT_PADDING)) {
    val media = call.peerMedia ?: call.localMedia
    Spacer(Modifier.fillMaxHeight().weight(1f))
    Row(
      Modifier
        .background(
          MaterialTheme.colors.primary.mixWith(MaterialTheme.colors.background, 0.5f),
          RoundedCornerShape(50)
        )
        .padding(start = DEFAULT_PADDING_HALF),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.Center
    ) {
      Box(contentAlignment = Alignment.BottomEnd) {
        ProfileImage(size = 30.dp, image = call.contact.profile.image)
        if (media == CallMediaType.Video) {
          Icon(painterResource(MR.images.ic_videocam_filled), stringResource(MR.strings.icon_descr_video_call), Modifier.size(10.dp), tint = SimplexGreen)
        } else {
          Icon(painterResource(MR.images.ic_call_filled), stringResource(MR.strings.icon_descr_audio_call), Modifier.size(10.dp), tint = SimplexGreen)
        }
      }
      IconButton(onClick = endCall) {
        Icon(painterResource(MR.images.ic_call_end_filled), stringResource(MR.strings.icon_descr_hang_up), tint = Color.Red, modifier = Modifier.size(24.dp))
      }
    }
  }
}

@Composable
private fun CallInfoView(call: Call) {
  @Composable fun InfoText(text: String, style: TextStyle = MaterialTheme.typography.body2) =
    Text(text, color = Color(0xFFFFFFD8), style = style)
  Column(Modifier.sizeIn(minWidth = 180.dp, minHeight = 70.dp).padding(DEFAULT_PADDING), horizontalAlignment = Alignment.CenterHorizontally) {
    InfoText(call.callState.text)

    val connInfo = call.connectionInfo
    //    val connInfoText = if (connInfo == null) ""  else " (${connInfo.text}, ${connInfo.protocolText})"
    val connInfoText = if (connInfo == null) ""  else " (${connInfo.text})"
    if ((call.encryptionStatus + connInfoText).isNotEmpty()) {
      InfoText(call.encryptionStatus + connInfoText)
    }
  }
}

@Composable
fun WebRTCController(callCommand: SnapshotStateList<WCallCommand?>, onResponse: (WVAPIMessage) -> Unit) {
  val uriHandler = LocalUriHandler.current
  val server = remember {
    uriHandler.openUri("http://${SERVER_HOST}:$SERVER_PORT/simplex/call/")
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
  LaunchedEffect(callCommand.firstOrNull()) {
    while (connections.isEmpty()) {
      delay(100)
    }
    val cmd = callCommand.removeFirstOrNull()
    if (cmd != null) {
      Log.d(TAG, "WebRTCController LaunchedEffect executing $cmd")
      processCommand(cmd)
    }
  }
}

fun startServer(onResponse: (WVAPIMessage) -> Unit): NanoWSD {
  val server = object: NanoWSD(SERVER_HOST, SERVER_PORT) {
    override fun openWebSocket(session: IHTTPSession): WebSocket = MyWebSocket(onResponse, session)

    @Suppress("NewApi")
    fun resourcesToResponse(path: String): Response {
      val uri = Class.forName("chat.simplex.common.AppKt").getResource("/assets/www$path") ?: return resourceNotFound
      val response = newFixedLengthResponse(
        Status.OK, getMimeTypeForFile(uri.file),
        uri.openStream().readAllBytes()
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
