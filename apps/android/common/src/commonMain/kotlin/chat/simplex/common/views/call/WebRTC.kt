package chat.simplex.common.views.call

import androidx.compose.runtime.Composable
import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.generalGetString
import kotlinx.datetime.Instant
import kotlinx.serialization.SerialName
import java.net.URI
import java.util.*
import kotlin.collections.ArrayList
import kotlinx.serialization.Serializable
import dev.icerock.moko.resources.compose.stringResource
import com.icerockdev.library.MR

data class Call(
  val contact: Contact,
  val callState: CallState,
  val localMedia: CallMediaType,
  val localCapabilities: CallCapabilities? = null,
  val peerMedia: CallMediaType? = null,
  val sharedKey: String? = null,
  val audioEnabled: Boolean = true,
  val videoEnabled: Boolean = localMedia == CallMediaType.Video,
  val soundSpeaker: Boolean = localMedia == CallMediaType.Video,
  var localCamera: VideoCamera = VideoCamera.User,
  val connectionInfo: ConnectionInfo? = null
) {
  val encrypted: Boolean get() = localEncrypted && sharedKey != null
  val localEncrypted: Boolean get() = localCapabilities?.encryption ?: false

  val encryptionStatus: String @Composable get() = when(callState) {
    CallState.WaitCapabilities -> ""
    CallState.InvitationSent -> stringResource(if (localEncrypted) MR.strings.status_e2e_encrypted else MR.strings.status_no_e2e_encryption)
    CallState.InvitationAccepted -> stringResource(if (sharedKey == null) MR.strings.status_contact_has_no_e2e_encryption else MR.strings.status_contact_has_e2e_encryption)
    else -> stringResource(if (!localEncrypted) MR.strings.status_no_e2e_encryption else if (sharedKey == null) MR.strings.status_contact_has_no_e2e_encryption else MR.strings.status_e2e_encrypted)
  }

  val hasMedia: Boolean get() = callState == CallState.OfferSent || callState == CallState.Negotiated || callState == CallState.Connected
}

enum class CallState {
  WaitCapabilities,
  InvitationSent,
  InvitationAccepted,
  OfferSent,
  OfferReceived,
  AnswerReceived,
  Negotiated,
  Connected,
  Ended;

  val text: String @Composable get() = when(this) {
    WaitCapabilities -> stringResource(MR.strings.callstate_starting)
    InvitationSent -> stringResource(MR.strings.callstate_waiting_for_answer)
    InvitationAccepted -> stringResource(MR.strings.callstate_starting)
    OfferSent -> stringResource(MR.strings.callstate_waiting_for_confirmation)
    OfferReceived -> stringResource(MR.strings.callstate_received_answer)
    AnswerReceived -> stringResource(MR.strings.callstate_received_confirmation)
    Negotiated -> stringResource(MR.strings.callstate_connecting)
    Connected -> stringResource(MR.strings.callstate_connected)
    Ended -> stringResource(MR.strings.callstate_ended)
  }
}

@Serializable data class WVAPICall(val corrId: Int? = null, val command: WCallCommand)
@Serializable data class WVAPIMessage(val corrId: Int? = null, val resp: WCallResponse, val command: WCallCommand? = null)

@Serializable
sealed class WCallCommand {
  @Serializable @SerialName("capabilities") object Capabilities: WCallCommand()
  @Serializable @SerialName("start") data class Start(val media: CallMediaType, val aesKey: String? = null, val iceServers: List<RTCIceServer>? = null, val relay: Boolean? = null): WCallCommand()
  @Serializable @SerialName("offer") data class Offer(val offer: String, val iceCandidates: String, val media: CallMediaType, val aesKey: String? = null, val iceServers: List<RTCIceServer>? = null, val relay: Boolean? = null): WCallCommand()
  @Serializable @SerialName("answer") data class Answer (val answer: String, val iceCandidates: String): WCallCommand()
  @Serializable @SerialName("ice") data class Ice(val iceCandidates: String): WCallCommand()
  @Serializable @SerialName("media") data class Media(val media: CallMediaType, val enable: Boolean): WCallCommand()
  @Serializable @SerialName("camera") data class Camera(val camera: VideoCamera): WCallCommand()
  @Serializable @SerialName("end") object End: WCallCommand()
}

@Serializable
sealed class WCallResponse {
  @Serializable @SerialName("capabilities") data class Capabilities(val capabilities: CallCapabilities): WCallResponse()
  @Serializable @SerialName("offer") data class Offer(val offer: String, val iceCandidates: String, val capabilities: CallCapabilities): WCallResponse()
  @Serializable @SerialName("answer") data class Answer(val answer: String, val iceCandidates: String): WCallResponse()
  @Serializable @SerialName("ice") data class Ice(val iceCandidates: String): WCallResponse()
  @Serializable @SerialName("connection") data class Connection(val state: ConnectionState): WCallResponse()
  @Serializable @SerialName("connected") data class Connected(val connectionInfo: ConnectionInfo): WCallResponse()
  @Serializable @SerialName("ended") object Ended: WCallResponse()
  @Serializable @SerialName("ok") object Ok: WCallResponse()
  @Serializable @SerialName("error") data class Error(val message: String): WCallResponse()
}

@Serializable data class WebRTCCallOffer(val callType: CallType, val rtcSession: WebRTCSession)
@Serializable data class WebRTCSession(val rtcSession: String, val rtcIceCandidates: String)
@Serializable data class WebRTCExtraInfo(val rtcIceCandidates: String)
@Serializable data class CallType(val media: CallMediaType, val capabilities: CallCapabilities)
@Serializable data class RcvCallInvitation(val user: User, val contact: Contact, val callType: CallType, val sharedKey: String? = null, val callTs: Instant) {
  val callTypeText: String get() = generalGetString(when(callType.media) {
    CallMediaType.Video -> if (sharedKey == null) MR.strings.video_call_no_encryption else MR.strings.encrypted_video_call
    CallMediaType.Audio -> if (sharedKey == null) MR.strings.audio_call_no_encryption else MR.strings.encrypted_audio_call
  })
  val callTitle: String get() = generalGetString(when(callType.media) {
    CallMediaType.Video -> MR.strings.incoming_video_call
    CallMediaType.Audio -> MR.strings.incoming_audio_call
  })
}
@Serializable data class CallCapabilities(val encryption: Boolean)
@Serializable data class ConnectionInfo(private val localCandidate: RTCIceCandidate?, private val remoteCandidate: RTCIceCandidate?) {
  val text: String @Composable get() {
    val local = localCandidate?.candidateType
    val remote = remoteCandidate?.candidateType
    return when {
      local == RTCIceCandidateType.Host && remote == RTCIceCandidateType.Host ->
        stringResource(MR.strings.call_connection_peer_to_peer)
      local == RTCIceCandidateType.Relay && remote == RTCIceCandidateType.Relay ->
        stringResource(MR.strings.call_connection_via_relay)
      else ->
        "${local?.value ?: "unknown"} / ${remote?.value ?: "unknown"}"
    }
  }

  val protocolText: String get() {
    val local = localCandidate?.protocol?.uppercase(Locale.ROOT) ?: "unknown"
    val localRelay = localCandidate?.relayProtocol?.uppercase(Locale.ROOT) ?: "unknown"
    val remote = remoteCandidate?.protocol?.uppercase(Locale.ROOT) ?: "unknown"
    val localText = if (localRelay == local || localCandidate?.relayProtocol == null) local else "$local ($localRelay)"
    return if (local == remote) localText else "$localText / $remote"
  }
}

// https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate
@Serializable data class RTCIceCandidate(val candidateType: RTCIceCandidateType?, val protocol: String?, val relayProtocol: String?)
// https://developer.mozilla.org/en-US/docs/Web/API/RTCIceServer
@Serializable data class RTCIceServer(val urls: List<String>, val username: String? = null, val credential: String? = null)

// https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate/type
@Serializable
enum class RTCIceCandidateType(val value: String) {
  @SerialName("host") Host("host"),
  @SerialName("srflx") ServerReflexive("srflx"),
  @SerialName("prflx") PeerReflexive("prflx"),
  @SerialName("relay") Relay("relay")
}

@Serializable
enum class WebRTCCallStatus(val value: String) {
  @SerialName("connected") Connected("connected"),
  @SerialName("connecting") Connecting("connecting"),
  @SerialName("disconnected") Disconnected("disconnected"),
  @SerialName("failed") Failed("failed")
}

@Serializable
enum class CallMediaType {
  @SerialName("video") Video,
  @SerialName("audio") Audio
}

@Serializable
enum class VideoCamera {
  @SerialName("user") User,
  @SerialName("environment") Environment;
  val flipped: VideoCamera get() = if (this == User) Environment else User
}

@Serializable
data class ConnectionState(
  val connectionState: String,
  val iceConnectionState: String,
  val iceGatheringState: String,
  val signalingState: String
)

// the servers are expected in this format:
// stun:stun.simplex.im:443?transport=tcp
// turn:private:yleob6AVkiNI87hpR94Z@turn.simplex.im:443?transport=tcp
fun parseRTCIceServer(str: String): RTCIceServer? {
  var s = replaceScheme(str, "stun:")
  s = replaceScheme(s, "turn:")
  s = replaceScheme(s, "turns:")
  val u = runCatching { URI(s) }.getOrNull()
  if (u != null) {
    val scheme = u.scheme
    val host = u.host
    val port = u.port
    if (u.path == "" && (scheme == "stun" || scheme == "turn" || scheme == "turns")) {
      val userInfo = u.userInfo?.split(":")
      val query = if (u.query == null || u.query == "") "" else "?${u.query}"
      return RTCIceServer(
        urls = listOf("$scheme:$host:$port$query"),
        username = userInfo?.getOrNull(0),
        credential = userInfo?.getOrNull(1)
      )
    }
  }
  return null
}

private fun replaceScheme(s: String, scheme: String): String = if (s.startsWith(scheme)) s.replace(scheme, "$scheme//") else s

fun parseRTCIceServers(servers: List<String>): List<RTCIceServer>? {
  val iceServers: ArrayList<RTCIceServer> = ArrayList()
  for (s in servers) {
    val server = parseRTCIceServer(s)
    if (server != null) {
      iceServers.add(server)
    } else {
      return null
    }
  }
  return if (iceServers.isEmpty()) null else iceServers
}

fun getIceServers(): List<RTCIceServer>? {
  val value = ChatController.appPrefs.webrtcIceServers.get() ?: return null
  val servers: List<String> = value.split("\n")
  return parseRTCIceServers(servers)
}
