package chat.simplex.common.views.call

import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.common.model.*
import chat.simplex.res.MR
import kotlinx.datetime.Instant
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import java.net.URI
import kotlin.collections.ArrayList

data class Call(
  val remoteHostId: Long?,
  val userProfile: Profile,
  val contact: Contact,
  val callState: CallState,
  val localMedia: CallMediaType,
  val localCapabilities: CallCapabilities? = null,
  val peerMedia: CallMediaType? = null,
  val sharedKey: String? = null,
  val audioEnabled: Boolean = true,
  val videoEnabled: Boolean = localMedia == CallMediaType.Video,
  var localCamera: VideoCamera = VideoCamera.User,
  val connectionInfo: ConnectionInfo? = null,
  var connectedAt: Instant? = null,
) {
  val encrypted: Boolean get() = localEncrypted && sharedKey != null
  val localEncrypted: Boolean get() = localCapabilities?.encryption ?: false

  val encryptionStatus: String get() = when(callState) {
    CallState.WaitCapabilities -> ""
    CallState.InvitationSent -> generalGetString(if (localEncrypted) MR.strings.status_e2e_encrypted else MR.strings.status_no_e2e_encryption)
    CallState.InvitationAccepted -> generalGetString(if (sharedKey == null) MR.strings.status_contact_has_no_e2e_encryption else MR.strings.status_contact_has_e2e_encryption)
    else -> generalGetString(if (!localEncrypted) MR.strings.status_no_e2e_encryption else if (sharedKey == null) MR.strings.status_contact_has_no_e2e_encryption else MR.strings.status_e2e_encrypted)
  }

  val hasMedia: Boolean get() = callState == CallState.OfferSent || callState == CallState.Negotiated || callState == CallState.Connected

  fun supportsVideo(): Boolean = peerMedia == CallMediaType.Video || localMedia == CallMediaType.Video

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

  val text: String get() = when(this) {
    WaitCapabilities -> generalGetString(MR.strings.callstate_starting)
    InvitationSent -> generalGetString(MR.strings.callstate_waiting_for_answer)
    InvitationAccepted -> generalGetString(MR.strings.callstate_starting)
    OfferSent -> generalGetString(MR.strings.callstate_waiting_for_confirmation)
    OfferReceived -> generalGetString(MR.strings.callstate_received_answer)
    AnswerReceived -> generalGetString(MR.strings.callstate_received_confirmation)
    Negotiated -> generalGetString(MR.strings.callstate_connecting)
    Connected -> generalGetString(MR.strings.callstate_connected)
    Ended -> generalGetString(MR.strings.callstate_ended)
  }
}

@Serializable data class WVAPICall(val corrId: Int? = null, val command: WCallCommand)
@Serializable data class WVAPIMessage(val corrId: Int? = null, val resp: WCallResponse, val command: WCallCommand? = null)

@Serializable
sealed class WCallCommand {
  @Serializable @SerialName("capabilities") data class Capabilities(val media: CallMediaType): WCallCommand()
  @Serializable @SerialName("start") data class Start(val media: CallMediaType, val aesKey: String? = null, val iceServers: List<RTCIceServer>? = null, val relay: Boolean? = null): WCallCommand()
  @Serializable @SerialName("offer") data class Offer(val offer: String, val iceCandidates: String, val media: CallMediaType, val aesKey: String? = null, val iceServers: List<RTCIceServer>? = null, val relay: Boolean? = null): WCallCommand()
  @Serializable @SerialName("answer") data class Answer (val answer: String, val iceCandidates: String): WCallCommand()
  @Serializable @SerialName("ice") data class Ice(val iceCandidates: String): WCallCommand()
  @Serializable @SerialName("media") data class Media(val media: CallMediaType, val enable: Boolean): WCallCommand()
  @Serializable @SerialName("camera") data class Camera(val camera: VideoCamera): WCallCommand()
  @Serializable @SerialName("description") data class Description(val state: String, val description: String): WCallCommand()
  @Serializable @SerialName("layout") data class Layout(val layout: LayoutType): WCallCommand()
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
  @Serializable @SerialName("end") object End: WCallResponse()
  @Serializable @SerialName("ended") object Ended: WCallResponse()
  @Serializable @SerialName("ok") object Ok: WCallResponse()
  @Serializable @SerialName("error") data class Error(val message: String): WCallResponse()
}

@Serializable data class WebRTCCallOffer(val callType: CallType, val rtcSession: WebRTCSession)
@Serializable data class WebRTCSession(val rtcSession: String, val rtcIceCandidates: String)
@Serializable data class WebRTCExtraInfo(val rtcIceCandidates: String)
@Serializable data class CallType(val media: CallMediaType, val capabilities: CallCapabilities)
@Serializable data class RcvCallInvitation(
  val remoteHostId: Long?,
  val user: User,
  val contact: Contact,
  val callType: CallType,
  val sharedKey: String? = null,
  val callTs: Instant
) {
  val callTypeText: String get() = generalGetString(when(callType.media) {
    CallMediaType.Video -> if (sharedKey == null) MR.strings.video_call_no_encryption else MR.strings.encrypted_video_call
    CallMediaType.Audio -> if (sharedKey == null) MR.strings.audio_call_no_encryption else MR.strings.encrypted_audio_call
  })
  val callTitle: String get() = generalGetString(when(callType.media) {
    CallMediaType.Video -> MR.strings.incoming_video_call
    CallMediaType.Audio -> MR.strings.incoming_audio_call
  })

  // Shows whether notification was shown or not to prevent playing sound twice in both notification and in-app
  var sentNotification: Boolean = false
}
@Serializable data class CallCapabilities(val encryption: Boolean)
@Serializable data class ConnectionInfo(private val localCandidate: RTCIceCandidate?, private val remoteCandidate: RTCIceCandidate?) {
  val text: String get() {
    val local = localCandidate?.candidateType
    val remote = remoteCandidate?.candidateType
    return when {
      local == RTCIceCandidateType.Host && remote == RTCIceCandidateType.Host ->
        generalGetString(MR.strings.call_connection_peer_to_peer)
      local == RTCIceCandidateType.Relay && remote == RTCIceCandidateType.Relay ->
        generalGetString(MR.strings.call_connection_via_relay)
      else ->
        "${local?.value ?: "unknown"} / ${remote?.value ?: "unknown"}"
    }
  }
}

// https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate
@Serializable data class RTCIceCandidate(val candidateType: RTCIceCandidateType?, val protocol: String?)
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
enum class LayoutType {
  @SerialName("default") Default,
  @SerialName("localVideo") LocalVideo,
  @SerialName("remoteVideo") RemoteVideo
}

@Serializable
data class ConnectionState(
  val connectionState: String,
  val iceConnectionState: String,
  val iceGatheringState: String,
  val signalingState: String
)

// the servers are expected in this format:
// stuns:stun.simplex.im:443?transport=tcp
// turns:private2:Hxuq2QxUjnhj96Zq2r4HjqHRj@turn.simplex.im:443?transport=tcp
fun parseRTCIceServer(str: String): RTCIceServer? {
  var s = replaceScheme(str, "stun:")
  s = replaceScheme(s, "stuns:")
  s = replaceScheme(s, "turn:")
  s = replaceScheme(s, "turns:")
  val u = runCatching { URI(s) }.getOrNull()
  if (u != null) {
    val scheme = u.scheme
    val host = u.host
    val port = u.port
    if (u.path == "" && (scheme == "stun" || scheme == "stuns" || scheme == "turn" || scheme == "turns")) {
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
