package chat.simplex.app.views.call

import androidx.compose.runtime.Composable
import androidx.compose.ui.res.stringResource
import chat.simplex.app.R
import chat.simplex.app.model.Contact
import chat.simplex.app.views.helpers.generalGetString
import kotlinx.datetime.Instant
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

data class Call(
  val contact: Contact,
  val callState: CallState,
  val localMedia: CallMediaType,
  val localCapabilities: CallCapabilities? = null,
  val peerMedia: CallMediaType? = null,
  val sharedKey: String? = null,
  val audioEnabled: Boolean = true,
  val videoEnabled: Boolean = localMedia == CallMediaType.Video,
  var localCamera: VideoCamera = VideoCamera.User,
  val connectionInfo: ConnectionInfo? = null
) {
  val encrypted: Boolean get() = localEncrypted && sharedKey != null
  val localEncrypted: Boolean get() = localCapabilities?.encryption ?: false

  val encryptionStatus: String @Composable get() = when(callState) {
    CallState.WaitCapabilities -> ""
    CallState.InvitationSent -> stringResource(if (localEncrypted) R.string.status_e2e_encrypted else R.string.status_no_e2e_encryption)
    CallState.InvitationAccepted -> stringResource(if (sharedKey == null) R.string.status_contact_has_no_e2e_encryption else R.string.status_contact_has_e2e_encryption)
    else -> stringResource(if (!localEncrypted) R.string.status_no_e2e_encryption else if (sharedKey == null) R.string.status_contact_has_no_e2e_encryption else R.string.status_e2e_encrypted)
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
    WaitCapabilities -> stringResource(R.string.callstate_starting)
    InvitationSent -> stringResource(R.string.callstate_waiting_for_answer)
    InvitationAccepted -> stringResource(R.string.callstate_starting)
    OfferSent -> stringResource(R.string.callstate_waiting_for_confirmation)
    OfferReceived -> stringResource(R.string.callstate_received_answer)
    AnswerReceived -> stringResource(R.string.callstate_received_confirmation)
    Negotiated -> stringResource(R.string.callstate_connecting)
    Connected -> stringResource(R.string.callstate_connected)
    Ended -> stringResource(R.string.callstate_ended)
  }
}

@Serializable class WVAPICall(val corrId: Int? = null, val command: WCallCommand)
@Serializable class WVAPIMessage(val corrId: Int? = null, val resp: WCallResponse, val command: WCallCommand? = null)

@Serializable
sealed class WCallCommand {
  @Serializable @SerialName("capabilities") object Capabilities: WCallCommand()
  @Serializable @SerialName("start") class Start(val media: CallMediaType, val aesKey: String? = null, val iceServers: List<RTCIceServer>? = null, val relay: Boolean? = null): WCallCommand()
  @Serializable @SerialName("offer") class Offer(val offer: String, val iceCandidates: String, val media: CallMediaType, val aesKey: String? = null, val iceServers: List<RTCIceServer>? = null, val relay: Boolean? = null): WCallCommand()
  @Serializable @SerialName("answer") class Answer (val answer: String, val iceCandidates: String): WCallCommand()
  @Serializable @SerialName("ice") class Ice(val iceCandidates: String): WCallCommand()
  @Serializable @SerialName("media") class Media(val media: CallMediaType, val enable: Boolean): WCallCommand()
  @Serializable @SerialName("camera") class Camera(val camera: VideoCamera): WCallCommand()
  @Serializable @SerialName("end") object End: WCallCommand()
}

@Serializable
sealed class WCallResponse {
  @Serializable @SerialName("capabilities") class Capabilities(val capabilities: CallCapabilities): WCallResponse()
  @Serializable @SerialName("offer") class Offer(val offer: String, val iceCandidates: String, val capabilities: CallCapabilities): WCallResponse()
  @Serializable @SerialName("answer") class Answer(val answer: String, val iceCandidates: String): WCallResponse()
  @Serializable @SerialName("ice") class Ice(val iceCandidates: String): WCallResponse()
  @Serializable @SerialName("connection") class Connection(val state: ConnectionState): WCallResponse()
  @Serializable @SerialName("connected") class Connected(val connectionInfo: ConnectionInfo): WCallResponse()
  @Serializable @SerialName("ended") object Ended: WCallResponse()
  @Serializable @SerialName("ok") object Ok: WCallResponse()
  @Serializable @SerialName("error") class Error(val message: String): WCallResponse()
}

@Serializable class WebRTCCallOffer(val callType: CallType, val rtcSession: WebRTCSession)
@Serializable class WebRTCSession(val rtcSession: String, val rtcIceCandidates: String)
@Serializable class WebRTCExtraInfo(val rtcIceCandidates: String)
@Serializable class CallType(val media: CallMediaType, val capabilities: CallCapabilities)
@Serializable class CallInvitation(val contact: Contact, val callType: CallType, val sharedKey: String?, val callTs: Instant) {
  val callTypeText: String get() = generalGetString(when(callType.media) {
    CallMediaType.Video -> if (sharedKey == null) R.string.video_call_no_encryption else R.string.encrypted_video_call
    CallMediaType.Audio -> if (sharedKey == null) R.string.audio_call_no_encryption else R.string.encrypted_audio_call
  })
  val callTitle: String get() = generalGetString(when(callType.media) {
    CallMediaType.Video -> R.string.incoming_video_call
    CallMediaType.Audio -> R.string.incoming_audio_call
  })
}
@Serializable class CallCapabilities(val encryption: Boolean)
@Serializable class ConnectionInfo(private val localCandidate: RTCIceCandidate?, private val remoteCandidate: RTCIceCandidate?) {
  val text: String @Composable get() = when {
    localCandidate?.candidateType == RTCIceCandidateType.Host && remoteCandidate?.candidateType == RTCIceCandidateType.Host ->
      stringResource(R.string.call_connection_peer_to_peer)
    localCandidate?.candidateType == RTCIceCandidateType.Relay && remoteCandidate?.candidateType == RTCIceCandidateType.Relay ->
      stringResource(R.string.call_connection_via_relay)
    else ->
      "${localCandidate?.candidateType?.value ?: "unknown"} / ${remoteCandidate?.candidateType?.value ?: "unknown"}"
  }
}
// https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate
@Serializable class RTCIceCandidate(val candidateType: RTCIceCandidateType?)
// https://developer.mozilla.org/en-US/docs/Web/API/RTCIceServer
@Serializable class RTCIceServer(val urls: List<String>, val username: String? = null, val credential: String? = null)

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
class ConnectionState(
  val connectionState: String,
  val iceConnectionState: String,
  val iceGatheringState: String,
  val signalingState: String
)