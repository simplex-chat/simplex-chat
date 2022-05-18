package chat.simplex.app.views.call

import chat.simplex.app.R
import chat.simplex.app.model.Contact
import chat.simplex.app.views.helpers.generalGetString
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
  val connectionInfo: ConnectionInfo? = null
) {
  val encrypted: Boolean get() = (localCapabilities?.encryption ?: false) && sharedKey != null
}

enum class CallState {
  WaitCapabilities,
  InvitationSent,
  InvitationReceived,
  OfferSent,
  OfferReceived,
  Negotiated,
  Connected;

  val text: String get() = when(this) {
    WaitCapabilities -> generalGetString(R.string.callstate_starting)
    InvitationSent -> generalGetString(R.string.callstate_waiting_for_answer)
    InvitationReceived -> generalGetString(R.string.callstate_starting)
    OfferSent -> generalGetString(R.string.callstate_waiting_for_confirmation)
    OfferReceived -> generalGetString(R.string.callstate_received_answer)
    Negotiated -> generalGetString(R.string.callstate_connecting)
    Connected -> generalGetString(R.string.callstate_connected)
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
@Serializable class CallInvitation(val peerMedia: CallMediaType, val sharedKey: String?) {
  val callTypeText: String get() = when(peerMedia) {
    CallMediaType.Video -> generalGetString(if (sharedKey == null) R.string.video_call_no_encryption else R.string.encrypted_video_call)
    CallMediaType.Audio -> generalGetString(if (sharedKey == null) R.string.audio_call_no_encryption else R.string.encrypted_audio_call)
  }
  val callTitle: String get() = when(peerMedia) {
    CallMediaType.Video -> generalGetString(R.string.incoming_video_call)
    CallMediaType.Audio -> generalGetString(R.string.incoming_audio_call)
  }
}
@Serializable class CallCapabilities(val encryption: Boolean)
@Serializable class ConnectionInfo(val localCandidate: RTCIceCandidate?, val remoteCandidate: RTCIceCandidate)
// https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate
@Serializable class RTCIceCandidate(val candidateType: RTCIceCandidateType?)
// https://developer.mozilla.org/en-US/docs/Web/API/RTCIceServer
@Serializable class RTCIceServer(val urls: List<String>, val username: String? = null, val credential: String? = null)

// https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate/type
@Serializable
enum class RTCIceCandidateType {
  @SerialName("host") Host,
  @SerialName("srflx") ServerReflexive,
  @SerialName("prflx") PeerReflexive,
  @SerialName("relay") Relay
}

@Serializable
enum class WebRTCCallStatus {
  @SerialName("connected") Connected,
  @SerialName("disconnected") Disconnected,
  @SerialName("failed") Failed
}

@Serializable
enum class CallMediaType {
  @SerialName("video") Video,
  @SerialName("audio") Audio
}

@Serializable
class ConnectionState(
  val connectionState: String,
  val iceConnectionState: String,
  val iceGatheringState: String,
  val signalingState: String
)