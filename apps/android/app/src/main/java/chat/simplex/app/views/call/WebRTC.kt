package chat.simplex.app.views.call

import chat.simplex.app.model.CR
import chat.simplex.app.model.User
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
sealed class WCallCommand {
  @Serializable @SerialName("capabilities") class Capabilities(): WCallCommand()
  @Serializable @SerialName("start") class Start(val media: CallMediaType, val aesKey: String? = null): WCallCommand()
  @Serializable @SerialName("accept") class Accept(val offer: String, val iceCandidates: List<String>, val media: CallMediaType, val aesKey: String? = null): WCallCommand()
  @Serializable @SerialName("answer") class Answer (val answer: String, val iceCandidates: List<String>): WCallCommand()
  @Serializable @SerialName("ice") class Ice(val iceCandidates: List<String>): WCallCommand()
  @Serializable @SerialName("end") class End(): WCallCommand()
}

@Serializable
sealed class WCallResponse {
  @Serializable @SerialName("capabilities") class Capabilities(val capabilities: CallCapabilities): WCallResponse()
  @Serializable @SerialName("offer") class Offer(val offer: String, val iceCandidates: List<String>): WCallResponse()
  // TODO remove accept, it is needed for debugging
  @Serializable @SerialName("accept") class Accept(val offer: String, val iceCandidates: List<String>, val media: CallMediaType, val aesKey: String? = null): WCallResponse()
  @Serializable @SerialName("answer") class Answer(val answer: String, val iceCandidates: List<String>): WCallResponse()
  @Serializable @SerialName("ice") class Ice(val iceCandidates: List<String>): WCallResponse()
  @Serializable @SerialName("connection") class Connection(val state: ConnectionState): WCallResponse()
  @Serializable @SerialName("ended") class Ended(): WCallResponse()
  @Serializable @SerialName("ok") class Ok(): WCallResponse()
  @Serializable @SerialName("error") class Error(val message: String): WCallResponse()
  @Serializable class Invalid(val str: String): WCallResponse()
}

@Serializable
class WebRTCCallOffer(val callType: CallType, val rtcSession: WebRTCSession)

@Serializable
class WebRTCSession(val rtcSession: String, val rtcIceCandidates: List<String>)

@Serializable
class WebRTCExtraInfo(val rtcIceCandidates: List<String>)

@Serializable
class CallType(val media: CallMediaType, val capabilities: CallCapabilities)

enum class WebRTCCallStatus(val status: String) {
  Connected("connected"),
  Disconnected("disconnected"),
  Failed("failed")
}

@Serializable
enum class CallMediaType(val media: String) {
  Video("video"),
  Audio("audio")
}

@Serializable
class CallCapabilities(val encryption: Boolean)

@Serializable
class ConnectionState(
  val connectionState: String,
  val iceConnectionState: String,
  val iceGatheringState: String,
  val signalingState: String
)