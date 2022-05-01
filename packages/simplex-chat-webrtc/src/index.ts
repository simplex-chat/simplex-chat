// Inspired by
// https://github.com/webrtc/samples/blob/gh-pages/src/content/insertable-streams/endtoend-encryption

let incomingVideo = document.getElementById("incoming-video-stream")
let outgoingVideo = document.getElementById("outgoing-video-stream")
incomingVideo.style.opacity = 0
outgoingVideo.style.opacity = 0
incomingVideo.onplaying = () => {
  incomingVideo.style.opacity = 1
}
outgoingVideo.onplaying = () => {
  outgoingVideo.style.opacity = 1
}

type WCallMessage = WCallCommand | WCallResponse

type WCallCommand = WCCapabilities | WCStartCall | WCEndCall | WCallCommandResponse

type WCallResponse = WRCapabilities | WROk | WRError | WCallCommandResponse

type WCallCommandResponse = WCallOffer | WCallAnswer | WCallIceCandidates

type WCallMessageTag = "capabilities" | "start" | "offer" | "answer" | "ice" | "end" | "ok" | "error"

enum CallMediaType {
  Audio = "audio",
  Video = "video",
}

interface IWebCallMessage {
  type: WCallMessageTag
}

interface WCCapabilities extends IWebCallMessage {
  type: "capabilities"
}

interface WCStartCall extends IWebCallMessage {
  type: "start"
  media: CallMediaType
  aesKey?: Uint8Array
}

interface WCEndCall extends IWebCallMessage {
  type: "end"
}

interface WCallOffer extends IWebCallMessage {
  type: "offer"
  offer: RTCSessionDescriptionInit
  iceCandidates: RTCIceCandidateInit[]
}

interface WCallAnswer extends IWebCallMessage {
  type: "answer"
  answer: RTCSessionDescriptionInit
  iceCandidates: RTCIceCandidateInit[]
}

interface WCallIceCandidates extends IWebCallMessage {
  type: "ice"
  iceCandidates: RTCIceCandidateInit[]
}

interface WRCapabilities {
  type: "capabilities"
  capabilities: CallCapabilities
}

interface CallCapabilities {
  encryption: boolean
}

interface WROk extends IWebCallMessage {
  type: "ok"
}

interface WRError extends IWebCallMessage {
  type: "error"
  message: string
}

type RTCRtpSenderWithEncryption = RTCRtpSender & {
  createEncodedStreams: () => TransformStream
}

type RTCRtpReceiverWithEncryption = RTCRtpReceiver & {
  createEncodedStreams: () => TransformStream
}

// STUN servers
const peerConnectionConfig: RTCConfiguration = {
  iceServers: [{urls: ["stun:stun.l.google.com:19302"]}],
  iceCandidatePoolSize: 10,
  encodedInsertableStreams: true,
}
let keyGenConfig: AesKeyGenParams = {
  name: "AES-GCM",
  length: 256,
  tagLength: 128,
}
let keyUsages = ["encrypt", "decrypt"]

// Hardcode a key for development
let keyData = {
  alg: "A256GCM",
  ext: true,
  k: "JCMDWkhxLmPDhua0BUdhgv6Ac6hOtB9frSxJlnkTAK8",
  key_ops: keyUsages,
  kty: "oct",
}

let pc: RTCPeerConnection | undefined
let key
let IV_LENGTH = 12
const initialPlainTextRequired = {
  key: 10,
  delta: 3,
  undefined: 1,
}

// let encryptKeyRepresentation
let candidates: RTCIceCandidate[] = []
run()

async function run() {
  pc = new RTCPeerConnection(peerConnectionConfig)

  pc.onicecandidate = (event) => {
    // add candidate to maintained list to be sent all at once
    if (event.candidate) {
      candidates.push(event.candidate)
    }
  }
  pc.onicegatheringstatechange = (_) => {
    if (pc.iceGatheringState == "complete") {
      // Give command for other caller to use
      console.log(JSON.stringify({action: "processIceCandidates", content: candidates}))
    }
  }
  let remoteStream = new MediaStream()
  key = await crypto.subtle.importKey("jwk", keyData, keyGenConfig, true, keyUsages)
  let localStream = await getLocalVideoStream()
  setUpMediaStreams(pc, localStream, remoteStream)
}

interface CallPeerConnection {
  connection: RTCPeerConnection
  iceCandidates: RTCIceCandidate[]
  extraIceCandidates?: Promise<ExtraIceCandidates>
}

interface ExtraIceCandidates {
  iceCandidates: RTCIceCandidate[]
  complete: boolean
}

interface CallConfig {
  waitForIceCandidates: number
  timeoutIceCandidates: number
}

function createPeerConnection(config: CallConfig): Promise<CallPeerConnection> {
  let connection = new RTCPeerConnection(peerConnectionConfig)
  return new Promise((resolve, _) => {
    const extraIceCandidates = new Promise<ExtraIceCandidates>((resolveExtra, _) => {
      let candidates: RTCIceCandidate[] = []
      let ok = false
      let okExtra = false
      let waitExtra: number | undefined
      const wait = setTimeout(() => {
        if (!ok) {
          const iceCandidates = candidates.slice()
          candidates = []
          ok = true
          waitExtra = setTimeout(() => {
            if (!okExtra) {
              okExtra = true
              resolveExtra({
                iceCandidates: candidates.slice(),
                complete: false,
              })
            }
          }, config.timeoutIceCandidates)
          resolve({connection, iceCandidates, extraIceCandidates})
        }
      }, config.waitForIceCandidates)

      connection.onicecandidate = (e) => e.candidate && candidates.push(e.candidate)
      connection.onicegatheringstatechange = (_) => {
        if (connection.iceGatheringState == "complete") {
          if (!ok) {
            ok = true
            clearTimeout(wait)
            resolve({connection, iceCandidates: candidates})
          } else if (!okExtra) {
            okExtra = true
            waitExtra && clearTimeout(waitExtra)
            resolveExtra({iceCandidates: candidates, complete: true})
          }
        }
      }
    })
  })
}

function sendMessageToNative(msg: WCallResponse) {
  console.log(JSON.stringify(msg))
}

async function processCommand(command: WCallCommand): Promise<WCallResponse> {
  let resp: WCallResponse
  switch (command.type) {
    case "capabilities":
      const encryption = ("createEncodedStreams" in RTCRtpSender.prototype && "createEncodedStreams" in RTCRtpReceiver.prototype)
      resp = {type: "capabilities", capabilities: {encryption}}
      break
    case "start":
      console.log("starting call")
      if (pc) {
        resp = {type: "error", message: "call alread started"}
      } else {
        const {connection, iceCandidates, extraIceCandidates} = await createPeerConnection({
          waitForIceCandidates: 1000,
          timeoutIceCandidates: 4000
        })
        pc = connection
        const offer = await pc.createOffer()
        await pc.setLocalDescription(offer)
        // same as command for callee to use
        resp = {type: "offer", offer, iceCandidates}
        extraIceCandidates?.then(({iceCandidates, complete}) => {
          if (!complete) console.log("ICE candidates gathering not completed")
          if (iceCandidates.length > 0) sendMessageToNative({type: "ice", iceCandidates})
        })
      }
      break
    case "offer":
      if (pc) {
        await setRemoteDescription(pc, command.offer)
        const answer = await pc.createAnswer()
        await pc.setLocalDescription(answer)
        await addIceCandidates(pc, command.iceCandidates)
        // same as command for caller to use
        resp = {type: "answer", answer, iceCandidates: []}
      } else {
        resp = {type: "error", message: "call not started"}
      }
      break
    case "answer":
      if (pc) {
        await setRemoteDescription(pc, command.answer)
        await addIceCandidates(pc, command.iceCandidates)
        resp = {type: "ok"}
      } else {
        resp = {type: "error", message: "call not started"}
      }
      break
    case "ice":
      if (pc) {
        addIceCandidates(pc, command.iceCandidates)
        resp = {type: "ok"}
      } else {
        resp = {type: "error", message: "call not started"}
      }
      break
    case "end":
      if (pc) {
        pc.close()
        pc = undefined
        resp = {type: "ok"}
      } else {
        resp = {type: "error", message: "call not started"}
      }
      break
    default:
      resp = {type: "error", message: "unknown command"}
      break
  }
  sendMessageToNative(resp)
  return resp
}

function addIceCandidates(conn: RTCPeerConnection, iceCandidates: RTCIceCandidateInit[]) {
  for (const c of iceCandidates) {
    conn.addIceCandidate(new RTCIceCandidate(c))
  }
}

async function setRemoteDescription(pc: RTCPeerConnection, remoteDescr: RTCSessionDescriptionInit) {
  if (!pc.currentRemoteDescription) {
    await pc.setRemoteDescription(new RTCSessionDescription(remoteDescr))
  }
}

function setUpMediaStreams(pc: RTCPeerConnection, localStream: MediaStream, remoteStream: MediaStream) {
  for (const track of localStream.getTracks()) {
    pc.addTrack(track, localStream)
  }
  for (const sender of pc.getSenders() as RTCRtpSenderWithEncryption[]) {
    setupPeerTransform(sender, encodeFunction)
  } 
  // Pull tracks from remote stream as they arrive add them to remoteStream video
  pc.ontrack = (event) => {
    setupPeerTransform(event.receiver as RTCRtpReceiverWithEncryption, decodeFunction)
    event.streams[0].getTracks().forEach((track) => {
      remoteStream.addTrack(track)
    })
  }
  // We assume VP8 encoding in the decode/encode stages to get the initial
  // bytes to pass as plaintext so we enforce that here.
  // VP8 is supported by all supports of webrtc.
  // Use of VP8 by default may also reduce depacketisation issues.
  // We do not encrypt the first couple of bytes of the payload so that the
  // video elements can work by determining video keyframes and the opus mode
  // being used. This appears to be necessary for any video feed at all.
  // For VP8 this is the content described in
  //   https://tools.ietf.org/html/rfc6386#section-9.1
  // which is 10 bytes for key frames and 3 bytes for delta frames.
  // For opus (where encodedFrame.type is not set) this is the TOC byte from
  //   https://tools.ietf.org/html/rfc6716#section-3.1

  const {codecs} = RTCRtpSender.getCapabilities("video")
  const selectedCodecIndex = codecs.findIndex((c) => c.mimeType === "video/VP8")
  const selectedCodec = codecs[selectedCodecIndex]
  codecs.splice(selectedCodecIndex, 1)
  codecs.unshift(selectedCodec)
  const transceiver = pc.getTransceivers().find((t) => t.sender?.track?.kind === "video")
  transceiver.setCodecPreferences(codecs)

  outgoingVideo.srcObject = localStream
  incomingVideo.srcObject = remoteStream
}

async function getLocalVideoStream(mediaType: CallMediaType) {
  let constraints: MediaStreamConstraints
  switch (mediaType) {
    case CallMediaType.Audio:
      constraints = {audio: true}
      break
    case CallMediaType.Video:
      constraints = {
        audio: true,
        video: {
          frameRate: 24,
          width: {
            min: 480,
            ideal: 720,
            max: 1280,
          },
          aspectRatio: 1.33,
        },
      }
      break
  }
  return await navigator.mediaDevices.getUserMedia(constraints)
}

// what does it do?
// function toggleVideo(b) {
//   if (b == "true") {
//     localStream.getVideoTracks()[0].enabled = true
//   } else {
//     localStream.getVideoTracks()[0].enabled = false
//   }
// }

function f() {
  console.log("Debug Function")
  return "Debugging Return"
}

/* Stream Transforms */
function setupPeerTransform(peer: RTCRtpSenderWithEncryption | RTCRtpReceiverWithEncryption, transform: (frame: RTCEncodedVideoFrame, controller: TransformStreamDefaultController) => void) {
  const streams = peer.createEncodedStreams()
  streams.readable.pipeThrough(new TransformStream({transform})).pipeTo(streams.writable)
}

/* Cryptography */
function encodeFunction(frame: RTCEncodedVideoFrame, controller: TransformStreamDefaultController) {
  // frame is an RTCEncodedAudioFrame
  // frame.data is ArrayBuffer
  let data = new Uint8Array(frame.data)
  let n = frame instanceof RTCEncodedVideoFrame ? initialPlainTextRequired[frame.type] : 0
  let iv = randomIV()
  let initial = data.subarray(0, n)
  let plaintext = data.subarray(n, data.byteLength)
  crypto.subtle
    .encrypt({name: "AES-GCM", iv: iv.buffer}, key, plaintext)
    .then((c) => {
      frame.data = concatN(initial, new Uint8Array(c), iv).buffer
      controller.enqueue(frame)
    })
    .catch((e) => {
      console.log("encrypt error")
      pc.close()
      throw e
    })
}

function decodeFunction(frame: RTCEncodedVideoFrame, controller: TransformStreamDefaultController) {
  let data = new Uint8Array(frame.data)
  let n = frame instanceof RTCEncodedVideoFrame ? initialPlainTextRequired[frame.type] : 0
  let initial = data.subarray(0, n)
  let ciphertext = data.subarray(n, data.byteLength - IV_LENGTH)
  let iv = data.subarray(data.byteLength - IV_LENGTH, data.byteLength)
  crypto.subtle
    .decrypt({name: "AES-GCM", iv: iv}, key, ciphertext)
    .then((p) => {
      frame.data = concatN(initial, new Uint8Array(p)).buffer
      controller.enqueue(frame)
    })
    .catch((e) => {
      console.log("decrypt error")
      pc.close()
      throw e
    })
}

class RTCEncodedVideoFrame {
  constructor(public type: "key" | "delta", public data: ArrayBuffer) {}
}

function randomIV() {
  return crypto.getRandomValues(new Uint8Array(IV_LENGTH))
}

async function loadKey(keyData) {
  key = await crypto.subtle.importKey("jwk", keyData, keyGenConfig, false, keyUsages)
}

function concatN(...bs: Uint8Array[]): Uint8Array {
  const a = new Uint8Array(bs.reduce((size, b) => size + b.byteLength, 0))
  bs.reduce((offset, b) => {
    a.set(b, offset)
    return offset + b.byteLength
  }, 0)
  return a
}

async function generateKey() {
  let rawKey = await crypto.subtle.generateKey(keyGenConfig, true, keyUsages)
  let key = await crypto.subtle.exportKey("jwk", rawKey)
  console.log(
    JSON.stringify({
      action: "processDecryptionKey",
      content: {
        key,
        iv: encryptIv,
      },
    })
  )
}
