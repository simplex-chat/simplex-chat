// Inspired by
// https://github.com/webrtc/samples/blob/gh-pages/src/content/insertable-streams/endtoend-encryption

interface WVAPICall {
  corrId?: number
  command: WCallCommand
}

interface WVApiMessage {
  corrId?: number
  resp: WCallResponse
  command?: WCallCommand
}

type WCallCommand = WCCapabilities | WCStartCall | WCAcceptOffer | WCallAnswer | WCallIceCandidates | WCEndCall

type WCallResponse = WRCapabilities | WCallOffer | WCallAnswer | WCallIceCandidates | WRConnection | WRCallEnded | WROk | WRError

type WCallCommandTag = "capabilities" | "start" | "accept" | "answer" | "ice" | "end"

type WCallResponseTag = "capabilities" | "offer" | "answer" | "ice" | "connection" | "ended" | "ok" | "error"

enum CallMediaType {
  Audio = "audio",
  Video = "video",
}

interface IWCallCommand {
  type: WCallCommandTag
}

interface IWCallResponse {
  type: WCallResponseTag
}

interface WCCapabilities extends IWCallCommand {
  type: "capabilities"
}

interface WCStartCall extends IWCallCommand {
  type: "start"
  media: CallMediaType
  aesKey?: string
}

interface WCEndCall extends IWCallCommand {
  type: "end"
}

interface WCAcceptOffer extends IWCallCommand {
  type: "accept"
  offer: string // JSON string for RTCSessionDescriptionInit
  iceCandidates: string[] // JSON strings for RTCIceCandidateInit
  media: CallMediaType
  aesKey?: string
}

interface WCallOffer extends IWCallResponse {
  type: "offer"
  offer: string // JSON string for RTCSessionDescriptionInit
  iceCandidates: string[] // JSON strings for RTCIceCandidateInit
  capabilities: CallCapabilities
}

interface WCallAnswer extends IWCallCommand, IWCallResponse {
  type: "answer"
  answer: string // JSON string for RTCSessionDescriptionInit
  iceCandidates: string[] // JSON strings for RTCIceCandidateInit
}

interface WCallIceCandidates extends IWCallCommand, IWCallResponse {
  type: "ice"
  iceCandidates: string[] // JSON strings for RTCIceCandidateInit
}

interface WRCapabilities extends IWCallResponse {
  type: "capabilities"
  capabilities: CallCapabilities
}

interface CallCapabilities {
  encryption: boolean
}

interface WRConnection extends IWCallResponse {
  type: "connection"
  state: {
    connectionState: string
    iceConnectionState: string
    iceGatheringState: string
    signalingState: string
  }
}

interface WRCallEnded extends IWCallResponse {
  type: "ended"
}

interface WROk extends IWCallResponse {
  type: "ok"
}

interface WRError extends IWCallResponse {
  type: "error"
  message: string
}

type RTCRtpSenderWithEncryption = RTCRtpSender & {
  createEncodedStreams: () => TransformStream
}

type RTCRtpReceiverWithEncryption = RTCRtpReceiver & {
  createEncodedStreams: () => TransformStream
}

type RTCConfigurationWithEncryption = RTCConfiguration & {
  encodedInsertableStreams: boolean
}

const keyAlgorithm: AesKeyAlgorithm = {
  name: "AES-GCM",
  length: 256,
}

const keyUsages: KeyUsage[] = ["encrypt", "decrypt"]

let pc: RTCPeerConnection | undefined

const IV_LENGTH = 12

const initialPlainTextRequired = {
  key: 10,
  delta: 3,
  undefined: 1,
}

interface Call {
  connection: RTCPeerConnection
  iceCandidates: Promise<string[]> // JSON strings for RTCIceCandidate
}

interface CallConfig {
  peerConnectionConfig: RTCConfigurationWithEncryption
  iceCandidates: {
    delay: number
    extrasInterval: number
    extrasTimeout: number
  }
}

function defaultCallConfig(encodedInsertableStreams: boolean): CallConfig {
  return {
    peerConnectionConfig: {
      iceServers: [{urls: ["stun:stun.l.google.com:19302"]}],
      iceCandidatePoolSize: 10,
      encodedInsertableStreams,
    },
    iceCandidates: {
      delay: 2000,
      extrasInterval: 2000,
      extrasTimeout: 8000,
    },
  }
}

async function initializeCall(config: CallConfig, mediaType: CallMediaType, aesKey?: string): Promise<Call> {
  const conn = new RTCPeerConnection(config.peerConnectionConfig)
  const remoteStream = new MediaStream()
  const localStream = await navigator.mediaDevices.getUserMedia(callMediaConstraints(mediaType))
  await setUpMediaStreams(conn, localStream, remoteStream, aesKey)
  conn.addEventListener("connectionstatechange", connectionStateChange)
  const iceCandidates = new Promise<string[]>((resolve, _) => {
    let candidates: RTCIceCandidate[] = []
    let resolved = false
    let extrasInterval: number | undefined
    let extrasTimeout: number | undefined
    const delay = setTimeout(() => {
      if (!resolved) {
        resolveIceCandidates()
        extrasInterval = setInterval(() => {
          sendIceCandidates()
        }, config.iceCandidates.extrasInterval)
        extrasTimeout = setTimeout(() => {
          clearInterval(extrasInterval)
          sendIceCandidates()
        }, config.iceCandidates.extrasTimeout)
      }
    }, config.iceCandidates.delay)

    conn.onicecandidate = ({candidate: c}) => c && candidates.push(c)
    conn.onicegatheringstatechange = () => {
      if (conn.iceGatheringState == "complete") {
        if (resolved) {
          if (extrasInterval) clearInterval(extrasInterval)
          if (extrasTimeout) clearTimeout(extrasTimeout)
          sendIceCandidates()
        } else {
          resolveIceCandidates()
        }
      }
    }

    function resolveIceCandidates() {
      if (delay) clearTimeout(delay)
      resolved = true
      const iceCandidates = candidates.map((c) => JSON.stringify(c))
      candidates = []
      resolve(iceCandidates)
    }

    function sendIceCandidates() {
      if (candidates.length === 0) return
      const iceCandidates = candidates.map((c) => JSON.stringify(c))
      candidates = []
      sendMessageToNative({resp: {type: "ice", iceCandidates}})
    }
  })

  return {connection: conn, iceCandidates}

  function connectionStateChange() {
    sendMessageToNative({
      resp: {
        type: "connection",
        state: {
          connectionState: conn.connectionState,
          iceConnectionState: conn.iceConnectionState,
          iceGatheringState: conn.iceGatheringState,
          signalingState: conn.signalingState,
        },
      },
    })
    if (conn.connectionState == "disconnected" || conn.connectionState == "failed") {
      conn.removeEventListener("connectionstatechange", connectionStateChange)
      sendMessageToNative({resp: {type: "ended"}})
      conn.close()
      pc = undefined
      resetVideoElements()
    }
  }
}

var sendMessageToNative = (msg: WVApiMessage) => console.log(JSON.stringify(msg))

async function processCommand(body: WVAPICall): Promise<WVApiMessage> {
  const {corrId, command} = body
  let resp: WCallResponse
  try {
    switch (command.type) {
      case "capabilities":
        const encryption = supportsInsertableStreams()
        resp = {type: "capabilities", capabilities: {encryption}}
        break
      case "start":
        console.log("starting call")
        if (pc) {
          resp = {type: "error", message: "start: call already started"}
        } else if (!supportsInsertableStreams() && command.aesKey) {
          resp = {type: "error", message: "start: encryption is not supported"}
        } else {
          const encryption = supportsInsertableStreams()
          const {media, aesKey} = command
          const call = await initializeCall(defaultCallConfig(encryption && !!aesKey), media, encryption ? aesKey : undefined)
          const {connection, iceCandidates} = call
          pc = connection
          const offer = await pc.createOffer()
          await pc.setLocalDescription(offer)
          // for debugging, returning the command for callee to use
          // resp = {type: "accept", offer: JSON.stringify(offer), iceCandidates: await iceCandidates, media, aesKey}
          resp = {
            type: "offer",
            offer: JSON.stringify(offer),
            iceCandidates: await iceCandidates,
            capabilities: {encryption},
          }
        }
        break
      case "accept":
        if (pc) {
          resp = {type: "error", message: "accept: call already started"}
        } else if (!supportsInsertableStreams() && command.aesKey) {
          resp = {type: "error", message: "accept: encryption is not supported"}
        } else {
          const offer = JSON.parse(command.offer)
          const remoteIceCandidates = command.iceCandidates.map((c) => JSON.parse(c))
          const call = await initializeCall(defaultCallConfig(!!command.aesKey), command.media, command.aesKey)
          const {connection, iceCandidates} = call
          pc = connection
          await pc.setRemoteDescription(new RTCSessionDescription(offer))
          const answer = await pc.createAnswer()
          await pc.setLocalDescription(answer)
          addIceCandidates(pc, remoteIceCandidates)
          // same as command for caller to use
          resp = {
            type: "answer",
            answer: JSON.stringify(answer),
            iceCandidates: await iceCandidates,
          }
        }
        break
      case "answer":
        if (!pc) {
          resp = {type: "error", message: "answer: call not started"}
        } else if (!pc.localDescription) {
          resp = {type: "error", message: "answer: local description is not set"}
        } else if (pc.currentRemoteDescription) {
          resp = {type: "error", message: "answer: remote description already set"}
        } else {
          const answer = JSON.parse(command.answer)
          const remoteIceCandidates = command.iceCandidates.map((c) => JSON.parse(c))
          await pc.setRemoteDescription(new RTCSessionDescription(answer))
          addIceCandidates(pc, remoteIceCandidates)
          resp = {type: "ok"}
        }
        break
      case "ice":
        if (pc) {
          const remoteIceCandidates = command.iceCandidates.map((c) => JSON.parse(c))
          addIceCandidates(pc, remoteIceCandidates)
          resp = {type: "ok"}
        } else {
          resp = {type: "error", message: "ice: call not started"}
        }
        break
      case "end":
        if (pc) {
          pc.close()
          pc = undefined
          resetVideoElements()
          resp = {type: "ok"}
        } else {
          resp = {type: "error", message: "end: call not started"}
        }
        break
      default:
        resp = {type: "error", message: "unknown command"}
        break
    }
  } catch (e) {
    resp = {type: "error", message: (e as Error).message}
  }
  const apiResp = {corrId, resp, command}
  sendMessageToNative(apiResp)
  return apiResp
}

function addIceCandidates(conn: RTCPeerConnection, iceCandidates: RTCIceCandidateInit[]) {
  for (const c of iceCandidates) {
    conn.addIceCandidate(new RTCIceCandidate(c))
  }
}

async function setUpMediaStreams(
  pc: RTCPeerConnection,
  localStream: MediaStream,
  remoteStream: MediaStream,
  aesKey?: string
): Promise<void> {
  const videos = getVideoElements()
  if (!videos) throw Error("no video elements")

  let key: CryptoKey | undefined
  if (aesKey) {
    const keyData = decodeBase64(encodeAscii(aesKey))
    if (keyData) key = await crypto.subtle.importKey("raw", keyData, keyAlgorithm, false, keyUsages)
  }
  for (const track of localStream.getTracks()) {
    pc.addTrack(track, localStream)
  }
  if (key) {
    console.log("set up encryption for sending")
    for (const sender of pc.getSenders() as RTCRtpSenderWithEncryption[]) {
      setupPeerTransform(sender, encodeFunction(key))
    }
  }
  // Pull tracks from remote stream as they arrive add them to remoteStream video
  pc.ontrack = (event) => {
    if (key) {
      console.log("set up decryption for receiving")
      setupPeerTransform(event.receiver as RTCRtpReceiverWithEncryption, decodeFunction(key))
    }
    for (const track of event.streams[0].getTracks()) {
      remoteStream.addTrack(track)
    }
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

  const capabilities = RTCRtpSender.getCapabilities("video")
  if (capabilities) {
    const {codecs} = capabilities
    const selectedCodecIndex = codecs.findIndex((c) => c.mimeType === "video/VP8")
    const selectedCodec = codecs[selectedCodecIndex]
    codecs.splice(selectedCodecIndex, 1)
    codecs.unshift(selectedCodec)
    for (const t of pc.getTransceivers()) {
      if (t.sender.track?.kind === "video") {
        t.setCodecPreferences(codecs)
      }
    }
  }
  // setupVideoElement(videos.local)
  // setupVideoElement(videos.remote)
  videos.local.srcObject = localStream
  videos.remote.srcObject = remoteStream
}

function callMediaConstraints(mediaType: CallMediaType): MediaStreamConstraints {
  switch (mediaType) {
    case CallMediaType.Audio:
      return {audio: true, video: false}
    case CallMediaType.Video:
      return {
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
  }
}

function supportsInsertableStreams(): boolean {
  return "createEncodedStreams" in RTCRtpSender.prototype && "createEncodedStreams" in RTCRtpReceiver.prototype
}

interface VideoElements {
  local: HTMLMediaElement
  remote: HTMLMediaElement
}

function resetVideoElements() {
  const videos = getVideoElements()
  if (!videos) return
  videos.local.srcObject = null
  videos.remote.srcObject = null
}

function getVideoElements(): VideoElements | undefined {
  const local = document.getElementById("local-video-stream")
  const remote = document.getElementById("remote-video-stream")
  if (!(local && remote && local instanceof HTMLMediaElement && remote instanceof HTMLMediaElement)) return
  return {local, remote}
}

// function setupVideoElement(video: HTMLElement) {
//   // TODO use display: none
//   video.style.opacity = "0"
//   video.onplaying = () => {
//     video.style.opacity = "1"
//   }
// }

// what does it do?
// function toggleVideo(b) {
//   if (b == "true") {
//     localStream.getVideoTracks()[0].enabled = true
//   } else {
//     localStream.getVideoTracks()[0].enabled = false
//   }
// }

/* Stream Transforms */
function setupPeerTransform(
  peer: RTCRtpSenderWithEncryption | RTCRtpReceiverWithEncryption,
  transform: (frame: RTCEncodedVideoFrame, controller: TransformStreamDefaultController) => void
) {
  const streams = peer.createEncodedStreams()
  streams.readable.pipeThrough(new TransformStream({transform})).pipeTo(streams.writable)
}

/* Cryptography */
function encodeFunction(key: CryptoKey): (frame: RTCEncodedVideoFrame, controller: TransformStreamDefaultController) => void {
  return async (frame, controller) => {
    const data = new Uint8Array(frame.data)
    const n = frame instanceof RTCEncodedVideoFrame ? initialPlainTextRequired[frame.type] : 0
    const iv = randomIV()
    const initial = data.subarray(0, n)
    const plaintext = data.subarray(n, data.byteLength)
    try {
      const ciphertext = await crypto.subtle.encrypt({name: "AES-GCM", iv: iv.buffer}, key, plaintext)
      frame.data = concatN(initial, new Uint8Array(ciphertext), iv).buffer
      controller.enqueue(frame)
    } catch (e) {
      console.log(`encryption error ${e}`)
      throw e
    }
  }
}

function decodeFunction(key: CryptoKey): (frame: RTCEncodedVideoFrame, controller: TransformStreamDefaultController) => Promise<void> {
  return async (frame, controller) => {
    const data = new Uint8Array(frame.data)
    const n = frame instanceof RTCEncodedVideoFrame ? initialPlainTextRequired[frame.type] : 0
    const initial = data.subarray(0, n)
    const ciphertext = data.subarray(n, data.byteLength - IV_LENGTH)
    const iv = data.subarray(data.byteLength - IV_LENGTH, data.byteLength)
    try {
      const plaintext = await crypto.subtle.decrypt({name: "AES-GCM", iv}, key, ciphertext)
      frame.data = concatN(initial, new Uint8Array(plaintext)).buffer
      controller.enqueue(frame)
    } catch (e) {
      console.log(`decryption error ${e}`)
      throw e
    }
  }
}

class RTCEncodedVideoFrame {
  constructor(public type: "key" | "delta", public data: ArrayBuffer) {}
}

function randomIV() {
  return crypto.getRandomValues(new Uint8Array(IV_LENGTH))
}

const char_equal = "=".charCodeAt(0)

function concatN(...bs: Uint8Array[]): Uint8Array {
  const a = new Uint8Array(bs.reduce((size, b) => size + b.byteLength, 0))
  bs.reduce((offset, b: Uint8Array) => {
    a.set(b, offset)
    return offset + b.byteLength
  }, 0)
  return a
}

function encodeAscii(s: string): Uint8Array {
  const a = new Uint8Array(s.length)
  let i = s.length
  while (i--) a[i] = s.charCodeAt(i)
  return a
}

function decodeAscii(a: Uint8Array): string {
  let s = ""
  for (let i = 0; i < a.length; i++) s += String.fromCharCode(a[i])
  return s
}

const base64chars = new Uint8Array("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".split("").map((c) => c.charCodeAt(0)))

const base64lookup = new Array(256) as (number | undefined)[]
base64chars.forEach((c, i) => (base64lookup[c] = i))

function encodeBase64(a: Uint8Array): Uint8Array {
  const len = a.length
  const b64len = Math.ceil(len / 3) * 4
  const b64 = new Uint8Array(b64len)

  let j = 0
  for (let i = 0; i < len; i += 3) {
    b64[j++] = base64chars[a[i] >> 2]
    b64[j++] = base64chars[((a[i] & 3) << 4) | (a[i + 1] >> 4)]
    b64[j++] = base64chars[((a[i + 1] & 15) << 2) | (a[i + 2] >> 6)]
    b64[j++] = base64chars[a[i + 2] & 63]
  }

  if (len % 3) b64[b64len - 1] = char_equal
  if (len % 3 === 1) b64[b64len - 2] = char_equal

  return b64
}

function decodeBase64(b64: Uint8Array): Uint8Array | undefined {
  let len = b64.length
  if (len % 4) return
  let bLen = (len * 3) / 4

  if (b64[len - 1] === char_equal) {
    len--
    bLen--
    if (b64[len - 1] === char_equal) {
      len--
      bLen--
    }
  }

  const bytes = new Uint8Array(bLen)

  let i = 0
  let pos = 0
  while (i < len) {
    const enc1 = base64lookup[b64[i++]]
    const enc2 = i < len ? base64lookup[b64[i++]] : 0
    const enc3 = i < len ? base64lookup[b64[i++]] : 0
    const enc4 = i < len ? base64lookup[b64[i++]] : 0
    if (enc1 === undefined || enc2 === undefined || enc3 === undefined || enc4 === undefined) return
    bytes[pos++] = (enc1 << 2) | (enc2 >> 4)
    bytes[pos++] = ((enc2 & 15) << 4) | (enc3 >> 2)
    bytes[pos++] = ((enc3 & 3) << 6) | (enc4 & 63)
  }

  return bytes
}
