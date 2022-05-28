// Inspired by
// https://github.com/webrtc/samples/blob/gh-pages/src/content/insertable-streams/endtoend-encryption

interface WVApiMessage {
  corrId?: number
  resp: WCallResponse
  command?: WCallCommand
}

type WCallCommand =
  | WCCapabilities
  | WCStartCall
  | WCAcceptOffer
  | WCallAnswer
  | WCallIceCandidates
  | WCEnableMedia
  | WCToggleCamera
  | WCEndCall

type WCallResponse =
  | WRCapabilities
  | WCallOffer
  | WCallAnswer
  | WCallIceCandidates
  | WRConnection
  | WRCallConnected
  | WRCallEnded
  | WROk
  | WRError
  | WCAcceptOffer

type WCallCommandTag = "capabilities" | "start" | "offer" | "answer" | "ice" | "media" | "camera" | "end"

type WCallResponseTag = "capabilities" | "offer" | "answer" | "ice" | "connection" | "connected" | "ended" | "ok" | "error"

enum CallMediaType {
  Audio = "audio",
  Video = "video",
}

enum VideoCamera {
  User = "user",
  Environment = "environment",
}

interface IWCallCommand {
  type: WCallCommandTag
}

interface IWCallResponse {
  type: WCallResponseTag
}

interface WCCapabilities extends IWCallCommand {
  type: "capabilities"
  media?: CallMediaType
  useWorker?: boolean
}

interface WCStartCall extends IWCallCommand {
  type: "start"
  media: CallMediaType
  aesKey?: string
  useWorker?: boolean
  iceServers?: RTCIceServer[]
  relay?: boolean
}

interface WCEndCall extends IWCallCommand {
  type: "end"
}

interface WCAcceptOffer extends IWCallCommand {
  type: "offer"
  offer: string // JSON string for RTCSessionDescriptionInit
  iceCandidates: string // JSON strings for RTCIceCandidateInit
  media: CallMediaType
  aesKey?: string
  useWorker?: boolean
  iceServers?: RTCIceServer[]
  relay?: boolean
}

interface WCallOffer extends IWCallResponse {
  type: "offer"
  offer: string // JSON string for RTCSessionDescriptionInit
  iceCandidates: string // JSON strings for RTCIceCandidateInit[]
  capabilities: CallCapabilities
}

interface WCallAnswer extends IWCallCommand, IWCallResponse {
  type: "answer"
  answer: string // JSON string for RTCSessionDescriptionInit
  iceCandidates: string // JSON strings for RTCIceCandidateInit[]
}

interface WCallIceCandidates extends IWCallCommand, IWCallResponse {
  type: "ice"
  iceCandidates: string // JSON strings for RTCIceCandidateInit[]
}

interface WCEnableMedia extends IWCallCommand {
  type: "media"
  media: CallMediaType
  enable: boolean
}

interface WCToggleCamera extends IWCallCommand {
  type: "camera"
  camera: VideoCamera
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

interface WRCallConnected extends IWCallResponse {
  type: "connected"
  connectionInfo: ConnectionInfo
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

interface ConnectionInfo {
  iceCandidatePair: RTCIceCandidatePairStats
  localCandidate?: RTCIceCandidate
  remoteCandidate?: RTCIceCandidate
}

// for debugging
// var sendMessageToNative = ({resp}: WVApiMessage) => console.log(JSON.stringify({command: resp}))
var sendMessageToNative = (msg: WVApiMessage) => console.log(JSON.stringify(msg))

// Global object with cryptrographic/encoding functions
const callCrypto = callCryptoFunction()

declare var RTCRtpScriptTransform: {
  prototype: RTCRtpScriptTransform
  new (worker: Worker, options?: any): RTCRtpScriptTransform
}

enum TransformOperation {
  Encrypt = "encrypt",
  Decrypt = "decrypt",
}

interface RTCRtpScriptTransform {}

interface WVAPICall {
  corrId?: number
  command: WCallCommand
}

interface Call {
  connection: RTCPeerConnection
  iceCandidates: Promise<string> // JSON strings for RTCIceCandidate
  localMedia: CallMediaType
  localCamera: VideoCamera
  localStream: MediaStream
  remoteStream: MediaStream
  aesKey?: string
  useWorker?: boolean
  worker?: Worker
  key?: CryptoKey
}

let activeCall: Call | undefined

const processCommand = (function () {
  type RTCRtpSenderWithEncryption = RTCRtpSender & {
    createEncodedStreams: () => TransformStream
    transform: RTCRtpScriptTransform
  }

  type RTCRtpReceiverWithEncryption = RTCRtpReceiver & {
    createEncodedStreams: () => TransformStream
    transform: RTCRtpScriptTransform
  }

  type RTCConfigurationWithEncryption = RTCConfiguration & {
    encodedInsertableStreams: boolean
  }

  interface CallConfig {
    peerConnectionConfig: RTCConfigurationWithEncryption
    iceCandidates: {
      delay: number
      extrasInterval: number
      extrasTimeout: number
    }
  }

  const defaultIceServers: RTCIceServer[] = [
    {urls: ["stun:stun.simplex.chat:5349"]},
    {urls: ["turn:turn.simplex.chat:5349"], username: "private", credential: "yleob6AVkiNI87hpR94Z"},
  ]

  function getCallConfig(encodedInsertableStreams: boolean, iceServers?: RTCIceServer[], relay?: boolean): CallConfig {
    return {
      peerConnectionConfig: {
        iceServers: iceServers ?? defaultIceServers,
        iceCandidatePoolSize: 10,
        encodedInsertableStreams,
        iceTransportPolicy: relay ? "relay" : "all",
      },
      iceCandidates: {
        delay: 3000,
        extrasInterval: 2000,
        extrasTimeout: 8000,
      },
    }
  }

  function getIceCandidates(conn: RTCPeerConnection, config: CallConfig) {
    return new Promise<string>((resolve, _) => {
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
        const iceCandidates = serialize(candidates)
        candidates = []
        resolve(iceCandidates)
      }

      function sendIceCandidates() {
        if (candidates.length === 0) return
        const iceCandidates = serialize(candidates)
        candidates = []
        sendMessageToNative({resp: {type: "ice", iceCandidates}})
      }
    })
  }

  async function initializeCall(config: CallConfig, mediaType: CallMediaType, aesKey?: string, useWorker?: boolean): Promise<Call> {
    const pc = new RTCPeerConnection(config.peerConnectionConfig)
    const remoteStream = new MediaStream()
    const localCamera = VideoCamera.User
    const localStream = await getLocalMediaStream(mediaType, localCamera)
    const iceCandidates = getIceCandidates(pc, config)
    const call = {connection: pc, iceCandidates, localMedia: mediaType, localCamera, localStream, remoteStream, aesKey, useWorker}
    await setupMediaStreams(call)
    pc.addEventListener("connectionstatechange", connectionStateChange)
    return call

    async function connectionStateChange() {
      sendMessageToNative({
        resp: {
          type: "connection",
          state: {
            connectionState: pc.connectionState,
            iceConnectionState: pc.iceConnectionState,
            iceGatheringState: pc.iceGatheringState,
            signalingState: pc.signalingState,
          },
        },
      })
      if (pc.connectionState == "disconnected" || pc.connectionState == "failed") {
        pc.removeEventListener("connectionstatechange", connectionStateChange)
        if (activeCall) {
          setTimeout(() => sendMessageToNative({resp: {type: "ended"}}), 0)
        }
        endCall()
      } else if (pc.connectionState == "connected") {
        const stats = (await pc.getStats()) as Map<string, any>
        for (const stat of stats.values()) {
          const {type, state} = stat
          if (type === "candidate-pair" && state === "succeeded") {
            const iceCandidatePair = stat as RTCIceCandidatePairStats
            const resp: WRCallConnected = {
              type: "connected",
              connectionInfo: {
                iceCandidatePair,
                localCandidate: stats.get(iceCandidatePair.localCandidateId),
                remoteCandidate: stats.get(iceCandidatePair.remoteCandidateId),
              },
            }
            setTimeout(() => sendMessageToNative({resp}), 500)
            break
          }
        }
      }
    }
  }

  function serialize<T>(x: T): string {
    return LZString.compressToBase64(JSON.stringify(x))
  }

  function parse<T>(s: string): T {
    return JSON.parse(LZString.decompressFromBase64(s)!)
  }

  async function processCommand(body: WVAPICall): Promise<WVApiMessage> {
    const {corrId, command} = body
    const pc = activeCall?.connection
    let resp: WCallResponse
    try {
      switch (command.type) {
        case "capabilities":
          console.log("starting outgoing call - capabilities")
          if (activeCall) endCall()
          // This request for local media stream is made to prompt for camera/mic permissions on call start
          if (command.media) await getLocalMediaStream(command.media, VideoCamera.User)
          const encryption = supportsInsertableStreams(command.useWorker)
          resp = {type: "capabilities", capabilities: {encryption}}
          break
        case "start": {
          console.log("starting incoming call - create webrtc session")
          if (activeCall) endCall()
          const {media, useWorker, iceServers, relay} = command
          const encryption = supportsInsertableStreams(useWorker)
          const aesKey = encryption ? command.aesKey : undefined
          activeCall = await initializeCall(getCallConfig(encryption && !!aesKey, iceServers, relay), media, aesKey, useWorker)
          const pc = activeCall.connection
          const offer = await pc.createOffer()
          await pc.setLocalDescription(offer)
          // for debugging, returning the command for callee to use
          // resp = {
          //   type: "offer",
          //   offer: serialize(offer),
          //   iceCandidates: await activeCall.iceCandidates,
          //   capabilities: {encryption},
          //   media,
          //   iceServers,
          //   relay,
          //   aesKey,
          //   useWorker,
          // }
          resp = {
            type: "offer",
            offer: serialize(offer),
            iceCandidates: await activeCall.iceCandidates,
            capabilities: {encryption},
          }
          break
        }
        case "offer":
          if (activeCall) {
            resp = {type: "error", message: "accept: call already started"}
          } else if (!supportsInsertableStreams(command.useWorker) && command.aesKey) {
            resp = {type: "error", message: "accept: encryption is not supported"}
          } else {
            const offer: RTCSessionDescriptionInit = parse(command.offer)
            const remoteIceCandidates: RTCIceCandidateInit[] = parse(command.iceCandidates)
            const {media, aesKey, useWorker, iceServers, relay} = command
            activeCall = await initializeCall(getCallConfig(!!aesKey, iceServers, relay), media, aesKey, useWorker)
            const pc = activeCall.connection
            await pc.setRemoteDescription(new RTCSessionDescription(offer))
            const answer = await pc.createAnswer()
            await pc.setLocalDescription(answer)
            addIceCandidates(pc, remoteIceCandidates)
            // same as command for caller to use
            resp = {
              type: "answer",
              answer: serialize(answer),
              iceCandidates: await activeCall.iceCandidates,
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
            const answer: RTCSessionDescriptionInit = parse(command.answer)
            const remoteIceCandidates: RTCIceCandidateInit[] = parse(command.iceCandidates)
            await pc.setRemoteDescription(new RTCSessionDescription(answer))
            addIceCandidates(pc, remoteIceCandidates)
            resp = {type: "ok"}
          }
          break
        case "ice":
          if (pc) {
            const remoteIceCandidates: RTCIceCandidateInit[] = parse(command.iceCandidates)
            addIceCandidates(pc, remoteIceCandidates)
            resp = {type: "ok"}
          } else {
            resp = {type: "error", message: "ice: call not started"}
          }
          break
        case "media":
          if (!activeCall) {
            resp = {type: "error", message: "media: call not started"}
          } else if (activeCall.localMedia == CallMediaType.Audio && command.media == CallMediaType.Video) {
            resp = {type: "error", message: "media: no video"}
          } else {
            enableMedia(activeCall.localStream, command.media, command.enable)
            resp = {type: "ok"}
          }
          break
        case "camera":
          if (!activeCall || !pc) {
            resp = {type: "error", message: "camera: call not started"}
          } else {
            await replaceMedia(activeCall, command.camera)
            resp = {type: "ok"}
          }
          break
        case "end":
          endCall()
          resp = {type: "ok"}
          break
        default:
          resp = {type: "error", message: "unknown command"}
          break
      }
    } catch (e) {
      resp = {type: "error", message: `${command.type}: ${(e as Error).message}`}
    }
    const apiResp = {corrId, resp, command}
    sendMessageToNative(apiResp)
    return apiResp
  }

  function endCall() {
    try {
      activeCall?.connection?.close()
    } catch (e) {
      console.log(e)
    }
    activeCall = undefined
    resetVideoElements()
  }

  function addIceCandidates(conn: RTCPeerConnection, iceCandidates: RTCIceCandidateInit[]) {
    for (const c of iceCandidates) {
      conn.addIceCandidate(new RTCIceCandidate(c))
    }
  }

  async function setupMediaStreams(call: Call): Promise<void> {
    const videos = getVideoElements()
    if (!videos) throw Error("no video elements")
    await setupEncryptionWorker(call)
    setupLocalStream(call)
    setupRemoteStream(call)
    setupCodecPreferences(call)
    // setupVideoElement(videos.local)
    // setupVideoElement(videos.remote)
    videos.local.srcObject = call.localStream
    videos.remote.srcObject = call.remoteStream
  }

  async function setupEncryptionWorker(call: Call) {
    if (call.aesKey) {
      if (!call.key) call.key = await callCrypto.decodeAesKey(call.aesKey)
      if (call.useWorker && !call.worker) {
        const workerCode = `const callCrypto = (${callCryptoFunction.toString()})(); (${workerFunction.toString()})()`
        call.worker = new Worker(URL.createObjectURL(new Blob([workerCode], {type: "text/javascript"})))
        call.worker.onerror = ({error, filename, lineno, message}: ErrorEvent) =>
          console.log(JSON.stringify({error, filename, lineno, message}))
        call.worker.onmessage = ({data}) => console.log(JSON.stringify({message: data}))
      }
    }
  }

  function setupLocalStream(call: Call) {
    const videos = getVideoElements()
    if (!videos) throw Error("no video elements")
    const pc = call.connection
    let {localStream} = call

    for (const track of localStream.getTracks()) {
      pc.addTrack(track, localStream)
    }

    if (call.aesKey && call.key) {
      console.log("set up encryption for sending")
      for (const sender of pc.getSenders() as RTCRtpSenderWithEncryption[]) {
        setupPeerTransform(TransformOperation.Encrypt, sender, call.worker, call.aesKey, call.key)
      }
    }
  }

  function setupRemoteStream(call: Call) {
    // Pull tracks from remote stream as they arrive add them to remoteStream video
    const pc = call.connection
    pc.ontrack = (event) => {
      try {
        if (call.aesKey && call.key) {
          console.log("set up decryption for receiving")
          setupPeerTransform(TransformOperation.Decrypt, event.receiver as RTCRtpReceiverWithEncryption, call.worker, call.aesKey, call.key)
        }
        for (const stream of event.streams) {
          for (const track of stream.getTracks()) {
            call.remoteStream.addTrack(track)
          }
        }
        console.log(`ontrack success`)
      } catch (e) {
        console.log(`ontrack error: ${(e as Error).message}`)
      }
    }
  }

  function setupCodecPreferences(call: Call) {
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
      for (const t of call.connection.getTransceivers()) {
        if (t.sender.track?.kind === "video") {
          t.setCodecPreferences(codecs)
        }
      }
    }
  }

  async function replaceMedia(call: Call, camera: VideoCamera): Promise<void> {
    const videos = getVideoElements()
    if (!videos) throw Error("no video elements")
    const pc = call.connection
    for (const t of call.localStream.getTracks()) t.stop()
    call.localCamera = camera
    const localStream = await getLocalMediaStream(call.localMedia, camera)
    replaceTracks(pc, localStream.getVideoTracks())
    replaceTracks(pc, localStream.getAudioTracks())
    call.localStream = localStream
    videos.local.srcObject = localStream
  }

  function replaceTracks(pc: RTCPeerConnection, tracks: MediaStreamTrack[]) {
    if (!tracks.length) return
    const sender = pc.getSenders().find((s) => s.track?.kind === tracks[0].kind)
    if (sender) for (const t of tracks) sender.replaceTrack(t)
  }

  function setupPeerTransform(
    operation: TransformOperation,
    peer: RTCRtpReceiverWithEncryption | RTCRtpSenderWithEncryption,
    worker: Worker | undefined,
    aesKey: string,
    key: CryptoKey
  ) {
    if (worker && "RTCRtpScriptTransform" in window) {
      console.log(`${operation} with worker & RTCRtpScriptTransform`)
      peer.transform = new RTCRtpScriptTransform(worker, {operation, aesKey})
    } else if ("createEncodedStreams" in peer) {
      const {readable, writable} = peer.createEncodedStreams()
      if (worker) {
        console.log(`${operation} with worker`)
        worker.postMessage({operation, readable, writable, aesKey}, [readable, writable] as unknown as Transferable[])
      } else {
        console.log(`${operation} without worker`)
        const transform = callCrypto.transformFrame[operation](key)
        readable.pipeThrough(new TransformStream({transform})).pipeTo(writable)
      }
    } else {
      console.log(`no ${operation}`)
    }
  }

  function getLocalMediaStream(mediaType: CallMediaType, facingMode: VideoCamera): Promise<MediaStream> {
    const constraints = callMediaConstraints(mediaType, facingMode)
    return navigator.mediaDevices.getUserMedia(constraints)
  }

  function callMediaConstraints(mediaType: CallMediaType, facingMode: VideoCamera): MediaStreamConstraints {
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
            facingMode,
          },
        }
    }
  }

  function supportsInsertableStreams(useWorker: boolean | undefined): boolean {
    return (
      ("createEncodedStreams" in RTCRtpSender.prototype && "createEncodedStreams" in RTCRtpReceiver.prototype) ||
      (!!useWorker && "RTCRtpScriptTransform" in window)
    )
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

  function enableMedia(s: MediaStream, media: CallMediaType, enable: boolean) {
    const tracks = media == CallMediaType.Video ? s.getVideoTracks() : s.getAudioTracks()
    for (const t of tracks) t.enabled = enable
  }

  return processCommand
})()

type TransformFrameFunc = (key: CryptoKey) => (frame: RTCEncodedVideoFrame, controller: TransformStreamDefaultController) => Promise<void>

interface CallCrypto {
  transformFrame: {[x in TransformOperation]: TransformFrameFunc}
  decodeAesKey: (aesKey: string) => Promise<CryptoKey>
  encodeAscii: (s: string) => Uint8Array
  decodeAscii: (a: Uint8Array) => string
  encodeBase64url: (a: Uint8Array) => Uint8Array
  decodeBase64url: (b64: Uint8Array) => Uint8Array | undefined
}

interface RTCEncodedVideoFrame {
  type: "key" | "delta"
  data: ArrayBuffer
}

// Cryptography function - it is loaded both in the main window and in worker context (if the worker is used)
function callCryptoFunction(): CallCrypto {
  const initialPlainTextRequired = {
    key: 10,
    delta: 3,
  }

  const IV_LENGTH = 12

  function encryptFrame(key: CryptoKey): (frame: RTCEncodedVideoFrame, controller: TransformStreamDefaultController) => Promise<void> {
    return async (frame, controller) => {
      const data = new Uint8Array(frame.data)
      const n = initialPlainTextRequired[frame.type] || 1
      const iv = randomIV()
      const initial = data.subarray(0, n)
      const plaintext = data.subarray(n, data.byteLength)
      try {
        const ciphertext = new Uint8Array(
          plaintext.length ? await crypto.subtle.encrypt({name: "AES-GCM", iv: iv.buffer}, key, plaintext) : 0
        )
        frame.data = concatN(initial, ciphertext, iv).buffer
        controller.enqueue(frame)
      } catch (e) {
        console.log(`encryption error ${e}`)
        throw e
      }
    }
  }

  function decryptFrame(key: CryptoKey): (frame: RTCEncodedVideoFrame, controller: TransformStreamDefaultController) => Promise<void> {
    return async (frame, controller) => {
      const data = new Uint8Array(frame.data)
      const n = initialPlainTextRequired[frame.type] || 1
      const initial = data.subarray(0, n)
      const ciphertext = data.subarray(n, data.byteLength - IV_LENGTH)
      const iv = data.subarray(data.byteLength - IV_LENGTH, data.byteLength)
      try {
        const plaintext = new Uint8Array(ciphertext.length ? await crypto.subtle.decrypt({name: "AES-GCM", iv}, key, ciphertext) : 0)
        frame.data = concatN(initial, plaintext).buffer
        controller.enqueue(frame)
      } catch (e) {
        console.log(`decryption error ${e}`)
        throw e
      }
    }
  }

  function decodeAesKey(aesKey: string): Promise<CryptoKey> {
    const keyData = callCrypto.decodeBase64url(callCrypto.encodeAscii(aesKey))
    return crypto.subtle.importKey("raw", keyData!, {name: "AES-GCM", length: 256}, true, ["encrypt", "decrypt"])
  }

  function concatN(...bs: Uint8Array[]): Uint8Array {
    const a = new Uint8Array(bs.reduce((size, b) => size + b.byteLength, 0))
    bs.reduce((offset, b: Uint8Array) => {
      a.set(b, offset)
      return offset + b.byteLength
    }, 0)
    return a
  }

  function randomIV() {
    return crypto.getRandomValues(new Uint8Array(IV_LENGTH))
  }

  const base64urlChars = new Uint8Array(
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".split("").map((c) => c.charCodeAt(0))
  )

  const base64urlLookup = new Array(256) as (number | undefined)[]
  base64urlChars.forEach((c, i) => (base64urlLookup[c] = i))

  const char_equal = "=".charCodeAt(0)

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

  function encodeBase64url(a: Uint8Array): Uint8Array {
    const len = a.length
    const b64len = Math.ceil(len / 3) * 4
    const b64 = new Uint8Array(b64len)

    let j = 0
    for (let i = 0; i < len; i += 3) {
      b64[j++] = base64urlChars[a[i] >> 2]
      b64[j++] = base64urlChars[((a[i] & 3) << 4) | (a[i + 1] >> 4)]
      b64[j++] = base64urlChars[((a[i + 1] & 15) << 2) | (a[i + 2] >> 6)]
      b64[j++] = base64urlChars[a[i + 2] & 63]
    }

    if (len % 3) b64[b64len - 1] = char_equal
    if (len % 3 === 1) b64[b64len - 2] = char_equal

    return b64
  }

  function decodeBase64url(b64: Uint8Array): Uint8Array | undefined {
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
      const enc1 = base64urlLookup[b64[i++]]
      const enc2 = i < len ? base64urlLookup[b64[i++]] : 0
      const enc3 = i < len ? base64urlLookup[b64[i++]] : 0
      const enc4 = i < len ? base64urlLookup[b64[i++]] : 0
      if (enc1 === undefined || enc2 === undefined || enc3 === undefined || enc4 === undefined) return
      bytes[pos++] = (enc1 << 2) | (enc2 >> 4)
      bytes[pos++] = ((enc2 & 15) << 4) | (enc3 >> 2)
      bytes[pos++] = ((enc3 & 3) << 6) | (enc4 & 63)
    }

    return bytes
  }

  return {
    transformFrame: {encrypt: encryptFrame, decrypt: decryptFrame},
    decodeAesKey,
    encodeAscii,
    decodeAscii,
    encodeBase64url,
    decodeBase64url,
  }
}

// If the worker is used for decryption, this function code (as string) is used to load the worker via Blob
// We have to use worker optionally, as it crashes in Android web view, regardless of how it is loaded
function workerFunction() {
  interface WorkerMessage {
    data: Transform
  }

  interface Transform {
    operation: TransformOperation
    readable: ReadableStream<RTCEncodedVideoFrame>
    writable: WritableStream<RTCEncodedVideoFrame>
    aesKey: string
  }

  // encryption with createEncodedStreams support
  self.addEventListener("message", async ({data}: WorkerMessage) => {
    await setupTransform(data)
  })

  // encryption using RTCRtpScriptTransform.
  if ("RTCTransformEvent" in self) {
    self.addEventListener("rtctransform", async ({transformer}: any) => {
      try {
        const {operation, aesKey} = transformer.options
        const {readable, writable} = transformer
        await setupTransform({operation, aesKey, readable, writable})
        self.postMessage({result: "setupTransform success"})
      } catch (e) {
        self.postMessage({message: `setupTransform error: ${(e as Error).message}`})
      }
    })
  }

  async function setupTransform({operation, aesKey, readable, writable}: Transform): Promise<void> {
    const key = await callCrypto.decodeAesKey(aesKey)
    const transform = callCrypto.transformFrame[operation](key)
    readable.pipeThrough(new TransformStream({transform})).pipeTo(writable)
  }
}
