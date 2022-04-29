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

// STUN servers
const peerConnectionConfig = {
  iceServers: [{urls: ["stun:stun.l.google.com:19302"]}],
  iceCandidatePoolSize: 10,
  encodedInsertableStreams: true,
}
let keyGenConfig = {
  name: "AES-GCM",
  length: 256,
  tagLength: 128,
}
let keyUsages = ["encrypt", "decrypt"]

// Hardcode a key for development
let keyData = {alg: "A256GCM", ext: true, k: "JCMDWkhxLmPDhua0BUdhgv6Ac6hOtB9frSxJlnkTAK8", key_ops: keyUsages, kty: "oct"}

let pc
let key
// Hardcode iv as pulling iv from received data currently fails
let iv = Uint8Array.from([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12])
let IV_LENGTH = 12
// let encryptKeyRepresentation
let candidates = []
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
  setUpVideos(pc, localStream, remoteStream)
}

async function processCommand(data) {
  switch (data.action) {
    case "initiateCall":
      console.log("initiating call")
      let result = await makeOffer(pc)
      // Give command for callee to use
      console.log(
        JSON.stringify({
          action: "processAndAnswerOffer",
          content: result,
        })
      )
      return result
    case "processAndAnswerOffer":
      await processOffer(data.content)
      let answer = await answerOffer(pc)
      // Give command for callee to use
      console.log(
        JSON.stringify({
          action: "processOffer",
          content: answer,
        })
      )
      return answer
    case "processOffer":
      await processOffer(data.content)
      break
    case "processIceCandidates":
      processIceCandidates(data.content)
      break
    default:
      console.log("JS: Unknown Command")
  }
}

async function makeOffer(pc) {
  // For initiating a call. Send offer to callee
  let offerDescription = await pc.createOffer()
  await pc.setLocalDescription(offerDescription)
  let offer = {
    sdp: offerDescription.sdp,
    type: offerDescription.type,
  }
  return offer
}

async function answerOffer(pc) {
  let answerDescription = await pc.createAnswer()
  await pc.setLocalDescription(answerDescription)
  let answer = {
    sdp: answerDescription.sdp,
    type: answerDescription.type,
  }
  return answer
}

function processIceCandidates(iceCandidates) {
  iceCandidates.forEach((candidate) => processIceCandidate(candidate))
}

function processIceCandidate(iceCandidate) {
  let candidate = new RTCIceCandidate(iceCandidate)
  pc.addIceCandidate(candidate)
}

async function processOffer(offer) {
  // Negotiating initial connection
  if (!pc.currentRemoteDescription) {
    let remoteSessionDescription = new RTCSessionDescription(offer)
    await pc.setRemoteDescription(remoteSessionDescription)
  }
}

function setUpVideos(pc, localStream, remoteStream) {
  localStream.getTracks().forEach((track) => {
    pc.addTrack(track, localStream)
  })
  pc.getSenders().forEach(setupSenderTransform)
  // Pull tracks from remote stream as they arrive add them to remoteStream video
  pc.ontrack = (event) => {
    setupReceiverTransform(event.receiver)
    event.streams[0].getTracks().forEach((track) => {
      remoteStream.addTrack(track)
    })
  }
  // Use VP8 by default to limit depacketisation issues.
  // const {codecs} = RTCRtpSender.getCapabilities("video")
  // const selectedCodecIndex = codecs.findIndex((c) => c.mimeType === "video/VP8")
  // const selectedCodec = codecs[selectedCodecIndex]
  // codecs.splice(selectedCodecIndex, 1)
  // codecs.unshift(selectedCodec)
  // const transceiver = pc.getTransceivers().find((t) => t.sender && t.sender.track.kind === "video")
  // transceiver.setCodecPreferences(codecs)

  outgoingVideo.srcObject = localStream
  incomingVideo.srcObject = remoteStream
}

async function getLocalVideoStream() {
  return await navigator.mediaDevices.getUserMedia({
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
  })
}

function endCall() {
  pc.close()
}

function toggleVideo(b) {
  if (b == "true") {
    localStream.getVideoTracks()[0].enabled = true
  } else {
    localStream.getVideoTracks()[0].enabled = false
  }
}

function f() {
  console.log("Debug Function")
  return "Debugging Return"
}

/* Stream Transforms */
function setupSenderTransform(sender) {
  const senderStreams = sender.createEncodedStreams()
  const transformStream = new TransformStream({
    transform: encodeFunction,
  })
  senderStreams.readable.pipeThrough(transformStream).pipeTo(senderStreams.writable)
}

function setupReceiverTransform(receiver) {
  const receiverStreams = receiver.createEncodedStreams()
  const transformStream = new TransformStream({
    transform: decodeFunction,
  })
  receiverStreams.readable.pipeThrough(transformStream).pipeTo(receiverStreams.writable)
}

/* Cryptography */
function encodeFunction(frame, controller) {
  // frame is an RTCEncodedAudioFrame
  // frame.data is ArrayBuffer
  let data = new Uint8Array(frame.data)
  // let iv = randomIV()
  let initial = data.subarray(0, 10)
  let plaintext = data.subarray(10, data.byteLength)
  crypto.subtle
    .encrypt({name: "AES-GCM", iv: iv.buffer}, key, plaintext)
    .then((c) => {
      frame.data = concatN(initial, new Uint8Array(c), iv).buffer
      controller.enqueue(frame)
    })
    .catch((e) => {
      console.log("encrypt error")
      endCall()
      throw e
    })
}
function decodeFunction(frame, controller) {
  let data = new Uint8Array(frame.data)
  let initial = data.subarray(0, 10)
  let ciphertext = data.subarray(10, data.byteLength - IV_LENGTH)
  // Decrypt fails with IV pulled from received data
  // let iv = data.subarray(data.byteLength - IV_LENGTH, data.byteLength)
  crypto.subtle
    .decrypt({name: "AES-GCM", iv: iv.buffer}, key, ciphertext)
    .then((p) => {
      frame.data = concatN(initial, new Uint8Array(p)).buffer
      controller.enqueue(frame)
    })
    .catch((e) => {
      console.log("decrypt error")
      endCall()
      throw e
    })
}

function randomIV() {
  return crypto.getRandomValues(new Uint8Array(IV_LENGTH))
}
async function loadKey(keyData) {
  key = await crypto.subtle.importKey("jwk", keyData, keyGenConfig, false, keyUsages)
}

function concatN(...bs) {
  const a = new Uint8Array(bs.reduce((size, b) => size + b.byteLength, 0))
  bs.reduce((offset, b) => {
    a.set(b, offset)
    return offset + b.byteLength
  }, 0)
  return a
}

async function generateKey() {
  crypto.subtle
    .generateKey(keyGenConfig, true, keyUsages)
    .then((k) => {
      encryptKey = k
      return crypto.subtle.exportKey("jwk", encryptKey)
    })
    .then((r) => {
      encryptKeyRepresentation = r
      console.log(
        JSON.stringify({
          action: "processDecryptionKey",
          content: {
            key: encryptKeyRepresentation,
            iv: encryptIv,
          },
        })
      )
    })
}
