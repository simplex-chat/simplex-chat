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

let pc
let candidates = []
run().then(console.log("Setup Complete"))

async function run() {
  pc = new RTCPeerConnection(peerConnectionConfig)

  pc.onicecandidate = (event) => {
    // add candidate to maintained list to be sent all at once
    if (event.candidate) {
      candidates.push(event.candidate)
    }
  }
  pc.onicegatheringstatechange = (event) => {
    if (pc.iceGatheringState == "complete") {
      // Give command for other caller to use
      console.log(JSON.stringify({action: "processIceCandidates", content: candidates}))
    }
  }

  let remoteStream = new MediaStream()
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
  let receiverStreams = receiver.createEncodedStreams()
  let transformStream = new TransformStream({
    transform: decodeFunction,
  })
  receiverStreams.readable.pipeThrough(transformStream).pipeTo(receiverStreams.writable)
}

/* Cryptography */
function encodeFunction(frame, controller) {
  controller.enqueue(frame)
}
function decodeFunction(frame, controller) {
  controller.enqueue(frame)
}

/*
AESKey = CryptoKey & {type: "secret", algorithm: AesKeyGenParams}
function randomAESKey(length = 256) {
  return crypto.subtle.generateKey({name: "AES-GCM", length}, true, ["encrypt", "decrypt"])
}
async function encryptAES(key, iv, padTo, data) {
  if (data.byteLength >= padTo) throw new CryptoError("large message")
  const padded = new Uint8Array(padTo)
  padded.set(new Uint8Array(data), 0)
  padded.fill(PADDING, data.byteLength)
  return crypto.subtle.encrypt({name: "AES-GCM", iv}, key, padded)
}
function decryptAES(key, iv, encryptedAndTag) {
  return crypto.subtle.decrypt({name: "AES-GCM", iv}, key, encryptedAndTag)
}
function randomIV() {
  return crypto.getRandomValues(new Uint8Array(16))
}
function encodeAESKey(key) {
  return crypto.subtle.exportKey("raw", key)
}

function decodeAESKey(rawKey) {
  return crypto.subtle.importKey("raw", rawKey, "AES-GCM", true, ["encrypt", "decrypt"])
}
*/

// use AES-GCP
// increment none on each frame
// sequential/random av
// turn servers https://github.com/coturn/coturn
//
