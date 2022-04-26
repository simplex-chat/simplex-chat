let incomingVideo = document.getElementById("incoming-video-stream")
let outgoingVideo = document.getElementById("outgoing-video-stream")
incomingVideo.style.opacity = 0
//outgoingVideo.style.opacity = 0
incomingVideo.onplaying = () => {
  incomingVideo.style.opacity = 1
}
outgoingVideo.onplaying = () => {
  outgoingVideo.style.opacity = 1
}

// STUN servers
const servers = {
  iceServers: [{urls: ["stun:stun1.1.google.com:19302", "stun:stun2.1.google.com:19302"]}],
  iceCandidatePoolSize: 10,
}

let pc = RTCPeerConnection(servers)

let remoteStream = new MediaStream()
let localStream = getLocalVideoStream()
setUpVideos()

pc.onicecandidate = (event) => {
  // send candidate to recipient via simplex
  event.candidate && console.log("ICECANDIDATE\n" + event.candidate.toJSON())
}

function processInbound(data) {
  switch (data.action) {
    case "initiateCall":
      return makeRTCOffer(pc)
    case "iceCandidate":
      return processIceCandidate(data.content)
    case "rtcOffer":
      return processInboundOffer(data.content)
    default:
      console.log("JS: Unknown Command")
  }
}

async function makeRTCOffer(pc) {
  // For initiating a call
  let offerDescription = await pc.createOffer()
  await pc.setLocalDescription(offerDescription)
  let offer = {
    sdp: offerDescription.sdp,
    type: offerDescription.type,
  }
  return offer
}

async function answerRTCOffer(pc) {
  let answerDescription = await pc.createAnswer()
  await pc.setLocalDescription(answerDescription)
  let answer = {
    sdp: answerDescription.sdp,
    type: answerDescription.type,
  }
  return answer
}

function processIceCandidate(iceCandidateJSON) {
  let candidate = new RTCIceCandidate(iceCandidateJSON)
  pc.addIceCandidate(candidate)
}

function processInboundOffer(incomingJSON) {
  // Negotiating initial connection
  if (!pc.currentRemoteDescription) {
    let answer = new RTCSessionDescription(incomingJSON)
    pc.setRemoteDescription(answer)
  }
}

function setUpVideos() {
  localStream.getTracks().forEach((track) => {
    pc.addTrack(track, localStream)
  })
  // Pull tracks from remote stream as they arrive add them to remoteStream video
  pc.ontrack = (event) => {
    event.streams[0].getTracks().forEach((track) => {
      remoteStream.addTrack(track)
    })
  }
  outgoingVideo.srcObject = localStream
  incomingVideo.srcObject = remoteStream
}

async function getLocalVideoStream() {
  localStream = await navigator.mediaDevices.getUserMedia({
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
  return localStream
}

function toggleVideo(b) {
  if (b == "true") {
    localStream.getVideoTracks()[0].enabled = true
  } else {
    localStream.getVideoTracks()[0].enabled = false
  }
}
