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
  iceServers: [{urls: ["stun:stun.l.google.com:19302"]}],
  iceCandidatePoolSize: 10,
}

let pc
run().then(() => console.log("finished"))

async function run() {
  pc = new RTCPeerConnection(servers)

  // This handler 'sends' any ICE candidates to the other peer, as they are received.
  pc.onicecandidate = (event) => {
    // send candidate to recipient via simplex
    console.log("ICECANDIDATE\n" + JSON.stringify(event))
    event.candidate && console.log("ICECANDIDATE\n" + JSON.stringify(event.candidate))
  }

  let remoteStream = new MediaStream()
  let localStream = await getLocalVideoStream()
  setUpVideos(pc, localStream, remoteStream)
}

async function processInbound(data) {
  switch (data.action) {
    case "initiateCall":
      console.log("initiating call")
      let result = await makeRTCOffer(pc)
      console.log(JSON.stringify(result))
      return
    case "iceCandidate":
      result = processIceCandidate(data.content)
      console.log(JSON.stringify(result))
      return
    case "rtcOffer":
      result = processInboundOffer(data.content)
      console.log(JSON.stringify(result))
      return
    default:
      console.log("JS: Unknown Command")
  }
}

async function makeRTCOffer(pc) {
  // For initiating a call. Send offer to callee
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
  return JSON.stringify(answer)
}

function processIceCandidate(iceCandidateJSON) {
  let candidate = new RTCIceCandidate(iceCandidateJSON.candidate)
  pc.addIceCandidate(candidate)
}

function processInboundOffer(incomingJSON) {
  // Negotiating initial connection
  if (!pc.currentRemoteDescription) {
    let answer = new RTCSessionDescription(incomingJSON.sdp)
    console.log("ANSWER\n\n\n" + JSON.stringify(answer))
    pc.setRemoteDescription(answer)
      .then((_) => {
        answerRTCOffer(pc)
      })
      .then((x) => console.log(x))
  }
}

function setUpVideos(pc, localStream, remoteStream) {
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

function toggleVideo(b) {
  if (b == "true") {
    localStream.getVideoTracks()[0].enabled = true
  } else {
    localStream.getVideoTracks()[0].enabled = false
  }
}
