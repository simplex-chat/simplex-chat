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
let candidates = []
run().then(console.log("Setup Complete"))

async function run() {
  pc = new RTCPeerConnection(servers)

  // This handler 'sends' any ICE candidates to the other peer, as they are received.
  pc.onicecandidate = (event) => {
    // add candidate to maintained list to be sent all at once
    if (event.candidate) {
      candidates.push(event.candidate)
    }
  }
  pc.onicegatheringstatechange = (event) => {
    if (pc.iceGatheringState == "complete") {
      // Give command for other caller to use
      console.log({action: "processIceCandidates", content: candidates})
    }
  }

  let remoteStream = new MediaStream()
  let localStream = await getLocalVideoStream()
  setUpVideos(pc, localStream, remoteStream)
}

function f() {
  console.log("Debug Function")
  return "Debugging Return"
}

async function processCommand(data) {
  switch (data.action) {
    case "initiateCall":
      console.log("initiating call")
      let result = await makeRTCOffer(pc)
      // Give command for callee to use
      console.log({
        action: "processAndAnswerOffer",
        content: result,
      })
      return result
    case "processIceCandidates":
      processIceCandidates(data.content)
      break
    case "processOffer":
      await processOffer(data.content)
      break
    case "processAndAnswerOffer":
      await processOffer(data.content)
      let answer = await answerOffer(pc)
      // Give command for callee to use
      console.log({
        action: "processOffer",
        content: answer,
      })
      return answer
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
