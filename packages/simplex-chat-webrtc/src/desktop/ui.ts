// Override defaults to enable worker on Chrome and Safari
useWorker = typeof window.Worker !== "undefined"
isDesktop = true

// Create WebSocket connection.
const socket = new WebSocket(`ws://${location.host}`)

socket.addEventListener("open", (_event) => {
  console.log("Opened socket")
  sendMessageToNative = (msg: WVApiMessage) => {
    console.log(`Message to server will be sent: ${msg.command?.type}`)
    socket.send(JSON.stringify(msg))
  }
})

socket.addEventListener("message", (event) => {
  const parsed = JSON.parse(event.data)
  reactOnMessageFromServer(parsed)
  processCommand(parsed)
  console.log(`Message from server finished processing: ${parsed.command?.type}`)
})

socket.addEventListener("close", (_event) => {
  console.log("Closed socket")
  sendMessageToNative = (_msg: WVApiMessage) => {
    console.log("Tried to send message to native but the socket was closed already")
  }
  window.close()
})

function endCallManually() {
  sendMessageToNative({resp: {type: "end"}})
}

function toggleAudioManually() {
  if (activeCall?.localMedia) {
    document.getElementById("toggle-audio")!!.innerHTML = toggleMedia(activeCall.localStream, CallMediaType.Audio)
      ? '<img src="/desktop/images/ic_mic.svg" />'
      : '<img src="/desktop/images/ic_mic_off.svg" />'
  }
}

function toggleSpeakerManually() {
  if (activeCall?.remoteStream) {
    document.getElementById("toggle-speaker")!!.innerHTML = toggleMedia(activeCall.remoteStream, CallMediaType.Audio)
      ? '<img src="/desktop/images/ic_volume_up.svg" />'
      : '<img src="/desktop/images/ic_volume_down.svg" />'
  }
}

function toggleVideoManually() {
  if (activeCall?.localMedia) {
    let res: boolean
    if (activeCall?.screenShareEnabled) {
      activeCall.cameraEnabled = !activeCall.cameraEnabled
      res = activeCall.cameraEnabled
    } else {
      res = toggleMedia(activeCall.localStream, CallMediaType.Video)
    }
    document.getElementById("toggle-video")!!.innerHTML = res
      ? '<img src="/desktop/images/ic_videocam_filled.svg" />'
      : '<img src="/desktop/images/ic_videocam_off.svg" />'
  }
}

async function toggleScreenManually() {
  const was = activeCall?.screenShareEnabled
  await toggleScreenShare()
  if (was != activeCall?.screenShareEnabled) {
    document.getElementById("toggle-screen")!!.innerHTML = activeCall?.screenShareEnabled
      ? '<img src="/desktop/images/ic_stop_screen_share.svg" />'
      : '<img src="/desktop/images/ic_screen_share.svg" />'
  }
}

function reactOnMessageFromServer(msg: WVApiMessage) {
  switch (msg.command?.type) {
    case "capabilities":
      document.getElementById("info-block")!!.className = msg.command.media
      break
    case "offer":
    case "start":
      document.getElementById("toggle-audio")!!.style.display = "inline-block"
      document.getElementById("toggle-speaker")!!.style.display = "inline-block"
      if (msg.command.media == CallMediaType.Video) {
        document.getElementById("toggle-video")!!.style.display = "inline-block"
        document.getElementById("toggle-screen")!!.style.display = "inline-block"
      }
      document.getElementById("info-block")!!.className = msg.command.media
      break
    case "description":
      updateCallInfoView(msg.command.state, msg.command.description)
      if (activeCall?.connection.connectionState == "connected") {
        document.getElementById("progress")!.style.display = "none"
        if (document.getElementById("info-block")!!.className == CallMediaType.Audio) {
          document.getElementById("audio-call-icon")!.style.display = "block"
        }
      }
      break
  }
}

function updateCallInfoView(state: string, description: string) {
  document.getElementById("state")!!.innerText = state
  document.getElementById("description")!!.innerText = description
}
