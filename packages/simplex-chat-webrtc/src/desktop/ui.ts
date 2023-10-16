// Override defaults to enable worker on Chrome and Safari
useWorker = (window as any).safari !== undefined || navigator.userAgent.indexOf("Chrome") != -1

// Create WebSocket connection.
const socket = new WebSocket(`ws://${location.host}`)

socket.addEventListener("open", (_event) => {
  console.log("Opened socket")
  sendMessageToNative = (msg: WVApiMessage) => {
    reactOnMessageToServer(msg)
    console.log("Message to server: ", msg)
    socket.send(JSON.stringify(msg))
  }
})

socket.addEventListener("message", (event) => {
  const parsed = JSON.parse(event.data)
  processCommand(parsed)
  reactOnMessageFromServer(parsed)
  console.log("Message from server: ", event.data)
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
    document.getElementById("toggle-video")!!.innerHTML = toggleMedia(activeCall.localStream, CallMediaType.Video)
      ? '<img src="/desktop/images/ic_videocam_filled.svg" />'
      : '<img src="/desktop/images/ic_videocam_off.svg" />'
  }
}

function reactOnMessageToServer(msg: WVApiMessage) {
  if (msg.resp.type == "connection" && msg.resp.state.connectionState == "connected") {
    document.getElementById("progress")!.style.display = "none"
  }
}

function reactOnMessageFromServer(msg: WVApiMessage) {
  switch (msg.command?.type) {
    case "offer":
    case "start":
      document.getElementById("toggle-audio")!!.style.display = "inline-block"
      document.getElementById("toggle-speaker")!!.style.display = "inline-block"
      if (msg.command.media == "video") {
        document.getElementById("toggle-video")!!.style.display = "inline-block"
      }
      break
    case "description":
      updateCallInfoView(msg.command.state, msg.command.description)
      break
  }
}

function updateCallInfoView(state: string, description: string) {
  document.getElementById("state")!!.innerText = state
  document.getElementById("description")!!.innerText = description
}
