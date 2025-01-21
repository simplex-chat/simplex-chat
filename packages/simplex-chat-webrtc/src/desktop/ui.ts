// Override defaults to enable worker on Chrome and Safari
useWorker = typeof window.Worker !== "undefined"
isDesktop = true

// Create WebSocket connection.
const socket = new WebSocket(`ws://${location.host}`)

socket.addEventListener("open", (_event) => {
  console.log("Opened socket")
  sendMessageToNative = (msg: WVApiMessage) => {
    console.log("Message to server")
    socket.send(JSON.stringify(msg))
    reactOnMessageToServer(msg)
  }
})

socket.addEventListener("message", (event) => {
  const parsed = JSON.parse(event.data)
  reactOnMessageFromServer(parsed)
  processCommand(parsed)
  console.log("Message from server")
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

function toggleMicManually() {
  const enable = activeCall ? !activeCall.localMediaSources.mic : !inactiveCallMediaSources.mic
  const apiCall: WVAPICall = {
    command: {type: "media", source: CallMediaSource.Mic, enable: enable},
  }
  processCommand(apiCall)
}

function toggleSpeakerManually() {
  if (activeCall?.remoteStream && activeCall.peerMediaSources.mic) {
    enableSpeakerIcon(togglePeerMedia(activeCall.remoteStream, CallMediaType.Audio), !activeCall.peerMediaSources.mic)
  }
}

function toggleCameraManually() {
  const enable = activeCall ? !activeCall.localMediaSources.camera : !inactiveCallMediaSources.camera
  const apiCall: WVAPICall = {
    command: {type: "media", source: CallMediaSource.Camera, enable: enable},
  }
  processCommand(apiCall)
}

async function toggleScreenManually() {
  await toggleScreenShare()
}

// override function in call.ts to adapt UI to enabled media sources
localOrPeerMediaSourcesChanged = (call: Call) => {
  enableMicIcon(call.localMediaSources.mic)
  enableCameraIcon(call.localMediaSources.camera)
  enableScreenIcon(call.localMediaSources.screenVideo)

  const className =
    localMedia(call) == CallMediaType.Video || peerMedia(call) == CallMediaType.Video ? CallMediaType.Video : CallMediaType.Audio
  document.getElementById("info-block")!.className = className

  if (call.connection.iceConnectionState == "connected") {
    document.getElementById("audio-call-icon")!.style.display = className == CallMediaType.Audio ? "block" : "none"
  }

  // document.getElementById("media-sources")!.innerText = mediaSourcesStatus(call)
  document.getElementById("manage-call")!.className = localMedia(call) == CallMediaType.Video ? CallMediaType.Video : ""
}

// override function in call.ts to adapt UI to enabled media sources
inactiveCallMediaSourcesChanged = (inactiveCallMediaSources: CallMediaSources) => {
  const mic = inactiveCallMediaSources.mic
  const camera = inactiveCallMediaSources.camera
  const screenVideo = inactiveCallMediaSources.screenVideo
  enableMicIcon(mic)
  enableCameraIcon(camera)
  enableScreenIcon(screenVideo)

  const className = camera ? CallMediaType.Video : CallMediaType.Audio
  document.getElementById("info-block")!.className = className
  // document.getElementById("media-sources")!.innerText = inactiveCallMediaSourcesStatus(inactiveCallMediaSources)
}

failedToGetPermissions = (title: string, description: string) => {
  document.getElementById("info-block")!.style.visibility = "hidden"
  document.getElementById("progress")!.style.visibility = "hidden"

  document.getElementById("permission-denied-title")!.innerText = title
  document.getElementById("permission-denied-desc")!.innerText = description

  document.getElementById("toggle-mic")!.style.visibility = "hidden"
  document.getElementById("toggle-camera")!.style.visibility = "hidden"
  document.getElementById("toggle-screen")!.style.visibility = "hidden"
  document.getElementById("toggle-speaker")!.style.visibility = "hidden"
}

function enableMicIcon(enabled: boolean) {
  document.getElementById("toggle-mic")!.innerHTML = enabled
    ? '<img src="/desktop/images/ic_mic.svg" />'
    : '<img src="/desktop/images/ic_mic_off.svg" />'
}

function enableCameraIcon(enabled: boolean) {
  document.getElementById("toggle-camera")!.innerHTML = enabled
    ? '<img src="/desktop/images/ic_videocam_filled.svg" />'
    : '<img src="/desktop/images/ic_videocam_off.svg" />'
}

function enableScreenIcon(enabled: boolean) {
  document.getElementById("toggle-screen")!.innerHTML = enabled
    ? '<img src="/desktop/images/ic_stop_screen_share.svg" />'
    : '<img src="/desktop/images/ic_screen_share.svg" />'
}

function enableSpeakerIcon(enabled: boolean, muted: boolean) {
  document.getElementById("toggle-speaker")!!.innerHTML = muted
    ? '<img src="/desktop/images/ic_volume_off.svg" />'
    : enabled
    ? '<img src="/desktop/images/ic_volume_up.svg" />'
    : '<img src="/desktop/images/ic_volume_down.svg" />'
  document.getElementById("toggle-speaker")!!.style.opacity = muted ? "0.7" : "1"
}

function mediaSourcesStatus(call: Call): string {
  let status = "local"
  if (call.localMediaSources.mic) status += " mic"
  if (call.localMediaSources.camera) status += " cam"
  if (call.localMediaSources.screenAudio) status += " scrA"
  if (call.localMediaSources.screenVideo) status += " scrV"

  status += " | peer"

  if (call.peerMediaSources.mic) status += " mic"
  if (call.peerMediaSources.camera) status += " cam"
  if (call.peerMediaSources.screenAudio) status += " scrA"
  if (call.peerMediaSources.screenVideo) status += " scrV"
  return status
}

function inactiveCallMediaSourcesStatus(inactiveCallMediaSources: CallMediaSources): string {
  let status = "local"
  const mic = inactiveCallMediaSources.mic
  const camera = inactiveCallMediaSources.camera
  const screenAudio = inactiveCallMediaSources.screenAudio
  const screenVideo = inactiveCallMediaSources.screenVideo
  if (mic) status += " mic"
  if (camera) status += " cam"
  if (screenAudio) status += " scrA"
  if (screenVideo) status += " scrV"
  return status
}

function reactOnMessageFromServer(msg: WVApiMessage) {
  // screen is not allowed to be enabled before connection estabilished
  if (msg.command?.type == "capabilities" || msg.command?.type == "offer") {
    document.getElementById("toggle-screen")!!.style.opacity = "0.7"
  } else if (activeCall) {
    document.getElementById("toggle-screen")!!.style.opacity = "1"
  }
  switch (msg.command?.type) {
    case "capabilities":
    case "offer":
    case "start":
      document.getElementById("info-block")!!.className = msg.command.media
      document.getElementById("toggle-mic")!!.style.display = "inline-block"
      document.getElementById("toggle-speaker")!!.style.display = "inline-block"
      document.getElementById("toggle-camera")!!.style.display = "inline-block"
      document.getElementById("toggle-screen")!!.style.display = "inline-block"
      enableSpeakerIcon(true, true)
      break
    case "description":
      updateCallInfoView(msg.command.state, msg.command.description)
      if (activeCall?.connection.connectionState == "connected") {
        document.getElementById("progress")!.style.display = "none"
        document.getElementById("audio-call-icon")!.style.display =
          document.getElementById("info-block")!!.className == CallMediaType.Audio ? "block" : "none"
      }
      break
  }
}

function reactOnMessageToServer(msg: WVApiMessage) {
  if (!activeCall) return

  switch (msg.resp?.type) {
    case "peerMedia":
      const className = localMedia(activeCall) == CallMediaType.Video || peerMedia(activeCall) == CallMediaType.Video ? "video" : "audio"
      document.getElementById("info-block")!!.className = className
      document.getElementById("audio-call-icon")!.style.display = className == CallMediaType.Audio ? "block" : "none"
      enableSpeakerIcon(
        activeCall.remoteStream.getAudioTracks().every((elem) => elem.enabled),
        !activeCall.peerMediaSources.mic
      )
      break
  }
}

function updateCallInfoView(state: string, description: string) {
  document.getElementById("state")!!.innerText = state
  document.getElementById("description")!!.innerText = description
}
