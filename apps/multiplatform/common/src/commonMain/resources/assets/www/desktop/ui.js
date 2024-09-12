"use strict";
// Override defaults to enable worker on Chrome and Safari
useWorker = typeof window.Worker !== "undefined";
isDesktop = true;
// Create WebSocket connection.
const socket = new WebSocket(`ws://${location.host}`);
socket.addEventListener("open", (_event) => {
    console.log("Opened socket");
    sendMessageToNative = (msg) => {
        console.log("Message to server");
        socket.send(JSON.stringify(msg));
        reactOnMessageToServer(msg);
    };
});
socket.addEventListener("message", (event) => {
    const parsed = JSON.parse(event.data);
    reactOnMessageFromServer(parsed);
    processCommand(parsed);
    console.log("Message from server");
});
socket.addEventListener("close", (_event) => {
    console.log("Closed socket");
    sendMessageToNative = (_msg) => {
        console.log("Tried to send message to native but the socket was closed already");
    };
    window.close();
});
function endCallManually() {
    sendMessageToNative({ resp: { type: "end" } });
}
function toggleMicManually() {
    if (activeCall === null || activeCall === void 0 ? void 0 : activeCall.localStream) {
        const apiCall = {
            command: { type: "media", source: CallMediaSource.Mic, enable: !activeCall.localMediaSources.mic },
        };
        processCommand(apiCall);
    }
}
function toggleSpeakerManually() {
    if (activeCall === null || activeCall === void 0 ? void 0 : activeCall.remoteStream) {
        document.getElementById("toggle-speaker").innerHTML = togglePeerMedia(activeCall.remoteStream, CallMediaType.Audio)
            ? '<img src="/desktop/images/ic_volume_up.svg" />'
            : '<img src="/desktop/images/ic_volume_down.svg" />';
    }
}
function toggleCameraManually() {
    if (activeCall) {
        const apiCall = {
            command: { type: "media", source: CallMediaSource.Camera, enable: activeCall.localMediaSources.camera != true },
        };
        processCommand(apiCall);
    }
}
async function toggleScreenManually() {
    await toggleScreenShare();
}
// override function in call.ts to adapt UI to enabled media sources
localOrPeerMediaSourcesChanged = (call) => {
    enableMicIcon(call.localMediaSources.mic);
    enableCameraIcon(call.localMediaSources.camera);
    enableScreenIcon(call.localMediaSources.screenVideo);
    const className = localMedia(call) == CallMediaType.Video || peerMedia(call) == CallMediaType.Video ? CallMediaType.Video : CallMediaType.Audio;
    document.getElementById("info-block").className = className;
    if (call.connection.iceConnectionState == "connected") {
        document.getElementById("audio-call-icon").style.display = className == CallMediaType.Audio ? "block" : "none";
    }
    document.getElementById("media-sources").innerText = mediaSourcesStatus(call);
};
// override function in call.ts to adapt UI to enabled media sources
inactiveCallMediaSourcesChanged = (inactiveCallMediaSources) => {
    const mic = inactiveCallMediaSources.get(CallMediaSource.Mic);
    const camera = inactiveCallMediaSources.get(CallMediaSource.Camera);
    const screenVideo = inactiveCallMediaSources.get(CallMediaSource.ScreenVideo);
    if (mic != undefined) {
        enableMicIcon(mic);
    }
    if (camera != undefined) {
        enableCameraIcon(camera);
    }
    if (screenVideo != undefined) {
        enableScreenIcon(screenVideo);
    }
    const className = camera ? CallMediaType.Video : CallMediaType.Audio;
    document.getElementById("info-block").className = className;
    document.getElementById("media-sources").innerText = inactiveCallMediaSourcesStatus(inactiveCallMediaSources);
};
function enableMicIcon(enabled) {
    document.getElementById("toggle-mic").innerHTML = enabled
        ? '<img src="/desktop/images/ic_mic.svg" />'
        : '<img src="/desktop/images/ic_mic_off.svg" />';
}
function enableCameraIcon(enabled) {
    document.getElementById("toggle-camera").innerHTML = enabled
        ? '<img src="/desktop/images/ic_videocam_filled.svg" />'
        : '<img src="/desktop/images/ic_videocam_off.svg" />';
}
function enableScreenIcon(enabled) {
    document.getElementById("toggle-screen").innerHTML = enabled
        ? '<img src="/desktop/images/ic_stop_screen_share.svg" />'
        : '<img src="/desktop/images/ic_screen_share.svg" />';
}
function mediaSourcesStatus(call) {
    let status = "local";
    if (call.localMediaSources.mic)
        status += " mic";
    if (call.localMediaSources.camera)
        status += " cam";
    if (call.localMediaSources.screenAudio)
        status += " scrA";
    if (call.localMediaSources.screenVideo)
        status += " scrV";
    status += " | peer";
    if (call.peerMediaSources.mic)
        status += " mic";
    if (call.peerMediaSources.camera)
        status += " cam";
    if (call.peerMediaSources.screenAudio)
        status += " scrA";
    if (call.peerMediaSources.screenVideo)
        status += " scrV";
    return status;
}
function inactiveCallMediaSourcesStatus(inactiveCallMediaSources) {
    let status = "local";
    const mic = inactiveCallMediaSources.get(CallMediaSource.Mic);
    const camera = inactiveCallMediaSources.get(CallMediaSource.Camera);
    const screenAudio = inactiveCallMediaSources.get(CallMediaSource.ScreenAudio);
    const screenVideo = inactiveCallMediaSources.get(CallMediaSource.ScreenVideo);
    if (mic == true)
        status += " mic";
    if (camera == true)
        status += " cam";
    if (screenAudio == true)
        status += " scrA";
    if (screenVideo == true)
        status += " scrV";
    return status;
}
function reactOnMessageFromServer(msg) {
    var _a;
    switch ((_a = msg.command) === null || _a === void 0 ? void 0 : _a.type) {
        case "capabilities":
            document.getElementById("info-block").className = msg.command.media;
            break;
        case "offer":
        case "start":
            document.getElementById("toggle-mic").style.display = "inline-block";
            document.getElementById("toggle-speaker").style.display = "inline-block";
            document.getElementById("toggle-camera").style.display = "inline-block";
            document.getElementById("toggle-screen").style.display = "inline-block";
            document.getElementById("info-block").className = msg.command.media;
            break;
        case "description":
            updateCallInfoView(msg.command.state, msg.command.description);
            if ((activeCall === null || activeCall === void 0 ? void 0 : activeCall.connection.connectionState) == "connected") {
                document.getElementById("progress").style.display = "none";
                document.getElementById("audio-call-icon").style.display =
                    document.getElementById("info-block").className == CallMediaType.Audio ? "block" : "none";
            }
            break;
    }
}
function reactOnMessageToServer(msg) {
    var _a;
    if (!activeCall)
        return;
    switch ((_a = msg.resp) === null || _a === void 0 ? void 0 : _a.type) {
        case "peerMedia":
            const className = localMedia(activeCall) == CallMediaType.Video || activeCall.peerMediaSources.camera || activeCall.peerMediaSources.screenVideo
                ? "video"
                : "audio";
            document.getElementById("info-block").className = className;
            document.getElementById("audio-call-icon").style.display = className == CallMediaType.Audio ? "block" : "none";
            break;
    }
}
function updateCallInfoView(state, description) {
    document.getElementById("state").innerText = state;
    document.getElementById("description").innerText = description;
}
//# sourceMappingURL=ui.js.map