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
    const enable = activeCall ? !activeCall.localMediaSources.mic : !inactiveCallMediaSources.mic;
    const apiCall = {
        command: { type: "media", source: CallMediaSource.Mic, enable: enable },
    };
    processCommand(apiCall);
}
function toggleSpeakerManually() {
    if ((activeCall === null || activeCall === void 0 ? void 0 : activeCall.remoteStream) && activeCall.peerMediaSources.mic) {
        enableSpeakerIcon(togglePeerMedia(activeCall.remoteStream, CallMediaType.Audio), !activeCall.peerMediaSources.mic);
    }
}
function toggleCameraManually() {
    const enable = activeCall ? !activeCall.localMediaSources.camera : !inactiveCallMediaSources.camera;
    const apiCall = {
        command: { type: "media", source: CallMediaSource.Camera, enable: enable },
    };
    processCommand(apiCall);
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
    // document.getElementById("media-sources")!.innerText = mediaSourcesStatus(call)
    document.getElementById("manage-call").className = localMedia(call) == CallMediaType.Video ? CallMediaType.Video : "";
};
// override function in call.ts to adapt UI to enabled media sources
inactiveCallMediaSourcesChanged = (inactiveCallMediaSources) => {
    const mic = inactiveCallMediaSources.mic;
    const camera = inactiveCallMediaSources.camera;
    const screenVideo = inactiveCallMediaSources.screenVideo;
    enableMicIcon(mic);
    enableCameraIcon(camera);
    enableScreenIcon(screenVideo);
    const className = camera ? CallMediaType.Video : CallMediaType.Audio;
    document.getElementById("info-block").className = className;
    // document.getElementById("media-sources")!.innerText = inactiveCallMediaSourcesStatus(inactiveCallMediaSources)
};
failedToGetPermissions = (title, description) => {
    document.getElementById("info-block").style.visibility = "hidden";
    document.getElementById("progress").style.visibility = "hidden";
    document.getElementById("permission-denied-title").innerText = title;
    document.getElementById("permission-denied-desc").innerText = description;
    document.getElementById("toggle-mic").style.visibility = "hidden";
    document.getElementById("toggle-camera").style.visibility = "hidden";
    document.getElementById("toggle-screen").style.visibility = "hidden";
    document.getElementById("toggle-speaker").style.visibility = "hidden";
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
function enableSpeakerIcon(enabled, muted) {
    document.getElementById("toggle-speaker").innerHTML = muted
        ? '<img src="/desktop/images/ic_volume_off.svg" />'
        : enabled
            ? '<img src="/desktop/images/ic_volume_up.svg" />'
            : '<img src="/desktop/images/ic_volume_down.svg" />';
    document.getElementById("toggle-speaker").style.opacity = muted ? "0.7" : "1";
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
    const mic = inactiveCallMediaSources.mic;
    const camera = inactiveCallMediaSources.camera;
    const screenAudio = inactiveCallMediaSources.screenAudio;
    const screenVideo = inactiveCallMediaSources.screenVideo;
    if (mic)
        status += " mic";
    if (camera)
        status += " cam";
    if (screenAudio)
        status += " scrA";
    if (screenVideo)
        status += " scrV";
    return status;
}
function reactOnMessageFromServer(msg) {
    var _a, _b, _c;
    // screen is not allowed to be enabled before connection estabilished
    if (((_a = msg.command) === null || _a === void 0 ? void 0 : _a.type) == "capabilities" || ((_b = msg.command) === null || _b === void 0 ? void 0 : _b.type) == "offer") {
        document.getElementById("toggle-screen").style.opacity = "0.7";
    }
    else if (activeCall) {
        document.getElementById("toggle-screen").style.opacity = "1";
    }
    switch ((_c = msg.command) === null || _c === void 0 ? void 0 : _c.type) {
        case "capabilities":
        case "offer":
        case "start":
            document.getElementById("info-block").className = msg.command.media;
            document.getElementById("toggle-mic").style.display = "inline-block";
            document.getElementById("toggle-speaker").style.display = "inline-block";
            document.getElementById("toggle-camera").style.display = "inline-block";
            document.getElementById("toggle-screen").style.display = "inline-block";
            enableSpeakerIcon(true, true);
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
            const className = localMedia(activeCall) == CallMediaType.Video || peerMedia(activeCall) == CallMediaType.Video ? "video" : "audio";
            document.getElementById("info-block").className = className;
            document.getElementById("audio-call-icon").style.display = className == CallMediaType.Audio ? "block" : "none";
            enableSpeakerIcon(activeCall.remoteStream.getAudioTracks().every((elem) => elem.enabled), !activeCall.peerMediaSources.mic);
            break;
    }
}
function updateCallInfoView(state, description) {
    document.getElementById("state").innerText = state;
    document.getElementById("description").innerText = description;
}
//# sourceMappingURL=ui.js.map