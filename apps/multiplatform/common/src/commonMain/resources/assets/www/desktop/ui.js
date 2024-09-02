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
function toggleAudioManually() {
    if (activeCall && localMedia(activeCall)) {
        document.getElementById("toggle-audio").innerHTML = toggleMedia(activeCall.localStream, CallMediaType.Audio)
            ? '<img src="/desktop/images/ic_mic.svg" />'
            : '<img src="/desktop/images/ic_mic_off.svg" />';
    }
}
function toggleSpeakerManually() {
    if (activeCall === null || activeCall === void 0 ? void 0 : activeCall.remoteStream) {
        document.getElementById("toggle-speaker").innerHTML = toggleMedia(activeCall.remoteStream, CallMediaType.Audio)
            ? '<img src="/desktop/images/ic_volume_up.svg" />'
            : '<img src="/desktop/images/ic_volume_down.svg" />';
    }
}
function toggleVideoManually() {
    if (activeCall) {
        if (activeCall.localMediaSources.screen) {
            activeCall.localMediaSources.camera = !activeCall.localMediaSources.camera;
            enableVideoIcon(activeCall.localMediaSources.camera);
            // } else if (activeCall.localMedia == CallMediaType.Video) {
            //   enableVideoIcon(toggleMedia(activeCall.localStream, CallMediaType.Video))
        }
        else {
            const apiCall = { command: { type: "media", media: CallMediaType.Video, enable: activeCall.localMediaSources.camera != true } };
            reactOnMessageFromServer(apiCall);
            processCommand(apiCall).then(() => {
                var _a;
                enableVideoIcon(((_a = activeCall === null || activeCall === void 0 ? void 0 : activeCall.localMediaSources) === null || _a === void 0 ? void 0 : _a.camera) == true);
            });
        }
    }
}
async function toggleScreenManually() {
    var _a;
    const was = activeCall === null || activeCall === void 0 ? void 0 : activeCall.localMediaSources.screen;
    await toggleScreenShare();
    if (was != (activeCall === null || activeCall === void 0 ? void 0 : activeCall.localMediaSources.screen)) {
        document.getElementById("toggle-screen").innerHTML = ((_a = activeCall === null || activeCall === void 0 ? void 0 : activeCall.localMediaSources) === null || _a === void 0 ? void 0 : _a.screen)
            ? '<img src="/desktop/images/ic_stop_screen_share.svg" />'
            : '<img src="/desktop/images/ic_screen_share.svg" />';
    }
}
function enableVideoIcon(enabled) {
    document.getElementById("toggle-video").innerHTML = enabled
        ? '<img src="/desktop/images/ic_videocam_filled.svg" />'
        : '<img src="/desktop/images/ic_videocam_off.svg" />';
}
function reactOnMessageFromServer(msg) {
    var _a;
    switch ((_a = msg.command) === null || _a === void 0 ? void 0 : _a.type) {
        case "capabilities":
            document.getElementById("info-block").className = msg.command.media;
            break;
        case "offer":
        case "start":
            document.getElementById("toggle-audio").style.display = "inline-block";
            document.getElementById("toggle-speaker").style.display = "inline-block";
            document.getElementById("toggle-video").style.display = "inline-block";
            document.getElementById("toggle-screen").style.display = "inline-block";
            document.getElementById("info-block").className = msg.command.media;
            break;
        case "media":
            const className = (msg.command.media == CallMediaType.Video && msg.command.enable) ||
                (activeCall === null || activeCall === void 0 ? void 0 : activeCall.peerMediaSources.camera) ||
                (activeCall === null || activeCall === void 0 ? void 0 : activeCall.peerMediaSources.screen)
                ? "video"
                : "audio";
            document.getElementById("info-block").className = className;
            document.getElementById("audio-call-icon").style.display = className == CallMediaType.Audio ? "block" : "none";
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
            const className = localMedia(activeCall) == CallMediaType.Video || activeCall.peerMediaSources.camera || activeCall.peerMediaSources.screen
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