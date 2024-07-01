"use strict";
// Override defaults to enable worker on Chrome and Safari
useWorker = typeof window.Worker !== "undefined";
isDesktop = true;
// Create WebSocket connection.
const socket = new WebSocket(`ws://${location.host}`);
socket.addEventListener("open", (_event) => {
    console.log("Opened socket");
    sendMessageToNative = (msg) => {
        var _a;
        console.log(`Message to server will be sent: ${(_a = msg.command) === null || _a === void 0 ? void 0 : _a.type}`);
        socket.send(JSON.stringify(msg));
    };
});
socket.addEventListener("message", (event) => {
    var _a;
    const parsed = JSON.parse(event.data);
    reactOnMessageFromServer(parsed);
    processCommand(parsed);
    console.log(`Message from server finished processing: ${(_a = parsed.command) === null || _a === void 0 ? void 0 : _a.type}`);
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
    if (activeCall === null || activeCall === void 0 ? void 0 : activeCall.localMedia) {
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
    if (activeCall === null || activeCall === void 0 ? void 0 : activeCall.localMedia) {
        let res;
        if (activeCall === null || activeCall === void 0 ? void 0 : activeCall.screenShareEnabled) {
            activeCall.cameraEnabled = !activeCall.cameraEnabled;
            res = activeCall.cameraEnabled;
        }
        else {
            res = toggleMedia(activeCall.localStream, CallMediaType.Video);
        }
        document.getElementById("toggle-video").innerHTML = res
            ? '<img src="/desktop/images/ic_videocam_filled.svg" />'
            : '<img src="/desktop/images/ic_videocam_off.svg" />';
    }
}
async function toggleScreenManually() {
    const was = activeCall === null || activeCall === void 0 ? void 0 : activeCall.screenShareEnabled;
    await toggleScreenShare();
    if (was != (activeCall === null || activeCall === void 0 ? void 0 : activeCall.screenShareEnabled)) {
        document.getElementById("toggle-screen").innerHTML = (activeCall === null || activeCall === void 0 ? void 0 : activeCall.screenShareEnabled)
            ? '<img src="/desktop/images/ic_stop_screen_share.svg" />'
            : '<img src="/desktop/images/ic_screen_share.svg" />';
    }
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
            if (msg.command.media == CallMediaType.Video) {
                document.getElementById("toggle-video").style.display = "inline-block";
                document.getElementById("toggle-screen").style.display = "inline-block";
            }
            document.getElementById("info-block").className = msg.command.media;
            break;
        case "description":
            updateCallInfoView(msg.command.state, msg.command.description);
            if ((activeCall === null || activeCall === void 0 ? void 0 : activeCall.connection.connectionState) == "connected") {
                document.getElementById("progress").style.display = "none";
                if (document.getElementById("info-block").className == CallMediaType.Audio) {
                    document.getElementById("audio-call-icon").style.display = "block";
                }
            }
            break;
    }
}
function updateCallInfoView(state, description) {
    document.getElementById("state").innerText = state;
    document.getElementById("description").innerText = description;
}
//# sourceMappingURL=ui.js.map