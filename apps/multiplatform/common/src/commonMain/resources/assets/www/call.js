"use strict";
// Inspired by
// https://github.com/webrtc/samples/blob/gh-pages/src/content/insertable-streams/endtoend-encryption
var CallMediaType;
(function (CallMediaType) {
    CallMediaType["Audio"] = "audio";
    CallMediaType["Video"] = "video";
})(CallMediaType || (CallMediaType = {}));
var CallMediaSource;
(function (CallMediaSource) {
    CallMediaSource["Mic"] = "mic";
    CallMediaSource["Camera"] = "camera";
    CallMediaSource["ScreenAudio"] = "screenAudio";
    CallMediaSource["ScreenVideo"] = "screenVideo";
    CallMediaSource["Unknown"] = "unknown";
})(CallMediaSource || (CallMediaSource = {}));
var VideoCamera;
(function (VideoCamera) {
    VideoCamera["User"] = "user";
    VideoCamera["Environment"] = "environment";
})(VideoCamera || (VideoCamera = {}));
var LayoutType;
(function (LayoutType) {
    LayoutType["Default"] = "default";
    LayoutType["LocalVideo"] = "localVideo";
    LayoutType["RemoteVideo"] = "remoteVideo";
})(LayoutType || (LayoutType = {}));
// for debugging
// var sendMessageToNative = ({resp}: WVApiMessage) => console.log(JSON.stringify({command: resp}))
var sendMessageToNative = (msg) => console.log(JSON.stringify(msg));
var toggleScreenShare = async () => { };
var localOrPeerMediaSourcesChanged = (_call) => { };
var inactiveCallMediaSourcesChanged = (_inactiveCallMediaSources) => { };
var failedToGetPermissions = (_title, _description) => { };
// Global object with cryptrographic/encoding functions
const callCrypto = callCryptoFunction();
var TransformOperation;
(function (TransformOperation) {
    TransformOperation["Encrypt"] = "encrypt";
    TransformOperation["Decrypt"] = "decrypt";
})(TransformOperation || (TransformOperation = {}));
function localMedia(call) {
    return call.localMediaSources.camera || call.localMediaSources.screenVideo ? CallMediaType.Video : CallMediaType.Audio;
}
function peerMedia(call) {
    return call.peerMediaSources.camera || call.peerMediaSources.screenVideo ? CallMediaType.Video : CallMediaType.Audio;
}
let inactiveCallMediaSources = {
    mic: false,
    camera: false,
    screenAudio: false,
    screenVideo: false,
};
let activeCall;
let notConnectedCall;
let answerTimeout = 30000;
var useWorker = false;
var isDesktop = false;
var localizedState = "";
var localizedDescription = "";
// Passing true here will send audio in screen record stream
const allowSendScreenAudio = false;
// When one side of a call sends candidates tot fast (until local & remote descriptions are set), that candidates
// will be stored here and then set when the call will be ready to process them
let afterCallInitializedCandidates = [];
const stopTrackOnAndroid = false;
const processCommand = (function () {
    const defaultIceServers = [
        { urls: ["stuns:stun.simplex.im:443"] },
        { urls: ["stun:stun.simplex.im:443"] },
        //{urls: ["turns:turn.simplex.im:443?transport=udp"], username: "private2", credential: "Hxuq2QxUjnhj96Zq2r4HjqHRj"},
        { urls: ["turns:turn.simplex.im:443?transport=tcp"], username: "private2", credential: "Hxuq2QxUjnhj96Zq2r4HjqHRj" },
    ];
    function getCallConfig(encodedInsertableStreams, iceServers, relay) {
        return {
            peerConnectionConfig: {
                iceServers: iceServers !== null && iceServers !== void 0 ? iceServers : defaultIceServers,
                iceCandidatePoolSize: 10,
                encodedInsertableStreams,
                iceTransportPolicy: relay ? "relay" : "all",
                // needed for Android WebView >= 69 && <= 72 where default was "plan-b" which is incompatible with transceivers
                sdpSemantics: "unified-plan",
            },
            iceCandidates: {
                delay: 750,
                extrasInterval: 1500,
                extrasTimeout: 12000,
            },
        };
    }
    function getIceCandidates(conn, config) {
        return new Promise((resolve, _) => {
            let candidates = [];
            let resolved = false;
            let extrasInterval;
            let extrasTimeout;
            const delay = setTimeout(() => {
                if (!resolved) {
                    resolveIceCandidates();
                    extrasInterval = setInterval(() => {
                        sendIceCandidates();
                    }, config.iceCandidates.extrasInterval);
                    extrasTimeout = setTimeout(() => {
                        clearInterval(extrasInterval);
                        sendIceCandidates();
                    }, config.iceCandidates.extrasTimeout);
                }
            }, config.iceCandidates.delay);
            conn.onicecandidate = ({ candidate: c }) => c && candidates.push(c);
            conn.onicegatheringstatechange = () => {
                if (conn.iceGatheringState == "complete") {
                    if (resolved) {
                        if (extrasInterval)
                            clearInterval(extrasInterval);
                        if (extrasTimeout)
                            clearTimeout(extrasTimeout);
                        sendIceCandidates();
                    }
                    else {
                        resolveIceCandidates();
                    }
                }
            };
            function resolveIceCandidates() {
                if (delay)
                    clearTimeout(delay);
                resolved = true;
                // console.log("resolveIceCandidates", JSON.stringify(candidates))
                console.log("resolveIceCandidates");
                const iceCandidates = serialize(candidates);
                candidates = [];
                resolve(iceCandidates);
            }
            function sendIceCandidates() {
                if (candidates.length === 0)
                    return;
                // console.log("sendIceCandidates", JSON.stringify(candidates))
                console.log("sendIceCandidates");
                const iceCandidates = serialize(candidates);
                candidates = [];
                sendMessageToNative({ resp: { type: "ice", iceCandidates } });
            }
        });
    }
    async function initializeCall(config, mediaType, aesKey) {
        var _a, _b, _c;
        let pc;
        try {
            pc = new RTCPeerConnection(config.peerConnectionConfig);
        }
        catch (e) {
            console.log("Error while constructing RTCPeerConnection, will try without 'stuns' specified: " + e);
            const withoutStuns = (_a = config.peerConnectionConfig.iceServers) === null || _a === void 0 ? void 0 : _a.filter((elem) => typeof elem.urls === "string" ? !elem.urls.startsWith("stuns:") : !elem.urls.some((url) => url.startsWith("stuns:")));
            config.peerConnectionConfig.iceServers = withoutStuns;
            pc = new RTCPeerConnection(config.peerConnectionConfig);
        }
        const remoteStream = new MediaStream();
        const remoteScreenStream = new MediaStream();
        const localCamera = (_b = notConnectedCall === null || notConnectedCall === void 0 ? void 0 : notConnectedCall.localCamera) !== null && _b !== void 0 ? _b : VideoCamera.User;
        let localStream;
        try {
            localStream = (notConnectedCall === null || notConnectedCall === void 0 ? void 0 : notConnectedCall.localStream)
                ? notConnectedCall.localStream
                : await getLocalMediaStream(inactiveCallMediaSources.mic, inactiveCallMediaSources.camera && (await browserHasCamera()), localCamera);
        }
        catch (e) {
            console.log("Error while getting local media stream", e);
            if (isDesktop) {
                desktopShowPermissionsAlert(mediaType);
                localStream = new MediaStream();
            }
            else {
                // On Android all streams should be present
                throw e;
            }
        }
        const localScreenStream = new MediaStream();
        // Will become video when any video tracks will be added
        const iceCandidates = getIceCandidates(pc, config);
        const call = {
            connection: pc,
            iceCandidates,
            localMediaSources: {
                mic: localStream.getAudioTracks().length > 0,
                camera: localStream.getVideoTracks().length > 0,
                screenAudio: localScreenStream.getAudioTracks().length > 0,
                screenVideo: localScreenStream.getVideoTracks().length > 0,
            },
            localCamera,
            localStream,
            localScreenStream,
            remoteStream,
            remoteTracks: new Map(),
            remoteScreenStream,
            peerMediaSources: {
                mic: false,
                camera: false,
                screenAudio: false,
                screenVideo: false,
            },
            aesKey,
            layout: (_c = notConnectedCall === null || notConnectedCall === void 0 ? void 0 : notConnectedCall.layout) !== null && _c !== void 0 ? _c : LayoutType.Default,
            cameraTrackWasSetBefore: localStream.getVideoTracks().length > 0,
            peerHasOldVersion: false,
        };
        localOrPeerMediaSourcesChanged(call);
        await setupMediaStreams(call);
        let connectionTimeout = setTimeout(connectionHandler, answerTimeout);
        if (pc.connectionState) {
            pc.addEventListener("connectionstatechange", connectionStateChange);
        }
        else {
            pc.addEventListener("iceconnectionstatechange", connectionStateChange);
        }
        return call;
        async function connectionStateChange() {
            // "failed" means the second party did not answer in time (15 sec timeout in Chrome WebView)
            // See https://source.chromium.org/chromium/chromium/src/+/main:third_party/webrtc/p2p/base/p2p_constants.cc;l=70)
            if (pc.connectionState !== "failed")
                connectionHandler();
        }
        async function connectionHandler() {
            var _a;
            sendMessageToNative({
                resp: {
                    type: "connection",
                    state: {
                        connectionState: (_a = pc.connectionState) !== null && _a !== void 0 ? _a : (pc.iceConnectionState != "completed" && pc.iceConnectionState != "checking"
                            ? pc.iceConnectionState
                            : pc.iceConnectionState == "completed"
                                ? "connected"
                                : "connecting") /* webView 69-70 doesn't have connectionState yet */,
                        iceConnectionState: pc.iceConnectionState,
                        iceGatheringState: pc.iceGatheringState,
                        signalingState: pc.signalingState,
                    },
                },
            });
            if (pc.connectionState == "disconnected" ||
                pc.connectionState == "failed" ||
                (!pc.connectionState && (pc.iceConnectionState == "disconnected" || pc.iceConnectionState == "failed"))) {
                clearConnectionTimeout();
                if (pc.connectionState) {
                    pc.removeEventListener("connectionstatechange", connectionStateChange);
                }
                else {
                    pc.removeEventListener("iceconnectionstatechange", connectionStateChange);
                }
                if (activeCall) {
                    setTimeout(() => sendMessageToNative({ resp: { type: "ended" } }), 0);
                }
                endCall();
            }
            else if (pc.connectionState == "connected" || (!pc.connectionState && pc.iceConnectionState == "connected")) {
                clearConnectionTimeout();
                const stats = (await pc.getStats());
                for (const stat of stats.values()) {
                    const { type, state } = stat;
                    if (type === "candidate-pair" && state === "succeeded") {
                        const iceCandidatePair = stat;
                        const resp = {
                            type: "connected",
                            connectionInfo: {
                                iceCandidatePair,
                                localCandidate: stats.get(iceCandidatePair.localCandidateId),
                                remoteCandidate: stats.get(iceCandidatePair.remoteCandidateId),
                            },
                        };
                        setTimeout(() => sendMessageToNative({ resp }), 500);
                        break;
                    }
                }
            }
        }
        function clearConnectionTimeout() {
            if (connectionTimeout) {
                clearTimeout(connectionTimeout);
                connectionTimeout = undefined;
            }
        }
    }
    function serialize(x) {
        return LZString.compressToBase64(JSON.stringify(x));
    }
    function parse(s) {
        return JSON.parse(LZString.decompressFromBase64(s));
    }
    async function processCommand(body) {
        var _a;
        const { corrId, command } = body;
        const pc = activeCall === null || activeCall === void 0 ? void 0 : activeCall.connection;
        let resp;
        try {
            switch (command.type) {
                case "capabilities":
                    console.log("starting outgoing call - capabilities");
                    if (activeCall)
                        endCall();
                    let localStream = null;
                    try {
                        localStream = await getLocalMediaStream(true, command.media == CallMediaType.Video && (await browserHasCamera()), VideoCamera.User);
                        const videos = getVideoElements();
                        if (videos) {
                            setupLocalVideoRatio(videos.local);
                            videos.local.srcObject = localStream;
                            videos.local.play().catch((e) => console.log(e));
                        }
                    }
                    catch (e) {
                        console.log(e);
                        // Do not allow to continue the call without audio permission
                        resp = { type: "error", message: "capabilities: no permissions were granted for mic and/or camera" };
                        break;
                        localStream = new MediaStream();
                        // Will be shown on the next stage of call estabilishing, can work without any streams
                        //desktopShowPermissionsAlert(command.media)
                    }
                    // Specify defaults that can be changed via UI before call estabilished. It's only used before activeCall instance appears
                    inactiveCallMediaSources.mic = localStream != null && localStream.getAudioTracks().length > 0;
                    inactiveCallMediaSources.camera = localStream != null && localStream.getVideoTracks().length > 0;
                    inactiveCallMediaSourcesChanged(inactiveCallMediaSources);
                    notConnectedCall = {
                        localCamera: VideoCamera.User,
                        localStream: localStream,
                        layout: LayoutType.Default,
                    };
                    const encryption = supportsInsertableStreams(useWorker);
                    resp = { type: "capabilities", capabilities: { encryption } };
                    break;
                case "start": {
                    console.log("starting incoming call - create webrtc session");
                    if (activeCall)
                        endCall();
                    // It can be already defined on Android when switching calls (if the previous call was outgoing)
                    notConnectedCall = undefined;
                    inactiveCallMediaSources.mic = true;
                    inactiveCallMediaSources.camera = command.media == CallMediaType.Video;
                    inactiveCallMediaSourcesChanged(inactiveCallMediaSources);
                    setupLocalVideoRatio(getVideoElements().local);
                    const { media, iceServers, relay } = command;
                    const encryption = supportsInsertableStreams(useWorker);
                    const aesKey = encryption ? command.aesKey : undefined;
                    activeCall = await initializeCall(getCallConfig(encryption && !!aesKey, iceServers, relay), media, aesKey);
                    await setupLocalStream(true, activeCall);
                    setupCodecPreferences(activeCall);
                    const pc = activeCall.connection;
                    const offer = await pc.createOffer();
                    await pc.setLocalDescription(offer);
                    // should be called after setLocalDescription in order to have transceiver.mid set
                    setupEncryptionForLocalStream(activeCall);
                    addIceCandidates(pc, afterCallInitializedCandidates);
                    afterCallInitializedCandidates = [];
                    // for debugging, returning the command for callee to use
                    // resp = {
                    //   type: "offer",
                    //   offer: serialize(offer),
                    //   iceCandidates: await activeCall.iceCandidates,
                    //   capabilities: {encryption},
                    //   media,
                    //   iceServers,
                    //   relay,
                    //   aesKey,
                    // }
                    resp = {
                        type: "offer",
                        offer: serialize(offer),
                        iceCandidates: await activeCall.iceCandidates,
                        capabilities: { encryption },
                    };
                    // console.log("offer response", JSON.stringify(resp))
                    break;
                }
                case "offer":
                    if (activeCall) {
                        resp = { type: "error", message: "accept: call already started" };
                    }
                    else if (!supportsInsertableStreams(useWorker) && command.aesKey) {
                        resp = { type: "error", message: "accept: encryption is not supported" };
                    }
                    else {
                        const offer = parse(command.offer);
                        const remoteIceCandidates = parse(command.iceCandidates);
                        const { media, aesKey, iceServers, relay } = command;
                        activeCall = await initializeCall(getCallConfig(!!aesKey, iceServers, relay), media, aesKey);
                        const pc = activeCall.connection;
                        // console.log("offer remoteIceCandidates", JSON.stringify(remoteIceCandidates))
                        await pc.setRemoteDescription(new RTCSessionDescription(!webView69Or70() ? offer : adaptSdpToOldWebView(offer)));
                        // setting up local stream only after setRemoteDescription in order to have transceivers set
                        await setupLocalStream(false, activeCall);
                        setupEncryptionForLocalStream(activeCall);
                        setupCodecPreferences(activeCall);
                        // enable using the same transceivers for sending media too, so total number of transceivers will be: audio, camera, screen audio, screen video
                        pc.getTransceivers().forEach((elem) => (elem.direction = "sendrecv"));
                        // setting media streams after remote description in order to have all transceivers ready (so ordering will be preserved)
                        // console.log(
                        //   "Transceivers",
                        //   pc.getTransceivers(),
                        //   pc.getTransceivers().map((elem) => "" + elem.mid + " " + elem.sender.track?.kind + " " + elem.sender.track?.label)
                        // )
                        adaptToOldVersion(pc.getTransceivers().length <= 2, activeCall);
                        let answer = await pc.createAnswer();
                        await pc.setLocalDescription(answer);
                        addIceCandidates(pc, remoteIceCandidates);
                        addIceCandidates(pc, afterCallInitializedCandidates);
                        afterCallInitializedCandidates = [];
                        // same as command for caller to use
                        resp = {
                            type: "answer",
                            answer: serialize(answer),
                            iceCandidates: await activeCall.iceCandidates,
                        };
                    }
                    // console.log("answer response", JSON.stringify(resp))
                    break;
                case "answer":
                    if (!pc) {
                        resp = { type: "error", message: "answer: call not started" };
                    }
                    else if (!pc.localDescription) {
                        resp = { type: "error", message: "answer: local description is not set" };
                    }
                    else if (pc.currentRemoteDescription) {
                        resp = { type: "error", message: "answer: remote description already set" };
                    }
                    else {
                        const answer = parse(command.answer);
                        const remoteIceCandidates = parse(command.iceCandidates);
                        // console.log("answer remoteIceCandidates", JSON.stringify(remoteIceCandidates))
                        await pc.setRemoteDescription(new RTCSessionDescription(!webView69Or70() ? answer : adaptSdpToOldWebView(answer)));
                        adaptToOldVersion(pc.getTransceivers()[2].currentDirection == "sendonly", activeCall);
                        addIceCandidates(pc, remoteIceCandidates);
                        addIceCandidates(pc, afterCallInitializedCandidates);
                        afterCallInitializedCandidates = [];
                        resp = { type: "ok" };
                    }
                    break;
                case "ice":
                    const remoteIceCandidates = parse(command.iceCandidates);
                    if (pc) {
                        addIceCandidates(pc, remoteIceCandidates);
                        resp = { type: "ok" };
                    }
                    else {
                        afterCallInitializedCandidates.push(...remoteIceCandidates);
                        resp = { type: "error", message: "ice: call not started yet, will add candidates later" };
                    }
                    break;
                case "media":
                    if (!activeCall) {
                        if (!notConnectedCall) {
                            // call can have a slow startup and be in this place even before "capabilities" stage
                            resp = { type: "error", message: "media: call has not yet pass capabilities stage" };
                            break;
                        }
                        switch (command.source) {
                            case CallMediaSource.Mic:
                                inactiveCallMediaSources.mic = command.enable;
                                break;
                            case CallMediaSource.Camera:
                                inactiveCallMediaSources.camera = command.enable;
                                break;
                            case CallMediaSource.ScreenAudio:
                                inactiveCallMediaSources.screenAudio = command.enable;
                                break;
                            case CallMediaSource.ScreenVideo:
                                inactiveCallMediaSources.screenVideo = command.enable;
                                break;
                        }
                        inactiveCallMediaSourcesChanged(inactiveCallMediaSources);
                        recreateLocalStreamWhileNotConnected((_a = notConnectedCall === null || notConnectedCall === void 0 ? void 0 : notConnectedCall.localCamera) !== null && _a !== void 0 ? _a : VideoCamera.User);
                        resp = { type: "ok" };
                    }
                    else if (activeCall.peerHasOldVersion && command.source == CallMediaSource.Camera && !activeCall.peerMediaSources.camera) {
                        resp = { type: "error", message: "media: cannot enable camera since the peer has an old version" };
                    }
                    else if (!activeCall.cameraTrackWasSetBefore && command.source == CallMediaSource.Camera && command.enable) {
                        await startSendingCamera(activeCall, activeCall.localCamera);
                        resp = { type: "ok" };
                    }
                    else if ((command.source == CallMediaSource.Mic && activeCall.localStream.getAudioTracks().length > 0) ||
                        (command.source == CallMediaSource.Camera && activeCall.localStream.getVideoTracks().length > 0)) {
                        if (enableMedia(activeCall.localStream, command.source, command.enable)) {
                            resp = { type: "ok" };
                        }
                        else {
                            resp = { type: "error", message: "media: cannot enable media source" };
                        }
                    }
                    else {
                        if (await replaceMedia(activeCall, command.source, command.enable, activeCall.localCamera)) {
                            resp = { type: "ok" };
                        }
                        else {
                            resp = { type: "error", message: "media: cannot replace media source" };
                        }
                    }
                    break;
                case "camera":
                    if (!activeCall || !pc) {
                        if (notConnectedCall) {
                            recreateLocalStreamWhileNotConnected(command.camera);
                            resp = { type: "ok" };
                        }
                        else {
                            resp = { type: "error", message: "camera: call has not yet pass capabilities stage" };
                        }
                    }
                    else {
                        if (await replaceMedia(activeCall, CallMediaSource.Camera, true, command.camera)) {
                            resp = { type: "ok" };
                        }
                        else {
                            resp = { type: "error", message: "camera: cannot replace media source" };
                        }
                    }
                    break;
                case "description":
                    localizedState = command.state;
                    localizedDescription = command.description;
                    resp = { type: "ok" };
                    break;
                case "layout":
                    if (activeCall) {
                        activeCall.layout = command.layout;
                    }
                    else if (notConnectedCall) {
                        notConnectedCall.layout = command.layout;
                    }
                    changeLayout(command.layout);
                    resp = { type: "ok" };
                    break;
                case "end":
                    endCall();
                    resp = { type: "ok" };
                    break;
                case "permission":
                    failedToGetPermissions(command.title, permissionDescription(command));
                    resp = { type: "ok" };
                    break;
                default:
                    resp = { type: "error", message: "unknown command" };
                    break;
            }
        }
        catch (e) {
            resp = { type: "error", message: `${command.type}: ${e.message}` };
        }
        const apiResp = { corrId, resp, command };
        sendMessageToNative(apiResp);
        return apiResp;
    }
    function endCall() {
        var _a;
        shutdownCameraAndMic();
        try {
            (_a = activeCall === null || activeCall === void 0 ? void 0 : activeCall.connection) === null || _a === void 0 ? void 0 : _a.close();
        }
        catch (e) {
            console.log(e);
        }
        activeCall = undefined;
        resetVideoElements();
    }
    function addIceCandidates(conn, iceCandidates) {
        for (const c of iceCandidates) {
            conn.addIceCandidate(new RTCIceCandidate(c));
            // console.log("addIceCandidates", JSON.stringify(c))
        }
    }
    async function setupMediaStreams(call) {
        const videos = getVideoElements();
        if (!videos)
            throw Error("no video elements");
        await setupEncryptionWorker(call);
        setupRemoteStream(call);
        videos.localScreen.srcObject = call.localScreenStream;
        videos.remote.srcObject = call.remoteStream;
        videos.remoteScreen.srcObject = call.remoteScreenStream;
        // videos.localScreen.play()
        // For example, exception can be: NotAllowedError: play() failed because the user didn't interact with the document first
        videos.remote.play().catch((e) => console.log(e));
        videos.remoteScreen.play().catch((e) => console.log(e));
    }
    async function setupEncryptionWorker(call) {
        if (call.aesKey) {
            if (!call.key)
                call.key = await callCrypto.decodeAesKey(call.aesKey);
            if (useWorker && !call.worker) {
                const workerCode = `const callCrypto = (${callCryptoFunction.toString()})(); (${workerFunction.toString()})()`;
                call.worker = new Worker(URL.createObjectURL(new Blob([workerCode], { type: "text/javascript" })));
                call.worker.onerror = ({ error, filename, lineno, message }) => console.log({ error, filename, lineno, message });
                // call.worker.onmessage = ({data}) => console.log(JSON.stringify({message: data}))
            }
        }
    }
    async function setupLocalStream(incomingCall, call) {
        var _a, _b, _c, _d;
        const videos = getVideoElements();
        if (!videos)
            throw Error("no video elements");
        const pc = call.connection;
        let { localStream } = call;
        const transceivers = call.connection.getTransceivers();
        const audioTracks = localStream.getAudioTracks();
        const videoTracks = localStream.getVideoTracks();
        if (incomingCall) {
            // incoming call, no transceivers yet. But they should be added in order: mic, camera, screen audio, screen video
            // mid = 0
            const audioTransceiver = pc.addTransceiver("audio", { streams: [localStream] });
            if (audioTracks.length != 0) {
                audioTransceiver.sender.replaceTrack(audioTracks[0]);
            }
            // mid = 1
            const videoTransceiver = pc.addTransceiver("video", { streams: [localStream] });
            if (videoTracks.length != 0) {
                videoTransceiver.sender.replaceTrack(videoTracks[0]);
            }
            if (call.localScreenStream.getAudioTracks().length == 0) {
                // mid = 2
                pc.addTransceiver("audio", { streams: [call.localScreenStream] });
            }
            if (call.localScreenStream.getVideoTracks().length == 0) {
                // mid = 3
                pc.addTransceiver("video", { streams: [call.localScreenStream] });
            }
        }
        else {
            // new version
            if (transceivers.length > 2) {
                // Outgoing call. All transceivers are ready. Don't addTrack() because it will create new transceivers, replace existing (null) tracks
                await ((_b = (_a = transceivers
                    .find((elem) => mediaSourceFromTransceiverMid(elem.mid) == CallMediaSource.Mic)) === null || _a === void 0 ? void 0 : _a.sender) === null || _b === void 0 ? void 0 : _b.replaceTrack(audioTracks[0]));
                await ((_d = (_c = transceivers
                    .find((elem) => mediaSourceFromTransceiverMid(elem.mid) == CallMediaSource.Camera)) === null || _c === void 0 ? void 0 : _c.sender) === null || _d === void 0 ? void 0 : _d.replaceTrack(videoTracks[0]));
            }
            else {
                // old version, only two transceivers
                for (const track of localStream.getTracks()) {
                    pc.addTrack(track, localStream);
                }
            }
        }
        // src can be set to notConnectedCall.localStream which is the same as call.localStream
        if (!videos.local.srcObject) {
            videos.local.srcObject = call.localStream;
        }
        // Without doing it manually Firefox shows black screen but video can be played in Picture-in-Picture
        videos.local.play().catch((e) => console.log(e));
    }
    function setupLocalVideoRatio(local) {
        local.addEventListener("loadedmetadata", function () {
            console.log("Local video videoWidth: " + local.videoWidth + "px,  videoHeight: " + local.videoHeight + "px");
            if (local.videoWidth == 0 || local.videoHeight == 0)
                return;
            const ratio = local.videoWidth > local.videoHeight ? 0.2 : 0.3;
            local.style.height = `${(ratio / (local.videoWidth / local.videoHeight)) * 100}vw`;
        });
        local.onresize = function () {
            console.log("Local video size changed to " + local.videoWidth + "x" + local.videoHeight);
            if (local.videoWidth == 0 || local.videoHeight == 0)
                return;
            const ratio = local.videoWidth > local.videoHeight ? 0.2 : 0.3;
            local.style.height = `${(ratio / (local.videoWidth / local.videoHeight)) * 100}vw`;
        };
    }
    function setupEncryptionForLocalStream(call) {
        if (call.aesKey && call.key) {
            const pc = call.connection;
            console.log("set up encryption for sending");
            let mid = 0;
            for (const transceiver of pc.getTransceivers()) {
                const sender = transceiver.sender;
                const source = mediaSourceFromTransceiverMid(mid.toString());
                setupPeerTransform(TransformOperation.Encrypt, sender, call.worker, call.aesKey, call.key, source == CallMediaSource.Camera || source == CallMediaSource.ScreenVideo ? CallMediaType.Video : CallMediaType.Audio);
                mid++;
            }
        }
    }
    function setupRemoteStream(call) {
        // Pull tracks from remote stream as they arrive add them to remoteStream video
        const pc = call.connection;
        pc.ontrack = (event) => {
            const track = event.track;
            //console.log("On track", event)
            try {
                if (call.aesKey && call.key) {
                    console.log("set up decryption for receiving");
                    setupPeerTransform(TransformOperation.Decrypt, event.receiver, call.worker, call.aesKey, call.key, event.receiver.track.kind == "video" ? CallMediaType.Video : CallMediaType.Audio);
                }
                setupMuteUnmuteListener(event.transceiver, track);
                const mediaSource = mediaSourceFromTransceiverMid(event.transceiver.mid);
                call.remoteTracks.set(mediaSource, track);
                console.log(`ontrack success`);
            }
            catch (e) {
                console.log(`ontrack error: ${e.message}`);
            }
        };
    }
    function setupCodecPreferences(call) {
        // We assume VP8 encoding in the decode/encode stages to get the initial
        // bytes to pass as plaintext so we enforce that here.
        // VP8 is supported by all supports of webrtc.
        // Use of VP8 by default may also reduce depacketisation issues.
        // We do not encrypt the first couple of bytes of the payload so that the
        // video elements can work by determining video keyframes and the opus mode
        // being used. This appears to be necessary for any video feed at all.
        // For VP8 this is the content described in
        //   https://tools.ietf.org/html/rfc6386#section-9.1
        // which is 10 bytes for key frames and 3 bytes for delta frames.
        // For opus (where encodedFrame.type is not set) this is the TOC byte from
        //   https://tools.ietf.org/html/rfc6716#section-3.1
        // Using RTCRtpReceiver instead of RTCRtpSender, see these lines:
        // -    if (!is_recv_codec && !is_send_codec) {
        // +    if (!is_recv_codec) {
        // https://webrtc.googlesource.com/src.git/+/db2f52ba88cf9f98211df2dabb3f8aca9251c4a2%5E%21/
        const capabilities = RTCRtpReceiver.getCapabilities("video");
        if (capabilities) {
            const { codecs } = capabilities;
            const selectedCodecIndex = codecs.findIndex((c) => c.mimeType === "video/VP8");
            const selectedCodec = codecs[selectedCodecIndex];
            codecs.splice(selectedCodecIndex, 1);
            codecs.unshift(selectedCodec);
            // On this stage transceiver.mid may not be set so using a sequence starting from 0 to decide which track.kind is inside
            let mid = 0;
            for (const t of call.connection.getTransceivers()) {
                // Firefox doesn't have this function implemented:
                // https://bugzilla.mozilla.org/show_bug.cgi?id=1396922
                const source = mediaSourceFromTransceiverMid(mid.toString());
                if ((source == CallMediaSource.Camera || source == CallMediaSource.ScreenVideo) && t.setCodecPreferences) {
                    try {
                        t.setCodecPreferences(codecs);
                    }
                    catch (error) {
                        // Shouldn't be here but in case something goes wrong, it will allow to make a call with auto-selected codecs
                        console.log("Failed to set codec preferences, trying without any preferences: " + error);
                    }
                }
                mid++;
            }
        }
    }
    async function startSendingCamera(call, camera) {
        console.log("Starting sending video...");
        const videos = getVideoElements();
        if (!videos)
            throw Error("no video elements");
        const pc = call.connection;
        // Taking the first video transceiver and use it for sending video from camera. Following tracks are for other purposes
        const tc = pc.getTransceivers().find((tc) => tc.receiver.track.kind == "video" && tc.direction == "sendrecv");
        if (!tc) {
            console.log("No camera transceiver. Probably, calling to an old version");
            return;
        }
        console.log(pc.getTransceivers().map((elem) => { var _a, _b; return "" + ((_a = elem.sender.track) === null || _a === void 0 ? void 0 : _a.kind) + " " + ((_b = elem.receiver.track) === null || _b === void 0 ? void 0 : _b.kind) + " " + elem.direction; }));
        let localStream;
        try {
            localStream = await getLocalMediaStream(call.localMediaSources.mic, true, camera);
            for (const t of localStream.getVideoTracks()) {
                call.localStream.addTrack(t);
                tc === null || tc === void 0 ? void 0 : tc.sender.replaceTrack(t);
                localStream.removeTrack(t);
            }
            call.localMediaSources.camera = true;
            call.cameraTrackWasSetBefore = true;
            localOrPeerMediaSourcesChanged(call);
            changeLayout(call.layout);
        }
        catch (e) {
            console.log("Start sending camera error", e);
            desktopShowPermissionsAlert(CallMediaType.Video);
            return;
        }
        // Without doing it manually Firefox shows black screen but video can be played in Picture-in-Picture
        videos.local.play().catch((e) => console.log(e));
    }
    toggleScreenShare = async function () {
        const call = activeCall;
        if (!call)
            return;
        const videos = getVideoElements();
        if (!videos)
            throw Error("no video elements");
        const pc = call.connection;
        if (!call.localMediaSources.screenVideo) {
            let localScreenStream;
            try {
                localScreenStream = await getLocalScreenCaptureStream();
            }
            catch (e) {
                return;
            }
            for (const t of localScreenStream.getTracks())
                call.localScreenStream.addTrack(t);
            for (const t of localScreenStream.getTracks())
                localScreenStream.removeTrack(t);
            pc.getTransceivers().forEach((elem) => {
                const source = mediaSourceFromTransceiverMid(elem.mid);
                const screenAudioTrack = call.localScreenStream.getTracks().find((elem) => elem.kind == "audio");
                const screenVideoTrack = call.localScreenStream.getTracks().find((elem) => elem.kind == "video");
                if (source == CallMediaSource.ScreenAudio && screenAudioTrack) {
                    elem.sender.replaceTrack(screenAudioTrack);
                    console.log("Replaced audio screen track");
                }
                else if (source == CallMediaSource.ScreenVideo && screenVideoTrack) {
                    elem.sender.replaceTrack(screenVideoTrack);
                    screenVideoTrack.onended = () => {
                        console.log("Ended screen video track");
                        toggleScreenShare();
                    };
                    console.log("Replaced screen video track");
                }
            });
            // videos.localScreen.pause()
            // videos.localScreen.srcObject = call.localScreenStream
            videos.localScreen.play().catch((e) => console.log(e));
        }
        else {
            pc.getTransceivers().forEach((elem) => {
                const source = mediaSourceFromTransceiverMid(elem.mid);
                if (source == CallMediaSource.ScreenAudio || source == CallMediaSource.ScreenVideo) {
                    elem.sender.replaceTrack(null);
                }
            });
            for (const t of call.localScreenStream.getTracks())
                t.stop();
            for (const t of call.localScreenStream.getTracks())
                call.localScreenStream.removeTrack(t);
        }
        if (allowSendScreenAudio) {
            call.localMediaSources.screenAudio = !call.localMediaSources.screenAudio;
        }
        call.localMediaSources.screenVideo = !call.localMediaSources.screenVideo;
        localOrPeerMediaSourcesChanged(call);
        changeLayout(call.layout);
    };
    async function replaceMedia(call, source, enable, camera) {
        const videos = getVideoElements();
        if (!videos)
            throw Error("no video elements");
        const pc = call.connection;
        // disabling track first, then asking for a new one.
        // doing it vice versa gives an error like "too many cameras were open" on some Android devices or webViews
        // which means the second camera will never be opened
        for (const t of source == CallMediaSource.Mic ? call.localStream.getAudioTracks() : call.localStream.getVideoTracks()) {
            if (isDesktop || source != CallMediaSource.Mic || stopTrackOnAndroid)
                t.stop();
            else
                t.enabled = false;
            call.localStream.removeTrack(t);
        }
        let localStream;
        try {
            localStream = await getLocalMediaStream(source == CallMediaSource.Mic ? enable : false, source == CallMediaSource.Camera ? enable : false, camera);
        }
        catch (e) {
            console.log("Replace media error", e);
            desktopShowPermissionsAlert(source == CallMediaSource.Mic ? CallMediaType.Audio : CallMediaType.Video);
            return false;
        }
        for (const t of localStream.getTracks()) {
            call.localStream.addTrack(t);
            localStream.removeTrack(t);
        }
        call.localCamera = camera;
        const audioTracks = call.localStream.getAudioTracks();
        const videoTracks = call.localStream.getVideoTracks();
        replaceTracks(pc, CallMediaSource.Mic, audioTracks);
        replaceTracks(pc, CallMediaSource.Camera, videoTracks);
        videos.local.play().catch((e) => console.log("replace media: local play", JSON.stringify(e)));
        call.localMediaSources.mic = call.localStream.getAudioTracks().length > 0;
        call.localMediaSources.camera = call.localStream.getVideoTracks().length > 0;
        localOrPeerMediaSourcesChanged(call);
        changeLayout(call.layout);
        return true;
    }
    function replaceTracks(pc, source, tracks) {
        var _a;
        const sender = (_a = pc.getTransceivers().find((elem) => mediaSourceFromTransceiverMid(elem.mid) == source)) === null || _a === void 0 ? void 0 : _a.sender;
        if (sender) {
            if (tracks.length > 0)
                for (const t of tracks) {
                    console.log("Replaced media track");
                    sender.replaceTrack(t);
                }
            else {
                console.log("Media track set to null");
                sender.replaceTrack(null);
            }
        }
    }
    async function recreateLocalStreamWhileNotConnected(newCamera) {
        const videos = getVideoElements();
        const localStream = notConnectedCall === null || notConnectedCall === void 0 ? void 0 : notConnectedCall.localStream;
        const oldCamera = notConnectedCall === null || notConnectedCall === void 0 ? void 0 : notConnectedCall.localCamera;
        if (!localStream || !oldCamera || !videos)
            return;
        if (!inactiveCallMediaSources.mic) {
            localStream.getAudioTracks().forEach((elem) => (isDesktop || stopTrackOnAndroid ? elem.stop() : (elem.enabled = false)));
            localStream.getAudioTracks().forEach((elem) => localStream.removeTrack(elem));
        }
        if (!inactiveCallMediaSources.camera || oldCamera != newCamera) {
            localStream.getVideoTracks().forEach((elem) => elem.stop());
            localStream.getVideoTracks().forEach((elem) => localStream.removeTrack(elem));
        }
        await getLocalMediaStream(inactiveCallMediaSources.mic && localStream.getAudioTracks().length == 0, inactiveCallMediaSources.camera && (localStream.getVideoTracks().length == 0 || oldCamera != newCamera) && (await browserHasCamera()), newCamera)
            .then((stream) => {
            stream.getTracks().forEach((elem) => {
                localStream.addTrack(elem);
                stream.removeTrack(elem);
            });
            if (notConnectedCall && localStream.getVideoTracks().length > 0) {
                notConnectedCall.localCamera = newCamera;
            }
        })
            .catch((e) => console.log("Error while enabling camera in not connected call", e));
        if (!videos.local.srcObject && localStream.getTracks().length > 0) {
            videos.local.srcObject = localStream;
        }
        videos.local.play().catch((e) => console.log(e));
    }
    function mediaSourceFromTransceiverMid(mid) {
        switch (mid) {
            case "0":
                return CallMediaSource.Mic;
            case "1":
                return CallMediaSource.Camera;
            case "2":
                return CallMediaSource.ScreenAudio;
            case "3":
                return CallMediaSource.ScreenVideo;
            default:
                return CallMediaSource.Unknown;
        }
    }
    function setupPeerTransform(operation, peer, worker, aesKey, key, media) {
        if (worker && "RTCRtpScriptTransform" in window) {
            console.log(`${operation} with worker & RTCRtpScriptTransform`);
            peer.transform = new RTCRtpScriptTransform(worker, { operation, aesKey, media });
        }
        else if ("createEncodedStreams" in peer) {
            const { readable, writable } = peer.createEncodedStreams();
            if (worker) {
                console.log(`${operation} with worker`);
                worker.postMessage({ operation, readable, writable, aesKey, media }, [readable, writable]);
            }
            else {
                console.log(`${operation} without worker`);
                const transform = callCrypto.transformFrame[operation](key);
                readable.pipeThrough(new TransformStream({ transform })).pipeTo(writable);
            }
        }
        else {
            console.log(`no ${operation}`);
        }
    }
    function setupMuteUnmuteListener(transceiver, track) {
        // console.log("Setting up mute/unmute listener in the call without encryption for mid = ", transceiver.mid)
        let inboundStatsId = "";
        let lastBytesReceived = 0;
        // muted initially
        let mutedSeconds = 4;
        let statsInterval = setInterval(async () => {
            var _a;
            const stats = await transceiver.receiver.getStats();
            if (!inboundStatsId) {
                stats.forEach((elem) => {
                    if (elem.type == "inbound-rtp") {
                        inboundStatsId = elem.id;
                    }
                });
            }
            if (inboundStatsId) {
                // even though MSDN site says `bytesReceived` is available in WebView 80+, in reality it's available even in 69
                const bytes = (_a = stats.get(inboundStatsId)) === null || _a === void 0 ? void 0 : _a.bytesReceived;
                if (bytes <= lastBytesReceived) {
                    mutedSeconds++;
                    if (mutedSeconds == 3) {
                        onMediaMuteUnmute(transceiver.mid, true);
                    }
                }
                else {
                    if (mutedSeconds >= 3) {
                        onMediaMuteUnmute(transceiver.mid, false);
                    }
                    lastBytesReceived = bytes;
                    mutedSeconds = 0;
                }
            }
        }, 1000);
        track.onended = (_) => {
            clearInterval(statsInterval);
        };
    }
    function onMediaMuteUnmute(transceiverMid, mute) {
        const videos = getVideoElements();
        if (!videos)
            throw Error("no video elements");
        if (!activeCall)
            return;
        const source = mediaSourceFromTransceiverMid(transceiverMid);
        console.log(`Mute/unmute ${source} track = ${mute} with mid = ${transceiverMid}`);
        const sources = activeCall.peerMediaSources;
        if (source == CallMediaSource.Mic && activeCall.peerMediaSources.mic == mute) {
            const resp = {
                type: "peerMedia",
                source: source,
                enabled: !mute,
            };
            sources.mic = !mute;
            activeCall.peerMediaSources = sources;
            sendMessageToNative({ resp: resp });
            if (!mute)
                videos.remote.play().catch((e) => console.log(e));
        }
        else if (source == CallMediaSource.Camera && activeCall.peerMediaSources.camera == mute) {
            const resp = {
                type: "peerMedia",
                source: source,
                enabled: !mute,
            };
            sources.camera = !mute;
            activeCall.peerMediaSources = sources;
            sendMessageToNative({ resp: resp });
            if (!mute)
                videos.remote.play().catch((e) => console.log(e));
        }
        else if (source == CallMediaSource.ScreenAudio && activeCall.peerMediaSources.screenAudio == mute) {
            const resp = {
                type: "peerMedia",
                source: source,
                enabled: !mute,
            };
            sources.screenAudio = !mute;
            activeCall.peerMediaSources = sources;
            sendMessageToNative({ resp: resp });
            if (!mute)
                videos.remoteScreen.play().catch((e) => console.log(e));
        }
        else if (source == CallMediaSource.ScreenVideo && activeCall.peerMediaSources.screenVideo == mute) {
            const resp = {
                type: "peerMedia",
                source: source,
                enabled: !mute,
            };
            sources.screenVideo = !mute;
            activeCall.peerMediaSources = sources;
            sendMessageToNative({ resp: resp });
            if (!mute)
                videos.remoteScreen.play().catch((e) => console.log(e));
        }
        if (!mute)
            addRemoteTracksWhenUnmuted(source, activeCall);
        localOrPeerMediaSourcesChanged(activeCall);
        // Make sure that remote camera and remote screen video in their places and shown/hidden based on layout type currently in use
        changeLayout(activeCall.layout);
    }
    /*
      When new remote tracks are coming, they don't get added to remote streams. They are stored in a map and once any of them "unmuted",
      that track is added to the stream. Such workaround needed because Safari doesn't play one stream
      if another one is not playing too, eg. no audio if only audio is playing while video track is present too but muted.
      But we have possibility to have only one currently active track, even no active track at all.
    */
    function addRemoteTracksWhenUnmuted(source, call) {
        const track = call.remoteTracks.get(source);
        if (track) {
            const stream = source == CallMediaSource.Mic || source == CallMediaSource.Camera ? call.remoteStream : call.remoteScreenStream;
            stream.addTrack(track);
            call.remoteTracks.delete(source);
        }
    }
    async function getLocalMediaStream(mic, camera, facingMode) {
        if (!mic && !camera)
            return new MediaStream();
        const constraints = callMediaConstraints(mic, camera, facingMode);
        return await navigator.mediaDevices.getUserMedia(constraints);
    }
    function getLocalScreenCaptureStream() {
        const constraints /* DisplayMediaStreamConstraints */ = {
            video: {
                frameRate: 24,
                //width: {
                //min: 480,
                //ideal: 720,
                //max: 1280,
                //},
                //aspectRatio: 1.33,
            },
            audio: allowSendScreenAudio,
            // This works with Chrome, Edge, Opera, but not with Firefox and Safari
            // systemAudio: "include"
        };
        return navigator.mediaDevices.getDisplayMedia(constraints);
    }
    async function browserHasCamera() {
        try {
            const devices = await navigator.mediaDevices.enumerateDevices();
            const hasCamera = devices.some((elem) => elem.kind == "videoinput");
            console.log("Camera is available: " + hasCamera);
            return hasCamera;
        }
        catch (error) {
            console.log("Error while enumerating devices: " + error, error);
            return false;
        }
    }
    function callMediaConstraints(mic, camera, facingMode) {
        return {
            audio: mic,
            video: !camera
                ? false
                : {
                    frameRate: 24,
                    width: {
                        min: 480,
                        ideal: 720,
                        max: 1280,
                    },
                    aspectRatio: 1.33,
                    facingMode,
                },
        };
    }
    function supportsInsertableStreams(useWorker) {
        return (("createEncodedStreams" in RTCRtpSender.prototype && "createEncodedStreams" in RTCRtpReceiver.prototype) ||
            (!!useWorker && "RTCRtpScriptTransform" in window));
    }
    function shutdownCameraAndMic() {
        if (activeCall) {
            activeCall.localStream.getTracks().forEach((track) => track.stop());
            activeCall.localScreenStream.getTracks().forEach((track) => track.stop());
        }
    }
    function resetVideoElements() {
        const videos = getVideoElements();
        if (!videos)
            return;
        videos.local.srcObject = null;
        videos.localScreen.srcObject = null;
        videos.remote.srcObject = null;
        videos.remoteScreen.srcObject = null;
    }
    // function setupVideoElement(video: HTMLElement) {
    //   // TODO use display: none
    //   video.style.opacity = "0"
    //   video.onplaying = () => {
    //     video.style.opacity = "1"
    //   }
    // }
    function enableMedia(s, source, enable) {
        if (!activeCall)
            return false;
        const tracks = source == CallMediaSource.Camera ? s.getVideoTracks() : s.getAudioTracks();
        let changedSource = false;
        for (const t of tracks) {
            for (const transceiver of activeCall.connection.getTransceivers()) {
                if ((t.kind == CallMediaType.Audio && mediaSourceFromTransceiverMid(transceiver.mid) == CallMediaSource.Mic) ||
                    (t.kind == CallMediaType.Video && mediaSourceFromTransceiverMid(transceiver.mid) == CallMediaSource.Camera)) {
                    if (enable) {
                        transceiver.sender.replaceTrack(t);
                    }
                    else {
                        if (isDesktop || t.kind == CallMediaType.Video || stopTrackOnAndroid)
                            t.stop();
                        else
                            t.enabled = false;
                        s.removeTrack(t);
                        transceiver.sender.replaceTrack(null);
                    }
                    if (source == CallMediaSource.Mic) {
                        activeCall.localMediaSources.mic = enable;
                        changedSource = true;
                    }
                    else if (source == CallMediaSource.Camera) {
                        activeCall.localMediaSources.camera = enable;
                        changedSource = true;
                    }
                }
            }
        }
        if (changedSource) {
            localOrPeerMediaSourcesChanged(activeCall);
            changeLayout(activeCall.layout);
            return true;
        }
        else {
            console.log("Enable media error");
            desktopShowPermissionsAlert(source == CallMediaSource.Mic ? CallMediaType.Audio : CallMediaType.Video);
            return false;
        }
    }
    function adaptToOldVersion(peerHasOldVersion, activeCall) {
        activeCall.peerHasOldVersion = peerHasOldVersion;
        if (peerHasOldVersion) {
            console.log("The peer has an old version.", "Tracks size:", activeCall.remoteStream.getAudioTracks().length, activeCall.remoteStream.getVideoTracks().length);
            onMediaMuteUnmute("0", false);
            if (activeCall.remoteStream.getVideoTracks().length > 0 || activeCall.remoteTracks.get(CallMediaSource.Camera)) {
                onMediaMuteUnmute("1", false);
            }
            if (activeCall.localMediaSources.camera && !activeCall.peerMediaSources.camera) {
                console.log("Stopping video track for the old version");
                activeCall.localStream.getVideoTracks().forEach((elem) => {
                    elem.stop();
                    activeCall.localStream.removeTrack(elem);
                });
                activeCall.localMediaSources.camera = false;
                sendMessageToNative({ resp: { type: "ok" }, command: { type: "media", source: CallMediaSource.Camera, enable: false } });
                localOrPeerMediaSourcesChanged(activeCall);
                changeLayout(activeCall.layout);
            }
        }
    }
    function webView69Or70() {
        return !isDesktop && (navigator.userAgent.includes("Chrome/69.") || navigator.userAgent.includes("Chrome/70."));
    }
    // Adding `a=extmap-allow-mixed` causes exception on old WebViews
    // https://groups.google.com/a/chromium.org/g/blink-dev/c/7z3uvp0-ZAc/m/8Z7qpp71BgAJ
    function adaptSdpToOldWebView(desc) {
        var _a;
        const res = [];
        (_a = desc.sdp) === null || _a === void 0 ? void 0 : _a.split("\n").forEach((line) => {
            // Chrome has a bug related to SDP parser in old web view versions
            if (!line.includes("a=extmap-allow-mixed")) {
                res.push(line);
            }
        });
        return { sdp: res.join("\n"), type: desc.type };
    }
    return processCommand;
})();
function toggleRemoteVideoFitFill() {
    const remote = document.getElementById("remote-video-stream");
    remote.style.objectFit = remote.style.objectFit != "contain" ? "contain" : "cover";
}
function toggleRemoteScreenVideoFitFill() {
    const remoteScreen = document.getElementById("remote-screen-video-stream");
    remoteScreen.style.objectFit = remoteScreen.style.objectFit != "contain" ? "contain" : "cover";
}
function togglePeerMedia(s, media) {
    if (!activeCall)
        return false;
    let res = false;
    const tracks = media == CallMediaType.Video ? s.getVideoTracks() : s.getAudioTracks();
    for (const t of tracks) {
        t.enabled = !t.enabled;
        res = t.enabled;
    }
    return res;
}
function changeLayout(layout) {
    var _a, _b;
    const videos = getVideoElements();
    const localSources = (_a = activeCall === null || activeCall === void 0 ? void 0 : activeCall.localMediaSources) !== null && _a !== void 0 ? _a : inactiveCallMediaSources;
    const peerSources = (_b = activeCall === null || activeCall === void 0 ? void 0 : activeCall.peerMediaSources) !== null && _b !== void 0 ? _b : { mic: false, camera: false, screenAudio: false, screenVideo: false };
    if (!videos || !localSources || !peerSources)
        return;
    switch (layout) {
        case LayoutType.Default:
            videos.local.className = "inline";
            videos.remote.className = peerSources.screenVideo ? "collapsed" : "inline";
            videos.local.style.visibility = "visible";
            videos.remote.style.visibility = peerSources.camera ? "visible" : "hidden";
            videos.remoteScreen.style.visibility = peerSources.screenVideo ? "visible" : "hidden";
            break;
        case LayoutType.LocalVideo:
            videos.local.className = "fullscreen";
            videos.local.style.visibility = "visible";
            videos.remote.style.visibility = "hidden";
            videos.remoteScreen.style.visibility = "hidden";
            break;
        case LayoutType.RemoteVideo:
            if (peerSources.screenVideo && peerSources.camera) {
                videos.remoteScreen.className = "fullscreen";
                videos.remoteScreen.style.visibility = "visible";
                videos.remote.style.visibility = "visible";
                videos.remote.className = "collapsed-pip";
            }
            else if (peerSources.screenVideo) {
                videos.remoteScreen.className = "fullscreen";
                videos.remoteScreen.style.visibility = "visible";
                videos.remote.style.visibility = "hidden";
                videos.remote.className = "inline";
            }
            else if (peerSources.camera) {
                videos.remote.className = "fullscreen";
                videos.remote.style.visibility = "visible";
                videos.remoteScreen.style.visibility = "hidden";
                videos.remoteScreen.className = "inline";
            }
            else {
                videos.remote.style.visibility = "hidden";
            }
            videos.local.style.visibility = "hidden";
            break;
    }
    videos.localScreen.style.visibility = localSources.screenVideo ? "visible" : "hidden";
    if (!isDesktop && !localSources.camera) {
        resetLocalVideoElementHeight(videos.local);
    }
}
function getVideoElements() {
    const local = document.getElementById("local-video-stream");
    const localScreen = document.getElementById("local-screen-video-stream");
    const remote = document.getElementById("remote-video-stream");
    const remoteScreen = document.getElementById("remote-screen-video-stream");
    if (!(local &&
        localScreen &&
        remote &&
        remoteScreen &&
        local instanceof HTMLVideoElement &&
        localScreen instanceof HTMLMediaElement &&
        remote instanceof HTMLMediaElement &&
        remoteScreen instanceof HTMLMediaElement))
        return;
    return { local, localScreen, remote, remoteScreen };
}
// Allow CSS to figure out the size of view by itself on Android because rotating to different orientation
// without dropping override will cause the view to have not normal proportion while no video is present
function resetLocalVideoElementHeight(local) {
    local.style.height = "";
}
function desktopShowPermissionsAlert(mediaType) {
    if (!isDesktop)
        return;
    if (mediaType == CallMediaType.Audio) {
        window.alert("Permissions denied. Please, allow access to mic to make the call working and hit unmute button. Don't reload the page.");
    }
    else {
        window.alert("Permissions denied. Please, allow access to mic and camera to make the call working and hit unmute/camera button. Don't reload the page.");
    }
}
function permissionDescription(command) {
    if (window.safari) {
        return command.safari;
    }
    else if ((navigator.userAgent.includes("Chrome") && navigator.vendor.includes("Google Inc")) ||
        navigator.userAgent.includes("Firefox")) {
        return command.chrome;
    }
    else {
        return "";
    }
}
// Cryptography function - it is loaded both in the main window and in worker context (if the worker is used)
function callCryptoFunction() {
    const initialPlainTextRequired = {
        key: 10,
        delta: 3,
        empty: 1,
    };
    const IV_LENGTH = 12;
    function encryptFrame(key) {
        return async (frame, controller) => {
            const data = new Uint8Array(frame.data);
            const n = initialPlainTextRequired[frame.type] || 1;
            const iv = randomIV();
            const initial = data.subarray(0, n);
            const plaintext = data.subarray(n, data.byteLength);
            try {
                const ciphertext = plaintext.length
                    ? new Uint8Array(await crypto.subtle.encrypt({ name: "AES-GCM", iv: iv.buffer }, key, plaintext))
                    : new Uint8Array(0);
                frame.data = concatN(initial, ciphertext, iv).buffer;
                controller.enqueue(frame);
            }
            catch (e) {
                console.log(`encryption error ${e}`);
                throw e;
            }
        };
    }
    function decryptFrame(key) {
        return async (frame, controller) => {
            const data = new Uint8Array(frame.data);
            const n = initialPlainTextRequired[frame.type] || 1;
            const initial = data.subarray(0, n);
            const ciphertext = data.subarray(n, data.byteLength - IV_LENGTH);
            const iv = data.subarray(data.byteLength - IV_LENGTH, data.byteLength);
            try {
                const plaintext = ciphertext.length
                    ? new Uint8Array(await crypto.subtle.decrypt({ name: "AES-GCM", iv }, key, ciphertext))
                    : new Uint8Array(0);
                frame.data = concatN(initial, plaintext).buffer;
                controller.enqueue(frame);
            }
            catch (e) {
                console.log(`decryption error ${e}`);
                throw e;
            }
        };
    }
    function decodeAesKey(aesKey) {
        const keyData = callCrypto.decodeBase64url(callCrypto.encodeAscii(aesKey));
        return crypto.subtle.importKey("raw", keyData, { name: "AES-GCM", length: 256 }, true, ["encrypt", "decrypt"]);
    }
    function concatN(...bs) {
        const a = new Uint8Array(bs.reduce((size, b) => size + b.byteLength, 0));
        bs.reduce((offset, b) => {
            a.set(b, offset);
            return offset + b.byteLength;
        }, 0);
        return a;
    }
    function randomIV() {
        return crypto.getRandomValues(new Uint8Array(IV_LENGTH));
    }
    const base64urlChars = new Uint8Array("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".split("").map((c) => c.charCodeAt(0)));
    const base64urlLookup = new Array(256);
    base64urlChars.forEach((c, i) => (base64urlLookup[c] = i));
    const char_equal = "=".charCodeAt(0);
    function encodeAscii(s) {
        const a = new Uint8Array(s.length);
        let i = s.length;
        while (i--)
            a[i] = s.charCodeAt(i);
        return a;
    }
    function decodeAscii(a) {
        let s = "";
        for (let i = 0; i < a.length; i++)
            s += String.fromCharCode(a[i]);
        return s;
    }
    function encodeBase64url(a) {
        const len = a.length;
        const b64len = Math.ceil(len / 3) * 4;
        const b64 = new Uint8Array(b64len);
        let j = 0;
        for (let i = 0; i < len; i += 3) {
            b64[j++] = base64urlChars[a[i] >> 2];
            b64[j++] = base64urlChars[((a[i] & 3) << 4) | (a[i + 1] >> 4)];
            b64[j++] = base64urlChars[((a[i + 1] & 15) << 2) | (a[i + 2] >> 6)];
            b64[j++] = base64urlChars[a[i + 2] & 63];
        }
        if (len % 3)
            b64[b64len - 1] = char_equal;
        if (len % 3 === 1)
            b64[b64len - 2] = char_equal;
        return b64;
    }
    function decodeBase64url(b64) {
        let len = b64.length;
        if (len % 4)
            return;
        let bLen = (len * 3) / 4;
        if (b64[len - 1] === char_equal) {
            len--;
            bLen--;
            if (b64[len - 1] === char_equal) {
                len--;
                bLen--;
            }
        }
        const bytes = new Uint8Array(bLen);
        let i = 0;
        let pos = 0;
        while (i < len) {
            const enc1 = base64urlLookup[b64[i++]];
            const enc2 = i < len ? base64urlLookup[b64[i++]] : 0;
            const enc3 = i < len ? base64urlLookup[b64[i++]] : 0;
            const enc4 = i < len ? base64urlLookup[b64[i++]] : 0;
            if (enc1 === undefined || enc2 === undefined || enc3 === undefined || enc4 === undefined)
                return;
            bytes[pos++] = (enc1 << 2) | (enc2 >> 4);
            bytes[pos++] = ((enc2 & 15) << 4) | (enc3 >> 2);
            bytes[pos++] = ((enc3 & 3) << 6) | (enc4 & 63);
        }
        return bytes;
    }
    return {
        transformFrame: { encrypt: encryptFrame, decrypt: decryptFrame },
        decodeAesKey,
        encodeAscii,
        decodeAscii,
        encodeBase64url,
        decodeBase64url,
    };
}
// If the worker is used for decryption, this function code (as string) is used to load the worker via Blob
// We have to use worker optionally, as it crashes in Android web view, regardless of how it is loaded
function workerFunction() {
    // encryption with createEncodedStreams support
    self.addEventListener("message", async ({ data }) => {
        await setupTransform(data);
    });
    // encryption using RTCRtpScriptTransform.
    if ("RTCTransformEvent" in self) {
        self.addEventListener("rtctransform", async ({ transformer }) => {
            try {
                const { operation, aesKey } = transformer.options;
                const { readable, writable } = transformer;
                await setupTransform({ operation, aesKey, readable, writable });
                self.postMessage({ result: "setupTransform success" });
            }
            catch (e) {
                self.postMessage({ message: `setupTransform error: ${e.message}` });
            }
        });
    }
    async function setupTransform({ operation, aesKey, readable, writable }) {
        const key = await callCrypto.decodeAesKey(aesKey);
        const transform = callCrypto.transformFrame[operation](key);
        readable.pipeThrough(new TransformStream({ transform })).pipeTo(writable);
    }
}
//# sourceMappingURL=call.js.map