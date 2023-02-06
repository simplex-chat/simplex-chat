"use strict";
// Inspired by
// https://github.com/webrtc/samples/blob/gh-pages/src/content/insertable-streams/endtoend-encryption
var CallMediaType;
(function (CallMediaType) {
    CallMediaType["Audio"] = "audio";
    CallMediaType["Video"] = "video";
})(CallMediaType || (CallMediaType = {}));
var VideoCamera;
(function (VideoCamera) {
    VideoCamera["User"] = "user";
    VideoCamera["Environment"] = "environment";
})(VideoCamera || (VideoCamera = {}));
// for debugging
// var sendMessageToNative = ({resp}: WVApiMessage) => console.log(JSON.stringify({command: resp}))
var sendMessageToNative = (msg) => console.log(JSON.stringify(msg));
// Global object with cryptrographic/encoding functions
const callCrypto = callCryptoFunction();
var TransformOperation;
(function (TransformOperation) {
    TransformOperation["Encrypt"] = "encrypt";
    TransformOperation["Decrypt"] = "decrypt";
})(TransformOperation || (TransformOperation = {}));
let activeCall;
let answerTimeout = 30000;
const processCommand = (function () {
    const defaultIceServers = [
        { urls: ["stun:stun.simplex.im:443"] },
        { urls: ["turn:turn.simplex.im:443?transport=udp"], username: "private", credential: "yleob6AVkiNI87hpR94Z" },
        { urls: ["turn:turn.simplex.im:443?transport=tcp"], username: "private", credential: "yleob6AVkiNI87hpR94Z" },
    ];
    function getCallConfig(encodedInsertableStreams, iceServers, relay) {
        return {
            peerConnectionConfig: {
                iceServers: iceServers !== null && iceServers !== void 0 ? iceServers : defaultIceServers,
                iceCandidatePoolSize: 10,
                encodedInsertableStreams,
                iceTransportPolicy: relay ? "relay" : "all",
            },
            iceCandidates: {
                delay: 3000,
                extrasInterval: 2000,
                extrasTimeout: 8000,
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
                const iceCandidates = serialize(candidates);
                candidates = [];
                resolve(iceCandidates);
            }
            function sendIceCandidates() {
                if (candidates.length === 0)
                    return;
                const iceCandidates = serialize(candidates);
                candidates = [];
                sendMessageToNative({ resp: { type: "ice", iceCandidates } });
            }
        });
    }
    async function initializeCall(config, mediaType, aesKey, useWorker) {
        const pc = new RTCPeerConnection(config.peerConnectionConfig);
        const remoteStream = new MediaStream();
        const localCamera = VideoCamera.User;
        const localStream = await getLocalMediaStream(mediaType, localCamera);
        const iceCandidates = getIceCandidates(pc, config);
        const call = { connection: pc, iceCandidates, localMedia: mediaType, localCamera, localStream, remoteStream, aesKey, useWorker };
        await setupMediaStreams(call);
        let connectionTimeout = setTimeout(connectionHandler, answerTimeout);
        pc.addEventListener("connectionstatechange", connectionStateChange);
        return call;
        async function connectionStateChange() {
            // "failed" means the second party did not answer in time (15 sec timeout in Chrome WebView)
            // See https://source.chromium.org/chromium/chromium/src/+/main:third_party/webrtc/p2p/base/p2p_constants.cc;l=70)
            if (pc.connectionState !== "failed")
                connectionHandler();
        }
        async function connectionHandler() {
            sendMessageToNative({
                resp: {
                    type: "connection",
                    state: {
                        connectionState: pc.connectionState,
                        iceConnectionState: pc.iceConnectionState,
                        iceGatheringState: pc.iceGatheringState,
                        signalingState: pc.signalingState,
                    },
                },
            });
            if (pc.connectionState == "disconnected" || pc.connectionState == "failed") {
                clearConnectionTimeout();
                pc.removeEventListener("connectionstatechange", connectionStateChange);
                if (activeCall) {
                    setTimeout(() => sendMessageToNative({ resp: { type: "ended" } }), 0);
                }
                endCall();
            }
            else if (pc.connectionState == "connected") {
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
        const { corrId, command } = body;
        const pc = activeCall === null || activeCall === void 0 ? void 0 : activeCall.connection;
        let resp;
        try {
            switch (command.type) {
                case "capabilities":
                    console.log("starting outgoing call - capabilities");
                    if (activeCall)
                        endCall();
                    // This request for local media stream is made to prompt for camera/mic permissions on call start
                    if (command.media)
                        await getLocalMediaStream(command.media, VideoCamera.User);
                    const encryption = supportsInsertableStreams(command.useWorker);
                    resp = { type: "capabilities", capabilities: { encryption } };
                    break;
                case "start": {
                    console.log("starting incoming call - create webrtc session");
                    if (activeCall)
                        endCall();
                    const { media, useWorker, iceServers, relay } = command;
                    const encryption = supportsInsertableStreams(useWorker);
                    const aesKey = encryption ? command.aesKey : undefined;
                    activeCall = await initializeCall(getCallConfig(encryption && !!aesKey, iceServers, relay), media, aesKey, useWorker);
                    const pc = activeCall.connection;
                    const offer = await pc.createOffer();
                    await pc.setLocalDescription(offer);
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
                    //   useWorker,
                    // }
                    resp = {
                        type: "offer",
                        offer: serialize(offer),
                        iceCandidates: await activeCall.iceCandidates,
                        capabilities: { encryption },
                    };
                    break;
                }
                case "offer":
                    if (activeCall) {
                        resp = { type: "error", message: "accept: call already started" };
                    }
                    else if (!supportsInsertableStreams(command.useWorker) && command.aesKey) {
                        resp = { type: "error", message: "accept: encryption is not supported" };
                    }
                    else {
                        const offer = parse(command.offer);
                        const remoteIceCandidates = parse(command.iceCandidates);
                        const { media, aesKey, useWorker, iceServers, relay } = command;
                        activeCall = await initializeCall(getCallConfig(!!aesKey, iceServers, relay), media, aesKey, useWorker);
                        const pc = activeCall.connection;
                        await pc.setRemoteDescription(new RTCSessionDescription(offer));
                        const answer = await pc.createAnswer();
                        await pc.setLocalDescription(answer);
                        addIceCandidates(pc, remoteIceCandidates);
                        // same as command for caller to use
                        resp = {
                            type: "answer",
                            answer: serialize(answer),
                            iceCandidates: await activeCall.iceCandidates,
                        };
                    }
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
                        await pc.setRemoteDescription(new RTCSessionDescription(answer));
                        addIceCandidates(pc, remoteIceCandidates);
                        resp = { type: "ok" };
                    }
                    break;
                case "ice":
                    if (pc) {
                        const remoteIceCandidates = parse(command.iceCandidates);
                        addIceCandidates(pc, remoteIceCandidates);
                        resp = { type: "ok" };
                    }
                    else {
                        resp = { type: "error", message: "ice: call not started" };
                    }
                    break;
                case "media":
                    if (!activeCall) {
                        resp = { type: "error", message: "media: call not started" };
                    }
                    else if (activeCall.localMedia == CallMediaType.Audio && command.media == CallMediaType.Video) {
                        resp = { type: "error", message: "media: no video" };
                    }
                    else {
                        enableMedia(activeCall.localStream, command.media, command.enable);
                        resp = { type: "ok" };
                    }
                    break;
                case "camera":
                    if (!activeCall || !pc) {
                        resp = { type: "error", message: "camera: call not started" };
                    }
                    else {
                        await replaceMedia(activeCall, command.camera);
                        resp = { type: "ok" };
                    }
                    break;
                case "end":
                    endCall();
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
        }
    }
    async function setupMediaStreams(call) {
        const videos = getVideoElements();
        if (!videos)
            throw Error("no video elements");
        await setupEncryptionWorker(call);
        setupLocalStream(call);
        setupRemoteStream(call);
        setupCodecPreferences(call);
        // setupVideoElement(videos.local)
        // setupVideoElement(videos.remote)
        videos.local.srcObject = call.localStream;
        videos.remote.srcObject = call.remoteStream;
    }
    async function setupEncryptionWorker(call) {
        if (call.aesKey) {
            if (!call.key)
                call.key = await callCrypto.decodeAesKey(call.aesKey);
            if (call.useWorker && !call.worker) {
                const workerCode = `const callCrypto = (${callCryptoFunction.toString()})(); (${workerFunction.toString()})()`;
                call.worker = new Worker(URL.createObjectURL(new Blob([workerCode], { type: "text/javascript" })));
                call.worker.onerror = ({ error, filename, lineno, message }) => console.log(JSON.stringify({ error, filename, lineno, message }));
                call.worker.onmessage = ({ data }) => console.log(JSON.stringify({ message: data }));
            }
        }
    }
    function setupLocalStream(call) {
        const videos = getVideoElements();
        if (!videos)
            throw Error("no video elements");
        const pc = call.connection;
        let { localStream } = call;
        for (const track of localStream.getTracks()) {
            pc.addTrack(track, localStream);
        }
        if (call.aesKey && call.key) {
            console.log("set up encryption for sending");
            for (const sender of pc.getSenders()) {
                setupPeerTransform(TransformOperation.Encrypt, sender, call.worker, call.aesKey, call.key);
            }
        }
    }
    function setupRemoteStream(call) {
        // Pull tracks from remote stream as they arrive add them to remoteStream video
        const pc = call.connection;
        pc.ontrack = (event) => {
            try {
                if (call.aesKey && call.key) {
                    console.log("set up decryption for receiving");
                    setupPeerTransform(TransformOperation.Decrypt, event.receiver, call.worker, call.aesKey, call.key);
                }
                for (const stream of event.streams) {
                    for (const track of stream.getTracks()) {
                        call.remoteStream.addTrack(track);
                    }
                }
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
        var _a;
        const capabilities = RTCRtpSender.getCapabilities("video");
        if (capabilities) {
            const { codecs } = capabilities;
            const selectedCodecIndex = codecs.findIndex((c) => c.mimeType === "video/VP8");
            const selectedCodec = codecs[selectedCodecIndex];
            codecs.splice(selectedCodecIndex, 1);
            codecs.unshift(selectedCodec);
            for (const t of call.connection.getTransceivers()) {
                if (((_a = t.sender.track) === null || _a === void 0 ? void 0 : _a.kind) === "video") {
                    t.setCodecPreferences(codecs);
                }
            }
        }
    }
    async function replaceMedia(call, camera) {
        const videos = getVideoElements();
        if (!videos)
            throw Error("no video elements");
        const pc = call.connection;
        for (const t of call.localStream.getTracks())
            t.stop();
        call.localCamera = camera;
        const localStream = await getLocalMediaStream(call.localMedia, camera);
        replaceTracks(pc, localStream.getVideoTracks());
        replaceTracks(pc, localStream.getAudioTracks());
        call.localStream = localStream;
        videos.local.srcObject = localStream;
    }
    function replaceTracks(pc, tracks) {
        if (!tracks.length)
            return;
        const sender = pc.getSenders().find((s) => { var _a; return ((_a = s.track) === null || _a === void 0 ? void 0 : _a.kind) === tracks[0].kind; });
        if (sender)
            for (const t of tracks)
                sender.replaceTrack(t);
    }
    function setupPeerTransform(operation, peer, worker, aesKey, key) {
        if (worker && "RTCRtpScriptTransform" in window) {
            console.log(`${operation} with worker & RTCRtpScriptTransform`);
            peer.transform = new RTCRtpScriptTransform(worker, { operation, aesKey });
        }
        else if ("createEncodedStreams" in peer) {
            const { readable, writable } = peer.createEncodedStreams();
            if (worker) {
                console.log(`${operation} with worker`);
                worker.postMessage({ operation, readable, writable, aesKey }, [readable, writable]);
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
    function getLocalMediaStream(mediaType, facingMode) {
        const constraints = callMediaConstraints(mediaType, facingMode);
        return navigator.mediaDevices.getUserMedia(constraints);
    }
    function callMediaConstraints(mediaType, facingMode) {
        switch (mediaType) {
            case CallMediaType.Audio:
                return { audio: true, video: false };
            case CallMediaType.Video:
                return {
                    audio: true,
                    video: {
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
    }
    function supportsInsertableStreams(useWorker) {
        return (("createEncodedStreams" in RTCRtpSender.prototype && "createEncodedStreams" in RTCRtpReceiver.prototype) ||
            (!!useWorker && "RTCRtpScriptTransform" in window));
    }
    function resetVideoElements() {
        const videos = getVideoElements();
        if (!videos)
            return;
        videos.local.srcObject = null;
        videos.remote.srcObject = null;
    }
    function getVideoElements() {
        const local = document.getElementById("local-video-stream");
        const remote = document.getElementById("remote-video-stream");
        if (!(local && remote && local instanceof HTMLMediaElement && remote instanceof HTMLMediaElement))
            return;
        return { local, remote };
    }
    // function setupVideoElement(video: HTMLElement) {
    //   // TODO use display: none
    //   video.style.opacity = "0"
    //   video.onplaying = () => {
    //     video.style.opacity = "1"
    //   }
    // }
    function enableMedia(s, media, enable) {
        const tracks = media == CallMediaType.Video ? s.getVideoTracks() : s.getAudioTracks();
        for (const t of tracks)
            t.enabled = enable;
    }
    return processCommand;
})();
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