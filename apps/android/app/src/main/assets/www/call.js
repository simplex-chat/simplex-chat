"use strict";
// Inspired by
// https://github.com/webrtc/samples/blob/gh-pages/src/content/insertable-streams/endtoend-encryption
var CallMediaType;
(function (CallMediaType) {
    CallMediaType["Audio"] = "audio";
    CallMediaType["Video"] = "video";
})(CallMediaType || (CallMediaType = {}));
// STUN servers
const peerConnectionConfig = {
    iceServers: [{ urls: ["stun:stun.l.google.com:19302"] }],
    iceCandidatePoolSize: 10,
    encodedInsertableStreams: true,
};
// let keyGenConfig: AesKeyGenParams = {
//   name: "AES-GCM",
//   length: 256,
//   tagLength: 128,
// }
const keyAlgorithm = {
    name: "AES-GCM",
    length: 256
};
const keyUsages = ["encrypt", "decrypt"];
// Hardcode a key for development
// let keyData = {
//   alg: "A256GCM",
//   ext: true,
//   k: "JCMDWkhxLmPDhua0BUdhgv6Ac6hOtB9frSxJlnkTAK8",
//   key_ops: keyUsages,
//   kty: "oct",
// }
let pc;
// let key
let IV_LENGTH = 12;
const initialPlainTextRequired = {
    key: 10,
    delta: 3,
    undefined: 1,
};
// let encryptKeyRepresentation
let candidates = [];
function createPeerConnection(config) {
    let connection = new RTCPeerConnection(peerConnectionConfig);
    return new Promise((resolve, _) => {
        const extraIceCandidates = new Promise((resolveExtra, _) => {
            let candidates = [];
            let ok = false;
            let okExtra = false;
            let waitExtra;
            const wait = setTimeout(() => {
                if (!ok) {
                    const iceCandidates = candidates.slice();
                    candidates = [];
                    ok = true;
                    waitExtra = setTimeout(() => {
                        if (!okExtra) {
                            okExtra = true;
                            resolveExtra({
                                iceCandidates: candidates.slice(),
                                complete: false,
                            });
                        }
                    }, config.timeoutIceCandidates);
                    resolve({ connection, iceCandidates, extraIceCandidates });
                }
            }, config.waitForIceCandidates);
            connection.onicecandidate = (e) => e.candidate && candidates.push(e.candidate);
            connection.onicegatheringstatechange = (_) => {
                if (connection.iceGatheringState == "complete") {
                    if (!ok) {
                        ok = true;
                        clearTimeout(wait);
                        resolve({ connection, iceCandidates: candidates });
                    }
                    else if (!okExtra) {
                        okExtra = true;
                        waitExtra && clearTimeout(waitExtra);
                        resolveExtra({ iceCandidates: candidates, complete: true });
                    }
                }
            };
        });
    });
}
// TODO remove WCallCommand from parameter type
function sendMessageToNative(msg) {
    console.log(JSON.stringify(msg));
}
async function initializeCall(mediaType, keyData) {
    const call = await createPeerConnection({
        waitForIceCandidates: 1000,
        timeoutIceCandidates: 4000
    });
    const { connection, extraIceCandidates } = call;
    const remoteStream = new MediaStream();
    const localStream = await navigator.mediaDevices.getUserMedia(callMediaContraints(mediaType));
    await setUpMediaStreams(connection, localStream, remoteStream, keyData);
    extraIceCandidates === null || extraIceCandidates === void 0 ? void 0 : extraIceCandidates.then(({ iceCandidates, complete }) => {
        if (!complete)
            console.log("ICE candidates gathering not completed");
        if (iceCandidates.length > 0)
            sendMessageToNative({ type: "ice", iceCandidates });
    });
    return call;
}
// TODO remove WCallCommand from result type
async function processCommand(command) {
    let resp;
    switch (command.type) {
        case "capabilities":
            const encryption = supportsInsertableStreams();
            resp = { type: "capabilities", capabilities: { encryption } };
            break;
        case "start":
            console.log("starting call");
            if (pc) {
                resp = { type: "error", message: "start: call already started" };
            }
            else if (!supportsInsertableStreams() && command.aesKey) {
                resp = { type: "error", message: "start: encryption is not supported" };
            }
            else {
                const { media, aesKey } = command;
                const call = await initializeCall(media, aesKey);
                const { connection, iceCandidates } = call;
                pc = connection;
                const offer = await pc.createOffer();
                await pc.setLocalDescription(offer);
                // for debugging, returning the command for callee to use
                resp = { type: "accept", offer, iceCandidates, media, aesKey };
                // resp = {type: "offer", offer, iceCandidates}
            }
            break;
        case "accept":
            if (pc) {
                resp = { type: "error", message: "offer: call already started" };
            }
            else if (!supportsInsertableStreams() && command.aesKey) {
                resp = { type: "error", message: "offer: encryption is not supported" };
            }
            else {
                const call = await initializeCall(command.media, command.aesKey);
                const { connection, iceCandidates } = call;
                pc = connection;
                await pc.setRemoteDescription(new RTCSessionDescription(command.offer));
                const answer = await pc.createAnswer();
                await pc.setLocalDescription(answer);
                addIceCandidates(pc, command.iceCandidates);
                // same as command for caller to use
                resp = { type: "answer", answer, iceCandidates };
            }
            break;
        case "answer":
            if (!pc) {
                resp = { type: "error", message: "answer: call not started" };
            }
            else if (!pc.currentLocalDescription) {
                resp = { type: "error", message: "answer: local description is not set" };
            }
            else if (pc.currentRemoteDescription) {
                resp = { type: "error", message: "answer: remote description already set" };
            }
            else {
                await pc.setRemoteDescription(new RTCSessionDescription(command.answer));
                addIceCandidates(pc, command.iceCandidates);
                resp = { type: "ok" };
            }
            break;
        case "ice":
            if (pc) {
                addIceCandidates(pc, command.iceCandidates);
                resp = { type: "ok" };
            }
            else {
                resp = { type: "error", message: "ice: call not started" };
            }
            break;
        case "end":
            if (pc) {
                pc.close();
                pc = undefined;
                resp = { type: "ok" };
            }
            else {
                resp = { type: "error", message: "end: call not started" };
            }
            break;
        default:
            resp = { type: "error", message: "unknown command" };
            break;
    }
    sendMessageToNative(resp);
    return resp;
}
function addIceCandidates(conn, iceCandidates) {
    for (const c of iceCandidates) {
        conn.addIceCandidate(new RTCIceCandidate(c));
    }
}
async function setUpMediaStreams(pc, localStream, remoteStream, keyData) {
    var _a;
    const videos = getVideoElelements();
    if (!videos)
        throw Error("no video elements");
    let key;
    if (keyData)
        key = await crypto.subtle.importKey("raw", keyData, keyAlgorithm, false, keyUsages);
    for (const track of localStream.getTracks()) {
        pc.addTrack(track, localStream);
    }
    if (key) {
        for (const sender of pc.getSenders()) {
            setupPeerTransform(sender, encodeFunction(key));
        }
    }
    // Pull tracks from remote stream as they arrive add them to remoteStream video
    pc.ontrack = (event) => {
        if (key)
            setupPeerTransform(event.receiver, decodeFunction(key));
        for (const track of event.streams[0].getTracks()) {
            remoteStream.addTrack(track);
        }
    };
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
    const capabilities = RTCRtpSender.getCapabilities("video");
    if (capabilities) {
        const { codecs } = capabilities;
        const selectedCodecIndex = codecs.findIndex((c) => c.mimeType === "video/VP8");
        const selectedCodec = codecs[selectedCodecIndex];
        codecs.splice(selectedCodecIndex, 1);
        codecs.unshift(selectedCodec);
        for (const t of pc.getTransceivers()) {
            if (((_a = t.sender.track) === null || _a === void 0 ? void 0 : _a.kind) === "video") {
                t.setCodecPreferences(codecs);
            }
        }
    }
    videos.local.srcObject = localStream;
    videos.remote.srcObject = remoteStream;
}
function callMediaContraints(mediaType) {
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
                },
            };
    }
}
function supportsInsertableStreams() {
    return ("createEncodedStreams" in RTCRtpSender.prototype)
        && ("createEncodedStreams" in RTCRtpReceiver.prototype);
}
function getVideoElelements() {
    const local = document.getElementById("local-video-stream");
    const remote = document.getElementById("remote-video-stream");
    if (!(local && remote && local instanceof HTMLMediaElement && remote instanceof HTMLMediaElement))
        return;
    setupVideoElement(local);
    setupVideoElement(remote);
    return { local, remote };
}
function setupVideoElement(video) {
    // TODO use display: none
    video.style.opacity = "0";
    video.onplaying = () => {
        video.style.opacity = "1";
    };
}
// what does it do?
// function toggleVideo(b) {
//   if (b == "true") {
//     localStream.getVideoTracks()[0].enabled = true
//   } else {
//     localStream.getVideoTracks()[0].enabled = false
//   }
// }
function f() {
    console.log("Debug Function");
    return "Debugging Return";
}
/* Stream Transforms */
function setupPeerTransform(peer, transform) {
    const streams = peer.createEncodedStreams();
    streams.readable.pipeThrough(new TransformStream({ transform })).pipeTo(streams.writable);
}
/* Cryptography */
function encodeFunction(key) {
    // frame is an RTCEncodedAudioFrame
    // frame.data is ArrayBuffer
    return async (frame, controller) => {
        const data = new Uint8Array(frame.data);
        const n = frame instanceof RTCEncodedVideoFrame ? initialPlainTextRequired[frame.type] : 0;
        const iv = randomIV();
        const initial = data.subarray(0, n);
        const plaintext = data.subarray(n, data.byteLength);
        try {
            const ciphertext = await crypto.subtle.encrypt({ name: "AES-GCM", iv: iv.buffer }, key, plaintext);
            frame.data = concatN(initial, new Uint8Array(ciphertext), iv).buffer;
            controller.enqueue(frame);
        }
        catch (e) {
            console.log(`encryption error ${e}`);
            // pc.close()
            throw e;
        }
    };
}
function decodeFunction(key) {
    return async (frame, controller) => {
        const data = new Uint8Array(frame.data);
        const n = frame instanceof RTCEncodedVideoFrame ? initialPlainTextRequired[frame.type] : 0;
        const initial = data.subarray(0, n);
        const ciphertext = data.subarray(n, data.byteLength - IV_LENGTH);
        const iv = data.subarray(data.byteLength - IV_LENGTH, data.byteLength);
        try {
            const plaintext = await crypto.subtle.decrypt({ name: "AES-GCM", iv }, key, ciphertext);
            frame.data = concatN(initial, new Uint8Array(plaintext)).buffer;
            controller.enqueue(frame);
        }
        catch (e) {
            console.log(`decryption error ${e}`);
            // pc.close()
            throw e;
        }
    };
}
class RTCEncodedVideoFrame {
    constructor(type, data) {
        this.type = type;
        this.data = data;
    }
}
function randomIV() {
    return crypto.getRandomValues(new Uint8Array(IV_LENGTH));
}
// async function loadKey(keyData) {
//   key = await crypto.subtle.importKey("jwk", keyData, keyGenConfig, false, keyUsages)
// }
function concatN(...bs) {
    const a = new Uint8Array(bs.reduce((size, b) => size + b.byteLength, 0));
    bs.reduce((offset, b) => {
        a.set(b, offset);
        return offset + b.byteLength;
    }, 0);
    return a;
}
// async function generateKey() {
//   let rawKey = await crypto.subtle.generateKey(keyGenConfig, true, keyUsages)
//   let key = await crypto.subtle.exportKey("jwk", rawKey)
//   console.log(
//     JSON.stringify({
//       action: "processDecryptionKey",
//       content: {
//         key,
//         iv: encryptIv,
//       },
//     })
//   )
// }
//# sourceMappingURL=index.js.map