const crypto = require('isomorphic-webcrypto')

function callCryptoFunction() {
    const initialPlainTextRequired = {
        key: 10,
        delta: 3,
        empty: 1,
    };
    const IV_LENGTH = 12;
    function encryptFrame(key) {
        return async (frame, controller) => {
            const data = await encryptFrameData(key, new Uint8Array(frame.data), frame.type);
            if (data) {
                frame.data = data.buffer;
                controller.enqueue(frame);
            }
        };
    }
    async function encryptFrameData(key, data, frameType) {
        const n = initialPlainTextRequired[frameType] || 1;
        // const iv = randomIV();
        const iv = new Uint8Array([122, 157, 141, 235, 5, 92, 44, 35, 37, 244, 90, 254])
        const initial = data.subarray(0, n);
        const plaintext = data.subarray(n, data.byteLength);
        try {
            const ciphertext = plaintext.length
                ? new Uint8Array(await crypto.subtle.encrypt({name: "AES-GCM", iv: iv.buffer}, key, plaintext))
                : new Uint8Array(0);
            return concatN(initial, ciphertext, iv);
        }
        catch (e) {
            console.log(`encryption error ${e}`);
            throw e;
        }
    }
    function decryptFrame(key) {
        return async (frame, controller) => {
            const data = await decryptFrameData(key, new Uint8Array(frame.data), frame.type);
            if (data) {
                frame.data = data.buffer;
                controller.enqueue(frame);
            }
        };
    }
    async function decryptFrameData(key, data, frameType) {
        const n = initialPlainTextRequired[frameType] || 1;
        const initial = data.subarray(0, n);
        const ciphertext = data.subarray(n, data.byteLength - IV_LENGTH);
        const iv = data.subarray(data.byteLength - IV_LENGTH, data.byteLength);
        try {
            const plaintext = ciphertext.length
                ? new Uint8Array(await crypto.subtle.decrypt({name: "AES-GCM", iv}, key, ciphertext))
                : new Uint8Array(0);
            return concatN(initial, plaintext);
        }
        catch (e) {
            console.log(`decryption error ${e}`);
            throw e;
        }
    }
    function decodeAesKey(aesKey) {
        const keyData = callCrypto.decodeBase64url(callCrypto.encodeAscii(aesKey));
        return crypto.subtle.importKey("raw", keyData, {name: "AES-GCM", length: 256}, true, ["encrypt", "decrypt"]);
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
        transformFrame: {encrypt: encryptFrame, decrypt: decryptFrame},
        decodeAesKey,
        encryptFrameData,
        decryptFrameData,
        encodeAscii,
        decodeAscii,
        encodeBase64url,
        decodeBase64url,
    };
}

let callCrypto = callCryptoFunction()
let totalBytes = 20
let source = new Uint8Array(totalBytes)
source[0] = 1
for (let i = 1; i < totalBytes; i++) {
    source[i] = i
}
let isKeyFrame = source[0] & 1 == 1
let clearTextBytesSize = isKeyFrame ? 10 : 3
console.log(source, isKeyFrame, clearTextBytesSize)

run()

async function run() {
    let key = await callCrypto.decodeAesKey("PI-bV-FTgRqZM_lsDH9T21a0yRVMsvLFmvilJ9Ssk3g=")
    let encrypted = await callCrypto.encryptFrameData(key, source, "key")
    console.log(encrypted)
    // encrypted = new Uint8Array([1, 1, 2, 3, 4, 5, 6, 7, 8, 9].concat([59, 185, 196, 0, 38, 225, 152, 84, 90, 112, 235, 222, 132, 186, 123, 222, 206, 231, 187, 172, 166, 87, 120, 97, 223, 143, 220, 61, 164, 75, 103, 208, 186, 117, 27, 27, 142, 94]))
    let decrypted = await callCrypto.decryptFrameData(key, encrypted, "key")
    console.log(decrypted)
}
