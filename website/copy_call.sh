#!/bin/sh

mkdir -p ./src/call
cp ./node_modules/@simplex-chat/webrtc/dist/call.js          ./src/call/
cp ./node_modules/@simplex-chat/webrtc/dist/call.js.map      ./src/call/
cp ./node_modules/@simplex-chat/webrtc/dist/ui.js            ./src/call/
cp ./node_modules/@simplex-chat/webrtc/dist/style.css        ./src/call/
cp ./node_modules/@simplex-chat/webrtc/dist/webcall.html     ./src/call/index.html
cp ./node_modules/@simplex-chat/webrtc/dist/lz-string.min.js ./src/call/
