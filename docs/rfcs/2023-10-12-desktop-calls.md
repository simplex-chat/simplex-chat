# Desktop calls

To make audio and video calls on desktop there are some options:
- adapt [libwebrtc](webrtc.googlesource.com/) from Google which would be the most reliable, performant and seamless solution;
- include some kind of WebView to the app via libraries;
- implement a signaling server in the app and let users to use HTML page with WebRTC code that already exist for Android.

## WebRTC lib

To adapt libwebrtc we need to make SDK that is compatible with Java (JNI layer + desktop implementation of VideoCodecs and other features). There are two SDKs exist already: for Android and for Objective-C. Making Android SDK compatible with Java-only SDK gives a lot of problems and requires to have 5+ professional C++ developers with weeks/months for developing. Which is not something good.

Another considered option was adopting Jitsi Java SDK, but it's state is not very clear, and in any case it is much more effort.

## WebView

Including WebView is possible but requires a lot of megabytes of storage to waste on such libs. Because only Chromium-like WebViews support WebRTC features. Which means 100+ MB to the package on top of 200+ MB now.

## Standalone browser + WebRTC HTML page

The last solution is what can give the most useful result: the same package size as before + already existent code which can be reused with small modifications + quality of result will depend on Chromium/Firefox/Safari devs (which is good, since they are interested in making all features working for everyone). 

# Details of implementation

Since the code for WebView has already written (https://github.com/simplex-chat/simplex-chat/tree/0e4376bada2d0c4ec2ade7f30b0048dc8b13abd8/apps/multiplatform/android/src/main/assets/www) it can be used to make calls on a desktop browser too. The only differences are these:
- UI needs to be changed - buttons controlling calls should be added. For example, end call, disable camera/mic. This will be added to separate HTML and JS files that would communication with the existing WebRTC JS code via the existing functional API.
- signaling websocket server should be started in order to allow Haskell backend to talk with HTML page and to exchange messages between all parties. It is bidirectional communication between server and webpage since both parties have to send messages to each other.

There is no need to have TLS-secured connection between server and webpage since it's only used locally. And browser will not be happy with self-signed certificate too.

After accepting the call, webpage will be opened in the default browser. URL will look like: `https://localhost:123/simplex/call/` (to reduce questions and to namespace other files - the main UI page will be index.html, that would load via folder path) After that internal machinary will connect both parties together. Same as on Android but with a different signaling channel, in this case it's websockets. 

Ending the call by the user will also send a new action to signaling server to allow the backend to notify other party.

The solution will also allow to make screen sharing in the future that will be supported on every OS and graphics environment where the user's browser supports it.