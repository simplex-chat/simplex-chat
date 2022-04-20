# Adding Audio/Video Call Functionality to SimpleX Apps

To extend the functionality of the SimpleX mobile apps in pursuit of supporting all kinds of communication, we seek to add the ability for already connected users to call each other with audio and optionally video.

## Desired Functionality
- [ ] The content (audio/video data) of the calls is encrypted
- [ ] The setting up of the call session is secure and encrypted (via the SimpleX protocol)
- [ ] Contacts can audio call each other
- [ ] Contacts can video call each other
- [ ] When on a call users can mute/unmute their mic
- [ ] When on a video call users can show/hide their video feed
- [ ] Users will be notified of other calls when already engaged in a call
- [ ] Incoming calls trigger a notification which offers the chance to accept or reject the call. Accepting the call opens the app to a call page.
- [ ] (TBC) Calls will be entered into chat history as immutable messages with styling differing from typical messages

## Proposed Implementation
<!-- The calls themselves should be handled by [WebRTC](https://www.html5rocks.com/en/tutorials/webrtc/infrastructure). This requires some initial messaging to set up the details of the session (routing, codecs, message priorities) and then the data of the call is passed peer-to-peer through the WebRTC channel resulting from the session instantiation. In order to secure the communications, the initial communication to set up the session will be handled through the existing SimpleX communication channel between users. The content sent through the WebRTC session will also be encrypted using keys (exchanged through SimpleX). Full details of the workflow for setting up WebRTC calls can be found [here](https://developer.mozilla.org/en-US/docs/Web/API/WebRTC_API/Signaling_and_video_calling). -->

To take advantage of existing expertise and development in secure audio-visual communication, we propose to build this functionality using the [Jitsi](https://jitsi.github.io/handbook) [Android](https://jitsi.github.io/handbook/docs/dev-guide/dev-guide-android-sdk) and [iOS](https://jitsi.github.io/handbook/docs/dev-guide/dev-guide-ios-sdk) SDKs.

Note that Jitsi calls [support end-to-end encryption](https://jitsi.org/blog/e2ee/).


### Setting Up the Session
<!-- In essence, we can use SimpleX to [handle the signalling](https://developer.mozilla.org/en-US/docs/Web/API/WebRTC_API/Signaling_and_video_calling) with [ICE](https://developer.mozilla.org/en-US/docs/Glossary/ICE) agents performing negotiation at either end in the SimpleX mobile app. This requires the sharing of [Session Description Protocol](https://developer.mozilla.org/en-US/docs/Glossary/SDP) information which can be serialised as JSON. These can be passed as a new message type in the SimpleX API. -->

There are a few key features required to set up a jitsi call session. These features are TBC after discussion with jitsi team.

1. A room ID which should be unique and single-use
2. A private key for the call (note that this is a [relatively new feature](https://jitsi.org/blog/e2ee/) and may not yet have full support). This private key can be any url appropriate string.
3. [Possibly] A password to connect to the call. Depending on whether this is supported by the API. It seems likely that this is unnecessary if we have E2EE as per point 2.

These elements can be included in a new 'call' message type which includes the information alongside the nature of the call (i.e. audio only or video).


Issues with Jitsi
- We likely reveal user's IP addresses to Jitsi
- We lose control of encryption
- We're passing traffic to Jitsi Servers (although we could [self-host](https://jitsi.github.io/handbook/docs/devops-guide/))
- SDK and encryption are WIP


### Setting Up the Call
Using Jitsi SDKs, the JitsiMeetView ([Android](https://jitsi.github.io/handbook/docs/dev-guide/dev-guide-android-sdk#jitsimeetview), [iOS](https://jitsi.github.io/handbook/docs/dev-guide/dev-guide-ios-sdk#jitsimeetview-class)) can be shown to the user.

User state etc can be updated as calls are connected/disconnected.


## Queries
**Do we need to set up and destroy virtual IP addresses for additional security?**

For initial implementation it is sufficient to warn users that they may be exposing their IP to the recipient.

**Is it beneficial to have an additional layer of encryption for the media content (under the principle of zero trust or otherwise)?**

Yes. We can implement a 'frame encryption' method in the SimpleX API which given a key and some content returns the encrypted content. Similarly, we will have a decryption call. Keys can be call specific and formed using typical DH key exchange.

**Who runs the STUN/TURN servers?**
If using Jitsi SDK then we connect via Jitsi owned servers. In other methods (WebViews) SimpleX needs to run its own routing servers to support ICE.
