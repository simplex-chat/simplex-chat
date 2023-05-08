---
title: Audio & video Calls
---
# Audio and Video Calls

SimpleX Chat allows you to make end-to-end encrypted audio and video calls with your contacts via WebRTC. Note: Group calls are not supported at this time.

## Making and accepting the calls

### How to make an audio call

1. Tap on a contact.
2. Tap on the phone icon to the right of your contact's name at the top of the screen.

### How to make a video call

1. Tap on a contact.
2. Tap on the three vertical dots on the top right-hand corner of the screen to access more options.
3. Choose **Video call**.

### Accepting the calls

When there is an incoming call you have three options:

- accept: to connect the call
- reject: to reject the call, _without_ notifying the caller.
- ignore: to temporarily dismiss the call, but in a way that it can be accepted later, if the caller is still waiting, via the **Accept call** message in the conversation with this contact.

There is no time limit that the call invitation can remain active - as long as the caller is still waiting, you can accept the call any time later.

The call can be accepted from the lock screen, both on Android (it needs to be enabled via options) and on iOS (by default, using native iOS call interface that can be disabled).

### Calls on lock screen on Android

SimpleX Chat shows an incoming call on your device's lock screen by default. However, you can change this behavior in the app settings menu.

1. Open the app settings menu.
2. Tap **Audio and Video calls**.
3. On the **Calls on lock screen** drop-down list, choose from the following three options:
   - Disable - the call will show as a notification.
   - Show - the call will show on the lock screen, you need to unlock the device and the app to accept it.
   - Accept - the call can be accepted and rejected directly from the lock screen, without opening the app.

**Please note**: some Android systems/devices prohibit full screen views on lock screen - in this case the call will show as a usual notification.

### Calls on lock screen on iOS

<img src="../../blog/images/20230328-call1.png" width="288">

By default, SimpleX Chat uses native iOS call interface, where allowed, to show incoming calls on the lock screen. You can disable it:

1. Open the app settings menu.
2. Tap **Audio and Video calls**.
3. Toggle the **Use iOS call interface** switch off.

**Please note**: iOS call interface allows accepting the calls without unlocking the device and the app. If this is undesirable, please disable it – the calls will show as notifications in this case.

Read more in [this post](../../blog/20230328-simplex-chat-v4-6-hidden-profiles.md#improved-audiovideo-calls).

## Advanced call settings

### WebRTC ICE servers

<img src="../../blog/images/20220928-ice-servers.png" width="330">

SimpleX Chat uses a preset relay server to hide your IP address from your contacts by default, but it can also observe the duration of your calls. If you don't want that, you can configure and use your self-hosted WebRTC relay servers instead for further control of your calls.

1. Open the app settings menu.
2. Tap **Audio and Video calls**.
3. Tap **WebRTC ICE servers**.
4. Toggle the **Configure ICE servers** switch on.
5. Enter your ICE server addresses (one per line).
6. Tap **Save**.

**Please note**: unlike messaging relays (SMP servers), the configuration of WebRTC ICE servers is stored on the current device, not in the chat database. if you transfer the chat database to another device you need to update this configuration.

### Always use relay

Audio and video calls on SimpleX Chat are routed via a TURN relay server by default. Optionally you can disable this and use peer-to-peer (P2P) instead, when it is supported by your network. However, your IP address will be known to your contacts.

1. Open the app settings menu.
2. Tap **Audio and Video calls**.
3. Toggle the **Always use relay** switch on to use a relay server or off for P2P.

**Please note**: disabling this option allows P2P calls, but it does not prohibit the use of TURN relays – in case your network providers block P2P connections, the call will still use relays if the are available. To prohibit the use of relays you need to change WebRTC ICE server configuration to only include STUN servers, for example:
