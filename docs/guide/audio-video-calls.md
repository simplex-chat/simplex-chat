# Audio and Video Calls

SimpleX Chat allows you to make end-to-end encrypted audio and video calls with your contacts via WebRTC. Note: Group calls are not supported at this time.

## How to make an audio call

1. Open the app.
2. Tap on a contact.
3. Tap on the phone icon to the right of your contact's name at the top of the screen.

## How to make a video call

1. Open the app.
2. Tap on a contact.
3. Tap on the three vertical dots on the top right-hand corner of the screen to access more options.
4. Choose **Video call**.

## WebRTC ICE servers

SimpleX Chat uses a preset relay server to hide your IP address from your contacts by default, but it can also observe the duration of your calls. If you don't want that, you can configure and use your self-hosted WebRTC relay servers instead for further control of your calls.

1. Open the app.
2. Open the app settings menu.
3. Tap **Audio and Video calls**.
4. Tap **WebRTC ICE servers**.
5. Toggle the **Configure ICE servers** switch on.
6. Enter your ICE server addresses (one per line).
7. Tap **Save**.

_Please note_: unlike messaging relays (SMP servers), the configuration of WebRTC ICE servers is stored on the current device, not in the chat database. if you transfer the chat database to another device you need to update this configuration.

## Calls on lock screen

SimpleX Chat only shows an incoming call on your device's lock screen by default. However, you can change this behavior in the app settings menu.

1. Open the app.
2. Open the app settings menu.
3. Tap **Audio and Video calls**.
4. On the **Calls on lock screen** drop-down list, choose from the following three options:
   - Disable
   - Show
   - Accept

## Always use relay

Audio and video calls on SimpleX Chat are routed via a relay server by default. Optionally you can disable this and use peer-to-peer (P2P) instead, when it is supported by your network. However, your IP address will be known to your contacts.

1. Open the app.
2. Open the app settings menu.
3. Tap **Audio and Video calls**.
4. Toggle the **Always use relay** switch on to use a relay server or off for P2P.

_Please note_: disabling this option allows P2P calls, but it does not prohibit the use of TURN relays – in case your network providers block P2P connections, the call will still use relays if the are available. To prohibit the use of relays you need to change WebRTC ICE server configuration to only include STUN servers, for example:
