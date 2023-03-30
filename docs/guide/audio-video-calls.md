# Audio and Video Calls

SimpleX Chat supports end-to-end encrypted audio and video calls via relay servers or over peer-to-peer. Note: Group calls are not supported at this time. 

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

By default, SimpleX Chat uses a pre-set relay server to hide your IP address from your contacts. However, that same relay server can also observe the duration of your calls. If your threat model calls for it, you also have the option to configure and use your self-hosted WebRTC relay servers for further control of your calls.  

1. Open the app. 
2. Open the app settings menu.
3. Tap **Audio and Video calls**. 
4. Tap **WebRTC ICE servers**.
5. Toggle the **Configure ICE servers** switch on. 
6. Enter your ICE server addresses (one per line).
7. Tap **Save**.

## Calls on lock screen

By default, calls only show on your device's lock screen. In other words: When you receive an incoming call on your lock screen, you have to unlock your device in order to answer it. However, you can change this behaviour in settings. 

1. Open the app. 
2. Open the app settings menu.
3. Tap **Audio and Video calls**. 
4. On the **Calls on lock screen** drop-down menu, choose from the following three options:
    - Disable
    - Show
    - Accept

## Always use relay

By default, audio and video calls on SimpleX Chat are routed via a relay server. Optionally you can disable this and use peer-to-peer instead.

1. Open the app. 
2. Open the app settings menu. 
3. Tap **Audio and Video calls**. 
4. Toggle the **Always use relay** switch on to use a relay server or off for peer-to-peer.