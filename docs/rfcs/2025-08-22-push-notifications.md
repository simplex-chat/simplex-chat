# Push notifications

## Problem

Even with improved networking and optimized management of wake lock, the battery consumption on Android will remain higher than it can be for these reasons:
1. there are much more events for device to receive than there are messages that the user wants to see notifications for:
- messages in muted conversations,
- message updates, deletions and reactions,
- profile updates,
- other group events.
2. the device has to maintain connection for multiple servers.
3. network changes require reconnections that are much more expensive for SimpleX network, because of resubscriptions, than with push server.

## Solution

The solution is UnifiedPush that allows users to:
- receive push notifications without Google's FCM,
- use their own servers both to push messages via web push and to push notifications to their devices,
- use one app to receive notifications for the multiple apps.

For users who already use unified push using an additional app to receive notifications is an acceptable solution. For users who do not use UnifiedPush (and may have never heard about it) installing an additional app is not expected and not acceptable - this is not how they normally use messaging apps.

Offering these users Google FCM is an alternative is a bad option for several reasons:
- low trust to how data is used,
- the need to use an additional closed source library, requiring a separate build, or to use an open-source FCM notifications receiver (distributor, in UnifiedPush terminology), which has licensing concerns.

The solution that would avoid these bad choices is embedding UnifiedPush distributor into the app, so people can use it out of the box, simply by installing the app, and using our preconfigured servers, and have the option to change:
- notification server (e.g., to self-hosted)
- push server (e.g., to self-hosted or community)
- notifications distributor (e.g, ntfy.sh app)

[S1m](https://github.com/p1gp1g) implemented the changes for:
- [#1606](https://github.com/simplex-chat/simplexmq/pull/1606) notification server to add provider for web pushes,
- [#6205](https://github.com/simplex-chat/simplex-chat/pull/6205) simplex-chat: support for self-hosted notification servers and android app UI to manage UnifiedPush.

The proposed plan to release push notifications:
1. update notification server:
  - remove extras from URI and move them to protocol (TBC),
  - split PR to remove self-hosted servers and move file changes to a separate PR,
  - release test server to test it all works.
2. remove FCM distributor.
3. split simplex-chat PR to support only notifications servers that we provide. This support requires some changes, as we allow migrating the database between iOS and Android, and possibly we should treat self-hosted ntf servers that support web push differently from pre-configured.
4. add unified push distributor to the app, for zero configuration option - for v6.5 release we do not need to allow using it as a distributor for other apps, only for SimpleX app itself is necessary. We may add supporting other apps later. We still want to allow using other distributors with the app.
5. release as v6.5-beta.0
6. add caching connections to push servers in ntf server, as it happens with APNS. For the current traffic it seems necessary.
7. update onboarding to replace periodic notifications with push notifications, with the additional screen available when PUSH is chosen to change settings during onboarding.
8. release as v6.5-beta.1
9. add support for self-hosted notification servers.
10. release as v6.5-beta.2
11. release push notifications support in v6.5.

Questions:
1. [PR](https://github.com/simplex-chat/simplex-chat/pull/6205) proposes PING mode for notifications, it is not clear why it is needed - as is, SMP servers already coalesce frequent messages in one queue into a single notification, and ntf server also combines multiple notifications into one bundle.
2. Extras in servers URL - I understand that it contains the VAPID key, but I do not think it needs to be a part of server URL. As it is passed to push server when requesting a token, we probably should:
- make it a separate parameter/property of ntf server, not part of address for all servers.
- implement ntf protocol command to request this public key from ntf server - it can be additionally signed by server identity certificate. Delegating the discovery and configuration of this key to the end users feels like a bad UX. It prioritizes people who self-host ntf servers, but this is a minority - most would use some existing servers, ours or community-provided.
- generate this key during ntf server initialization. For our servers we can add it manually, but for self-hosted servers it would also be a bad UX to ask server owners to provide it manually.
3. HTTP/2 support - what existing UnifiedPush servers implementations do and do not support HTTP/2? Am I right that ntfy and many others support it? If so, we don't have to comply with RFC8030 by supporting HTTP/1.1 in ntf server if widely used push servers support HTTP/2.
