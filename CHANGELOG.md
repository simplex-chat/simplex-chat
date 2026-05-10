# Release History

## v6.5

April 2026

- Public channels — speak freely! Reliability (many relays per channel), ownership (run your own relays), security (owners hold channel keys), privacy (for owners and subscribers)
- Easier to invite friends: simpler connecting for new users
- Safe web links: opt-in link previews, SOCKS proxy for previews, prevent hyperlink phishing, remove link tracking
- Non-profit governance: to make SimpleX Network last

## v6.4

July 2025

- Connect faster: message instantly once you tap Connect
- Review group members: chat with new members before they join
- Chat with admins: send private feedback to group owners
- New group role: Moderator — can remove messages and block members
- Improved message delivery — less traffic on mobile networks

## v6.3

March 2025

- Better groups: mention members, private reports to moderators, batch delete/block/change role (Android and desktop), faster sending and deletion
- Better chat navigation: organize chats into lists, jump to found and forwarded messages
- Better privacy and security: private media file names, message expiration in chats

## v6.2

December 2024

- Flux servers included in the app — to improve metadata privacy
- Business chats — your customers' privacy
- Improved UX: open chat on first unread message, jump to quoted messages, see who reacted
- Improved iOS push notifications

## v6.1

October 2024

- Better security: SimpleX protocols reviewed by Trail of Bits, security improvements
- Better calls: switch audio/video during call, screen sharing from desktop
- Better iOS notifications: improved delivery, reduced traffic
- Better UX: switch profile for 1-time invitations, customizable message shape, forward up to 20 messages, delete or moderate up to 200 messages

## v6.0

August 2024

- Private message routing: protects IP address, now enabled by default
- New chat experience: connect faster, archive contacts, delete up to 20 messages at once, increase font size, reachable chat toolbar (one-hand use)
- New media options: share from other apps (iOS), play from chat list, blur for privacy
- Connection and servers information

## v5.8

June 2024

- Private message routing to protect IP addresses (opt-in in this version)
- Protect IP address when receiving files
- Chat themes with wallpapers (Android and desktop)
- Some group permissions can be granted to admins only
- Improved message and file delivery with reduced battery usage
- Persian interface language (Android and desktop)

## v5.7

April 2024

- Quantum resistant end-to-end encryption — enabled for all direct chats
- Forward and save messages and files without revealing the source
- Improved calls: in-call sounds, better bluetooth headphone support
- Customizable shapes of profile images (square to circle)
- More reliable network connection
- Lithuanian UI language (Android and desktop)

## v5.6

March 2024

- Quantum resistant end-to-end encryption in direct chats (BETA, opt-in)
- App data migration via QR code (upload to XFTP, scan from new device)
- Use the app during audio and video calls
- Admins can block a member for all other members
- Much faster leaving and deleting groups
- Reduced memory usage when sending large files
- Desktop: scrollbars in all views

## v5.5

January 2024

- Private notes — with encrypted files and media
- Paste link to connect — search bar accepts invitation links
- Optional recent history in groups
- Improved message delivery with reduced battery usage
- Reveal secrets in messages by tapping
- All files in local app storage encrypted by default
- Hungarian (Android) and Turkish interface

## v5.4

November 2023

- Link mobile and desktop apps via secure quantum-resistant protocol
- Better groups: faster to join, create with incognito, block members, prohibit files/media
- Better calls: faster connection, screen sharing on desktop
- Profile names allow spaces
- Contact deletion optionally notifies the contact

## v5.3

September 2023

- Encrypt local files in app storage (except videos)
- Improved groups: delivery receipts (up to 20 members), direct messages to members after contact deleted, faster and more stable
- Simplified incognito mode
- New privacy settings: show last messages, save draft
- Faster app loading, 40% reduced memory usage
- 6 new interface languages: Arabic, Bulgarian, Finnish, Hebrew, Thai, Ukrainian

## v5.2

July 2023

- Message delivery receipts — with opt out per contact
- Filter favorite and unread chats
- Keep connections working after restoring from backup
- Share address with group members via profile
- Improved disappearing messages
- Chat preference to prohibit message reactions
- Restart and shutdown buttons

## v5.1

May 2023

- Message reactions (6 emoji, up to 3 per message)
- Voice messages up to 5 minutes via XFTP
- Custom disappearing message timing (1 second to 3 months)
- Message editing history
- Customizable color themes (Android)
- Self-destruct passcode

## v5.0

April 2023

- Send videos and files up to 1GB (recipient needs v4.6.1+)
- Self-host XFTP servers and configure app to use them
- Passcode as alternative to system/device authentication
- Support for IPv6 server addresses
- Configurable SOCKS proxy host and port (Android)
- Polish interface language

## v4.6

March 2023

- Hidden chat profiles — protected with a password
- Audio/video calls: iOS completely re-implemented (native WebRTC, CallKit, works in background), Android bluetooth and proximity sensor
- Group moderation: admins can delete member messages, disable members (observer role)
- Group welcome message
- Reduced battery usage for large groups
- Android 8+ and ARMv7a support
- Chinese and Spanish interface

## v4.5

February 2023

- Multiple chat profiles: different names, avatars, transport isolation
- Transport isolation: separate transport connections per profile (default) or per connection (BETA)
- Message draft preserved when leaving conversation
- Private filenames: UTC timestamps instead of local timezone

## v4.4

January 2023

- Disappearing messages (requires mutual consent)
- Live messages (recipient sees you typing in real-time)
- Connection security verification (QR code comparison)
- Animated images on iOS
- French language

## v4.3

December 2022

- Instant voice messages (up to 30-42 seconds)
- Irreversible message deletion (configurable per-contact/group)
- SMP server passwords for private self-hosted servers
- App screen protection
- Improved invitation link security

## v4.2

November 2022

- Fixed issues from Trail of Bits security audit
- Group links — admins can create links for new members to join
- Auto-accept contact requests (configurable: accept incognito, welcome message)
- Change group member role
- Mark chat as unread
- Android: support for image/gif/sticker keyboards

## v4.1

October 2022

- Custom time to disappear in messages
- Improved stability

## v4.0

September 2022

- Encrypted local database (SQLCipher)
- Self-hosted WebRTC ICE servers for calls
- Improved connection stability
- File and media deletion from local storage
- TypeScript SDK for bots

## v3.2

September 2022

- Incognito mode — random display name per contact
- User-assignable contact names
- .onion server address management
- Endless scrolling and search
- Accent color and dark mode customization

## v3.1

August 2022

- Tor support via SOCKS proxy (all platforms)
- Secret chat groups in mobile apps
- Protocol batching (up to 90x traffic reduction)
- Docker configs for self-hosted servers
- Dual .onion server addresses
- Advanced network settings

## v3.0

July 2022

- iOS push notifications
- End-to-end encrypted WebRTC audio/video calls
- Database export/import for device migration

## v2.2

June 2022

- SimpleX Lock (biometric/PIN)
- Auto-download image controls
- Link preview controls
- Message integrity indicators

## v2.1

May 2022

- Clear conversations without deleting contacts

## v2.0

May 2022

- Images and files in mobile apps

## v1.0

January 2022

- Double ratchet E2E encryption (AES-256-GCM, X3DH, Curve448)
- Per-queue E2E encryption (NaCl crypto-box)
- Server-to-recipient encryption layer
- TLS 1.2+ transport
- Binary protocol (reduced overhead from 15% to 3.7%)
