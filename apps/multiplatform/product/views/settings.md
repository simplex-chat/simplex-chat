# Settings

> **Related spec:** [spec/client/navigation.md](../../spec/client/navigation.md) | [spec/services/theme.md](../../spec/services/theme.md) | [spec/services/notifications.md](../../spec/services/notifications.md)

## Purpose

Configure all aspects of app behavior including notifications, network/servers, privacy, appearance, database management, call settings, and developer tools. Accessed from the UserPicker or directly from the chat list toolbar.

## Route / Navigation

- **Entry point**: Tap user avatar in `ChatListView` toolbar -> `UserPicker` -> Settings option; or directly via `NavigationButtonMenu` when no users exist
- **Presented by**: `SettingsView` composable via `ModalManager.start.showModalCloseable`
- **Navigation title**: "Your settings" (`AppBarTitle`)
- **Sub-navigation**: Each settings row opens a dedicated view via `showSettingsModal` or `showCustomModal`

## Platform Differences

| Aspect | Android | Desktop |
|---|---|---|
| App section | Device settings, app version | App updates (`AppUpdater`), device settings, app version |
| Notifications | Full notification mode selection (instant/periodic/off) | Notification settings |
| Use from desktop/mobile | "Use from desktop" option in UserPicker | "Link a mobile" / "Linked mobiles" option in UserPicker |
| Database migration | "Migrate to another device" with auth | Same |

## Page Sections

### Settings Section

| Row | Icon | Destination | Description |
|---|---|---|---|
| Notifications | `ic_bolt` / `ic_bolt_off` | `NotificationsSettingsView` | Push notification mode and preview settings |
| Network & servers | `ic_wifi_tethering` | `NetworkAndServersView` | SMP/XFTP servers, proxy, .onion hosts, advanced network |
| Audio & video calls | `ic_videocam` | `CallSettingsView` | WebRTC relay policy, ICE servers |
| Privacy & security | `ic_lock` | `PrivacySettingsView` | SimpleX Lock, delivery receipts, link previews, auto-accept |
| Appearance | `ic_light_mode` | `AppearanceView` | Theme, language, profile images, chat bubbles |

All rows disabled when `chatModel.chatRunning != true` (except Appearance).

#### Notifications (`NotificationsSettingsView`)

| Setting | Options |
|---|---|
| Notification mode | Instant (background service) / Periodic (every 10 min) / Off |
| Notification preview | Configuration for notification content visibility |

#### Network & Servers (`NetworkAndServersView`)

| Setting | Description |
|---|---|
| SMP servers | Messaging relay servers; per-operator configuration |
| XFTP servers | File transfer servers; per-operator configuration |
| Server operators | `OperatorView` for each configured operator |
| Advanced network | `AdvancedNetworkSettings` -- timeouts, TCP keep-alive, reconnect intervals |
| Proxy configuration | SOCKS proxy, .onion host settings |

Sub-files: `NetworkAndServers.kt`, `ProtocolServersView.kt`, `ProtocolServerView.kt`, `NewServerView.kt`, `ScanProtocolServer.kt`, `AdvancedNetworkSettings.kt`, `OperatorView.kt`

#### Audio & Video Calls (`CallSettingsView`)

| Setting | Description |
|---|---|
| WebRTC relay policy | Always relay / relay when needed / never relay |
| ICE servers | Custom STUN/TURN server configuration |

#### Privacy & Security (`PrivacySettingsView`)

Organized in sections:

**Device Section** (`PrivacyDeviceSection`):

| Setting | Description |
|---|---|
| SimpleX Lock | `SimplexLockView` -- app lock with system auth or passcode (`LAMode.SYSTEM` / `LAMode.PASSCODE`) |

**Chats Section**:

| Setting | Preference Key | Description |
|---|---|---|
| Send link previews | `privacyLinkPreviews` | Auto-generate link preview cards |
| Sanitize links | `privacySanitizeLinks` | Strip tracking parameters from URLs |
| Show last messages | `privacyShowChatPreviews` | Show message previews in chat list |

#### Appearance (`AppearanceView`)

Platform-specific composable (`expect fun AppearanceView`):

| Setting | Description |
|---|---|
| Profile images | `ProfileImageSection` -- slider for profile image corner radius |
| Theme selection | Color scheme / theme picker |
| Language | App language selection |
| Chat wallpaper | Background image settings |
| Chat bubbles | Message bubble appearance configuration |
| Toolbar opacity | App bar transparency settings (`inAppBarsAlpha`) |
| Color picker | `ClassicColorPicker` for custom theme colors |

### Chat Database Section

| Row | Icon | Destination | Description |
|---|---|---|---|
| Database passphrase & export | `ic_database` | `DatabaseView` | Manage encryption, export/import database |
| Migrate to another device | `ic_ios_share` | `MigrateFromDeviceView` | Device migration (requires auth) |

Database icon shows warning color (`WarningOrange`) when database is not encrypted or passphrase is not saved.

### Help Section

| Row | Icon | Destination | Description |
|---|---|---|---|
| How to use SimpleX Chat | `ic_help` | `HelpView` | Usage guide |
| What's new | `ic_add` | `WhatsNewView` | Version changelog |
| About SimpleX Chat | `ic_info` | `SimpleXInfo` (non-onboarding mode) | App information |
| Chat with the founder | `ic_tag` | Opens SimpleX link | Direct chat with SimpleX team |
| Send us an email | `ic_mail` | Opens mailto: | Email support |

### Support Section

| Row | Icon | Description |
|---|---|---|
| Contribute | `ic_keyboard` | Opens GitHub contribution page (hidden for Android Bundle) |
| Rate the app | `ic_star` | Opens Google Play / app store listing |
| Star on GitHub | `ic_github` | Opens GitHub repository |

### App Section (`SettingsSectionApp`)

Platform-specific section (expect/actual composable):

| Row | Description |
|---|---|
| App updates (Desktop) | App update checker and installer |
| Developer tools | Toggle developer mode |
| Chat console | Opens `ChatConsoleView` terminal |
| Terminal always visible (Desktop) | Keep terminal window open |
| Install terminal app | Link to CLI app on GitHub |
| Reset all hints | Reset dismissed hint/card preferences |
| App version | Version string with build info; taps open `VersionInfoView` |

## Source Files

| File | Path |
|---|---|
| `SettingsView.kt` | `views/usersettings/SettingsView.kt` |
| `Appearance.kt` | `views/usersettings/Appearance.kt` |
| `PrivacySettings.kt` | `views/usersettings/PrivacySettings.kt` |
| `NetworkAndServers.kt` | `views/usersettings/networkAndServers/NetworkAndServers.kt` |
| `AdvancedNetworkSettings.kt` | `views/usersettings/networkAndServers/AdvancedNetworkSettings.kt` |
| `OperatorView.kt` | `views/usersettings/networkAndServers/OperatorView.kt` |
| `ProtocolServersView.kt` | `views/usersettings/networkAndServers/ProtocolServersView.kt` |
| `NewServerView.kt` | `views/usersettings/networkAndServers/NewServerView.kt` |
