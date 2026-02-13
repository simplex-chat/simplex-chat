# Settings

## Purpose

Configure all aspects of app behavior including notifications, network/servers, privacy, appearance, database management, call settings, and developer tools. Accessed from the UserPicker sheet on the chat list.

## Route / Navigation

- **Entry point**: Tap user avatar in `ChatListView` toolbar -> `UserPicker` -> Settings option
- **Presented by**: `UserPickerSheetView(sheet: .settings)` wrapping `SettingsView` in a `NavigationView`
- **Navigation title**: "Your settings"
- **Sub-navigation**: Each settings row is a `NavigationLink` to a dedicated settings view

## Page Sections

### Settings Section

| Row | Icon | Destination | Description |
|---|---|---|---|
| Notifications | `bolt` (color varies by token status) | `NotificationsView` | Push notification mode and preview settings |
| Network & servers | `externaldrive.connected.to.line.below` | `NetworkAndServers` | SMP/XFTP servers, proxy, .onion hosts, advanced network |
| Audio & video calls | `video` | `CallSettings` | WebRTC relay policy, ICE servers, CallKit options |
| Privacy & security | `lock` | `PrivacySettings` | SimpleX Lock, screen protection, delivery receipts, auto-accept |
| Appearance | `sun.max` | `AppearanceSettings` | Theme, language, wallpapers, chat bubbles, toolbar opacity |

All rows disabled when `chatModel.chatRunning != true`. Appearance row only shown when `UIApplication.shared.supportsAlternateIcons`.

#### Notifications (`NotificationsView`)

| Setting | Options |
|---|---|
| Notification mode | Instant (background connection) / Periodic (every 10 min) / Off |
| Notification preview | Hidden / Contact name only / Message preview |
| Token status indicator | Icon color reflects: new, registered, confirmed (yellow), active (green), expired, invalid |

#### Network & Servers (`NetworkAndServers`)

| Setting | Description |
|---|---|
| SMP servers | Messaging relay servers; per-operator configuration |
| XFTP servers | File transfer servers; per-operator configuration |
| Server operators | `OperatorView` for each configured operator |
| Advanced network | `AdvancedNetworkSettings` -- timeouts, TCP keep-alive, reconnect intervals |
| Proxy configuration | SOCKS proxy, .onion host settings |
| Show sent via proxy | Toggle to show proxy indicator on sent messages |
| Show subscription % | Toggle to show server subscription percentage |

Sub-files: `NetworkAndServers.swift`, `ProtocolServersView.swift`, `ProtocolServerView.swift`, `NewServerView.swift`, `ScanProtocolServer.swift`, `AdvancedNetworkSettings.swift`, `OperatorView.swift`, `ConditionsWebView.swift`

#### Privacy & Security (`PrivacySettings`)

| Setting | Description |
|---|---|
| SimpleX Lock | Enable biometric (Face ID / Touch ID) or passcode lock |
| Lock mode | System biometric or custom passcode |
| Lock timeout | Delay before lock activates (0s to 30min) |
| Self-destruct | Optional self-destruct passcode that wipes all data |
| Screen protection | Hide app content in app switcher |
| Encrypt local files | Encrypt media and files stored on device |
| Auto-accept images | Automatically download received images |
| Link previews | Generate link previews for sent URLs |
| SimpleX link mode | Description / Full link / Via browser |
| Chat previews | Show message previews in chat list |
| Save last draft | Remember unsent message drafts |
| Delivery receipts | Enable/disable read receipts globally |
| Media blur radius | Blur level for received media before tapping |

#### Appearance (`AppearanceSettings`)

| Setting | Description |
|---|---|
| App icon | Alternative app icon selection |
| Language | Interface language |
| Theme | System / Light / Dark |
| Dark theme variant | Dark / SimpleX / Black |
| Active theme colors | Accent color, chat bubble colors, text colors |
| Wallpapers | Chat background wallpaper selection and customization |
| Profile image corner radius | Adjust avatar roundness |
| Chat bubble roundness | Adjust message bubble corner radius |
| Chat bubble tail | Toggle message bubble tail/pointer |
| Toolbar opacity | `ToolbarMaterial` transparency setting |
| One-hand UI | Bottom toolbar layout for reachability |

#### Audio & Video Calls (`CallSettings`)

| Setting | Description |
|---|---|
| WebRTC relay policy | Always relay / Allow direct |
| ICE servers | Custom STUN/TURN server configuration |
| CallKit integration | Enable/disable native iOS call UI |
| Calls in recents | Show/hide calls in Phone app history |
| Lock screen calls | Show/accept on lock screen options |

### Chat Database Section

| Row | Icon | Destination | Description |
|---|---|---|---|
| Database passphrase & export | `internaldrive` (orange if unencrypted) | `DatabaseView` | Passphrase management, export/import database, file storage stats |
| Migrate to another device | `tray.and.arrow.up` | `MigrateFromDevice` | Export database and generate migration link |

Database row shows exclamation octagon icon in red when `chatRunning == false`.

### Help Section

| Row | Icon | Destination | Description |
|---|---|---|---|
| How to use it | `questionmark` | `ChatHelp` | Usage guide with user's display name |
| What's new | `plus` | `WhatsNewView` | Changelog and new features |
| About SimpleX Chat | `info` | `SimpleXInfo` | About page with privacy explanation |
| Send questions and ideas | `number` | Opens SimpleX team chat link | Direct contact with developers |
| Send us email | `envelope` | `mailto:chat@simplex.chat` | Email link |

### Support SimpleX Chat Section

| Row | Icon | Action |
|---|---|---|
| Contribute | `keyboard` | Opens GitHub contribution guide |
| Rate the app | `star` | `SKStoreReviewController.requestReview` |
| Star on GitHub | GitHub icon | Opens GitHub repository |

### Develop Section

| Row | Icon | Destination | Description |
|---|---|---|---|
| Developer tools | `chevron.left.forwardslash.chevron.right` | `DeveloperView` | Chat console/terminal, log level, confirm DB upgrades |
| App version | (none) | `VersionView` | Shows "v{version} ({build})" |

## Loading / Error States

| State | Behavior |
|---|---|
| Chat not running | Most navigation links disabled; database row shows warning |
| Database not encrypted | Database icon shown in orange |
| Migration in progress | `showProgress` overlays `ProgressView` on entire settings view |
| Terminal cleanup | On disappear: `chatModel.showingTerminal = false`, terminal items cleared |

## App Defaults

Key `UserDefaults` / `AppStorage` keys managed by settings:
- `DEFAULT_PERFORM_LA`, `DEFAULT_LA_MODE`, `DEFAULT_LA_LOCK_DELAY`, `DEFAULT_LA_SELF_DESTRUCT`
- `DEFAULT_PRIVACY_ACCEPT_IMAGES`, `DEFAULT_PRIVACY_LINK_PREVIEWS`, `DEFAULT_PRIVACY_PROTECT_SCREEN`
- `DEFAULT_PRIVACY_SHOW_CHAT_PREVIEWS`, `DEFAULT_PRIVACY_SAVE_LAST_DRAFT`
- `DEFAULT_PRIVACY_DELIVERY_RECEIPTS_SET`, `DEFAULT_PRIVACY_MEDIA_BLUR_RADIUS`
- `DEFAULT_WEBRTC_POLICY_RELAY`, `DEFAULT_WEBRTC_ICE_SERVERS`, `DEFAULT_CALL_KIT_CALLS_IN_RECENTS`
- `DEFAULT_CURRENT_THEME`, `DEFAULT_SYSTEM_DARK_THEME`, `DEFAULT_THEME_OVERRIDES`
- `DEFAULT_PROFILE_IMAGE_CORNER_RADIUS`, `DEFAULT_CHAT_ITEM_ROUNDNESS`, `DEFAULT_CHAT_ITEM_TAIL`
- `DEFAULT_TOOLBAR_MATERIAL`, `DEFAULT_ONE_HAND_UI_CARD_SHOWN`
- `DEFAULT_DEVELOPER_TOOLS`, `DEFAULT_SHOW_SENT_VIA_RPOXY`, `DEFAULT_SHOW_SUBSCRIPTION_PERCENTAGE`

## Related Specs

- `spec/architecture.md` -- App architecture overview
- `spec/services/theme.md` -- Theme system specification
- [Chat List](chat-list.md) -- Parent view via UserPicker
- [User Profiles](user-profiles.md) -- Profile management (separate UserPicker option)

## Source Files

- `Shared/Views/UserSettings/SettingsView.swift` -- Main settings view, section layout, app defaults definitions
- `Shared/Views/UserSettings/NotificationsView.swift` -- Notification mode and preview settings
- `Shared/Views/UserSettings/AppearanceSettings.swift` -- Theme, wallpaper, UI customization
- `Shared/Views/UserSettings/PrivacySettings.swift` -- Privacy and security settings
- `Shared/Views/UserSettings/NetworkAndServers/NetworkAndServers.swift` -- Server and network configuration
- `Shared/Views/UserSettings/NetworkAndServers/AdvancedNetworkSettings.swift` -- TCP/timeout settings
- `Shared/Views/UserSettings/NetworkAndServers/ProtocolServersView.swift` -- SMP/XFTP server list
- `Shared/Views/UserSettings/NetworkAndServers/ProtocolServerView.swift` -- Individual server edit
- `Shared/Views/UserSettings/NetworkAndServers/NewServerView.swift` -- Add new server
- `Shared/Views/UserSettings/NetworkAndServers/ScanProtocolServer.swift` -- Scan server QR code
- `Shared/Views/UserSettings/NetworkAndServers/OperatorView.swift` -- Server operator configuration
- `Shared/Views/UserSettings/NetworkAndServers/ConditionsWebView.swift` -- Operator conditions display
