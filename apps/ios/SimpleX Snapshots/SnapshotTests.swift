import XCTest
import SwiftUI
import UIKit
@testable import SimpleX
@testable import SimpleXChat

final class SnapshotTests: XCTestCase {
    @MainActor
    func testPreviews() throws {
        NSTimeZone.default = try XCTUnwrap(TimeZone(identifier: "UTC"))
        let env = ProcessInfo.processInfo.environment
        let dir = URL(fileURLWithPath: env["SNAPSHOT_DIR"] ?? NSTemporaryDirectory() + "snapshots", isDirectory: true)
        let size = env["SNAPSHOT_SIZE"] ?? "large"
        try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
        let windowScene = try XCTUnwrap(UIApplication.shared.connectedScenes.compactMap { $0 as? UIWindowScene }.first)
        ChatModel.shared.currentUser = User.sampleData
        var index: [[String: Any]] = []
        for dark in [false, true] {
            let theme = snapshotTheme(dark)
            CurrentColors = theme
            for scene in previewScenes() where scene.allSizes || size == "large" {
                let themeName = dark ? "dark" : "light"
                let file = "\(scene.name)-\(themeName)-\(size).png"
                let image = try renderScene(scene, theme.toAppTheme(), dark: dark, in: windowScene)
                try XCTUnwrap(image.pngData()).write(to: dir.appendingPathComponent(file))
                index.append([
                    "scene": scene.name,
                    "theme": themeName,
                    "size": size,
                    "file": file,
                    "width": Int((image.size.width * image.scale).rounded()),
                    "height": Int((image.size.height * image.scale).rounded()),
                    "scale": image.scale,
                    "contentSizeCategory": UIApplication.shared.preferredContentSizeCategory.rawValue
                ])
            }
        }
        let data = try JSONSerialization.data(withJSONObject: index, options: [.prettyPrinted, .sortedKeys])
        try data.write(to: dir.appendingPathComponent("index-\(size).json"))
    }

    @MainActor
    func testFirstLayout() throws {
        NSTimeZone.default = try XCTUnwrap(TimeZone(identifier: "UTC"))
        let env = ProcessInfo.processInfo.environment
        let size = env["SNAPSHOT_SIZE"] ?? "large"
        let dir = layoutCheckDir(env)
        try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
        let windowScene = try XCTUnwrap(UIApplication.shared.connectedScenes.compactMap { $0 as? UIWindowScene }.first)
        ChatModel.shared.currentUser = User.sampleData
        let theme = snapshotTheme(false)
        CurrentColors = theme
        let appTheme = theme.toAppTheme()
        let fixtures = SnapshotFixtures()
        var index: [[String: Any]] = []
        for terminal in [true, false] {
            let chats = [("direct", fixtures.overviewDirect(terminal: terminal)), ("group", fixtures.overviewGroup(terminal: terminal)), ("channel", fixtures.overviewChannel(terminal: terminal))]
            for (name, rows) in chats {
                let scene = "\(name)-\(terminal ? "terminal" : "classic")-\(size)"
                let scrollView = EndlessScrollView<MergedItem>(frame: .zero)
                let host = UIHostingController(rootView: ScrollRepresentable(scrollView: scrollView) { (i: Int, item: MergedItem) in
                    rows.row(i, item)
                        .environmentObject(ChatModel.shared)
                        .environmentObject(appTheme)
                })
                let window = UIWindow(windowScene: windowScene)
                window.overrideUserInterfaceStyle = .light
                window.rootViewController = host
                window.frame = windowScene.screen.bounds
                window.isHidden = false
                settle()
                scrollView.updateItems(rows.mergedItems.items)
                settle()
                let first = scrollView.listState.visibleItems.map { $0.view.frame.height }
                try XCTUnwrap(windowImage(window).pngData()).write(to: dir.appendingPathComponent("\(scene)-first.png"))
                scrollView.updateItems(rows.mergedItems.items, true)
                settle()
                let reloaded = scrollView.listState.visibleItems.map { $0.view.frame.height }
                try XCTUnwrap(windowImage(window).pngData()).write(to: dir.appendingPathComponent("\(scene)-reloaded.png"))
                window.isHidden = true
                window.rootViewController = nil
                index.append(["scene": scene, "first": first, "reloaded": reloaded])
                print("first layout \(scene): first \(first), reloaded \(reloaded)")
                XCTAssertEqual(first.count, reloaded.count, scene)
                for (a, b) in zip(first, reloaded) {
                    XCTAssertEqual(a, b, accuracy: 0.5, scene)
                }
            }
        }
        let data = try JSONSerialization.data(withJSONObject: index, options: [.prettyPrinted, .sortedKeys])
        try data.write(to: dir.appendingPathComponent("first-layout-\(size).json"))
    }

    @MainActor
    func testLayoutSwitch() throws {
        let env = ProcessInfo.processInfo.environment
        guard (env["SNAPSHOT_SIZE"] ?? "large") == "large" else { return }
        let dir = layoutCheckDir(env)
        try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
        let windowScene = try XCTUnwrap(UIApplication.shared.connectedScenes.compactMap { $0 as? UIWindowScene }.first)
        ChatModel.shared.currentUser = User.sampleData
        let theme = snapshotTheme(false)
        CurrentColors = theme
        let appTheme = theme.toAppTheme()
        let sceneDelegate = SceneDelegate()
        let appearance = {
            UIHostingController(rootView: AppearanceSettings()
                .environmentObject(ChatModel.shared)
                .environmentObject(sceneDelegate)
                .environmentObject(appTheme))
        }
        chatLayoutDefault.set(.classic)
        let window = UIWindow(windowScene: windowScene)
        window.overrideUserInterfaceStyle = .light
        window.rootViewController = appearance()
        window.frame = windowScene.screen.bounds
        window.isHidden = false
        settle()
        try XCTUnwrap(windowImage(window).pngData()).write(to: dir.appendingPathComponent("appearance-1-classic.png"))
        chatLayoutDefault.set(.terminal)
        settle()
        try XCTUnwrap(windowImage(window).pngData()).write(to: dir.appendingPathComponent("appearance-2-terminal-switched.png"))
        window.rootViewController = appearance()
        settle()
        try XCTUnwrap(windowImage(window).pngData()).write(to: dir.appendingPathComponent("appearance-3-terminal-reopened.png"))
        chatLayoutDefault.set(.classic)
        settle()
        try XCTUnwrap(windowImage(window).pngData()).write(to: dir.appendingPathComponent("appearance-4-classic-switched.png"))
        window.isHidden = true
        window.rootViewController = nil
    }
}

private func layoutCheckDir(_ env: [String: String]) -> URL {
    URL(fileURLWithPath: env["SNAPSHOT_DIR"] ?? NSTemporaryDirectory() + "snapshots", isDirectory: true)
        .deletingLastPathComponent()
        .appendingPathComponent("layout-checks", isDirectory: true)
}

private func windowImage(_ window: UIWindow) -> UIImage {
    let format = UIGraphicsImageRendererFormat.default()
    format.scale = 1
    format.preferredRange = .standard
    format.opaque = true
    return UIGraphicsImageRenderer(bounds: window.bounds, format: format).image { _ in
        _ = window.drawHierarchy(in: window.bounds, afterScreenUpdates: true)
    }
}

private struct SnapshotScene {
    var name: String
    var height: CGFloat? = nil
    var allSizes: Bool
    var view: AnyView

    init<P: PreviewProvider>(_ preview: P.Type, allSizes: Bool = false) {
        name = String(describing: preview).replacingOccurrences(of: "_Previews", with: "")
        self.allSizes = allSizes
        view = AnyView(preview.previews)
    }
}

private func previewScenes() -> [SnapshotScene] {
    [
        SnapshotScene(ActiveCallOverlay_Previews.self),
        SnapshotScene(AddChannelView_Previews.self),
        SnapshotScene(AddContactLearnMore_Previews.self),
        SnapshotScene(AddGroupMembersView_Previews.self),
        SnapshotScene(AddGroupView_Previews.self),
        SnapshotScene(AdvancedNetworkSettings_Previews.self),
        SnapshotScene(BadgesCheckOrderView_Previews.self),
        SnapshotScene(BadgesHowItWorksView_Previews.self),
        SnapshotScene(BadgesHowLongView_Previews.self),
        SnapshotScene(BadgesRedeemCodeView_Previews.self),
        SnapshotScene(BadgesSupportSimplexView_Previews.self),
        SnapshotScene(CallSettings_Previews.self),
        SnapshotScene(ChannelMembersView_Previews.self),
        SnapshotScene(ChannelRelaysView_Previews.self),
        SnapshotScene(ChatHelp_Previews.self),
        SnapshotScene(ChatInfoImage_Previews.self),
        SnapshotScene(ChatInfoToolbar_Previews.self),
        SnapshotScene(ChatInfoView_Previews.self),
        SnapshotScene(ChatItemForwardingView_Previews.self),
        SnapshotScene(ChatItemInfoView_Previews.self),
        SnapshotScene(ChatItemView_NonMsgContentDeleted_Previews.self, allSizes: true),
        SnapshotScene(ChatItemView_Previews.self, allSizes: true),
        SnapshotScene(ChatListNavLink_Previews.self),
        SnapshotScene(ChatListView_Previews.self),
        SnapshotScene(ChatPreviewView_Previews.self),
        SnapshotScene(ChatView_Previews.self),
        SnapshotScene(CIChatFeatureView_Previews.self, allSizes: true),
        SnapshotScene(CIEventView_Previews.self, allSizes: true),
        SnapshotScene(CIFeaturePreferenceView_Previews.self, allSizes: true),
        SnapshotScene(CIFileView_Previews.self, allSizes: true),
        SnapshotScene(CIGroupInvitationView_Previews.self, allSizes: true),
        SnapshotScene(CIInvalidJSONView_Previews.self, allSizes: true),
        SnapshotScene(CIMemberCreatedContactView_Previews.self, allSizes: true),
        SnapshotScene(CIMetaView_Previews.self, allSizes: true),
        SnapshotScene(CIVoiceView_Previews.self, allSizes: true),
        SnapshotScene(ClassicAvatars_Previews.self, allSizes: true),
        SnapshotScene(ClassicOverviewChannel_Previews.self),
        SnapshotScene(ClassicOverviewDirect_Previews.self),
        SnapshotScene(ClassicOverviewGroup_Previews.self),
        SnapshotScene(ComposeVoiceView_Previews.self),
        SnapshotScene(ConnectDesktopView_Previews.self),
        SnapshotScene(ContactConnectionInfo_Previews.self),
        SnapshotScene(ContactConnectionView_Previews.self),
        SnapshotScene(ContactPreferencesView_Previews.self),
        SnapshotScene(ContactRequestView_Previews.self),
        SnapshotScene(ContextContactRequestActionsView_Previews.self),
        SnapshotScene(ContextItemView_Previews.self),
        SnapshotScene(ContextMemberContactActionsView_Previews.self),
        SnapshotScene(ContextPendingMemberActionsView_Previews.self),
        SnapshotScene(ContextProfilePickerView_Previews.self),
        SnapshotScene(CreateProfile_Previews.self),
        SnapshotScene(CreateSimpleXAddress_Previews.self),
        SnapshotScene(CustomTimePicker_Previews.self),
        SnapshotScene(DatabaseEncryptionView_Previews.self),
        SnapshotScene(DatabaseErrorView_Previews.self),
        SnapshotScene(DatabaseView_Previews.self),
        SnapshotScene(DeletedItemView_Previews.self, allSizes: true),
        SnapshotScene(DetermineWidth_Previews.self),
        SnapshotScene(DeveloperView_Previews.self),
        SnapshotScene(EmojiItemView_Previews.self, allSizes: true),
        SnapshotScene(FramedCIVoiceView_Previews.self, allSizes: true),
        SnapshotScene(FramedItemView_Deleted_Previews.self, allSizes: true),
        SnapshotScene(FramedItemView_Edited_Previews.self, allSizes: true),
        SnapshotScene(FramedItemView_Previews.self, allSizes: true),
        SnapshotScene(GetStakeBanner_Previews.self),
        SnapshotScene(GroupChatInfoView_Previews.self),
        SnapshotScene(GroupLinkView_Previews.self),
        SnapshotScene(GroupMemberInfoView_Previews.self),
        SnapshotScene(GroupPreferencesView_Previews.self),
        SnapshotScene(GroupProfileView_Previews.self),
        SnapshotScene(GroupWelcomeView_Previews.self),
        SnapshotScene(IncognitoHelp_Previews.self),
        SnapshotScene(IncomingCallView_Previews.self),
        SnapshotScene(IntegrityErrorItemView_Previews.self, allSizes: true),
        SnapshotScene(LargeLinkPreview_Previews.self, allSizes: true),
        SnapshotScene(LocalAuthView_Previews.self),
        SnapshotScene(MarkdownHelp_Previews.self),
        SnapshotScene(MarkedDeletedItemView_Previews.self, allSizes: true),
        SnapshotScene(MemberAdmissionView_Previews.self),
        SnapshotScene(MemberSupportChatToolbar_Previews.self),
        SnapshotScene(MemberSupportView_Previews.self),
        SnapshotScene(MigrateToDevice_Previews.self),
        SnapshotScene(MigrateToGroupView_Previews.self),
        SnapshotScene(MsgContentView_Previews.self, allSizes: true),
        SnapshotScene(NativeTextEditor_Previews.self),
        SnapshotScene(NetworkServersView_Previews.self),
        SnapshotScene(NewChatMenuButton_Previews.self),
        SnapshotScene(NewFeaturesView_Previews.self),
        SnapshotScene(NewServerView_Previews.self),
        SnapshotScene(NotificationsModeView_Previews.self),
        SnapshotScene(NotificationsView_Previews.self),
        SnapshotScene(OnboardingConditionsView_Previews.self),
        SnapshotScene(OnboardingStepsView_Previews.self),
        SnapshotScene(OneHandUICard_Previews.self),
        SnapshotScene(OperatorView_Previews.self),
        SnapshotScene(PasscodeEntry_Previews.self),
        SnapshotScene(PasscodeViewView_Previews.self),
        SnapshotScene(PreferencesView_Previews.self),
        SnapshotScene(PrivacySettings_Previews.self),
        SnapshotScene(ProfileImage_Previews.self),
        SnapshotScene(ProfilePrivacyView_Previews.self),
        SnapshotScene(ProtocolServerView_Previews.self),
        SnapshotScene(QRCode_Previews.self),
        SnapshotScene(RTCServers_Previews.self),
        SnapshotScene(ScanCodeView_Previews.self),
        SnapshotScene(ScanProtocolServer_Previews.self),
        SnapshotScene(SecondaryChatView_Previews.self),
        SnapshotScene(SendMessageView_Previews.self),
        SnapshotScene(SetAppPasscodeView_Previews.self),
        SnapshotScene(SetDeliveryReceiptsView_Previews.self),
        SnapshotScene(SettingsView_Previews.self),
        SnapshotScene(SimpleXInfo_Previews.self),
        SnapshotScene(SmallLinkPreview_Previews.self),
        SnapshotScene(SupportSimpleXBanner_Previews.self),
        SnapshotScene(TerminalAvatars_Previews.self, allSizes: true),
        SnapshotScene(TerminalMedia_Previews.self),
        SnapshotScene(TerminalNames_Previews.self, allSizes: true),
        SnapshotScene(TerminalOverviewChannel_Previews.self),
        SnapshotScene(TerminalOverviewDirect_Previews.self),
        SnapshotScene(TerminalOverviewGroup_Previews.self),
        SnapshotScene(TerminalQuoteThumbnail_Previews.self, allSizes: true),
        SnapshotScene(TerminalRuleOnly_Previews.self, allSizes: true),
        SnapshotScene(UserAddressLearnMore_Previews.self),
        SnapshotScene(UserAddressView_Previews.self),
        SnapshotScene(UserPicker_Previews.self),
        SnapshotScene(VerifyCodeView_Previews.self),
        SnapshotScene(VersionView_Previews.self),
        SnapshotScene(WhySimpleX_Previews.self),
        SnapshotScene(YourServersView_Previews.self)
    ]
}

private func snapshotTheme(_ dark: Bool) -> ThemeManager.ActiveTheme {
    let base: DefaultTheme = dark ? .DARK : .LIGHT
    return ThemeManager.ActiveTheme(
        name: base.themeName,
        base: base,
        colors: (dark ? DarkColorPalette : LightColorPalette).clone(),
        appColors: (dark ? DarkColorPaletteApp : LightColorPaletteApp).clone(),
        wallpaper: AppWallpaper(background: nil, tint: nil, type: .empty)
    )
}

private let fixedQRCode = UIImage(named: "preview-qr-code", in: Bundle(for: SnapshotTests.self), compatibleWith: nil)

@MainActor
private func renderScene(_ scene: SnapshotScene, _ theme: AppTheme, dark: Bool, in windowScene: UIWindowScene) throws -> UIImage {
    let screen = windowScene.screen
    let width = screen.bounds.width
    let root = VStack(spacing: 0) { scene.view }
        .environmentObject(ChatModel.shared)
        .environmentObject(theme)
        .environment(\.fixedQRCode, fixedQRCode)
        .frame(width: width, height: scene.height)
    let host = UIHostingController(rootView: AnyView(root))
    if #available(iOS 16.4, *) {
        host.safeAreaRegions = []
    }
    let window = UIWindow(windowScene: windowScene)
    window.overrideUserInterfaceStyle = dark ? .dark : .light
    window.rootViewController = host
    window.frame = CGRect(x: 0, y: 0, width: width, height: scene.height ?? screen.bounds.height)
    window.isHidden = false
    settle()
    if scene.height == nil {
        for _ in 0..<2 {
            let fit = host.sizeThatFits(in: CGSize(width: width, height: .greatestFiniteMagnitude))
            let fill = host.sizeThatFits(in: screen.bounds.size)
            window.frame = CGRect(x: 0, y: 0, width: width, height: ceil(max(fit.height, fill.height)))
            settle()
        }
    }
    window.endEditing(true)
    removeAnimations(window.layer)
    let format = UIGraphicsImageRendererFormat.default()
    format.scale = 1
    format.preferredRange = .standard
    format.opaque = true
    let image = UIGraphicsImageRenderer(bounds: window.bounds, format: format).image { _ in
        _ = window.drawHierarchy(in: window.bounds, afterScreenUpdates: true)
    }
    window.isHidden = true
    window.rootViewController = nil
    return image
}

private func removeAnimations(_ layer: CALayer) {
    layer.removeAllAnimations()
    layer.sublayers?.forEach(removeAnimations)
}

private func settle() {
    RunLoop.main.run(until: Date().addingTimeInterval(0.6))
}

struct TerminalNames_Previews: PreviewProvider {
    static var previews: some View {
        SnapshotFixtures().names()
    }
}

struct TerminalQuoteThumbnail_Previews: PreviewProvider {
    static var previews: some View {
        SnapshotFixtures().quoteThumbnail()
    }
}

struct TerminalMedia_Previews: PreviewProvider {
    static var previews: some View {
        SnapshotFixtures().media()
    }
}

struct TerminalRuleOnly_Previews: PreviewProvider {
    static var previews: some View {
        SnapshotFixtures().ruleOnly()
    }
}

struct TerminalAvatars_Previews: PreviewProvider {
    static var previews: some View {
        SnapshotFixtures().avatars(terminal: true)
    }
}

struct ClassicAvatars_Previews: PreviewProvider {
    static var previews: some View {
        SnapshotFixtures().avatars(terminal: false)
    }
}

struct TerminalOverviewDirect_Previews: PreviewProvider {
    static var previews: some View {
        SnapshotFixtures().overviewDirect(terminal: true)
    }
}

struct ClassicOverviewDirect_Previews: PreviewProvider {
    static var previews: some View {
        SnapshotFixtures().overviewDirect(terminal: false)
    }
}

struct TerminalOverviewGroup_Previews: PreviewProvider {
    static var previews: some View {
        SnapshotFixtures().overviewGroup(terminal: true)
    }
}

struct ClassicOverviewGroup_Previews: PreviewProvider {
    static var previews: some View {
        SnapshotFixtures().overviewGroup(terminal: false)
    }
}

struct TerminalOverviewChannel_Previews: PreviewProvider {
    static var previews: some View {
        SnapshotFixtures().overviewChannel(terminal: true)
    }
}

struct ClassicOverviewChannel_Previews: PreviewProvider {
    static var previews: some View {
        SnapshotFixtures().overviewChannel(terminal: false)
    }
}

private struct ChatRows: View {
    @EnvironmentObject var theme: AppTheme
    let chat: Chat
    let im: ItemsModel
    let mergedItems: MergedItems
    let terminal: Bool
    var selected: Set<Int64>? = nil

    private var width: CGFloat {
        UIScreen.main.bounds.width
    }

    var body: some View {
        let wallpaperImage = theme.wallpaper.type.image
        let wallpaperType = theme.wallpaper.type
        let backgroundColor = theme.wallpaper.background ?? wallpaperType.defaultBackgroundColor(theme.base, theme.colors.background)
        let tintColor = theme.wallpaper.tint ?? wallpaperType.defaultTintColor(theme.base)
        let items = mergedItems.items
        VStack(spacing: 0) {
            ForEach(Array(items.indices.reversed()), id: \.self) { index in
                row(index, items[index]).fixedSize(horizontal: false, vertical: true)
            }
        }
        .padding(.vertical, 8)
        .frame(width: width)
        .background {
            if terminal {
                backgroundColor
            } else if let wallpaperImage {
                Color.clear.modifier(ChatViewBackground(image: wallpaperImage, imageType: wallpaperType, background: backgroundColor, tint: tintColor))
            } else {
                theme.colors.background
            }
        }
    }

    func row(_ index: Int, _ mergedItem: MergedItem) -> some View {
        let ci = switch mergedItem {
        case let .single(item, _, _): item.item
        case let .grouped(items, _, _, _, _, _, _, _): items.boxedValue.last!.item
        }
        return ChatView.ChatItemWithMenu(
            im: im,
            chat: .constant(ObservedObject(wrappedValue: chat)),
            index: index,
            isLastItem: index == mergedItems.items.count - 1,
            chatItem: ci,
            scrollToItem: { _ in },
            scrollToItemId: .constant(nil),
            merged: mergedItem,
            olderRow: mergedItems.items.indices.contains(index + 1) ? mergedItems.items[index + 1] : nil,
            listWidth: width,
            maxWidth: rowMaxWidth(ci, mergedItem, chat.chatInfo, width),
            composeState: .constant(ComposeState()),
            selectedMember: .constant(nil),
            showChatInfoSheet: .constant(false),
            revealedItems: .constant([]),
            selectedChatItems: .constant(selected),
            forwardedChatItems: .constant([]),
            searchText: .constant(""),
            contentFilter: .constant(nil),
            closeKeyboardAndRun: { $0() }
        )
        .environment(\.terminalLayout, terminal)
    }
}

private func rowMaxWidth(_ ci: ChatItem, _ merged: MergedItem, _ cInfo: ChatInfo, _ width: CGFloat) -> CGFloat {
    let voiceNoFrame = ci.content.msgContent?.isVoice == true && ci.content.text.count == 0 && ci.quotedItem == nil && ci.meta.itemForwarded == nil
    let channelReceived = !ci.chatDir.sent && cInfo.isChannel
    let channelReceivedNoAvatar = channelReceived && !showsAvatar(merged.newest().item, merged.oldest().nextItem)
    let avatarColumn = scaledMemberImageSize(DynamicTypeSize(UIApplication.shared.preferredContentSizeCategory) ?? .large) + 8
    return cInfo.chatType == .group
    ? channelReceivedNoAvatar
    ? width - 26
    : voiceNoFrame || channelReceived
    ? (width - 28) - avatarColumn
    : (width - 28) * 0.84 - avatarColumn
    : voiceNoFrame
    ? (width - 32)
    : (width - 32) * 0.84
}

private func showsAvatar(_ current: ChatItem, _ older: ChatItem?) -> Bool {
    let oldIsGroupRcv = switch older?.chatDir {
    case .groupRcv: true
    case .channelRcv: true
    default: false
    }
    let sameMember = switch (older?.chatDir, current.chatDir) {
    case let (.groupRcv(oldMember), .groupRcv(member)): oldMember.memberId == member.memberId
    case (.channelRcv, .channelRcv): true
    default: false
    }
    return switch current.chatDir {
    case .groupRcv, .channelRcv: older == nil || !oldIsGroupRcv || !sameMember
    default: false
    }
}

private func imageBase64(_ color: UIColor, _ size: CGSize) -> String {
    let format = UIGraphicsImageRendererFormat.default()
    format.scale = 1
    let image = UIGraphicsImageRenderer(size: size, format: format).image { ctx in
        color.setFill()
        ctx.fill(CGRect(origin: .zero, size: size))
        UIColor.white.withAlphaComponent(0.4).setFill()
        ctx.cgContext.fillEllipse(in: CGRect(x: size.width * 0.55, y: size.height * 0.15, width: size.height * 0.4, height: size.height * 0.4))
    }
    return "data:image/jpg;base64," + (image.jpegData(compressionQuality: 0.8)?.base64EncodedString() ?? "")
}

private final class SnapshotFixtures {
    private var nextId: Int64 = 1
    let photo = imageBase64(UIColor(red: 0.2, green: 0.4, blue: 0.8, alpha: 1), CGSize(width: 120, height: 80))
    let landscape = imageBase64(UIColor(red: 0.25, green: 0.55, blue: 0.35, alpha: 1), CGSize(width: 320, height: 200))
    let alice: GroupMember
    let bob: GroupMember
    let carol: GroupMember
    let team: GroupInfo
    let news: GroupInfo
    let contact: Contact
    private let markdown: [String: ParsedMarkdown] = [
        "https://simplex.chat": ParsedMarkdown(formattedText: [
            FormattedText(text: "https://simplex.chat", format: .uri)
        ]),
        "---": ParsedMarkdown(
            formattedText: [
                FormattedText(text: "---", format: .markup(token: .ruleMark))
            ],
            markdownBlocks: [
                .rule(mark: SegmentRange(from: 0, to: 1))
            ]
        ),
        "Hi! Did you see the **new release**?": ParsedMarkdown(formattedText: [
            FormattedText(text: "Hi! Did you see the "),
            FormattedText(text: "new release", format: .bold(singleStar: false)),
            FormattedText(text: "?")
        ]),
        "Yes, *great* changes 🎉": ParsedMarkdown(formattedText: [
            FormattedText(text: "Yes, "),
            FormattedText(text: "great", format: .bold(singleStar: true)),
            FormattedText(text: " changes 🎉")
        ]),
        "# Release notes\n- terminal layout\n- `code` blocks\n\n> quoted line": ParsedMarkdown(
            formattedText: [
                FormattedText(text: "# ", format: .markup(token: .headingMark(level: 1))),
                FormattedText(text: "Release notes"),
                FormattedText(text: "\n", format: .markup(token: .lineBreak)),
                FormattedText(text: "- ", format: .markup(token: .listMark(number: nil))),
                FormattedText(text: "terminal layout"),
                FormattedText(text: "\n", format: .markup(token: .lineBreak)),
                FormattedText(text: "- ", format: .markup(token: .listMark(number: nil))),
                FormattedText(text: "code", format: .snippet),
                FormattedText(text: " blocks"),
                FormattedText(text: "\n\n", format: .markup(token: .paragraphBreak(lineBreaks: 2))),
                FormattedText(text: "> ", format: .markup(token: .quoteMark)),
                FormattedText(text: "quoted line")
            ],
            markdownBlocks: [
                .heading(level: 1, content: SegmentRange(from: 1, to: 2)),
                .list(items: [
                    ListEntry(number: nil, content: SegmentRange(from: 4, to: 5), items: []),
                    ListEntry(number: nil, content: SegmentRange(from: 7, to: 9), items: [])
                ]),
                .quote(content: SegmentRange(from: 11, to: 12))
            ]
        ),
        "Plan for today:\n1. review\n2. **release**": ParsedMarkdown(
            formattedText: [
                FormattedText(text: "Plan for today:"),
                FormattedText(text: "\n", format: .markup(token: .lineBreak)),
                FormattedText(text: "1. ", format: .markup(token: .listMark(number: "1."))),
                FormattedText(text: "review"),
                FormattedText(text: "\n", format: .markup(token: .lineBreak)),
                FormattedText(text: "2. ", format: .markup(token: .listMark(number: "2."))),
                FormattedText(text: "release", format: .bold(singleStar: false))
            ],
            markdownBlocks: [
                .paragraph(content: SegmentRange(from: 0, to: 1)),
                .list(items: [
                    ListEntry(number: "1.", content: SegmentRange(from: 3, to: 4), items: []),
                    ListEntry(number: "2.", content: SegmentRange(from: 6, to: 7), items: [])
                ])
            ]
        ),
        "**Version 7.1** is out": ParsedMarkdown(formattedText: [
            FormattedText(text: "Version 7.1", format: .bold(singleStar: false)),
            FormattedText(text: " is out")
        ]),
        "## What's new\n- terminal layout\n- faster sync": ParsedMarkdown(
            formattedText: [
                FormattedText(text: "## ", format: .markup(token: .headingMark(level: 2))),
                FormattedText(text: "What's new"),
                FormattedText(text: "\n", format: .markup(token: .lineBreak)),
                FormattedText(text: "- ", format: .markup(token: .listMark(number: nil))),
                FormattedText(text: "terminal layout"),
                FormattedText(text: "\n", format: .markup(token: .lineBreak)),
                FormattedText(text: "- ", format: .markup(token: .listMark(number: nil))),
                FormattedText(text: "faster sync")
            ],
            markdownBlocks: [
                .heading(level: 2, content: SegmentRange(from: 1, to: 2)),
                .list(items: [
                    ListEntry(number: nil, content: SegmentRange(from: 4, to: 5), items: []),
                    ListEntry(number: nil, content: SegmentRange(from: 7, to: 8), items: [])
                ])
            ]
        )
    ]

    init() {
        alice = Self.member(2, "alice", .admin, image: imageBase64(UIColor(red: 0.85, green: 0.45, blue: 0.3, alpha: 1), CGSize(width: 64, height: 64)))
        bob = Self.member(3, "bob", .member)
        carol = Self.member(4, "carol", .member, image: imageBase64(UIColor(red: 0.55, green: 0.35, blue: 0.75, alpha: 1), CGSize(width: 64, height: 64)))
        team = Self.group(1, "team", channel: false)
        news = Self.group(2, "SimpleX news", channel: true)
        var c = Contact.sampleData
        c.profile = LocalProfile(profileId: 2, displayName: "bob", fullName: "", localAlias: "")
        c.localDisplayName = "bob"
        contact = c
    }

    private static func member(_ id: Int64, _ name: String, _ role: GroupMemberRole, image: String? = nil) -> GroupMember {
        var m = GroupMember.sampleData
        m.groupMemberId = id
        m.memberId = "member-\(id)"
        m.memberRole = role
        m.localDisplayName = name
        m.memberProfile = LocalProfile(profileId: id, displayName: name, fullName: "", image: image, localAlias: "")
        return m
    }

    private static func group(_ id: Int64, _ name: String, channel: Bool) -> GroupInfo {
        var g = GroupInfo.sampleData
        g.groupId = id
        g.localDisplayName = name
        g.groupProfile = GroupProfile(displayName: name, fullName: "")
        if channel {
            g.useRelays = true
            g.groupProfile.publicGroup = PublicGroupProfile(groupType: .channel, groupLink: "", publicGroupId: "channel-\(id)")
        }
        g.membership = member(99, "me", .member)
        return g
    }

    private func id() -> Int64 {
        nextId += 1
        return nextId
    }

    private func ts(_ minutes: Double) -> Date {
        previewDate.addingTimeInterval(minutes * 60)
    }

    private func msg(_ dir: CIDirection, _ text: String, _ minutes: Double, quote: CIQuote? = nil, reactions: [CIReactionCount] = [], content: MsgContent? = nil, file: CIFile? = nil) -> ChatItem {
        let parsed = markdown[text]
        let mc = content ?? .text(text)
        return ChatItem(
            chatDir: dir,
            meta: CIMeta.getSample(id(), ts(minutes), text, dir.sent ? .sndSent(sndProgress: .complete) : .rcvRead),
            content: dir.sent ? .sndMsgContent(msgContent: mc) : .rcvMsgContent(msgContent: mc),
            formattedText: parsed?.formattedText,
            markdownBlocks: parsed?.markdownBlocks,
            quotedItem: quote,
            reactions: reactions,
            file: file
        )
    }

    private func event(_ dir: CIDirection, _ content: CIContent, _ minutes: Double) -> ChatItem {
        ChatItem(chatDir: dir, meta: CIMeta.getSample(id(), ts(minutes), content.text, .rcvRead), content: content)
    }

    private func quote(_ dir: CIDirection?, _ text: String, image: String? = nil) -> CIQuote {
        let parsed = markdown[text]
        return CIQuote(
            chatDir: dir,
            itemId: 1,
            sentAt: ts(-300),
            content: image.map { .image(text: text, image: $0) } ?? .text(text),
            formattedText: parsed?.formattedText,
            markdownBlocks: parsed?.markdownBlocks
        )
    }

    private func reaction(_ emoji: MREmojiChar, _ count: Int, mine: Bool = false) -> CIReactionCount {
        CIReactionCount(reaction: .emoji(emoji: emoji), userReacted: mine, totalReacted: count)
    }

    private func imageFile(_ name: String) -> CIFile {
        CIFile.getSample(fileId: id(), fileName: name, fileSize: 184_320, filePath: nil, fileStatus: .rcvInvitation)
    }

    private func chatRows(_ chatInfo: ChatInfo, _ items: [ChatItem], terminal: Bool, selected: Set<Int64>? = nil) -> ChatRows {
        let im = ItemsModel(secondaryIMFilter: nil)
        im.reversedChatItems = items.reversed()
        let merged = MergedItems.create(im, [])
        return ChatRows(chat: Chat(chatInfo: chatInfo), im: im, mergedItems: merged, terminal: terminal, selected: selected)
    }

    private var direct: ChatInfo { .direct(contact: contact) }
    private var teamChat: ChatInfo { .group(groupInfo: team, groupChatScope: nil) }
    private var newsChat: ChatInfo { .group(groupInfo: news, groupChatScope: nil) }

    func names() -> some View {
        chatRows(teamChat, [
            msg(.groupRcv(groupMember: alice), "Hi everyone, the build is green.", -30),
            msg(.groupRcv(groupMember: alice), "Release notes are in the channel.", -29),
            msg(.groupRcv(groupMember: bob), "Great, thanks!", -20, quote: quote(.groupRcv(groupMember: alice), "Hi everyone, the build is green.")),
            msg(.groupSnd, "I will check the release notes today.", -10)
        ], terminal: true)
    }

    func quoteThumbnail() -> some View {
        chatRows(direct, [
            msg(.directRcv, "nice", -10, quote: quote(.directSnd, "photo", image: photo)),
            msg(.directRcv, "a longer reply that wraps over two lines in the terminal row below the quote with the image", -9, quote: quote(.directSnd, "a long caption of the quoted image that wraps over more than one line of the quote", image: photo)),
            msg(.directRcv, "ok", -8, quote: quote(.directRcv, "", image: photo)),
            msg(.directRcv, "a reply wider than the quote above it, so that the quote is narrower than the item", -7, quote: quote(.directSnd, "photo", image: photo)),
            msg(.directRcv, "a reply a bit wider than its quote", -6, quote: quote(.directSnd, "photo", image: photo))
        ], terminal: true)
    }

    func media() -> some View {
        let link = MsgContent.link(text: "https://simplex.chat", preview: LinkPreview(uri: "https://simplex.chat", title: "SimpleX Chat: private and secure messenger without any user IDs", image: landscape))
        return VStack(spacing: 0) {
            chatRows(direct, [
                msg(.directRcv, "https://simplex.chat", -20, content: link)
            ], terminal: true)
            chatRows(newsChat, [
                msg(.channelRcv, "Links and videos", -12),
                msg(.channelRcv, "https://simplex.chat", -11, content: link),
                msg(.channelRcv, "", -10, content: .video(text: "", image: landscape, duration: 42), file: CIFile.getSample(fileId: id(), fileName: "video.mp4", fileSize: 1_048_576, filePath: nil, fileStatus: .rcvInvitation))
            ], terminal: true)
            chatRows(newsChat, [
                msg(.channelRcv, "Selected post", -5, content: .image(text: "Selected post", image: landscape), file: imageFile("post.jpg"))
            ], terminal: true, selected: [])
        }
    }

    func ruleOnly() -> some View {
        chatRows(direct, [
            msg(.directRcv, "---", -10),
            msg(.directRcv, "---", -9, quote: quote(.directSnd, "a quoted message that is much wider than the rule"))
        ], terminal: true)
    }

    func avatars(terminal: Bool) -> some View {
        chatRows(teamChat, [
            msg(.groupRcv(groupMember: alice), "Morning! Who is joining the call?", -40),
            msg(.groupRcv(groupMember: bob), "I am", -38),
            msg(.groupRcv(groupMember: bob), "Starting in 5 minutes", -37),
            msg(.groupRcv(groupMember: carol), "Count me in, a bit late", -35),
            msg(.groupSnd, "Great", -30)
        ], terminal: terminal)
    }

    func overviewDirect(terminal: Bool) -> ChatRows {
        chatRows(direct, [
            event(.directRcv, .rcvChatFeature(feature: .voice, enabled: FeatureEnabled(forUser: true, forContact: true), param: nil), -120),
            msg(.directRcv, "Hi! Did you see the **new release**?", -60),
            msg(.directSnd, "Yes, *great* changes 🎉", -58, reactions: [reaction(.thumbsup, 1)]),
            msg(.directRcv, "# Release notes\n- terminal layout\n- `code` blocks\n\n> quoted line", -50),
            msg(.directSnd, "Thanks for the summary", -45, quote: quote(.directRcv, "Release notes")),
            msg(.directRcv, "", -40, content: .image(text: "", image: landscape), file: imageFile("photo.jpg")),
            msg(.directRcv, "", -35, content: .file(""), file: CIFile.getSample(fileId: id(), fileName: "report.pdf", fileSize: 245_760, filePath: nil, fileStatus: .rcvInvitation)),
            msg(.directSnd, "", -30, content: .voice(text: "", duration: 12), file: CIFile.getSample(fileId: id(), fileName: "voice.m4a", fileSize: 20_480, filePath: nil, fileStatus: .sndComplete))
        ], terminal: terminal)
    }

    func overviewGroup(terminal: Bool) -> ChatRows {
        chatRows(teamChat, [
            event(.groupRcv(groupMember: carol), .rcvGroupEvent(rcvGroupEvent: .memberConnected), -130),
            msg(.groupRcv(groupMember: alice), "Welcome, carol!", -120, reactions: [reaction(.heart, 2, mine: true)]),
            msg(.groupRcv(groupMember: carol), "Thanks! Glad to be here", -118),
            msg(.groupRcv(groupMember: bob), "", -100, content: .image(text: "", image: landscape), file: imageFile("view.jpg")),
            msg(.groupSnd, "nice view", -95, quote: quote(.groupRcv(groupMember: bob), "", image: landscape)),
            msg(.groupRcv(groupMember: alice), "Plan for today:\n1. review\n2. **release**", -90),
            msg(.groupRcv(groupMember: bob), "", -80, content: .voice(text: "", duration: 21), file: CIFile.getSample(fileId: id(), fileName: "voice.m4a", fileSize: 30_720, filePath: nil, fileStatus: .rcvInvitation)),
            msg(.groupRcv(groupMember: carol), "", -70, content: .file(""), file: CIFile.getSample(fileId: id(), fileName: "notes.txt", fileSize: 2_048, filePath: nil, fileStatus: .rcvInvitation))
        ], terminal: terminal)
    }

    func overviewChannel(terminal: Bool) -> ChatRows {
        chatRows(newsChat, [
            event(.channelRcv, .rcvGroupEvent(rcvGroupEvent: .groupUpdated(groupProfile: news.groupProfile)), -210),
            msg(.channelRcv, "**Version 7.1** is out", -200, reactions: [reaction(.thumbsup, 12), reaction(.heart, 5, mine: true)]),
            msg(.channelRcv, "## What's new\n- terminal layout\n- faster sync", -190),
            msg(.channelRcv, "Screenshot of the new layout", -180, content: .image(text: "Screenshot of the new layout", image: landscape), file: imageFile("screenshot.jpg"))
        ], terminal: terminal)
    }
}
