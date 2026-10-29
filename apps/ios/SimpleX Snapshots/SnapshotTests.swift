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

@MainActor
private func renderScene(_ scene: SnapshotScene, _ theme: AppTheme, dark: Bool, in windowScene: UIWindowScene) throws -> UIImage {
    let screen = windowScene.screen
    let width = screen.bounds.width
    let root = VStack(spacing: 0) { scene.view }
        .environmentObject(ChatModel.shared)
        .environmentObject(theme)
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
