//
//  NameBadge.swift
//  SimpleX
//
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// The badge is sized to a fraction of the font size (em), NOT the font's cap-height metric: the metric
// underestimates the rendered capital letters, so a cap-height-tall badge looks too small. These ratios
// are calibrated visually to match caps - the same constants as the Compose (Android/desktop) app.
private let fontCapHeightRatio: CGFloat = 0.85
// fraction of the badge height pushed below the text baseline (like the undershoot of round letters)
private let badgeBaselineOffsetRatio: CGFloat = 0.05
// the gap between the name and the badge (matching the visible gap to the verification shield)
private let badgeGap: CGFloat = 8

// A contact/member name with the supporter badge right after it. The name keeps its own styling
// (font, weight, color, even a verification shield concatenated into the Text); the badge is sized to
// the given text style and sits on the name's baseline. Use this everywhere a name may carry a badge.
// Pass onTap to make the badge open the info alert. The badge hides itself for a nil/long-expired badge.
struct NameWithBadge: View {
    let name: Text
    var badge: LocalBadge?
    var textStyle: Font.TextStyle = .body
    var onTap: (() -> Void)? = nil

    init(_ name: Text, _ badge: LocalBadge?, _ textStyle: Font.TextStyle = .body, onTap: (() -> Void)? = nil) {
        self.name = name
        self.badge = badge
        self.textStyle = textStyle
        self.onTap = onTap
    }

    var body: some View {
        HStack(alignment: .firstTextBaseline, spacing: 0) {
            name
            NameBadge(badge, textStyle, onTap: onTap)
        }
    }
}

// The badge glyph alone, sized to the given text style and sitting on the text baseline in an
// HStack(alignment: .firstTextBaseline). Renders nothing for a nil badge or a long-expired one
// (ExpiredOld); a failed or unknown-key badge shows a warning glyph. Prefer NameWithBadge; use this
// directly only where the name is not a single Text. Pass onTap to open the badge info alert.
struct NameBadge: View {
    var badge: LocalBadge?
    var textStyle: Font.TextStyle = .body
    var onTap: (() -> Void)? = nil

    init(_ badge: LocalBadge?, _ textStyle: Font.TextStyle = .body, onTap: (() -> Void)? = nil) {
        self.badge = badge
        self.textStyle = textStyle
        self.onTap = onTap
    }

    var body: some View {
        if let badge, badge.status != .expiredOld {
            // the leading padding is the gap to the name; it lives here so an absent badge adds no gap.
            // the alignment guide pushes the badge bottom slightly below the baseline (round-letter undershoot)
            let v = glyph(badge)
                .frame(height: badgeHeight)
                .alignmentGuide(.firstTextBaseline) { $0.height * (1 - badgeBaselineOffsetRatio) }
                .padding(.leading, badgeGap)
            if let onTap {
                v.onTapGesture(perform: onTap)
            } else {
                v
            }
        }
    }

    private var badgeHeight: CGFloat {
        UIFont.preferredFont(forTextStyle: uiTextStyle(textStyle)).pointSize * fontCapHeightRatio
    }

    @ViewBuilder private func glyph(_ badge: LocalBadge) -> some View {
        switch badge.status {
        case .failed, .unknownKey:
            Image(systemName: "exclamationmark.triangle.fill")
                .resizable().scaledToFit()
                .foregroundColor(.orange)
        default:
            Image(badgeImageName(badge.badge.badgeType))
                .resizable().scaledToFit()
                .opacity(badge.status == .expired ? 0.4 : 1)
        }
    }
}

// the badge shown for a chat: an active contact's or a contact request's (groups have none)
extension ChatInfo {
    var nameBadge: LocalBadge? {
        switch self {
        case let .direct(contact): contact.active ? contact.profile.localBadge : nil
        case let .contactRequest(contactRequest): contactRequest.profile.localBadge
        default: nil
        }
    }
}

extension GroupMember {
    var nameBadge: LocalBadge? { memberProfile.localBadge }
}

private func badgeImageName(_ t: BadgeType) -> String {
    switch t {
    case .legend: "badge-legend"
    case .investor: "badge-investor"
    default: "badge-supporter" // supporter + unknown
    }
}

// The badge as an inline attachment for a UIKit label, for the custom alert where the name is a UILabel
// and the SwiftUI NameBadge can't be used. Sized to the font's cap height with its bottom on the baseline,
// preceded by a space for the gap to the name. Returns nil for a nil/long-expired badge. Mirrors NameBadge's glyph.
func nameBadgeAttachment(_ badge: LocalBadge?, font: UIFont) -> NSAttributedString? {
    guard let badge, badge.status != .expiredOld else { return nil }
    var image: UIImage?
    switch badge.status {
    case .failed, .unknownKey:
        image = UIImage(systemName: "exclamationmark.triangle.fill")?
            .withTintColor(.systemOrange, renderingMode: .alwaysOriginal)
    default:
        image = UIImage(named: badgeImageName(badge.badge.badgeType))
        if badge.status == .expired, let img = image {
            // a recently expired badge is dimmed, matching NameBadge's 0.4 opacity
            image = UIGraphicsImageRenderer(size: img.size).image { _ in
                img.draw(at: .zero, blendMode: .normal, alpha: 0.4)
            }
        }
    }
    guard let image else { return nil }
    let attachment = NSTextAttachment()
    attachment.image = image
    let h = font.pointSize * fontCapHeightRatio
    // text coordinates: a negative y drops the image below the baseline by badgeBaselineOffsetRatio of its height
    attachment.bounds = CGRect(x: 0, y: -h * badgeBaselineOffsetRatio, width: h * image.size.width / image.size.height, height: h)
    let s = NSMutableAttributedString(string: " ") // the gap to the name
    s.append(NSAttributedString(attachment: attachment))
    return s
}

// SwiftUI Font.TextStyle -> UIFont.TextStyle, to read the resolved (Dynamic-Type-aware) cap height
private func uiTextStyle(_ s: Font.TextStyle) -> UIFont.TextStyle {
    switch s {
    case .largeTitle: .largeTitle
    case .title: .title1
    case .title2: .title2
    case .title3: .title3
    case .headline: .headline
    case .subheadline: .subheadline
    case .body: .body
    case .callout: .callout
    case .footnote: .footnote
    case .caption: .caption1
    case .caption2: .caption2
    @unknown default: .body
    }
}

func showBadgeInfoAlert(_ name: String, _ badge: LocalBadge) {
    switch badge.status {
    case .failed:
        showAlert(
            NSLocalizedString("Unverified badge", comment: "badge alert title"),
            message: NSLocalizedString("This badge could not be verified and may not be genuine.", comment: "badge alert")
        )
    case .unknownKey:
        showAlert(
            NSLocalizedString("Badge cannot be verified", comment: "badge alert title"),
            message: NSLocalizedString("The badge is signed with a key that this version of the app does not recognize. Update the app to verify this badge.", comment: "badge alert")
        )
    default:
        // a verified badge's type is signed and can't be faked, so the real (possibly unknown) type name is the title
        let t = badge.badge.badgeType.text
        let title = t.prefix(1).uppercased() + t.dropFirst()
        if case .investor = badge.badge.badgeType {
            let message = String.localizedStringWithFormat(NSLocalizedString("%@ invested in SimpleX Chat crowdfunding.", comment: "badge alert"), name)
            showAlert(title, message: message) {
                [ UIAlertAction(title: NSLocalizedString("Learn more", comment: "badge alert button"), style: .default) { _ in
                    if let url = URL(string: "https://simplex.chat/crowdfunding") {
                        UIApplication.shared.open(url)
                    }
                  },
                  okAlertAction ]
            }
        } else {
            // supporter, legend and unknown types use the supporter wording
            let supports =
                if badge.status == .expired, let expiry = badge.badge.badgeExpiry {
                    String.localizedStringWithFormat(NSLocalizedString("%1$@ supported SimpleX Chat. The badge expired on %2$@.", comment: "badge alert"), name, expiry.formatted(date: .abbreviated, time: .omitted))
                } else {
                    String.localizedStringWithFormat(NSLocalizedString("%@ supports SimpleX Chat.", comment: "badge alert"), name)
                }
            let v7 = NSLocalizedString("You can support SimpleX starting from v7 of the app.", comment: "badge alert")
            showAlert(title, message: supports + "\n\n" + v7)
        }
    }
}
