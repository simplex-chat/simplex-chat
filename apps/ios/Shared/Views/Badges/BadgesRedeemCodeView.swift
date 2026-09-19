//
//  BadgesRedeemCodeView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.07.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import CodeScanner

private let badgeCodePrefix = "SB"
private let badgeCodeBodyLength = 20
private let badgeCodeGroupLength = 5

// Regroups what was typed; validity and the folding of ambiguous characters are core's alone.
private func formatBadgeCodeInput(_ s: String) -> String {
    var normalized = ""
    for c in s.prefix(256).uppercased() {
        guard c.isLetter || c.isNumber else { continue }
        normalized.append(c)
        if normalized.count == badgeCodePrefix.count + badgeCodeBodyLength { break }
    }
    guard normalized.hasPrefix(badgeCodePrefix) else { return normalized }
    var groups = [badgeCodePrefix]
    var i = normalized.index(normalized.startIndex, offsetBy: badgeCodePrefix.count)
    while i < normalized.endIndex {
        let j = normalized.index(i, offsetBy: badgeCodeGroupLength, limitedBy: normalized.endIndex) ?? normalized.endIndex
        groups.append(String(normalized[i..<j]))
        i = j
    }
    return groups.joined(separator: "-")
}

struct BadgesRedeemCodeView: View {
    @EnvironmentObject var theme: AppTheme
    @EnvironmentObject var chatModel: ChatModel
    @AppStorage(DEFAULT_SUPPORTER_BANNER_SHOWN) private var supporterBannerShown = false
    @Environment(\.dismiss) var dismiss: DismissAction
    @State private var code = ""
    @State private var canonicalCode: String? = nil
    @State private var submitting = false
    @State private var showQRCodeScanner = true

    var body: some View {
        ZStack {
            GeometryReader { g in
                ScrollView {
                    VStack(alignment: .center, spacing: 16) {
                        entryContent(g)
                    }
                    .padding(.horizontal, 25)
                    .padding(.top, 8)
                    .padding(.bottom, 20)
                    .frame(minHeight: g.size.height)
                }
            }

            if submitting {
                ZStack {
                    Circle()
                        .fill(.white)
                        .opacity(0.7)
                        .frame(width: 56, height: 56)
                    ProgressView().scaleEffect(2)
                }
            }
        }
        .frame(maxHeight: .infinity)
        .navigationBarTitleDisplayMode(.inline)
    }

    @ViewBuilder
    private func entryContent(_ g: GeometryProxy) -> some View {
        Text("Redeem code")
            .font(.largeTitle)
            .bold()
            .foregroundColor(theme.colors.primary)
            .multilineTextAlignment(.center)
            .fixedSize(horizontal: false, vertical: true)

        Text("Paste the code from your receipt.")
            .font(.body)
            .multilineTextAlignment(.center)
            .fixedSize(horizontal: false, vertical: true)

        codeField()

        pasteButton()

        ScannerInView(
            showQRCodeScanner: $showQRCodeScanner,
            scannerPaused: $submitting,
            processQRCode: processQRCode,
            scanMode: .oncePerCode,
            placeholderBackground: Color(.tertiarySystemFill)
        )
        .fixedSize(horizontal: false, vertical: true)
        .padding(.top, 12)

        Spacer(minLength: 0)

        VStack(spacing: 10) {
            submitButton()
                .padding(.vertical, 10)
            Color.clear
                .frame(height: 22)
        }
        .padding(.bottom, g.safeAreaInsets.bottom == 0 ? 20 : 0)
    }

    private func codeField() -> some View {
        TextField(text: $code) { Text(verbatim: "SB-XXXXX-XXXXX-XXXXX-XXXXX") }
            .font(.body.monospaced())
            .multilineTextAlignment(.center)
            .autocorrectionDisabled(true)
            .textInputAutocapitalization(.characters)
            .disabled(submitting)
            .padding(EdgeInsets(top: 14, leading: 12, bottom: 14, trailing: 12))
            .background(Color(.tertiarySystemFill))
            .cornerRadius(10.0)
            .onChange(of: code) { applyCodeInput($0) }
    }

    private func pasteButton() -> some View {
        Button {
            if let pasted = UIPasteboard.general.string { applyCodeInput(pasted) }
        } label: {
            Text("Paste")
                .font(.body)
                .fontWeight(.medium)
                .foregroundColor(theme.colors.primary)
        }
        .disabled(submitting)
    }

    // a changed field re-enters through onChange with the formatted text, which is when it is parsed
    private func applyCodeInput(_ s: String) {
        let formatted = formatBadgeCodeInput(s)
        if formatted != code {
            code = formatted
        } else {
            canonicalCode = parseBadgeCode(formatted)
        }
    }

    private func processQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r):
            let formatted = formatBadgeCodeInput(r.string)
            if let canonical = parseBadgeCode(formatted) {
                // set here rather than through onChange, which runs after redeem() reads it
                code = formatted
                canonicalCode = canonical
                redeem()
            } else {
                showAlert(
                    NSLocalizedString("Invalid QR code", comment: "alert title"),
                    message: NSLocalizedString("The code you scanned is not a badge code.", comment: "alert message")
                )
            }
        case let .failure(e):
            logger.error("processQRCode QR code error: \(e.localizedDescription)")
            showAlert(
                NSLocalizedString("Invalid QR code", comment: "alert title"),
                message: String.localizedStringWithFormat(NSLocalizedString("Error scanning code: %@", comment: "alert message"), e.localizedDescription)
            )
        }
    }

    private func submitButton() -> some View {
        let disabled = canonicalCode == nil || submitting
        return Button {
            redeem()
        } label: {
            Text("Redeem")
        }
        .buttonStyle(OnboardingButtonStyle(isDisabled: disabled))
        .disabled(disabled)
    }

    private func redeem() {
        guard let sending = canonicalCode, let user = chatModel.currentUser else { return }
        submitting = true
        Task {
            do {
                guard let redeemed = try await apiRedeemBadgeCode(user.userId, sending) else {
                    await MainActor.run { submitting = false }
                    return
                }
                await MainActor.run {
                    submitting = false
                    // set before dismissing: BadgesView then switches Support to Your Badge while this screen
                    // still covers it, so the pop lands on Your Badge instead of showing the switch
                    BadgeModel.shared.set(userId: user.userId, badgeState: redeemed.badgeState)
                    chatModel.updateUser(redeemed.user)
                    if let badgeState = redeemed.badgeState, !badgeState.shown {
                        // a replay adds no purchase; a fresh code's badge can be retired on arrival
                        let message = redeemed.newBadge
                            ? NSLocalizedString("The code was accepted, but the badge it grants has already ended.", comment: "alert message")
                            : NSLocalizedString("This code has already been used.", comment: "alert message")
                        showAlert(NSLocalizedString("Cannot redeem code", comment: "alert title"), message: message)
                    } else {
                        supporterBannerShown = true
                        dismiss()
                    }
                }
            } catch let error {
                logger.error("apiRedeemBadgeCode: \(responseError(error))")
                await MainActor.run {
                    submitting = false
                    showAlert(NSLocalizedString("Cannot redeem code", comment: "alert title"), message: redeemErrorText(error))
                }
            }
        }
    }
}

struct BadgesRedeemCodeView_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            BadgesRedeemCodeView()
        }
    }
}
