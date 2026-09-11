//
//  BadgesRedeemCodeView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.07.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

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
    @State private var failure: BadgeRedeemError? = nil

    var body: some View {
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

        if let failure {
            Text(failureMessage(failure))
                .font(.callout)
                .foregroundColor(.red)
                .multilineTextAlignment(.center)
                .fixedSize(horizontal: false, vertical: true)
        }

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

    private func applyCodeInput(_ s: String) {
        let formatted = formatBadgeCodeInput(s)
        if formatted != code { code = formatted }
        canonicalCode = parseBadgeCode(formatted)
        failure = nil
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
        failure = nil
        Task {
            do {
                let (redeemedUser, newBadge) = try await apiRedeemBadgeCode(user.userId, sending)
                let badgeState = try? await apiGetBadgeState(user.userId)
                await MainActor.run {
                    submitting = false
                    // written before the pop: BadgesView swaps its content under this pushed view, so
                    // the pop reveals Your Badge already in place rather than animating it afterwards
                    if let badgeState {
                        BadgeModel.shared.set(userId: user.userId, badgeState: badgeState)
                    }
                    // the response is the only carrier: redeeming raises no event that refreshes the
                    // profile, so without this the badge beside the name is the one from before
                    chatModel.updateUser(redeemedUser)
                    if let badgeState, !badgeState.shown {
                        // a replay adds no purchase; a fresh code's badge can be retired on arrival
                        failure = newBadge ? .badgeEnded : .codeUsed
                    } else {
                        supporterBannerShown = true
                        dismiss()
                    }
                }
            } catch let error {
                let redeemError = error as? BadgeRedeemError ?? .unknown
                // the mapped case only - core embeds the service's response in some of these messages
                logger.error("apiRedeemBadgeCode: \(String(describing: redeemError))")
                await MainActor.run {
                    submitting = false
                    failure = redeemError
                }
            }
        }
    }

    private func failureMessage(_ failure: BadgeRedeemError) -> LocalizedStringKey {
        switch failure {
        case .invalidCode: "This code is not valid."
        case .serviceNotConfigured: "This app version cannot redeem badge codes."
        case .alreadyActive: "This profile already has a badge. Redeem the code on another profile, or once this badge ends."
        case .codeInvalid: "This code was not recognised."
        case .codeUsed: "This code has already been used."
        case .codeExpired: "This code has expired."
        case .rateLimited: "Too many attempts. Please try again later."
        case .serviceFailed: "The badge service is unavailable. Please try again later."
        case .badServiceResponse: "The badge service sent an unexpected response."
        case .credentialNotVerified: "This app version cannot verify this badge. Please update the app."
        case .unsupportedVersion: "This app version is too old for the badge service. Please update the app."
        case .networkError: "Connection error. Please check your network connection."
        case .badgeEnded: "The code was accepted, but the badge it grants has already ended."
        case .unknown: "The code could not be redeemed."
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
