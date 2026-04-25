//
//  NetworkCommitments.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 22/04/2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct NetworkCommitmentsView: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @State private var showConditionsSheet = false
    var selectedOperatorIds: Set<Int64>

    var body: some View {
        GeometryReader { g in
            VStack(alignment: .leading, spacing: 10) {
                Spacer(minLength: 0)

                heroImage().frame(maxWidth: .infinity, minHeight: 80)

                Text("Network\ncommitments")
                    .font(.largeTitle)
                    .bold()
                    .multilineTextAlignment(.center)
                    .frame(maxWidth: .infinity, alignment: .center)
                    .fixedSize(horizontal: false, vertical: true)

                Text("Operators commit to:\n- Be independent\n- Minimize metadata usage\n- Run verified open-source code")
                    .font(.callout)
                    .lineSpacing(2)
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .padding(.leading, 4)
                    .padding(.top, 10)
                    .fixedSize(horizontal: false, vertical: true)

                Text("You commit to:\n- Only legal content in public groups\n- Respect other users - no spam")
                    .font(.callout)
                    .lineSpacing(2)
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .padding(.leading, 4)
                    .padding(.top, 10)
                    .fixedSize(horizontal: false, vertical: true)

                Button {
                    showConditionsSheet = true
                } label: {
                    Text("Privacy policy and conditions of use.")
                        .fontWeight(.medium)
                        .fixedSize(horizontal: false, vertical: true)
                }
                .frame(maxWidth: .infinity, alignment: .leading)
                .padding(.leading, 4)
                .padding(.top, 10)
                .padding(.bottom, 15)

                Spacer(minLength: 0)

                acceptButton()
                    .padding(.bottom, g.safeAreaInsets.bottom == 0 ? 20 : 0)
            }
            .padding(.horizontal, 25)
            .padding(.top, 25)
            .padding(.bottom, 25)
            .frame(minHeight: g.size.height)
        }
        .frame(maxHeight: .infinity)
        .navigationBarHidden(true)
        .sheet(isPresented: $showConditionsSheet) {
            NavigationView {
                ConditionsTextView()
                    .padding()
                    .navigationTitle("Conditions of use")
                    .navigationBarTitleDisplayMode(.large)
                    .toolbar { ToolbarItem(placement: .navigationBarTrailing, content: conditionsLinkButton) }
                    .modifier(ThemedBackground(grouped: true))
            }
        }
    }

    @ViewBuilder
    private func heroImage() -> some View {
        #if SIMPLEX_ASSETS
        Image(colorScheme == .light ? "network-commitments" : "network-commitments-light")
            .resizable()
            .scaledToFit()
        #else
        ZStack {
            let gp = OnboardingCardView.gradientPoints(aspectRatio: 1.5, scale: colorScheme == .light ? 1.2 : 1.5)
            LinearGradient(
                stops: colorScheme == .light ? OnboardingCardView.lightStops : OnboardingCardView.darkStops,
                startPoint: gp.start,
                endPoint: gp.end
            )
            Image(systemName: "checkmark.shield")
                .font(.system(size: 72))
                .foregroundColor(theme.colors.primary)
        }
        .aspectRatio(1.5, contentMode: .fit)
        .clipShape(RoundedRectangle(cornerRadius: 24))
        .padding(.horizontal, 25)
        #endif
    }

    private func acceptButton() -> some View {
        Button {
            Task {
                do {
                    let conditionsId = ChatModel.shared.conditions.currentConditions.conditionsId
                    let r = try await acceptConditions(conditionsId: conditionsId, operatorIds: Array(selectedOperatorIds))
                    await MainActor.run {
                        ChatModel.shared.conditions = r
                    }
                    if let enabledOps = enabledOperators(r.serverOperators) {
                        let r2 = try await setServerOperators(operators: enabledOps)
                        await MainActor.run {
                            ChatModel.shared.conditions = r2
                            completeOnboarding()
                        }
                    } else {
                        await MainActor.run {
                            completeOnboarding()
                        }
                    }
                } catch let error {
                    await MainActor.run {
                        showAlert(
                            NSLocalizedString("Error accepting conditions", comment: "alert title"),
                            message: responseError(error)
                        )
                    }
                }
            }
        } label: {
            Text("Accept")
        }
        .buttonStyle(OnboardingButtonStyle(isDisabled: selectedOperatorIds.isEmpty))
        .disabled(selectedOperatorIds.isEmpty)
    }

    private func completeOnboarding() {
        let m = ChatModel.shared
        onboardingStageDefault.set(.onboardingComplete)
        m.onboardingStage = .onboardingComplete
    }

    private func enabledOperators(_ operators: [ServerOperator]) -> [ServerOperator]? {
        var ops = operators
        if !ops.isEmpty {
            for i in 0..<ops.count {
                var op = ops[i]
                op.enabled = selectedOperatorIds.contains(op.operatorId)
                ops[i] = op
            }
            let haveSMPStorage = ops.contains(where: { $0.enabled && $0.smpRoles.storage })
            let haveSMPProxy = ops.contains(where: { $0.enabled && $0.smpRoles.proxy })
            let haveXFTPStorage = ops.contains(where: { $0.enabled && $0.xftpRoles.storage })
            let haveXFTPProxy = ops.contains(where: { $0.enabled && $0.xftpRoles.proxy })
            if haveSMPStorage && haveSMPProxy && haveXFTPStorage && haveXFTPProxy {
                return ops
            } else if let firstEnabledIndex = ops.firstIndex(where: { $0.enabled }) {
                var op = ops[firstEnabledIndex]
                if !haveSMPStorage { op.smpRoles.storage = true }
                if !haveSMPProxy { op.smpRoles.proxy = true }
                if !haveXFTPStorage { op.xftpRoles.storage = true }
                if !haveXFTPProxy { op.xftpRoles.proxy = true }
                ops[firstEnabledIndex] = op
                return ops
            } else {
                return nil
            }
        } else {
            return nil
        }
    }
}
