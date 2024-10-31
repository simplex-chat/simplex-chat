//
//  ChooseServerOperators.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 31.10.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChooseServerOperators: View {
    @EnvironmentObject var theme: AppTheme
    @State private var showInfoSheet = false
    @State private var serverOperators: [ServerOperator] = []
    @State private var selectedOperators = Set<Int64>()
    @State private var customServersNavLinkActive = false

    var body: some View {
        NavigationView {
            GeometryReader { g in
                ScrollView {
                    VStack(alignment: .leading, spacing: 20) {
                        Text("Choose operators")
                            .font(.largeTitle)
                            .bold()
                            .padding(.top)

                        infoText()

                        Spacer()

                        ForEach(serverOperators) { srvOperator in
                            operatorCheckView(srvOperator)
                        }

                        customServersButton()

                        Spacer()
                        Spacer()

                        reviewConditionsButton()
                            .padding(.bottom)
                    }
                    .frame(minHeight: g.size.height)
                }
            }
            .frame(maxHeight: .infinity)
            .padding()
            .onAppear {
                serverOperators = ChatModel.shared.serverOperators
                selectedOperators = Set(serverOperators.filter { $0.enabled }.map { $0.operatorId })
            }
            .sheet(isPresented: $showInfoSheet) {
                Text("Info")
            }
        }
    }

    private func infoText() -> some View {
        HStack(spacing: 12) {
            Image(systemName: "info.circle")
                .resizable()
                .scaledToFit()
                .frame(width: 20, height: 20)
                .foregroundColor(theme.colors.primary)
                .onTapGesture {
                    showInfoSheet = true
                }

            Text("Select operators, whose servers you will be using.")
        }
    }

    @ViewBuilder private func operatorCheckView(_ serverOperator: ServerOperator) -> some View {
        let checked = selectedOperators.contains(serverOperator.operatorId)
        let icon = checked ? "checkmark.circle.fill" : "circle"
        let iconColor = checked ? theme.colors.primary : Color(uiColor: .tertiaryLabel).asAnotherColorFromSecondary(theme)
        HStack(spacing: 10) {
            Image(serverOperator.info.logo)
                .resizable()
                .scaledToFit()
                .frame(width: 30, height: 30)
            Text(serverOperator.name)
                .font(.title2)
            Spacer()
            Image(systemName: icon)
                .resizable()
                .scaledToFit()
                .frame(width: 26, height: 26)
                .foregroundColor(iconColor)
        }
        .background(Color(.systemBackground))
        .padding()
        .clipShape(RoundedRectangle(cornerRadius: 18))
        .overlay(
            RoundedRectangle(cornerRadius: 18)
                .stroke(Color(uiColor: .secondarySystemFill), lineWidth: 2)
        )
        .onTapGesture {
            if checked {
                selectedOperators.remove(serverOperator.operatorId)
            } else {
                selectedOperators.insert(serverOperator.operatorId)
            }
        }
    }

    private func customServersButton() -> some View {
        ZStack {
            HStack(spacing: 10) {
                ZStack {
                    Image(systemName: "externaldrive.connected.to.line.below")
                        .resizable()
                        .scaledToFit()
                        .frame(width: 22, height: 22)
                        .foregroundColor(theme.colors.secondary)
                }
                .frame(width: 28, height: 28)

                Text("Custom servers")
                    .font(.title3)

                Spacer()

                Text("Configure…")
                    .font(.callout)
                    .foregroundColor(theme.colors.primary)
            }
            .background(Color(.systemBackground))
            .padding()
            .clipShape(RoundedRectangle(cornerRadius: 18))
            .overlay(
                RoundedRectangle(cornerRadius: 18)
                    .stroke(Color(uiColor: .secondarySystemFill), lineWidth: 2)
            )
            .padding(.horizontal, 2)
            .onTapGesture {
                customServersNavLinkActive = true
            }

            NavigationLink(isActive: $customServersNavLinkActive) {
                customServersDestinationView()
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }

    private func customServersDestinationView() -> some View {
        YourServersView()
            .navigationTitle("Custom servers")
            .modifier(ThemedBackground(grouped: true))
    }

    private func reviewConditionsButton() -> some View {
        HStack {
            Spacer()
            
            Button {
                withAnimation {
                    onboardingStageDefault.set(.step4_SetNotificationsMode)
                    ChatModel.shared.onboardingStage = .step4_SetNotificationsMode
                }
            } label: {
                Text("Review conditions")
            }
            .buttonStyle(.borderedProminent)
            
            Spacer()
        }
        .disabled(selectedOperators.isEmpty)
    }
}

#Preview {
    ChooseServerOperators()
}
