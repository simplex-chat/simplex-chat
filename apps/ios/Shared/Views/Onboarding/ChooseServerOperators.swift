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

    var body: some View {
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
                            .padding(.horizontal)
                    }

                    Spacer()
                    Spacer()

                    continueButton()
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
        .onTapGesture {
            if checked {
                selectedOperators.remove(serverOperator.operatorId)
            } else {
                selectedOperators.insert(serverOperator.operatorId)
            }
        }
    }

    private func yourServersButton() -> some View {
        NavigationLink {
            YourServersView()
                .navigationTitle("Your servers")
                .modifier(ThemedBackground(grouped: true))
        } label: {
            Text("Your servers")
        }
    }

    private func continueButton() -> some View {
        HStack {
            Spacer()
            
            Button {
                withAnimation {
                    onboardingStageDefault.set(.step4_SetNotificationsMode)
                    ChatModel.shared.onboardingStage = .step4_SetNotificationsMode
                }
            } label: {
                Text("Continue")
            }
            .buttonStyle(.borderedProminent)
            
            Spacer()
        }
    }
}

#Preview {
    ChooseServerOperators()
}
