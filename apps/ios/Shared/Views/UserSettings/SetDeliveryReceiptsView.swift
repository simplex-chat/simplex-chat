//
//  SetDeliveryReceiptsView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 12/07/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct SetDeliveryReceiptsView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme

    var body: some View {
        VStack(spacing: 16) {
            Text("Delivery receipts!")
                .font(.title)
                .foregroundColor(theme.colors.secondary)
                .padding(.vertical)
                .multilineTextAlignment(.center)

            Spacer()

            Button("Enable") {
                Task {
                    do {
                        if let currentUser = m.currentUser {
                            try await apiSetAllContactReceipts(enable: true)
                            await MainActor.run {
                                var updatedUser = currentUser
                                updatedUser.sendRcptsContacts = true
                                m.updateUser(updatedUser)
                                m.setDeliveryReceipts = false
                                privacyDeliveryReceiptsSet.set(true)
                            }
                            do {
                                let users = try await listUsersAsync()
                                await MainActor.run { m.users = users }
                            } catch let error {
                                logger.debug("listUsers error: \(responseError(error))")
                            }
                        }
                    } catch let error {
                        AlertManager.shared.showAlert(Alert(
                            title: Text("Error enabling delivery receipts!"),
                            message: Text("Error: \(responseError(error))")
                        ))
                        await MainActor.run {
                            m.setDeliveryReceipts = false
                        }
                    }
                }
            }
            .font(.largeTitle)
            Group {
                if m.users.count > 1 {
                    Text("Sending delivery receipts will be enabled for all contacts in all visible chat profiles.")
                } else {
                    Text("Sending delivery receipts will be enabled for all contacts.")
                }
            }
            .multilineTextAlignment(.center)

            Spacer()

            VStack(spacing: 8) {
                Button {
                    AlertManager.shared.showAlert(Alert(
                        title: Text("Delivery receipts are disabled!"),
                        message: Text("You can enable them later via app Privacy & Security settings."),
                        primaryButton: .default(Text("Don't show again")) {
                            m.setDeliveryReceipts = false
                            privacyDeliveryReceiptsSet.set(true)
                        },
                        secondaryButton: .default(Text("Ok")) {
                            m.setDeliveryReceipts = false
                        }
                    ))
                } label: {
                    HStack {
                        Text("Don't enable")
                        Image(systemName: "chevron.right")
                    }
                }
                Text("You can enable later via Settings").font(.footnote)
            }
        }
        .padding()
        .padding(.horizontal)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .background(AppTheme.shared.colors.background)
    }
}

struct SetDeliveryReceiptsView_Previews: PreviewProvider {
    static var previews: some View {
        SetDeliveryReceiptsView()
    }
}
