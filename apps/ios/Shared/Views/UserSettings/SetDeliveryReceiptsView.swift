//
//  SetDeliveryReceiptsView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 12/07/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct SetDeliveryReceiptsView: View {
    @EnvironmentObject var m: ChatModel

    var body: some View {
        VStack(spacing: 16) {
            Text("Delivery receipts!")
                .font(.title)
                .foregroundColor(.secondary)
                .padding(.vertical)
                .multilineTextAlignment(.center)

            Spacer()

            Button("Enable") {
                m.setDeliveryReceipts = false
            }
            .font(.largeTitle)
            Group {
                if m.users.count > 1 {
                    Text("Delivery receipts will be enabled for all contacts in all visible chat profiles.")
                } else {
                    Text("Delivery receipts will be enabled for all contacts.")
                }
            }
            .multilineTextAlignment(.center)

            Spacer()

            Button("Enable later via Settings") {
                AlertManager.shared.showAlert(Alert(
                    title: Text("Delivery receipts are disabled!"),
                    message: Text("You can enable them later via app Privacy & Security settings."),
                    primaryButton: .default(Text("Don't show again")) {
                        m.setDeliveryReceipts = false
                    },
                    secondaryButton: .default(Text("Ok")) {
                        m.setDeliveryReceipts = false
                    }
                ))
            }
        }
        .padding()
        .padding(.horizontal)
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
        .background(Color(uiColor: .systemBackground))
    }
}

struct SetDeliveryReceiptsView_Previews: PreviewProvider {
    static var previews: some View {
        SetDeliveryReceiptsView()
    }
}
