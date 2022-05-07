//
//  MakeConnection.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct MakeConnection: View {
    @EnvironmentObject var chatModel: ChatModel

    var body: some View {
        VStack {
            Text("Make a connection")
                .font(.title)

            Spacer()

            Button {
                chatModel.onboardingStep = .step3b_ConnectViaLink
            } label: {
                HStack {
                    Text("Connect")
                    Image(systemName: "greaterthan")
                }
            }
            .frame(maxWidth: .infinity)

            Spacer()
        }
    }
}

struct MakeConnection_Previews: PreviewProvider {
    static var previews: some View {
        MakeConnection()
    }
}
