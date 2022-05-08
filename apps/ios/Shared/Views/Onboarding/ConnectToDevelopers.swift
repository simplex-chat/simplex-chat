//
//  ConnectToDevelopers.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ConnectToDevelopers: View {
    @EnvironmentObject var chatModel: ChatModel

    var body: some View {
        VStack {
            Text("Connect to developers")

            Spacer()

            Button {
                chatModel.onboardingStage = .step3a_MakeConnection
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

struct ConnectToDevelopers_Previews: PreviewProvider {
    static var previews: some View {
        ConnectToDevelopers()
    }
}
