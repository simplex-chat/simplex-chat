//
//  ChatHelp.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 10/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatHelp: View {
    @EnvironmentObject var chatModel: ChatModel
    @Binding var showSettings: Bool

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            Text("Thank you for installing SimpleX Chat!")

            VStack(alignment: .leading, spacing: 0) {
                Text("To ask any questions and to receive updates:")
                Button("connect to SimpleX Chat developers.") {
                    showSettings = false
                    DispatchQueue.main.async {
                        UIApplication.shared.open(simplexTeamURL)
                    }
                }
                .padding(.top, 2)
            }

            VStack(alignment: .leading, spacing: 10) {
                Text("To make a new connection")
                    .font(.title2)
                    .fontWeight(.bold)

                HStack(spacing: 8) {
                    Text("Tap button ")
                    NewChatButton()
                    Text("above, then choose:")
                }

                Text("**Create link / QR code** for your contact to use.")
                Text("**Paste received link** or open it in the browser and tap **Open in mobile app**.")
                Text("**Scan QR code**: to connect to your contact in person or via video call.")
            }
            .padding(.top, 24)
        }
        .padding()
    }
}

struct ChatHelp_Previews: PreviewProvider {
    static var previews: some View {
        @State var showSettings = false
        return ChatHelp(showSettings: $showSettings)
    }
}
