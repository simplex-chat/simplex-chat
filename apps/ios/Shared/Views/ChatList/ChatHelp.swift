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

            HStack(spacing: 4) {
                Text("You can")
                Button("connect to SimpleX Chat developers to ask any questions and to receive updates.") {
                    showSettings = false
                    DispatchQueue.main.async {
                        UIApplication.shared.open(simplexTeamURL)
                    }
                }
            }

            VStack(alignment: .leading, spacing: 10) {
                Text("To start a new chat")
                    .font(.title2)
                    .fontWeight(.bold)

                HStack(spacing: 8) {
                    Text("Tap button ")
                    NewChatButton()
                    Text("above, then:")
                }

                Text("**Add new contact**: to create your one-time QR Code for your contact.")
                Text("**Scan QR code**: to connect to your contact who shows QR code to you.")
            }
            .padding(.top, 24)

            VStack(alignment: .leading, spacing: 10) {
                Text("To connect via link")
                    .font(.title2)
                    .fontWeight(.bold)

                Text("If you received SimpleX Chat invitation link you can open it in your browser:")

                Text("💻 desktop: scan displayed QR code from the app, via **Scan QR code**.")
                Text("📱 mobile: tap **Open in mobile app**, then tap **Connect** in the app.")
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
