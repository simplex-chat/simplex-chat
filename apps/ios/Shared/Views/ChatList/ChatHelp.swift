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
    @State private var newChatMenuOption: NewChatMenuOption? = nil

    var body: some View {
        ScrollView { chatHelp() }
    }

    func chatHelp() -> some View {
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
                    NewChatMenuButton(newChatMenuOption: $newChatMenuOption)
                    Text("above, then choose:")
                }

                Text("**Add contact**: to create a new invitation link, or connect via a link you received.")
                Text("**Create group**: to create a new group.")
            }
            .padding(.top, 24)

            VStack(alignment: .leading, spacing: 10) {
                Text("Markdown in messages")
                    .font(.title2)
                    .fontWeight(.bold)

                MarkdownHelp()
            }
            .padding(.top, 24)
        }
        .padding()
    }
}

struct ChatHelp_Previews: PreviewProvider {
    static var previews: some View {
        return ChatHelp(showSettings: Binding.constant(false))
    }
}
