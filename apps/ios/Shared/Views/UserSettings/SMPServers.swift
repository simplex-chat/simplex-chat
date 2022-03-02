//
//  SMPServers.swift
//  SimpleX
//
//  Created by Efim Poberezkin on 02.03.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct SMPServers: View {
    @EnvironmentObject var chatModel: ChatModel
    @State private var isUserSMPServers: Bool = false // TODO check model
    @State private var userSMPServers: String = "abc" // TODO check model
    @State private var editSMPServers: Bool = true // TODO false is servers exist

    var body: some View {
        // TODO read servers from model
//        let user: User = chatModel.currentUser!

        return VStack(alignment: .leading) {
            Text("You can configure custom SMP servers.")
                .padding(.bottom)
            Toggle("Custom SMP servers", isOn: $isUserSMPServers) // TODO on toggle, alert

            if !isUserSMPServers {
                VStack(alignment: .leading) {
                    Text("You are using default SMP servers")
                }
                .frame(maxWidth: .infinity, minHeight: 120, alignment: .leading)
            } else {
                // TODO conditionally enable rich text box
                if editSMPServers {
                    VStack(alignment: .leading) {
                        TextField("Servers", text: $userSMPServers)
                            .textInputAutocapitalization(.never)
                            .disableAutocorrection(true)
                            .padding(.bottom)
                        HStack(spacing: 20) {
                            Button("Save") { saveServers() }
                        }
                    }
                    .frame(maxWidth: .infinity, minHeight: 120, alignment: .leading)
                } else {
                    VStack(alignment: .leading) {
                        TextField("Servers", text: $userSMPServers)
                            .textInputAutocapitalization(.never)
                            .disableAutocorrection(true)
                            .padding(.bottom)
                        Button("Edit") {
//                            profile = user.profile
                            editSMPServers = true
                        }
                    }
                    .frame(maxWidth: .infinity, minHeight: 120, alignment: .leading)
                }
            }
        }
        .padding()
        .frame(maxHeight: .infinity, alignment: .top)
    }

    func saveServers() {
        Task {
//            do {
//                if let newProfile = try await apiUpdateProfile(profile: profile) {
//                    DispatchQueue.main.async {
//                        chatModel.currentUser?.profile = newProfile
//                        profile = newProfile
//                    }
//                }
//            } catch {
//                logger.error("UserProfile apiUpdateProfile error: \(error.localizedDescription)")
//            }
            editSMPServers = false
        }
    }
}

struct SMPServers_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.currentUser = User.sampleData
        return SMPServers()
            .environmentObject(chatModel)
    }
}
