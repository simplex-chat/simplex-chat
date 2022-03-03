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
    @State var isUserSMPServers = false
    @State var editSMPServers = true
    @State var userSMPServersStr = ""
    @FocusState private var keyboardVisible: Bool

    var body: some View {
        return VStack(alignment: .leading) {
            Text("You can configure custom SMP servers.")
                .padding(.bottom)
            Toggle("Custom SMP servers", isOn: $isUserSMPServers)
                .onChange(of: isUserSMPServers) { _ in
                    if (!isUserSMPServers) {
                        // TODO alert
                        saveSMPServers(smpServers: [])
                    }
                }
            
            if !isUserSMPServers {
                VStack(alignment: .leading) {
                    Text("You are using default SMP servers.")
                }
                .frame(maxWidth: .infinity, alignment: .leading)
            } else {
                if editSMPServers {
                    VStack(alignment: .leading) {
                        TextEditor(text: $userSMPServersStr)
                            .focused($keyboardVisible)
//                            .font(teFont)
                            .textInputAutocapitalization(.sentences)
                            .padding(.horizontal, 5)
                            .allowsTightening(false)
                            .lineLimit(5)
//                            .frame(height: teHeight)
                        HStack(spacing: 20) {
                            Button("Save") { saveUserSMPServers() }
                        }
                    }
                    .frame(maxWidth: .infinity, minHeight: 120, alignment: .leading)
                } else {
                    VStack(alignment: .leading) {
                        TextEditor(text: $userSMPServersStr)
                            .focused($keyboardVisible)
//                            .font(teFont)
                            .textInputAutocapitalization(.sentences)
                            .padding(.horizontal, 5)
                            .allowsTightening(false)
                            .lineLimit(5)
                            .disabled(true)
//                            .frame(height: teHeight)
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
//        .frame(maxHeight: .infinity, alignment: .top)

    }
    
    func saveUserSMPServers() {
        let userSMPServers = userSMPServersStr.components(separatedBy: "\n")
        saveSMPServers(smpServers: userSMPServers)
    }
    
    func saveSMPServers(smpServers: [String]) {
        Task {
            do {
                try await setUserSMPServers(smpServers: smpServers)
                DispatchQueue.main.async {
                    chatModel.userSMPServers = smpServers
                }
                if smpServers.isEmpty {
                    isUserSMPServers = false
                    editSMPServers = true
                } else {
                    editSMPServers = false
                }
            } catch {
                logger.error("SMPServers.saveServers setUserSMPServers error: \(error.localizedDescription)")
                // TODO alert?
            }
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
