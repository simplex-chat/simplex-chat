//
//  SMPServers.swift
//  SimpleX
//
//  Created by Efim Poberezkin on 02.03.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

private let serversFont = Font.custom("Menlo", size: 14)

struct SMPServers: View {
    @EnvironmentObject var chatModel: ChatModel
    @State var isUserSMPServers = false
    @State var editSMPServers = true
    @State var userSMPServersStr = ""
    @FocusState private var keyboardVisible: Bool

    var body: some View {
        return VStack(alignment: .leading) {
            Text("Here you can configure custom SMP servers.")
                .padding(.bottom)
            Toggle("Use custom SMP servers", isOn: $isUserSMPServers)
                .onChange(of: isUserSMPServers) { _ in
                    if (!isUserSMPServers) {
                        // TODO alert
                        saveSMPServers(smpServers: [])
                        userSMPServersStr = ""
                    }
                }
                .padding(.bottom)
            
            if !isUserSMPServers {
                VStack(alignment: .leading) {
                    Text("You are using default SMP servers.")
                }
                .frame(maxWidth: .infinity, alignment: .leading)
            } else {
                VStack(alignment: .leading) {
                    Text("Specify addresses of SMP server(s) to be used for creating new connections. Each address has to be put on a new line.")
                        .allowsTightening(false)
                    if editSMPServers {
                        TextEditor(text: $userSMPServersStr)
                            .focused($keyboardVisible)
                            .font(serversFont)
                            .textInputAutocapitalization(.never)
                            .padding(.horizontal, 5)
                            .allowsTightening(false)
                            .frame(height: 160)
                            .overlay(
                                RoundedRectangle(cornerRadius: 10)
                                    .strokeBorder(.secondary, lineWidth: 0.3, antialiased: true)
                            )
                        HStack(spacing: 20) {
                            Button("Save") {
                                saveUserSMPServers()
                            }
                        }
                    } else {
                        // TODO scroll
                        Text(userSMPServersStr)
                            .font(serversFont)
                            .multilineTextAlignment(.leading) // TODO top
                            .padding(.horizontal, 10)
                            .allowsTightening(false)
                            .frame(height: 160)
                            .overlay(
                                RoundedRectangle(cornerRadius: 20)
                                    .strokeBorder(.secondary, lineWidth: 0.3, antialiased: true)
                            )
                        Button("Edit") {
                            editSMPServers = true
                        }
                    }
                }
                .frame(maxWidth: .infinity, minHeight: 240, alignment: .leading)
            }
        }
        .padding()
        .padding(.top)
        .frame(maxHeight: .infinity, alignment: .top)

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
                let err = error.localizedDescription
                logger.error("SMPServers.saveServers setUserSMPServers error: \(err)")
                // keyboardVisible = false
                saveSMPServersAlert() // TODO Alert is not being shown
            }
        }
    }
    
    func saveSMPServersAlert() {
        AlertManager.shared.showAlertMsg(
            title: "Error saving SMP servers",
            message: "Make sure SMP server addresses are in correct format, line separated and are not duplicated."
        )
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
