//
//  SMPServers.swift
//  SimpleX
//
//  Created by Efim Poberezkin on 02.03.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

private let serversFont = Font.custom("Menlo", size: 14)

private let howToUrl = URL(string: "https://github.com/simplex-chat/simplexmq#using-smp-server-and-smp-agent")!

struct SMPServers: View {
    @EnvironmentObject var chatModel: ChatModel
    @State var isUserSMPServers = false
    @State var isUserSMPServersToggle = false
    @State var editSMPServers = true
    @State var userSMPServersStr = ""
    @State var showBadServersAlert = false
    @State var showResetServersAlert = false
    @FocusState private var keyboardVisible: Bool

    var body: some View {
        List {
            Section {
                Toggle("Configure SMP servers", isOn: $isUserSMPServersToggle)
                    .onChange(of: isUserSMPServersToggle) { _ in
                        if (isUserSMPServersToggle) {
                            isUserSMPServers = true
                        } else {
                            let servers = chatModel.userSMPServers ?? []
                            if (!servers.isEmpty) {
                                showResetServersAlert = true
                            } else {
                                isUserSMPServers = false
                                userSMPServersStr = ""
                            }
                        }
                    }
                    .alert(isPresented: $showResetServersAlert) {
                        Alert(
                            title: Text("Use SimpleX Chat servers?"),
                            message: Text("Saved SMP servers will be removed"),
                            primaryButton: .destructive(Text("Confirm")) {
                                saveSMPServers(smpServers: [])
                                isUserSMPServers = false
                                userSMPServersStr = ""
                            }, secondaryButton: .cancel() {
                                withAnimation() {
                                    isUserSMPServersToggle = true
                                }
                            }
                        )
                    }
            } header: {
                Text("")
            } footer: {
                if !isUserSMPServers {
                    Text("Using SimpleX Chat servers.")
                }
            }
            
            if isUserSMPServers {
                Section {
                    if editSMPServers {
                        TextEditor(text: $userSMPServersStr)
                            .focused($keyboardVisible)
                            .font(serversFont)
                            .disableAutocorrection(true)
                            .textInputAutocapitalization(.never)
                            .padding(.horizontal, -5)
                            .padding(.top, -8)
                            .frame(height: 160, alignment: .topLeading)
                            .frame(maxWidth: .infinity, alignment: .leading)
                    } else {
                        ScrollView {
                            Text(userSMPServersStr)
                                .font(serversFont)
                                .frame(minHeight: 0, alignment: .topLeading)
                                .textSelection(.enabled)
                                .frame(maxWidth: .infinity, alignment: .leading)
                        }
                        .frame(height: 160)
                    }
                } header: {
                    Text("SMP servers (one per line)")
                } footer: {
                    HStack(spacing: 20) {
                        if editSMPServers {
                            Button("Cancel") {
                                initialize()
                            }
                            Button("Save") {
                                saveUserSMPServers()
                            }
                            .alert(isPresented: $showBadServersAlert) {
                                Alert(title: Text("Error saving SMP servers"), message: Text("Make sure SMP server addresses are in correct format, line separated and are not duplicated."))
                            }
                            Spacer()
                            howToButton()
                        } else {
                            Button("Edit") {
                                editSMPServers = true
                            }
                            Spacer()
                            howToButton()
                        }
                    }
                    .font(.body)
                }
            }
        }
        .onAppear { initialize() }
    }
    
    func initialize() {
        let userSMPServers = chatModel.userSMPServers ?? []
        isUserSMPServers = !userSMPServers.isEmpty
        isUserSMPServersToggle = isUserSMPServers
        editSMPServers = !isUserSMPServers
        userSMPServersStr = isUserSMPServers ? userSMPServers.joined(separator: "\n") : ""
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
                    if smpServers.isEmpty {
                        isUserSMPServers = false
                        editSMPServers = true
                    } else {
                        editSMPServers = false
                    }
                }
            } catch {
                let err = error.localizedDescription
                logger.error("SMPServers.saveServers setUserSMPServers error: \(err)")
                DispatchQueue.main.async {
                    showBadServersAlert = true
                }
            }
        }
    }
    
    func howToButton() -> some View {
        Button {
            DispatchQueue.main.async {
                UIApplication.shared.open(howToUrl)
            }
        } label: {
            HStack{
                Text("How to")
                Image(systemName: "arrow.up.right.circle")
            }
        }
    }
}

// TODO preview doesn't work
struct SMPServers_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.currentUser = User.sampleData
        chatModel.userSMPServers = []
        return SMPServers()
            .environmentObject(chatModel)
    }
}
