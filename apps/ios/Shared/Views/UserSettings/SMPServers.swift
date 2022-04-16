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
        return VStack(alignment: .leading) {
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
                .padding(.bottom)
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
            
            if !isUserSMPServers {
                Text("Using SimpleX Chat servers.")
                    .frame(maxWidth: .infinity, alignment: .leading)
            } else {
                VStack(alignment: .leading) {
                    Text("Enter one SMP server per line:")
                    if editSMPServers {
                        TextEditor(text: $userSMPServersStr)
                            .focused($keyboardVisible)
                            .font(serversFont)
                            .disableAutocorrection(true)
                            .textInputAutocapitalization(.never)
                            .padding(.horizontal, 5)
                            .padding(.top, 2)
                            .frame(height: 112)
                            .overlay(
                                RoundedRectangle(cornerRadius: 10)
                                    .strokeBorder(.secondary, lineWidth: 0.3, antialiased: true)
                            )
                        HStack(spacing: 20) {
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
                        }
                    } else {
                        ScrollView {
                            Text(userSMPServersStr)
                                .font(serversFont)
                                .padding(10)
                                .frame(minHeight: 0, alignment: .topLeading)
                                .textSelection(.enabled)
                                .frame(maxWidth: .infinity, alignment: .leading)
                        }
                        .frame(height: 160)
                        .overlay(
                            RoundedRectangle(cornerRadius: 10)
                                .strokeBorder(.secondary, lineWidth: 0.3, antialiased: true)
                        )
                        HStack {
                            Button("Edit") {
                                editSMPServers = true
                            }
                            Spacer()
                            howToButton()
                        }
                    }
                }
                .frame(maxWidth: .infinity)
            }
        }
        .padding()
        .frame(maxHeight: .infinity, alignment: .top)
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
