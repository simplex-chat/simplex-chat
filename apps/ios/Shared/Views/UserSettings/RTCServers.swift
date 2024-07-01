//
//  RTCServers.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 20/09/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

private let howToUrl = URL(string: "https://simplex.chat/docs/webrtc.html#configure-mobile-apps")!

let serversFont = Font.custom("Menlo", size: 14)

struct RTCServers: View {
    @EnvironmentObject var theme: AppTheme
    @State private var userRTCServers: [String] = []
    @State private var isUserRTCServers = false
    @State private var isUserRTCServersToggle = false
    @State private var editRTCServers = true
    @State private var userRTCServersStr = ""
    @State private var showBadServersAlert = false
    @State private var showResetServersAlert = false
    @FocusState private var keyboardVisible: Bool

    var body: some View {
        List {
            Section {
                Toggle("Configure ICE servers", isOn: $isUserRTCServersToggle)
                    .onChange(of: isUserRTCServersToggle) { _ in
                        if (isUserRTCServersToggle) {
                            isUserRTCServers = true
                        } else if (userRTCServers.isEmpty) {
                            resetRTCServers()
                        } else {
                            showResetServersAlert = true
                        }
                    }
                    .alert(isPresented: $showResetServersAlert) {
                        Alert(
                            title: Text("Use SimpleX Chat servers?"),
                            message: Text("Saved WebRTC ICE servers will be removed"),
                            primaryButton: .destructive(Text("Confirm")) {
                                resetRTCServers()
                            }, secondaryButton: .cancel() {
                                withAnimation() {
                                    isUserRTCServersToggle = true
                                }
                            }
                        )
                    }
            } header: {
                Text("")
            } footer: {
                if !isUserRTCServers {
                    Text("Using SimpleX Chat servers.")
                        .foregroundColor(theme.colors.secondary)
                }
            }

            if isUserRTCServers {
                Section {
                    if editRTCServers {
                        TextEditor(text: $userRTCServersStr)
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
                            Text(userRTCServersStr)
                                .font(serversFont)
                                .frame(minHeight: 0, alignment: .topLeading)
                                .textSelection(.enabled)
                                .frame(maxWidth: .infinity, alignment: .leading)
                        }
                        .frame(height: 160)
                    }
                } header: {
                    Text("ICE servers (one per line)")
                        .foregroundColor(theme.colors.secondary)
                } footer: {
                    HStack(spacing: 20) {
                        if editRTCServers {
                            Button("Cancel") {
                                initialize()
                            }
                            Button("Save") {
                                saveUserRTCServers()
                            }
                            .alert(isPresented: $showBadServersAlert) {
                                Alert(title: Text("Error saving ICE servers"), message: Text("Make sure WebRTC ICE server addresses are in correct format, line separated and are not duplicated."))
                            }
                            Spacer()
                            howToButton()
                        } else {
                            Button("Edit") {
                                editRTCServers = true
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

    private func initialize() {
        userRTCServers = UserDefaults.standard.stringArray(forKey: DEFAULT_WEBRTC_ICE_SERVERS) ?? []
        isUserRTCServers = !userRTCServers.isEmpty
        isUserRTCServersToggle = isUserRTCServers
        editRTCServers = !isUserRTCServers
        userRTCServersStr = isUserRTCServers ? userRTCServers.joined(separator: "\n") : ""
    }

    private func resetRTCServers() {
        isUserRTCServers = false
        userRTCServers = []
        UserDefaults.standard.removeObject(forKey: DEFAULT_WEBRTC_ICE_SERVERS)
    }

    private func saveUserRTCServers() {
        let srvs = userRTCServersStr.components(separatedBy: "\n")
        if srvs.count > 0 && Set(srvs).count == srvs.count && parseRTCIceServers(srvs) != nil {
            userRTCServers = srvs
            UserDefaults.standard.set(srvs, forKey: DEFAULT_WEBRTC_ICE_SERVERS)
            editRTCServers = false
        } else {
            showBadServersAlert = true
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

struct RTCServers_Previews: PreviewProvider {
    static var previews: some View {
        RTCServers()
    }
}
