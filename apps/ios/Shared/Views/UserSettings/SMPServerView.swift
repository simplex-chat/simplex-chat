//
//  SMPServerView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 15/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct SMPServerView: View {
    @State var server: ServerCfg

    var body: some View {
        if server.preset {
            presetServer()
        } else {
            customServer()
        }
    }

    private func presetServer() -> some View {
        return VStack {
            List {
                Section("Preset server address") {
                    Text(server.address)
                }
                useServerSection()
            }
        }
    }

    private func customServer() -> some View {
        VStack {
            List {
                Section("Your server address") {
                    TextEditor(text: $server.address)
                        .multilineTextAlignment(.leading)
                        .autocorrectionDisabled(true)
                        .autocapitalization(.none)
                        .lineLimit(10)
                        .frame(height: 108)
                        .padding(-6)
                }
                useServerSection()
                Section("Add to another device") {
                    QRCode(uri: server.address)
                        .listRowInsets(EdgeInsets(top: 12, leading: 12, bottom: 12, trailing: 12))
                }
            }
        }
    }

    private func useServerSection() -> some View {
        Section("Use server") {
            HStack {
                Button("Test server") {
                }
                Spacer()
                showTestStatus(server: server)
            }
            Toggle("Enabled", isOn: $server.enabled)
            Button("Remove server", role: .destructive) {

            }
        }
    }
}

struct SMPServerView_Previews: PreviewProvider {
    static var previews: some View {
        SMPServerView(server: ServerCfg.sampleData.custom)
    }
}
