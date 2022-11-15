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
    @State var server: SMPServerCfg

    var body: some View {
        switch server.server {
        case let .name(name): presetServer(name)
        case let .params(params): UserServer(server: $server, params: params)
        }
    }

    private func presetServer(_ name: SMPServerName) -> some View {
        return VStack {
            List {
                Section("Preset server") {
                    Text(name.hostname)
                    Toggle("Enabled", isOn: $server.enabled)
                    Button("Remove server", role: .destructive) {

                    }
                }
            }
        }
    }

    private struct UserServer: View {
        @Binding var server: SMPServerCfg
        @State var params: SMPServerParams
        @State var showAsAddress = true
        @State var serverAddress = ""

        var body: some View {
            VStack {
                List {
                    Section {
                        Toggle("Show as address", isOn: $showAsAddress)
                    }

                    if showAsAddress {
                        Section("Your server address") {
                            TextEditor(text: $serverAddress)
                                .multilineTextAlignment(.leading)
                                .lineLimit(10)
                                .frame(height: 108)
                                .padding(-6)
                        }
                    } else {
                        Section("Server hostnames") {
                            ForEach(params.hostnames, id: \.self) { host in
                                Text(host)
                            }
                            .onMove { indexSet, offset in
                                params.hostnames.move(fromOffsets: indexSet, toOffset: offset)
                            }
                            .onDelete { indexSet in
                                params.hostnames.remove(atOffsets: indexSet)
                            }
                            Button("Add hostname…") {

                            }
                        }
                        Section("Fingerprint") {
                            TextEditor(text: $params.keyHash)
                                .multilineTextAlignment(.leading)
                                .lineLimit(2)
                                .frame(height: 64, alignment: .top)
                                .padding(-6)
                        }
                        Section("Password") {
                            TextEditor(text: $params.basicAuth)
                                .padding(.horizontal, -6)
                        }
                        Section("Port") {
                            TextEditor(text: $params.port)
                                .padding(.horizontal, -6)
                        }
                    }
                    Section("Scan from another device") {
                        QRCode(uri: params.uri)
                            .listRowInsets(EdgeInsets(top: 12, leading: 12, bottom: 12, trailing: 12))
                    }
                    Section("Use server") {
                        Toggle("Enabled", isOn: $server.enabled)
                        Button("Remove server", role: .destructive) {

                        }
                    }
                }
            }
            .onAppear {
                serverAddress = params.uri
            }
            .toolbar { EditButton() }
        }
    }
}

struct SMPServerView_Previews: PreviewProvider {
    static var previews: some View {
        SMPServerView(server: SMPServerCfg.sampleData.params)
    }
}
