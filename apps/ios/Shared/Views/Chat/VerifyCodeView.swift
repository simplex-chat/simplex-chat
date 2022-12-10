//
//  VerifyCodeView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 10/12/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct VerifyCodeView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    var displayName: String
    var connectionCode: String
    @State var verified: Bool
    var verify: (String?) async -> Bool
    @State private var showCodeError = false

    var body: some View {
        ScrollView {
            let splitCode = splitToParts(connectionCode, length: 24)
            VStack(alignment: .leading) {
                Group {
                    HStack {
                        if verified {
                            Image(systemName: "checkmark.shield")
                                .foregroundColor(.secondary)
                            Text("\(displayName) is verified")
                        } else {
                            Text("\(displayName) is not verified")
                        }
                    }
                    .frame(height: 24)

                    QRCode(uri: connectionCode)
                        .padding(.horizontal)

                    Text(splitCode)
                        .multilineTextAlignment(.leading)
                        .font(.body.monospaced())
                        .lineLimit(20)
                        .padding(.bottom, 8)
                }
                .frame(maxWidth: .infinity, alignment: .center)

                Text("To verify end-to-end encryption with your contact compare (or scan) the code on your devices.")
                    .padding(.bottom)

                Group {
                    if verified {
                        Button {
                            Task {
                                let ok = await verify(nil)
                                await MainActor.run { verified = ok }
                            }
                        } label: {
                            Text("Clear verification")
                        }
                        .padding()
                    } else {
                        HStack {
                            NavigationLink {
                                ScanCodeView(verified: $verified, verify: verify)
                                    .navigationBarTitleDisplayMode(.inline)
                                    .navigationTitle("Scan code")
                            } label: {
                                Label("Scan code", systemImage: "qrcode")
                            }
                            .padding()
                            Button {
                                Task {
                                    let ok = await verify(connectionCode)
                                    await MainActor.run {
                                        verified = ok
                                        if !ok { showCodeError = true }
                                    }
                                }
                            } label: {
                                Label("Mark verified", systemImage: "checkmark")
                            }
                            .padding()
                            .alert(isPresented: $showCodeError) {
                                Alert(title: Text("Incorrect security code!"))
                            }
                        }
                    }
                }
                .frame(maxWidth: .infinity, alignment: .center)
            }
            .padding()
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button {
                        showShareSheet(items: [splitCode])
                    } label: {
                        Image(systemName: "square.and.arrow.up")
                    }
                }
            }
            .onChange(of: verified) { _ in
                if verified { dismiss() }
            }
        }
    }

    func splitToParts(_ s: String, length: Int) -> String {
        if length >= s.count { return s }
        return (0 ... (s.count - 1) / length)
            .map { s.dropFirst($0 * length).prefix(length) }
            .joined(separator: "\n")
    }
}

struct VerifyCodeView_Previews: PreviewProvider {
    static var previews: some View {
        VerifyCodeView(displayName: "alice", connectionCode: "12345 67890 12345 67890", verified: false, verify: {_ in true})
    }
}
