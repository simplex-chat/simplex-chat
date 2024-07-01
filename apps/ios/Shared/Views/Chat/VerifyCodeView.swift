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
    @EnvironmentObject var theme: AppTheme
    var displayName: String
    @State var connectionCode: String?
    @State var connectionVerified: Bool
    var verify: (String?) -> (Bool, String)?
    @State private var showCodeError = false

    var body: some View {
        if let code = connectionCode {
            verifyCodeView(code)
        }
    }

    private func verifyCodeView(_ code: String) -> some View {
        ScrollView {
            let splitCode = splitToParts(code, length: 24)
            VStack(alignment: .leading) {
                Group {
                    HStack {
                        if connectionVerified {
                            Image(systemName: "checkmark.shield")
                                .foregroundColor(theme.colors.secondary)
                            Text("\(displayName) is verified")
                        } else {
                            Text("\(displayName) is not verified")
                        }
                    }
                    .frame(height: 24)

                    QRCode(uri: code)
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
                    if connectionVerified {
                        Button {
                            verifyCode(nil)
                        } label: {
                            Label("Clear verification", systemImage: "shield")
                        }
                        .padding()
                    } else {
                        HStack {
                            NavigationLink {
                                ScanCodeView(connectionVerified: $connectionVerified, verify: verify)
                                    .navigationBarTitleDisplayMode(.large)
                                    .navigationTitle("Scan code")
                                    .modifier(ThemedBackground())
                            } label: {
                                Label("Scan code", systemImage: "qrcode")
                            }
                            .padding()
                            Button {
                                verifyCode(code) { verified in
                                    if !verified { showCodeError = true }
                                }
                            } label: {
                                Label("Mark verified", systemImage: "checkmark.shield")
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
            .onChange(of: connectionVerified) { _ in
                if connectionVerified { dismiss() }
            }
        }
    }

    private func verifyCode(_ code: String?, _ cb: ((Bool) -> Void)? = nil) {
        if let (verified, existingCode) = verify(code) {
            connectionVerified = verified
            connectionCode = existingCode
            cb?(verified)
        }
    }

    private func splitToParts(_ s: String, length: Int) -> String {
        if length >= s.count { return s }
        return (0 ... (s.count - 1) / length)
            .map { s.dropFirst($0 * length).prefix(length) }
            .joined(separator: "\n")
    }
}

struct VerifyCodeView_Previews: PreviewProvider {
    static var previews: some View {
        VerifyCodeView(displayName: "alice", connectionCode: "12345 67890 12345 67890", connectionVerified: false, verify: {_ in nil})
            .environmentObject(CurrentColors.toAppTheme())
    }
}
