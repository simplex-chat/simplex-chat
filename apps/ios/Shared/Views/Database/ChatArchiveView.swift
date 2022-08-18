//
//  ChatArchiveView.swift
//  SimpleXChat
//
//  Created by Evgeny on 23/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChatArchiveView: View {
    var archiveName: String
    @AppStorage(DEFAULT_CHAT_ARCHIVE_NAME) private var chatArchiveName: String?
    @AppStorage(DEFAULT_CHAT_ARCHIVE_TIME) private var chatArchiveTime: Double = 0
    @State private var showDeleteAlert = false

    var body: some View {
        let fileUrl = getDocumentsDirectory().appendingPathComponent(archiveName)
        let fileTs = chatArchiveTimeDefault.get()
        List {
            Section {
                settingsRow("square.and.arrow.up") {
                    Button {
                        showShareSheet(items: [fileUrl])
                    } label: {
                        Text("Save archive")
                    }
                }
                settingsRow("trash") {
                    Button {
                        showDeleteAlert = true
                    } label: {
                        Text("Delete archive").foregroundColor(.red)
                    }
                }
            } header: {
                Text("Chat archive")
            } footer: {
                Text("Created on \(fileTs)")
            }
        }
        .alert(isPresented: $showDeleteAlert) {
            Alert(
                title: Text("Delete chat archive?"),
                primaryButton: .destructive(Text("Delete")) {
                    do {
                        try FileManager.default.removeItem(atPath: fileUrl.path)
                        chatArchiveName = nil
                        chatArchiveTime = 0
                    } catch let error {
                        logger.error("removeItem error \(String(describing: error))")
                    }
                },
                secondaryButton: .cancel()
            )
        }
    }
}

struct ChatArchiveView_Previews: PreviewProvider {
    static var previews: some View {
        ChatArchiveView(archiveName: "")
    }
}
