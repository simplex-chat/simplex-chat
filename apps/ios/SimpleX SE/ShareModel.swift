//
//  ShareModel.swift
//  SimpleX SE
//
//  Created by User on 09/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import UniformTypeIdentifiers
import OSLog
import SwiftUI
import Combine
import SimpleXChat

let logger = Logger()

class ShareModel: ObservableObject {
    @Published var item: NSExtensionItem?
    @Published var chats = Array<ChatData>()
    @Published var search = String()
    @Published var comment = String()
    @Published var selected: ChatData?

    var completion: (() -> Void)!

    private let allChats = PassthroughSubject<Array<ChatData>, Never>()
    private var cancellables = Set<AnyCancellable>()

    init() {
        Task { await setup() }

        // Throttled search filters chats
        $search
            .removeDuplicates()
            .throttle(for: .milliseconds(300), scheduler: RunLoop.current, latest: true)
            .combineLatest(allChats)
            .map { (search, chats) in
                search.isEmpty
                ? chats
                : chats.filter { $0.chatInfo.chatViewName.localizedCaseInsensitiveContains(search) }
            }
            .receive(on: DispatchQueue.main)
            .assign(to: \.chats, on: self)
            .store(in: &cancellables)

        // Deselect a chat, when filtered by the search
        $chats
            .map { chats in
                Set(chats).contains(self.selected) ? self.selected : nil
            }
            .receive(on: DispatchQueue.main)
            .assign(to: \.selected, on: self)
            .store(in: &cancellables)
    }

    func send() async {
        guard let item else { fatalError("Missing Extension Item") }
        guard let chat = selected else { return }
        let url = try! await url(attachment: item.attachments!.first!, type: .data)!
        let cryptoFile = saveFileFromURL(url)!
        let _ = sendSimpleXCmd(
            .apiSendMessage(
                type: chat.chatInfo.chatType,
                id: chat.chatInfo.apiId,
                file: cryptoFile,
                quotedItemId: nil,
                msg: .file(comment),
                live: false,
                ttl: nil
            )
        )
        Task {
            try! await Task.sleep(for: .seconds(2))
            completion()
        }
    }

    private func url(attachment: NSItemProvider, type: UTType) async throws -> URL? {
        try await withCheckedThrowingContinuation { cont in
            // TODO: This call returns a progress for showing a loading bar
            let _ = attachment.loadFileRepresentation(for: type, openInPlace: true) { url, bool, error in
                if let url = url {
                    cont.resume(returning: url)
                } else if let error = error {
                    cont.resume(throwing: error)
                } else {
                    cont.resume(returning: nil)
                }
            }
        }
    }

    private func setup() async {
        registerGroupDefaults()
        haskell_init_se()
        let (_, result) = chatMigrateInit()
        guard (
            result == .ok
        ) else { fatalError("Unexpected database result") }

        guard case let .activeUser(user: user) = sendSimpleXCmd(
            .showActiveUser
        ) else { fatalError("No active user") }

        guard case .cmdOk = sendSimpleXCmd(
            .apiSetNetworkConfig(networkConfig: getNetCfg())
        ) else { fatalError("Error setting up networking") }

        guard case .cmdOk = sendSimpleXCmd(
            .apiSetAppFilePaths(
                filesFolder: getAppFilesDirectory().path,
                tempFolder: getTempFilesDirectory().path,
                assetsFolder: getWallpaperDirectory().deletingLastPathComponent().path
            )
        ) else { fatalError("Error settting up file paths") }

        guard case .chatStarted = sendSimpleXCmd(
            .startChat(mainApp: false)
        ) else { fatalError("Unable to start chat") }

        guard case let .apiChats(user: _, chats: chats) = sendSimpleXCmd(
            .apiGetChats(userId: user.userId)
        ) else { fatalError("No chats available") }

        await MainActor.run { allChats.send(chats) }
    }
}
