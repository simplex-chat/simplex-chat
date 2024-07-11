//
//  ShareModel.swift
//  SimpleX SE
//
//  Created by Levitating Pineapple on 09/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import UniformTypeIdentifiers
import SwiftUI
import Combine
import SimpleXChat

class ShareModel: ObservableObject {
    @Published var item: NSExtensionItem?
    @Published var chats = Array<ChatData>()
    @Published var search = String()
    @Published var comment = String()
    @Published var selected: ChatData?
    @Published var isLoaded = false
    @Published var error: ShareError?

    var completion: (() -> Void)?

    private let allChats = PassthroughSubject<Array<ChatData>, Never>()
    private var bag = Set<AnyCancellable>()

    init() {
        // Attempt loading chats
        Task {
            switch fetchChats() {
            case let .success(chats):
                await MainActor.run {
                    self.chats = chats
                    withAnimation { self.isLoaded = true }
                }
            case let .failure(error):
                await MainActor.run { self.error = .fetchChats(error) }
            }
        }

        // Binding: Filter chats based on debounced search query
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
            .store(in: &bag)

        // Binding: Filtering out chat deselects it
        $chats
            .map { chats in
                Set(chats).contains(self.selected) ? self.selected : nil
            }
            .receive(on: DispatchQueue.main)
            .assign(to: \.selected, on: self)
            .store(in: &bag)
    }

    func send() {
        Task {
            switch await self.sendMessage() {
            case .success:
                try? await Task.sleep(for: .seconds(2))
                completion?()
            case let .failure(error):
                self.error = .sendMessage(error)
            }
        }
    }

    private func sendMessage() async -> Result<AChatItem, ShareError.SendMessage> {
        guard let chat = selected else { return .failure(.noChatWasSelected) }
        guard let attachment = item?.attachments?.first else { return .failure(.missingAttachment) }
        do {
            let url = try await url(attachment: attachment, type: .data)
            guard let cryptoFile = saveFileFromURL(url) else{ return .failure(.encryptFileFailure) }
            let chatResponse = sendSimpleXCmd(
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
            return switch chatResponse {
            case let .newChatItem(_, chatItem): .success(chatItem)
            default: .failure(.sendMessageFailure)
            }
        } catch {
            return .failure(.loadFileRepresentation(error))
        }
    }

    private func fetchChats() -> Result<Array<ChatData>, ShareError.FetchChats> {
        registerGroupDefaults()
        haskell_init_se()
        let (_, result) = chatMigrateInit()
        guard (result == .ok) else {
            return .failure(.unexpectedMigrationResult(result))
        }
        guard case let .activeUser(user: user) = sendSimpleXCmd(.showActiveUser) else {
            return .failure(.noActiveUser)
        }
        guard case .cmdOk = sendSimpleXCmd(.apiSetNetworkConfig(networkConfig: getNetCfg())) else {
            return .failure(.networkConfigurationFailure)
        }
        guard case .cmdOk = sendSimpleXCmd(
            .apiSetAppFilePaths(
                filesFolder: getAppFilesDirectory().path,
                tempFolder: getTempFilesDirectory().path,
                assetsFolder: getWallpaperDirectory().deletingLastPathComponent().path
            )
        ) else {
            return .failure(.unableToSetupFilePaths)
        }
        guard case .chatStarted = sendSimpleXCmd(.startChat(mainApp: false)) else {
            return .failure(.unableToStartChat)
        }
        guard case let .apiChats(user: _, chats: chats) = sendSimpleXCmd(.apiGetChats(userId: user.userId)) else {
            return .failure(.unableToFetchChats(user))
        }
        return .success(chats)
    }
}

fileprivate func url(attachment: NSItemProvider, type: UTType) async throws -> URL {
    try await withCheckedThrowingContinuation { cont in
        let _ = attachment.loadFileRepresentation(for: type, openInPlace: true) { url, bool, error in
            if let url = url {
                cont.resume(returning: url)
            } else if let error = error {
                cont.resume(throwing: error)
            } else {
                fatalError("Either `url` or `error` must be present")
            }
        }
    }
}
