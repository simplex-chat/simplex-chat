//
//  GroupFilesTabView.swift
//  SimpleX (iOS)
//
//  Created by Suren Poghosyan on 14.01.26.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct GroupFilesTabView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    
    @State private var mediaItems: [ChatItem] = []
    @State private var isLoading = false
    @State private var isLoadingMore = false
    @State private var hasMore = true
  
    private let pageSize: Int = 100
    let groupInfo: GroupInfo

    var body: some View {
        Group {
            if isLoading {
                loadingView
            } else if mediaItems.isEmpty {
                emptyView
            } else {
                fileListView
            }
        }
        .onAppear {
            loadFiles()
        }
    }
    
    private var loadingView: some View {
        HStack {
            Spacer()
            ProgressView()
                .padding(.vertical, 40)
            Spacer()
        }
    }
    
    private var emptyView: some View {
        HStack {
            Spacer()
            VStack(spacing: 12) {
                Image(systemName: "doc")
                    .font(.system(size: 48))
                    .foregroundColor(theme.colors.secondary)
                Text("No files")
                    .foregroundColor(theme.colors.secondary)
            }
            .padding(.vertical, 60)
            Spacer()
        }
    }
    
    private var fileListView: some View {
        LazyVStack(spacing: 0) {
            ForEach(Array(mediaItems.enumerated()), id: \.element.id) { index, item in
                FileListRowView(chatItem: item)
                    .onTapGesture {
                        handleFileTap(item)
                    }
                    .onAppear {
                        let isLast = index == mediaItems.count - 1
                        if isLast {
                            loadMoreIfNeeded()
                        }
                    }
            }
            if isLoadingMore {
                HStack {
                    Spacer()
                    ProgressView().padding()
                    Spacer()
                }
            }
        }
    }
    
    private func loadFiles(reset: Bool = true) {
        if reset {
            guard !isLoading else { return }
            isLoading = true
            mediaItems = []
            hasMore = true
        } else {
            guard !isLoadingMore && hasMore else { return }
            isLoadingMore = true
        }

        let count = pageSize
        let pagination: ChatPagination = {
            if reset {
                return .last(count: count)
            } else if let lastId = mediaItems.last?.id {
                return .before(chatItemId: lastId, count: count)
            } else {
                return .last(count: count)
            }
        }()
        
        Task {
            do {
                let (chat, _) = try await apiGetChat(
                    chatId: groupInfo.id,
                    scope: nil,
                    contentTag: .file,
                    pagination: pagination,
                    search: ""
                )
                await MainActor.run {
                    if reset {
                        mediaItems = chat.chatItems
                        isLoading = false
                    } else {
                        mediaItems.append(contentsOf: chat.chatItems)
                        isLoadingMore = false
                    }
                    hasMore = chat.chatItems.count == pageSize
                }
            } catch {
                logger.error("loadFiles error: \(responseError(error))")
                await MainActor.run {
                    if reset { isLoading = false } else { isLoadingMore = false }
                }
            }
        }
    }
    
    private func loadMoreIfNeeded() {
        loadFiles(reset: false)
    }
    
    private func handleFileTap(_ item: ChatItem) {
        guard let file = item.file else { return }
        
        switch file.fileStatus {
        case .rcvInvitation, .rcvAborted:
            if fileSizeValid(file) {
                Task {
                    if let user = m.currentUser {
                        await receiveFile(user: user, fileId: file.fileId)
                    }
                }
            } else {
                let prettyMaxFileSize = ByteCountFormatter.string(fromByteCount: getMaxFileSize(file.fileProtocol), countStyle: .binary)
                AlertManager.shared.showAlertMsg(
                    title: "Large file!",
                    message: "Your contact sent a file that is larger than currently supported maximum size (\(prettyMaxFileSize))."
                )
            }
        case .rcvAccepted:
            switch file.fileProtocol {
            case .xftp:
                AlertManager.shared.showAlertMsg(
                    title: "Waiting for file",
                    message: "File will be received when your contact completes uploading it."
                )
            case .smp:
                AlertManager.shared.showAlertMsg(
                    title: "Waiting for file",
                    message: "File will be received when your contact is online, please wait or check later!"
                )
            case .local: ()
            }
        case .rcvComplete:
            if let fileSource = getLoadedFileSource(file) {
                saveCryptoFile(fileSource)
            }
        case let .rcvError(rcvFileError):
            showFileErrorAlert(rcvFileError)
        case let .rcvWarning(rcvFileError):
            showFileErrorAlert(rcvFileError, temporary: true)
        case .sndStored:
            if file.fileProtocol == .local, let fileSource = getLoadedFileSource(file) {
                saveCryptoFile(fileSource)
            }
        case .sndComplete:
            if let fileSource = getLoadedFileSource(file) {
                saveCryptoFile(fileSource)
            }
        case let .sndError(sndFileError):
            showFileErrorAlert(sndFileError)
        case let .sndWarning(sndFileError):
            showFileErrorAlert(sndFileError, temporary: true)
        default: break
        }
    }
}
