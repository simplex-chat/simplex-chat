//
//  DirectorySearchView.swift
//  SimpleX
//
//  Created by spaced4ndy on 13.08.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

enum DirectoryFooter {
    case retry, showMore
}

// Online results are not chats and are never saved: they live as long as the search text does.
@MainActor
class OnlineSearchModel: ObservableObject {
    @Published private(set) var names: [NameSearchResult] = []
    @Published private(set) var directory: [DirectoryResult] = []
    @Published private(set) var loading = false { didSet { updateSpinner() } }
    @Published private(set) var resolvingNames = false { didSet { updateSpinner() } }
    @Published private(set) var failed = false
    // tells "nothing searched yet" apart from "searched and found nothing"
    @Published private(set) var searched = false

    private var cursor: JSONValue? = nil
    private var lastPageIds: Set<String> = []
    private var searchedText = ""
    // bumped on every reset, so a reply that arrives too late cannot refill a list already cleared
    private var generation = 0
    private var spinnerShown = false

    var searching: Bool { loading || resolvingNames }
    var showResults: Bool { searching || searched || !names.isEmpty }
    var footer: DirectoryFooter? { failed ? .retry : cursor != nil ? .showMore : nil }

    func reset() {
        generation += 1
        names = []
        directory = []
        cursor = nil
        lastPageIds = []
        loading = false
        resolvingNames = false
        failed = false
        searched = false
        searchedText = ""
    }

    // a way out of the wait, not out of the results: what is already shown stays
    func cancelRequest() {
        generation += 1
        loading = false
        resolvingNames = false
    }

    func search(_ text: String, name: String?) async {
        let text = text.trimmingCharacters(in: .whitespaces)
        guard !text.isEmpty, !(searching && text == searchedText) else { return }
        reset()
        searchedText = text
        if let name {
            resolvingNames = true
            // captured here: the async let initializer runs later, in the child task, possibly after a reset
            let gen = generation
            async let resolution: Void = resolveNames(name, generation: gen)
            await request(append: false)
            await resolution
        } else {
            await request(append: false)
        }
    }

    func searchDirectory(_ text: String) async {
        let text = text.trimmingCharacters(in: .whitespaces)
        guard !text.isEmpty, !(loading && text == searchedText) else { return }
        if text == searchedText {
            directory = []
            cursor = nil
            lastPageIds = []
            failed = false
            searched = false
        } else {
            reset()
            searchedText = text
        }
        await request(append: false)
    }

    func loadMore() async {
        guard cursor != nil, !loading else { return }
        await request(append: true)
    }

    // a failed "Show more" keeps its cursor, so the retry resumes the page rather than starting over
    func retry() async {
        if cursor != nil {
            await loadMore()
        } else if !loading {
            await request(append: false)
        }
    }

    func rows(shownChatIds: Set<ChatId>) -> OnlineSearchRows {
        let nameChatIds = Set(names.compactMap { $0.localChatId })
        let nameLinks = Set(names.compactMap { $0.resolvedLink })
        func isAmong(_ ids: Set<ChatId>, _ id: ChatId?) -> Bool {
            id.map { ids.contains($0) } ?? false
        }
        let lastPage = directory.filter { lastPageIds.contains($0.id) }
        return OnlineSearchRows(
            names: names.filter { !isAmong(shownChatIds, $0.localChatId) },
            directory: directory.filter {
                !isAmong(shownChatIds, $0.localChatId) && !isAmong(nameChatIds, $0.localChatId) && !nameLinks.contains($0.entry.link)
            },
            // a result hidden only because a name resolved to its link is on screen, but not one of your chats
            pageInChats: !lastPage.isEmpty && lastPage.allSatisfy {
                isAmong(shownChatIds, $0.localChatId) || isAmong(nameChatIds, $0.localChatId)
            },
            footer: footer
        )
    }

    // a chat opened from a result exists only once its connection flow ends, so rows without a chat look again then
    func refreshLocalChatIds() async {
        let gen = generation
        var nameChatIds: [String: ChatId] = [:]
        for result in names where result.localChatId == nil {
            nameChatIds[result.id] = await localChatId(result.target)
        }
        var directoryChatIds: [String: ChatId] = [:]
        for result in directory where result.localChatId == nil {
            directoryChatIds[result.id] = await localChatId(result.entry.link)
        }
        guard gen == generation else { return }
        names = names.map { result in
            var r = result
            r.localChatId = r.localChatId ?? nameChatIds[r.id]
            return r
        }
        directory = directory.map { result in
            var r = result
            r.localChatId = r.localChatId ?? directoryChatIds[r.id]
            return r
        }
    }

    private func localChatId(_ link: String) async -> ChatId? {
        let plan = await apiConnectPlan(connLink: link, resolveMode: .never, inProgress: BoxedValue(false), onFailure: .log)
        return plan.flatMap { planLocalChatId($0.connectionPlan) }
    }

    private func resolveNames(_ name: String, generation gen: Int) async {
        let targets = name.hasPrefix("@") || name.hasPrefix("#") ? [name] : ["@" + name, "#" + name]
        var results: [NameSearchResult] = []
        for target in targets {
            // checked before each target: a cancelled or replaced search must not send more names to the server
            guard gen == generation else { return }
            if let r = await apiConnectPlan(connLink: target, resolveMode: .unknown, inProgress: BoxedValue(false), onFailure: .log),
               let result = NameSearchResult(target: target, result: r) {
                results.append(result)
            }
        }
        guard gen == generation else { return }
        resolvingNames = false
        names = results
    }

    private func request(append: Bool) async {
        let gen = generation
        loading = true
        failed = false
        let r = await apiSearchDirectory(searchedText, cursor: cursor)
        var page: [DirectoryResult] = []
        if let r {
            var seen = Set(directory.map { $0.id })
            for entry in r.entries {
                guard seen.insert(entry.id).inserted else { continue }
                page.append(DirectoryResult(entry: entry, localChatId: await localChatId(entry.link)))
            }
        }
        guard gen == generation else { return }
        loading = false
        searched = true
        guard let r else {
            failed = true
            return
        }
        cursor = r.cursor
        directory = append ? directory + page : page
        lastPageIds = Set(page.map { $0.id })
    }

    private func updateSpinner() {
        guard searching != spinnerShown else { return }
        spinnerShown = searching
        if spinnerShown {
            ConnectProgressManager.shared.startConnectProgress(
                NSLocalizedString("Searching directory…", comment: "in progress text"),
                owner: .directorySearch
            ) { [weak self] in
                Task { @MainActor in self?.cancelRequest() }
            }
        } else {
            ConnectProgressManager.shared.stopConnectProgress(.directorySearch)
        }
    }
}

struct SearchInDirectoryRow: View {
    @EnvironmentObject var theme: AppTheme
    @FocusState.Binding var searchFocussed: Bool
    var onSearch: () -> Void

    var body: some View {
        HStack(spacing: 4) {
            Image(systemName: "magnifyingglass")
                .foregroundColor(theme.colors.primary)
            Text("Search in Directory")
                .foregroundColor(theme.colors.primary)
            Spacer()
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .contentShape(Rectangle())
        .onTapGesture {
            searchFocussed = false
            onSearch()
        }
    }
}

struct DirectorySearchRow: View {
    @EnvironmentObject var theme: AppTheme
    var entry: DirectorySearchEntry

    var body: some View {
        HStack(spacing: 8) {
            ProfileImage(
                imageStr: entry.image,
                iconName: isChannel ? "antenna.radiowaves.left.and.right.circle.fill" : "person.2.circle.fill",
                size: 42
            )
            VStack(alignment: .leading, spacing: 2) {
                HStack(spacing: 4) {
                    Text(displayName).fontWeight(.bold).lineLimit(1)
                    if let simplexName = entry.simplexName {
                        // the directory's claim, shown as plain text: connecting uses the link
                        Text(simplexName)
                            .foregroundColor(theme.colors.secondary)
                            .lineLimit(1)
                    }
                }
                if let descr = entry.shortDescr, !descr.isEmpty {
                    Text(descr)
                        .foregroundColor(theme.colors.secondary)
                        .lineLimit(2)
                }
                Text(membersText)
                    .font(.caption)
                    .foregroundColor(theme.colors.secondary)
            }
            Spacer()
            Text(isChannel ? "Join channel" : "Join group")
                .foregroundColor(theme.colors.primary)
                .lineLimit(1)
                .fixedSize()
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .contentShape(Rectangle())
    }

    private var displayName: String { "#" + entry.displayName }

    private var isChannel: Bool { entry.entryType.groupType == .channel }

    private var membersText: String {
        let summary = entry.entryType.summary
        let count = summary.publicMemberCount ?? summary.currentMembers
        return isChannel
            ? subscriberCountStr(count)
            : String.localizedStringWithFormat(NSLocalizedString("%d members", comment: "directory search result"), Int(count))
    }
}

struct NameSearchRow: View {
    @EnvironmentObject var theme: AppTheme
    var result: NameSearchResult

    var body: some View {
        HStack(spacing: 8) {
            ProfileImage(imageStr: result.image, iconName: result.iconName, size: 42)
            VStack(alignment: .leading, spacing: 2) {
                if let profileName = result.profileName {
                    (Text(profileName).fontWeight(.bold) + Text(verbatim: " (\(result.simplexName))").foregroundColor(theme.colors.secondary))
                        .lineLimit(1)
                } else {
                    Text(result.simplexName).fontWeight(.bold).lineLimit(1)
                }
                if let descr = result.descr, !descr.isEmpty {
                    Text(descr)
                        .foregroundColor(theme.colors.secondary)
                        .lineLimit(2)
                }
            }
            Spacer()
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .contentShape(Rectangle())
    }
}

struct DirectoryPageInChatsRow: View {
    @EnvironmentObject var theme: AppTheme

    var body: some View {
        Text("All results on this page are already in your chats")
            .foregroundColor(theme.colors.secondary)
    }
}

struct DirectoryShowMoreRow: View {
    @EnvironmentObject var theme: AppTheme
    @ObservedObject var onlineSearch: OnlineSearchModel

    var body: some View {
        Button {
            Task { await onlineSearch.loadMore() }
        } label: {
            Text("Show more")
                .foregroundColor(onlineSearch.loading ? theme.colors.secondary : theme.colors.primary)
        }
        .disabled(onlineSearch.loading)
    }
}

struct DirectoryRetryRow: View {
    @EnvironmentObject var theme: AppTheme
    var onlineSearch: OnlineSearchModel

    var body: some View {
        Button {
            Task { await onlineSearch.retry() }
        } label: {
            Text("Search failed, tap to retry")
                .foregroundColor(theme.colors.primary)
        }
    }
}

func confirmDirectorySearch(_ search: @escaping () -> Void) {
    if UserDefaults.standard.bool(forKey: DEFAULT_DIRECTORY_SEARCH_ALERT_SHOWN) {
        search()
    } else {
        showDirectorySearchAlert(onSearch: search)
    }
}

private func showDirectorySearchAlert(onSearch: @escaping () -> Void) {
    showAlert(
        NSLocalizedString("Search in Directory?", comment: "alert title"),
        message: NSLocalizedString("The text in the search field will be sent to SimpleX Directory to find public groups and channels. If it is a SimpleX name, it will also be sent to your server that resolves names.\n\nNo contact is created and your profile is not sent.", comment: "alert message"),
        actions: {[
            UIAlertAction(title: NSLocalizedString("Cancel", comment: "alert action"), style: .cancel),
            UIAlertAction(
                title: NSLocalizedString("Search", comment: "alert action"),
                style: .default,
                handler: { _ in onSearch() }
            ),
            UIAlertAction(
                title: NSLocalizedString("Search and don't show again", comment: "alert action"),
                style: .default,
                handler: { _ in
                    UserDefaults.standard.set(true, forKey: DEFAULT_DIRECTORY_SEARCH_ALERT_SHOWN)
                    onSearch()
                }
            )
        ]}
    )
}
