//
//  DirectorySearch.swift
//  SimpleX
//
//  Created by spaced4ndy on 12.08.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import Foundation
import SimpleXChat

let DIRECTORY_SERVICE_LINK = "https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok"

let DIRECTORY_SEARCH_TIMEOUT_SEC: Double = 10

let DIRECTORY_SEARCH_MAX_TEXT_LENGTH = 100

let DIRECTORY_SEARCH_MAX_IMAGE_LENGTH = 12500

struct DirectoryPublicLink: Decodable, Hashable {
    var connFullLink: String? = nil
    var connShortLink: String? = nil
}

struct DirectoryEntryType: Decodable, Hashable {
    var groupType: GroupType? = nil
    var summary: GroupSummary
}

struct DirectorySearchEntry: Decodable, Hashable, Identifiable {
    var entryType: DirectoryEntryType
    var displayName: String
    var simplexName: String? = nil
    let link: String
    var shortDescr: String? = nil
    var image: String? = nil

    var id: String { link }

    enum CodingKeys: String, CodingKey {
        case entryType, displayName, simplexName, groupLink, shortDescr, image
    }

    init(from decoder: Decoder) throws {
        let c = try decoder.container(keyedBy: CodingKeys.self)
        entryType = try c.decode(DirectoryEntryType.self, forKey: .entryType)
        displayName = try c.decode(String.self, forKey: .displayName)
        simplexName = try c.decodeIfPresent(String.self, forKey: .simplexName)
        shortDescr = try c.decodeIfPresent(String.self, forKey: .shortDescr)
        image = try c.decodeIfPresent(String.self, forKey: .image).flatMap { isSafeImageData($0) ? $0 : nil }
        let groupLink = try c.decode(DirectoryPublicLink.self, forKey: .groupLink)
        // untrusted input: only group and channel links are kept
        guard let link = groupLink.connShortLink ?? groupLink.connFullLink,
              let linkType = simplexLinkType(link),
              linkType == .group || linkType == .channel else {
            throw DecodingError.dataCorruptedError(forKey: .groupLink, in: c, debugDescription: "not a group or channel link")
        }
        self.link = link
    }
}

func simplexLinkType(_ s: String) -> SimplexLinkType? {
    if case let .link(text, linkType, _) = strConnectTarget(s), text == s { linkType } else { nil }
}

private func isSafeImageData(_ s: String) -> Bool {
    s.utf8.count <= DIRECTORY_SEARCH_MAX_IMAGE_LENGTH
        && (s.hasPrefix("data:image/jpg;base64,/9j/") || s.hasPrefix("data:image/png;base64,iVBORw0KGgo"))
}

private struct DirectorySearchResponse: Decodable {
    var type: String
    var entries: [JSONValue]?
    var searchCursor: JSONValue?
    var moreCount: JSONValue?
}

struct DirectorySearchResults {
    var entries: [DirectorySearchEntry]
    var cursor: JSONValue?
    var moreCount: Int?
}

func isDirectorySearchable(_ text: String, showingLink: Bool) -> Bool {
    !text.trimmingCharacters(in: .whitespaces).isEmpty && !showingLink
}

func directorySearchRequest(_ text: String, _ cursor: JSONValue?) -> JSONValue {
    let searchText = String(String.UnicodeScalarView(text.unicodeScalars.prefix(DIRECTORY_SEARCH_MAX_TEXT_LENGTH)))
    var req: [String: JSONValue] = ["type": .string("search"), "searchText": .string(searchText)]
    if let cursor { req["searchCursor"] = cursor }
    return .object(req)
}

func parseDirectorySearchResponse(_ resp: JSONValue) -> DirectorySearchResults? {
    guard let r: DirectorySearchResponse = decodeJSONValue(resp), r.type == "searchResults" else {
        return nil
    }
    let entries = (r.entries ?? []).compactMap { (e: JSONValue) -> DirectorySearchEntry? in
        decodeJSONValue(e)
    }
    let moreCount: Int? = if case let .number(n)? = r.moreCount, n >= 0, n <= Double(Int32.max), n == n.rounded() { Int(n) } else { nil }
    return DirectorySearchResults(entries: entries, cursor: r.searchCursor, moreCount: moreCount)
}

enum DirectoryResultState {
    case yourChat(ChatId)
    case joining
    case open
}

func directoryResultState(_ plan: ConnectionPlan?) -> DirectoryResultState {
    guard case let .groupLink(groupLinkPlan)? = plan else { return .open }
    switch groupLinkPlan {
    case let .known(groupInfo), let .ownLink(groupInfo): return .yourChat(groupInfo.id)
    case let .connectingProhibit(groupInfo_?): return .yourChat(groupInfo_.id)
    case .connectingProhibit(nil), .connectingConfirmReconnect: return .joining
    case .ok, .noRelays, .updateRequired: return .open
    }
}

struct DirectoryResult: Identifiable {
    let entry: DirectorySearchEntry
    var state: DirectoryResultState

    var id: String { entry.id }
}

enum DirectoryFooter {
    case retry, loadMore
}

struct OnlineSearchRows {
    var yourChats: [ChatId]
    var results: [DirectoryResult]
    var footer: DirectoryFooter?

    var isEmpty: Bool { yourChats.isEmpty && results.isEmpty && footer == nil }
}
