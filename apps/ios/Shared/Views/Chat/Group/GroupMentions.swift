//
//  GroupMentions.swift
//  SimpleX (iOS)
//
//  Created by Diogo Cunha on 30/01/2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

let MENTION_START: Character = "@"
let QUOTE: Character = "'"
let MEMBER_ROW_SIZE: CGFloat = 60
let MAX_VISIBLE_MEMBER_ROWS: CGFloat = 4.8

struct GroupMentionsView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    var groupInfo: GroupInfo
    @Binding var composeState: ComposeState
    @Binding var selectedRange: NSRange
    @Binding var keyboardVisible: Bool

    @State private var isVisible = false
    @State private var currentMessage: String = ""
    @State private var mentionName: String = ""
    @State private var mentionRange: NSRange?
    @State private var mentionMemberId: String?
    @State private var sortedMembers: [GMember] = []

    var body: some View {
        ZStack(alignment: .bottom) {
            if isVisible {
                let filtered = filteredMembers()
                if filtered.count > 0 {
                    Color.white.opacity(0.01)
                        .edgesIgnoringSafeArea(.all)
                        .onTapGesture {
                            isVisible = false
                        }
                    VStack(spacing: 0) {
                        Spacer()
                        Divider()
                        let scroll = ScrollView {
                            LazyVStack(spacing: 0) {
                                ForEach(Array(filtered.enumerated()), id: \.element.wrapped.groupMemberId) { index, member in
                                    let mentioned = mentionMemberId == member.wrapped.memberId
                                    let disabled = composeState.mentions.count >= MAX_NUMBER_OF_MENTIONS && !mentioned
                                    ZStack(alignment: .bottom) {
                                        memberRowView(member.wrapped, mentioned)
                                            .contentShape(Rectangle())
                                            .disabled(disabled)
                                            .opacity(disabled ? 0.6 : 1)
                                            .onTapGesture {
                                                memberSelected(member)
                                            }
                                            .padding(.horizontal)
                                            .frame(height: MEMBER_ROW_SIZE)

                                        Divider()
                                            .padding(.leading)
                                            .padding(.leading, 48)
                                    }
                                }
                            }
                        }
                            .frame(maxHeight: MEMBER_ROW_SIZE * min(MAX_VISIBLE_MEMBER_ROWS, CGFloat(filtered.count)))
                            .background(Color(UIColor.systemBackground))

                        if #available(iOS 16.0, *) {
                            scroll.scrollDismissesKeyboard(.never)
                        } else {
                            scroll
                        }
                    }
                }
            }
        }
        .onChange(of: composeState.parsedMessage) { parsedMsg in
            currentMessage = composeState.message
            messageChanged(currentMessage, parsedMsg, selectedRange)
        }
        .onChange(of: selectedRange) { r in
            // This condition is needed to prevent messageChanged called twice,
            // because composeState.formattedText triggers later when message changes.
            // The condition is only true if position changed without text change
            if currentMessage == composeState.message {
                messageChanged(currentMessage, composeState.parsedMessage, r)
            }
        }
        .onAppear {
            currentMessage = composeState.message
        }
    }
    
    private func filteredMembers() -> [GMember] {
        let s = mentionName.lowercased()
        return s.isEmpty
        ? sortedMembers
        : sortedMembers.filter { $0.wrapped.localAliasAndFullName.localizedLowercase.contains(s) }
    }

    private func messageChanged(_ msg: String, _ parsedMsg: [FormattedText], _ range: NSRange) {
        removeUnusedMentions(parsedMsg)
        if let (ft, r) = selectedMarkdown(parsedMsg, range) {
            switch ft.format {
            case let .mention(name):
                isVisible = true
                mentionName = name
                mentionRange = r
                mentionMemberId = composeState.mentions[name]?.memberId
                if !m.membersLoaded {
                    Task {
                        await m.loadGroupMembers(groupInfo)
                        sortMembers()
                    }
                }
                return
            case .none: () //
                let pos = range.location
                if range.length == 0, let (at, atRange) = getCharacter(msg, pos - 1), at == "@" {
                    let prevChar = getCharacter(msg, pos - 2)?.char
                    if prevChar == nil || prevChar == " " || prevChar == "\n" {
                        isVisible = true
                        mentionName = ""
                        mentionRange = atRange
                        mentionMemberId = nil
                        Task {
                            await m.loadGroupMembers(groupInfo)
                            sortMembers()
                        }
                        return
                    }
                }
            default: ()
            }
        }
        closeMemberList()
    }

    private func sortMembers() {
        sortedMembers = m.groupMembers.filter({ m in
            let status = m.wrapped.memberStatus
            return status != .memLeft && status != .memRemoved && status != .memInvited
        })
        .sorted { $0.wrapped.memberRole > $1.wrapped.memberRole }
    }

    private func removeUnusedMentions(_ parsedMsg: [FormattedText]) {
        let usedMentions: Set<String> = Set(parsedMsg.compactMap { ft in
            if case let .mention(name) = ft.format { name } else { nil }
        })
        if usedMentions.count < composeState.mentions.count  {
            composeState = composeState.copy(mentions: composeState.mentions.filter({ usedMentions.contains($0.key) }))
        }
    }

    private func getCharacter(_ s: String, _ pos: Int) -> (char: String.SubSequence, range: NSRange)? {
        if pos < 0 || pos >= s.count { return nil }
        let r = NSRange(location: pos, length: 1)
        return if let range = Range(r, in: s) {
            (s[range], r)
        } else {
            nil
        }
    }
    
    private func selectedMarkdown(_ parsedMsg: [FormattedText], _ range: NSRange) -> (FormattedText, NSRange)? {
        if parsedMsg.isEmpty { return nil }
        var i = 0
        var pos: Int = 0
        while i < parsedMsg.count && pos + parsedMsg[i].text.count < range.location {
            pos += parsedMsg[i].text.count
            i += 1
        }
        // the second condition will be true when two markdowns are selected
        return i >= parsedMsg.count || range.location + range.length > pos + parsedMsg[i].text.count
            ? nil
            : (parsedMsg[i], NSRange(location: pos, length: parsedMsg[i].text.count))
    }
    
    private func memberSelected(_ member: GMember) {
        if let range = mentionRange, mentionMemberId == nil || mentionMemberId != member.wrapped.memberId {
            addMemberMention(member, range)
        }
    }
    
    private func addMemberMention(_ member: GMember, _ r: NSRange) {
        guard let range = Range(r, in: composeState.message) else { return }
        var mentions = composeState.mentions
        var newName: String
        if let mm = mentions.first(where: { $0.value.memberId == member.wrapped.memberId }) {
            newName = mm.key
        } else {
            newName = composeState.mentionMemberName(member.wrapped.memberProfile.displayName)
        }
        mentions[newName] = CIMention(groupMember: member.wrapped)
        var msgMention = newName.contains(" ") || newName.last?.isPunctuation == true
                        ? "@'\(newName)'"
                        : "@\(newName)"
        var newPos = r.location + msgMention.count
        let newMsgLength = composeState.message.count + msgMention.count - r.length
        print(newPos)
        print(newMsgLength)
        if newPos == newMsgLength {
            msgMention += " "
            newPos += 1
        }
        composeState = composeState.copy(
            message: composeState.message.replacingCharacters(in: range, with: msgMention),
            mentions: mentions
        )
        selectedRange = NSRange(location: newPos, length: 0)
        closeMemberList()
        keyboardVisible = true
    }
    
    private func closeMemberList() {
        isVisible = false
        mentionName = ""
        mentionRange = nil
        mentionMemberId = nil
    }
    
    private func memberRowView(_ member: GroupMember, _ mentioned: Bool) -> some View {
        return HStack{
            MemberProfileImage(member, size: 38)
                .padding(.trailing, 2)
            VStack(alignment: .leading) {
                let t = Text(member.localAliasAndFullName).foregroundColor(member.memberIncognito ? .indigo : theme.colors.onBackground)
                (member.verified ? memberVerifiedShield() + t : t)
                    .lineLimit(1)
            }
            Spacer()
            if mentioned {
                Image(systemName: "checkmark")
            }
        }

        func memberVerifiedShield() -> Text {
            (Text(Image(systemName: "checkmark.shield")) + textSpace)
                .font(.caption)
                .baselineOffset(2)
                .kerning(-2)
                .foregroundColor(theme.colors.secondary)
        }
    }
}
