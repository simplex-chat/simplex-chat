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
    var groupInfo: GroupInfo
    @Binding var composeState: ComposeState
    @Binding var selectedRange: NSRange
    @Binding var keyboardVisible: Bool

    @State private var isVisible = false
    @State private var currentMessage: String = ""
    @State private var mentionName: String = ""
    @State private var mentionRange: NSRange?
    @State private var mentionMember: GMember?

    var body: some View {
        ZStack {
            if isVisible {
                Color.white.opacity(0.01)
                    .edgesIgnoringSafeArea(.all)
                    .onTapGesture {
                        isVisible = false
                    }
            }
            VStack {
                Spacer()
                VStack {
                    Spacer()
                    VStack {
                        Divider()
                        let list = List {
                            let mentionedMemberId = mentionMember?.wrapped.groupMemberId ?? -1
                            ForEach(filteredMembers, id: \.wrapped.groupMemberId) { m in
                                let mentioned = mentionedMemberId == m.wrapped.groupMemberId
                                let disabled = composeState.mentions.count >= MAX_NUMBER_OF_MENTIONS && !mentioned
                                MemberRowView(
                                    groupInfo: groupInfo,
                                    groupMember: m,
                                    showInfo: false,
                                    swipesEnabled: false,
                                    showlocalAliasAndFullName: true,
                                    selectedMember: mentioned,
                                    alert: Binding.constant(nil)
                                )
                                .contentShape(Rectangle())
                                .disabled(disabled)
                                .opacity(disabled ? 0.6 : 1)
                                .onTapGesture {
                                    memberSelected(m)
                                }
                            }
                        }
                        .listStyle(PlainListStyle())
                        .frame(height: MEMBER_ROW_SIZE * min(MAX_VISIBLE_MEMBER_ROWS, CGFloat(filteredMembers.count)))
 
                        if #available(iOS 16.0, *) {
                            list.scrollDismissesKeyboard(.never)
                        } else {
                            list
                        }
                    }
                    .background(Color(UIColor.systemBackground))
                }
                .frame(maxWidth: .infinity, maxHeight: MEMBER_ROW_SIZE * MAX_VISIBLE_MEMBER_ROWS)
            }
            .offset(y: isVisible ? 0 : 300)
            .animation(.spring(), value: isVisible)
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
    }
    
    private var filteredMembers: [GMember] {
        let members = m.groupMembers
            .filter { m in
                let status = m.wrapped.memberStatus
                return status != .memLeft && status != .memRemoved && status != .memInvited
            }
            .sorted { $0.wrapped.memberRole > $1.wrapped.memberRole }
        let s = mentionName.lowercased()
        return s.isEmpty
        ? members
        : members.filter { $0.wrapped.localAliasAndFullName.localizedLowercase.contains(s) }
    }
    
    private func messageChanged(_ msg: String, _ parsedMsg: [FormattedText], _ range: NSRange) {
        removeUnusedMentions(parsedMsg)
        if let (ft, r) = selectedMarkdown(parsedMsg, range) {
            switch ft.format {
            case let .mention(name):
                isVisible = true
                mentionName = name
                mentionRange = r
                mentionMember = composeState.mentions[name]
                if !m.membersLoaded {
                    Task { await m.loadGroupMembers(groupInfo) }
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
                        mentionMember = nil
                        Task { await m.loadGroupMembers(groupInfo) }
                        return
                    }
                }
            default: ()
            }
        }
        closeMemberList()
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
        if let range = mentionRange {
            if let mentionMember {
                if mentionMember.wrapped.groupMemberId != member.wrapped.groupMemberId {
                    addMemberMention(member, range)
                }
            } else {
                addMemberMention(member, range)
            }
        }
    }
    
    private func addMemberMention(_ member: GMember, _ r: NSRange) {
        guard let range = Range(r, in: composeState.message) else { return }
        var mentions = composeState.mentions
        var newName: String
        if let mm = mentions.first(where: { $0.value.wrapped.groupMemberId == member.wrapped.groupMemberId }) {
            newName = mm.key
        } else {
            newName = composeState.mentionMemberName(member.wrapped.memberProfile.displayName)
        }
        mentions[newName] = member
        var msgMention = "@" + (newName.contains(" ") ? "'\(newName)'" : newName)
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
        mentionMember = nil
    }
}
