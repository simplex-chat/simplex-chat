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
    @State private var mentionRange: Range<String.Index>?
    @State private var mentionMember: (name: String, member: GMember)?

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
                            let mentionedMemberId = mentionMember?.member.wrapped.groupMemberId ?? -1
                            ForEach(filteredMembers, id: \.wrapped.groupMemberId) { m in
                                let notMentioned = mentionedMemberId != m.wrapped.groupMemberId
                                let disabled = composeState.mentions.count >= MAX_NUMBER_OF_MENTIONS && notMentioned
                                MemberRowView(
                                    groupInfo: groupInfo,
                                    groupMember: m,
                                    showInfo: false,
                                    swipesEnabled: false,
                                    showlocalAliasAndFullName: true,
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
        return mentionName.isEmpty
        ? members
        : members.filter { $0.wrapped.localAliasAndFullName.localizedLowercase.contains(mentionName) }
    }
    
    private func messageChanged(_ msg: String, _ parsedMsg: [FormattedText], _ range: NSRange) {
        if let (ft, r) = selectedMarkdown(parsedMsg, range) {
            switch ft.format {
            case let .mention(name): //
                isVisible = true
                mentionName = name
                mentionRange = Range(r, in: msg)
                mentionMember = if let mm = composeState.mentions[name] {
                    (name, mm.member)
                } else {
                    nil
                }
                //   - trigger loading member list if empty
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
        isVisible = false
        mentionName = ""
        mentionRange = nil
        mentionMember = nil
    }
    
    private func getCharacter(_ s: String, _ pos: Int) -> (char: String.SubSequence, range: Range<String.Index>)? {
        if pos >= 0 && pos < s.count, let range = Range(NSRange(location: pos, length: 1), in: s) {
            (s[range], range)
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
            var mentions = composeState.mentions
            if let (name, mem) = mentionMember {
                if mem.wrapped.groupMemberId != member.wrapped.groupMemberId {
                    if let mm = mentions[name], mm.count > 1 {
                        mentions[name] = (mm.member, mm.count - 1)
                    } else {
                        mentions.removeValue(forKey: name)
                    }
                    addMemberMention(mentions, member, range)
                }
            } else {
                addMemberMention(mentions, member, range)
            }
        }
    }
    
    private func addMemberMention(_ mentions: MentionedMembers, _ member: GMember, _ range: Range<String.Index>) {
        let (newName, nameCount) = if let mm = mentions.first(where: { $0.value.member.wrapped.groupMemberId == member.wrapped.groupMemberId }) {
            (mm.key, mm.value.count + 1)
        } else {
            (composeState.mentionMemberName(member.wrapped.memberProfile.displayName), 1)
        }
        var newMentions = mentions
        newMentions[newName] = (member, nameCount)
        let msgMention = "@" + (newName.contains(" ") ? "'\(newName)'" : newName)
        composeState = composeState.copy(
            message: composeState.message.replacingCharacters(in: range, with: msgMention),
            mentions: newMentions
        )
        // TODO update caret position correctly
        selectedRange = NSRange(location: composeState.message.count, length: 0)
    }
}
