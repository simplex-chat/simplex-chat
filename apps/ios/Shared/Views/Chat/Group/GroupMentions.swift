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

private class MentionRange: Equatable {
    static func == (lhs: MentionRange, rhs: MentionRange) -> Bool {
        return lhs.start == rhs.start && lhs.name == rhs.name
    }
    
    let start: Int
    var name: String
    
    init(start: Int, name: String) {
        self.start = start
        self.name = name
    }
    
    static var empty: MentionRange = MentionRange(start: -1, name: "")
}

private struct MentionsState {
    let ranges: [Int: MentionRange]
    let mentionMemberOccurrences: [String: Int]
}

struct GroupMentionsView: View {
    @EnvironmentObject var chatModel: ChatModel
    var groupInfo: GroupInfo
    @Binding var composeState: ComposeState
    @Binding var selectedRange: NSRange
    @Binding var keyboardVisible: Bool

    @State private var isVisible = false
    @State private var mentionsState: MentionsState = MentionsState(ranges: [:], mentionMemberOccurrences: [:])
    @State private var filteredMembers: [GMember] = []
    
    private var activeRangeMention: MentionRange {
        return mentionsState.ranges[selectedRange.location] ?? MentionRange.empty
    }
    
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
                            ForEach($filteredMembers, id: \.groupMemberId) { m in
                                let mentionedName = composeState.mentions.first(where: { $0.value.wrapped.groupMemberId == m.wrapped.groupMemberId.wrappedValue })?.key
                                let disabled = composeState.mentions.count >= MAX_NUMBER_OF_MENTIONS && mentionedName == nil
                                
                                MemberRowView(
                                    groupInfo: groupInfo,
                                    groupMember: m.wrappedValue,
                                    showInfo: false,
                                    swipesEnabled: false,
                                    showlocalAliasAndFullName: true,
                                    alert: Binding.constant(nil)
                                )
                                .contentShape(Rectangle())
                                .disabled(disabled)
                                .opacity(disabled ? 0.6 : 1)
                                .onTapGesture {
                                    onMemberSelected(m.wrappedValue, mentionedName)
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
            .onChange(of: composeState.message) { m in
                mentionsState = parseMentionRanges(m)
            }
            .onChange(of: activeRangeMention) { r in
                calculateVisibleMembers(searchText: r == MentionRange.empty ? nil : r.name)
            }
            .onAppear {
                mentionsState = parseMentionRanges(composeState.message)
                if let range = mentionsState.ranges[selectedRange.location] {
                    calculateVisibleMembers(searchText: range.name)
                }
            }
        }
    }
    
    private func calculateVisibleMembers(searchText: String?) {
        if let s = searchText {
            let search = s.replacingOccurrences(of: "\(QUOTE)", with: "")
            let txtAsMention = composeState.mentions.first(where: {
                if $0.value.wrapped.memberProfile.displayName == $0.key {
                    mentionsState.mentionMemberOccurrences[search] == 1 && search == $0.key
                } else {
                    search == $0.key
                }
            })
            if let txtAsMention,
               let m = chatModel.groupMembers.first(where: { $0.wrapped.memberId == txtAsMention.value.wrapped.memberId }
               ) {
                isVisible = true
                filteredMembers = [m]
                return;
            }
        } else {
            isVisible = false
            if filteredMembers.count == 1 && composeState.mentions.count < MAX_NUMBER_OF_MENTIONS {
                let memberToAutoTag = filteredMembers[0]
                if !composeState.mentions.contains(where: { $0.value.wrapped.memberId == memberToAutoTag.wrapped.memberId }) {
                    let newName = composeState.mentionMemberName(memberToAutoTag.wrapped.memberProfile.displayName)
                    var mentions = composeState.mentions
                    mentions[newName] = memberToAutoTag
                    composeState = composeState.copy(mentions: mentions)
                }
            }
            filteredMembers = []
            return;
        }
        
        if let searchText = searchText {
            Task {
                if searchText.isEmpty {
                    let groupMembers = await apiListMembers(groupInfo.groupId)
                    await MainActor.run {
                        chatModel.groupMembers = groupMembers.map { GMember.init($0) }
                        chatModel.populateGroupMembersIndexes()
                    }
                }
                await MainActor.run {
                    let members = chatModel.groupMembers
                        .filter { m in let status = m.wrapped.memberStatus; return status != .memLeft && status != .memRemoved }
                        .sorted { $0.wrapped.memberRole > $1.wrapped.memberRole }
                    
                    let s = searchText
                        .replacingOccurrences(of: "\(QUOTE)", with: "")
                        .trimmingCharacters(in: .whitespaces).localizedLowercase
                    
                    filteredMembers = s == "" ? members : members.filter { $0.wrapped.chatViewName.localizedLowercase.contains(s) }
                    isVisible = !filteredMembers.isEmpty
                }
            }
        }
    }
    
    private func onMemberSelected(_ member: GMember, _ mentionedName: String?) {
        if let activeRange = mentionsState.ranges[selectedRange.location] {
            isVisible = false
            let msg = composeState.message
            var newName: String
            var mentions = composeState.mentions
            if let mentionedName {
                newName = mentionedName
            } else {
                newName = composeState.mentionMemberName(member.wrapped.memberProfile.displayName)
                mentions[newName] = member
            }
            let endIndex = activeRange.start + activeRange.name.count
            newName = newName.contains(" ") ? "'\(newName)'" : newName
            if (endIndex == msg.count) {
                newName += " "
            }
            
            let rangeStart = msg.index(msg.startIndex, offsetBy: activeRange.start)
            let rangeEnd = msg.index(msg.startIndex, offsetBy: activeRange.start + activeRange.name.count)
            
            composeState = composeState.copy(
                message: msg.replacingCharacters(in: rangeStart..<rangeEnd, with: newName),
                mentions: mentions
            )
            selectedRange = NSRange(location: composeState.message.count, length: 0)
        }
        keyboardVisible = true
    }
}

private func parseMentionRanges(_ message: String) -> MentionsState {
    var mentionByRange: [Int: MentionRange] = [:]
    var parsedMentions: [String: Int] = [:]
    var currentRange: MentionRange? = nil
    
    let addToParsedMentions: (String?) -> Void = { n in
        if let n = n {
            let name = n.trimmingCharacters(in: CharacterSet(charactersIn: String(QUOTE)))
            if !name.isEmpty {
                if let existing = parsedMentions[name] {
                    parsedMentions[name] = existing + 1
                } else {
                    parsedMentions[name] = 1
                }
            }
        }
    }

    for (i, char) in message.enumerated() {
        let isInsideQuote = currentRange?.name.filter { $0 == QUOTE }.count == 1

        if isInsideQuote, char == QUOTE, currentRange != nil {
            currentRange?.name.append(char)
            mentionByRange[i + 1] = currentRange
            addToParsedMentions(currentRange?.name)
            currentRange = nil
            continue
        }

        if !isInsideQuote, (char == " " || char == "\n") {
            addToParsedMentions(currentRange?.name)
            currentRange = nil
            continue
        }

        if currentRange == nil, char == MENTION_START {
            currentRange = MentionRange(start: i + 1, name: "")
            mentionByRange[i + 1] = currentRange
        } else {
            currentRange?.name.append(char)
            mentionByRange[i + 1] = currentRange
        }
    }

    mentionByRange[message.count] = currentRange
    addToParsedMentions(currentRange?.name)
    

    return MentionsState(
        ranges: mentionByRange,
        mentionMemberOccurrences: parsedMentions
    )
}
