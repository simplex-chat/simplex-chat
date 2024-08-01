//
//  NewChatMenuButton.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.11.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

enum NewChatMenuOption: Identifiable {
    case newContact
    case scanPaste
    case newGroup

    var id: Self { self }
}

struct NewChatMenuButton: View {
    @State private var showNewChatSheet = false

    var body: some View {
            Button {
            showNewChatSheet = true
        } label: {
            Image(systemName: "square.and.pencil")
                .resizable()
                .scaledToFit()
                .frame(width: 24, height: 24)
        }
        .sheet(isPresented: $showNewChatSheet) {
            NewChatSheet()
        }
    }
}

private var indent: CGFloat = 36

struct NewChatSheet: View {
    @State var newChatMenuOption: NewChatMenuOption?
    @EnvironmentObject var theme: AppTheme

    var body: some View {
        NavigationView {
            viewBody()
                .navigationTitle("New Chat")
                .navigationBarTitleDisplayMode(.inline)
                .modifier(ThemedBackground(grouped: true))
        }
    }
    
    @ViewBuilder private func viewBody() -> some View {
        List {
            Section {
                Button {
                    newChatMenuOption = .newContact
                } label: {
                    newChatActionButton("link.badge.plus", color: theme.colors.secondary) { Text("Add contact") }
                }
                Button {
                    newChatMenuOption = .scanPaste
                } label: {
                    newChatActionButton("qrcode", color: theme.colors.secondary) { Text("Scan / Paste link") }
                }
                Button {
                    newChatMenuOption = .newGroup
                } label: {
                    newChatActionButton("person.2", color: theme.colors.secondary) { Text("Create group") }
                }
            }
        }
        .sheet(item: $newChatMenuOption) { opt in
            switch opt {
            case .newContact: NewChatView(selection: .invite)
            case .scanPaste: NewChatView(selection: .connect, showQRCodeScanner: true)
            case .newGroup: AddGroupView()
            }
        }
    }
    
    func newChatActionButton<Content : View>(_ icon: String, color: Color/* = .secondary*/, content: @escaping () -> Content) -> some View {
        ZStack(alignment: .leading) {
            Image(systemName: icon).frame(maxWidth: 24, maxHeight: 24, alignment: .center)
                .symbolRenderingMode(.monochrome)
                .foregroundColor(color)
            content().foregroundColor(theme.colors.onBackground).padding(.leading, indent)
        }
    }
}

#Preview {
    NewChatMenuButton()
}
