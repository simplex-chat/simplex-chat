//
//  ShareView.swift
//  SimpleX SE
//
//  Created by User on 09/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ShareView: View {
    @ObservedObject var model: ShareModel

    var body: some View {
        NavigationStack {
            VStack(spacing: .zero) {
                List(model.chats, selection: $model.selected) { chat in
                    HStack {
                        ProfileImage(imageStr: chat.chatInfo.image)
                        Text(chat.chatInfo.displayName)
                        Spacer()
                        Image(systemName: chat == model.selected ? "record.circle" : "circle")
                            .imageScale(.large)
                            .foregroundStyle(Color.accentColor)
                    }.tag(chat)
                }
                HStack {
                    TextField("Comment", text: $model.comment, axis: .vertical)
                        .textFieldStyle(.roundedBorder)
                    Button() {
                        Task { await model.send() }
                    } label: {
                        Image(systemName: "arrow.up.circle.fill")
                            .resizable()
                            .frame(width: 32, height: 32)
                    }.disabled(model.selected == nil)
                }.padding()
            }
            .navigationTitle("Share")
            .environment(\.editMode, .constant(.active))
        }.searchable(
            text: $model.search,
            placement: .navigationBarDrawer(displayMode: .always)
        )
    }
}

// TODO: Reuse view from the App?
struct ProfileImage: View {
    let imageStr: String?
    let size: Double = 32

    var body: some View {
        Group {
            if let image = imageStr,
               let data = Data(base64Encoded: dropImagePrefix(image)),
               let uiImage = UIImage(data: data) {
                Image(uiImage: uiImage)
                    .resizable()
            } else {
                Image(systemName: "person.crop.circle.fill")
                    .resizable()
            }
        }
        .foregroundStyle(Color(.tertiaryLabel))
        .frame(width: size, height: size)
        .clipShape(Circle())
    }

    func dropImagePrefix(_ s: String) -> String {
        func dropPrefix(_ s: String, _ prefix: String) -> String {
            s.hasPrefix(prefix) ? String(s.dropFirst(prefix.count)) : s
        }
        return dropPrefix(dropPrefix(s, "data:image/png;base64,"), "data:image/jpg;base64,")
    }
}
