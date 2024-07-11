//
//  ShareView.swift
//  SimpleX SE
//
//  Created by User on 09/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ShareView: View {
    let assetSize: Double = 32
    @ObservedObject var model: ShareModel

    var body: some View {
        NavigationStack {
            VStack(spacing: .zero) {
                List(model.chats, selection: $model.selected) { chat in
                    HStack {
                        profileImage(imageString: chat.chatInfo.image, size: assetSize)
                        Text(chat.chatInfo.displayName)
                        Spacer()
                        radioButton(selected: chat == model.selected)
                    }.tag(chat)
                }
                compose
            }
            .navigationTitle("Share")
            .environment(\.editMode, .constant(.active))
        }.searchable(
            text: $model.search,
            placement: .navigationBarDrawer(displayMode: .always)
        )
    }

    private var compose: some View {
        HStack {
            TextField("Comment", text: $model.comment, axis: .vertical)
                .textFieldStyle(.roundedBorder)
            Button() {
                Task { await model.send() }
            } label: {
                Image(systemName: "arrow.up.circle.fill")
                    .resizable()
                    .frame(width: assetSize, height: assetSize)
            }.disabled(model.selected == nil)
        }.padding()
    }

    private func profileImage(imageString: String?, size: Double) -> some View {
        Group {
            if let uiImage = UIImage(base64Encoded: imageString) {
                Image(uiImage: uiImage).resizable()
            } else {
                Image(systemName: "person.crop.circle.fill").resizable()
            }
        }
        .foregroundStyle(Color(.tertiaryLabel))
        .frame(width: size, height: size)
        .clipShape(Circle())
    }

    private func radioButton(selected: Bool) -> some View {
        Image(systemName: selected ? "record.circle" : "circle")
            .imageScale(.large)
            .foregroundStyle(Color.accentColor)
    }
}
