//
//  ShareView.swift
//  SimpleX SE
//
//  Created by Levitating Pineapple on 09/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ShareView: View {
    @ObservedObject var model: ShareModel

    var body: some View {
        NavigationStack {
            ZStack(alignment: .bottom) {
                if model.isLoaded {
                    List(selection: $model.selected) {
                        ForEach(model.chats) { chat in
                            HStack {
                                profileImage(imageString: chat.chatInfo.image, size: 36)
                                Text(chat.chatInfo.displayName)
                                Spacer()
                                radioButton(selected: chat == model.selected)
                            }.tag(chat)
                        }
                        composeSpacer
                    }.listStyle(.plain)
                } else {
                    ProgressView().frame(maxHeight: .infinity)
                }
                compose
            }
            .navigationTitle("Share")
        }
        .searchable(
            text: $model.search,
            placement: .navigationBarDrawer(displayMode: .always)
        )
        .alert(
            isPresented: .constant(model.error != nil),
            error: model.error
        ) { Button("Dismiss") { model.completion?() } }
    }

    private var compose: some View {
        HStack {
            TextField("Comment", text: $model.comment, axis: .vertical)
                .textFieldStyle(.roundedBorder)
            Button(action: model.send) {
                Image(systemName: "arrow.up.circle.fill")
                    .resizable()
                    .frame(width: 36, height: 36)
            }.disabled(model.selected == nil)
        }
        .padding(8)
        .background(Material.bar)
    }

    private var composeSpacer: some View {
        Text(" " + model.comment)
            .padding(.trailing, 36)
            .padding(8)
            .opacity(.zero)
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
