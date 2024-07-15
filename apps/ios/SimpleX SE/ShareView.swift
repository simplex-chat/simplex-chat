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
                    List(model.chats, selection: $model.selected) { chat in
                        HStack {
                            profileImage(
                                base64Encoded: chat.chatInfo.image,
                                size: 36,
                                isLocal: chat.chatInfo.chatType == .local
                            )
                            Text(chat.chatInfo.displayName)
                            Spacer()
                            radioButton(selected: chat == model.selected)
                        }
                        .tag(chat)
                    }
                } else {
                    ProgressView().frame(maxHeight: .infinity)
                }
            }
            .navigationTitle("Share With")
            .safeAreaInset(edge: .bottom) {
                if let progress = model.progress {
                    loadingBar(progress: progress)
                } else if model.selected != nil {
                    compose
                }
            }
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
        VStack(spacing: .zero) {
            Divider()
            HStack {
                TextField("Comment", text: $model.comment, axis: .vertical)
                    .contentShape(Rectangle())
                    .padding(.horizontal, 12)
                    .padding(.vertical, 4)
                Button(action: model.send) {
                    Image(systemName: "arrow.up.circle.fill")
                        .resizable()
                        .frame(width: 28, height: 28)
                        .padding(6)
                }
            }
            .background(Color(.systemBackground))
            .clipShape(RoundedRectangle(cornerRadius: 20))
            .overlay(
                RoundedRectangle(cornerRadius: 20)
                    .strokeBorder(.tertiary, lineWidth: 1).opacity(0.6)
            )
            .padding(8)
            .background(Material.bar)
        }
    }

    private func loadingBar(progress: Double) -> some View {
        VStack {
            Text("Sending File")
            ProgressView(value: progress)
        }
        .padding()
        .background(Material.ultraThin)
    }

    private func profileImage(base64Encoded: String?, size: Double, isLocal: Bool) -> some View {
        Group {
            if let uiImage = model.decodedImages[base64Encoded?.hashValue ?? .zero] {
                Image(uiImage: uiImage).resizable()
            } else {
                Image(
                    systemName: isLocal
                    ? "folder.circle.fill"
                    : "person.crop.circle.fill"
                ).resizable()
            }
        }
        .foregroundStyle(Color(.tertiaryLabel))
        .frame(width: size, height: size)
        .clipShape(Circle())
    }

    private func radioButton(selected: Bool) -> some View {
        Image(systemName: selected ? "checkmark.circle.fill" : "circle")
            .imageScale(.large)
            .foregroundStyle(selected ? Color.accentColor : Color(.tertiaryLabel))
    }
}
