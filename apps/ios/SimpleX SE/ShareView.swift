//
//  ShareView.swift
//  SimpleX SE
//
//  Created by Levitating Pineapple on 09/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ShareView: View {
    @ObservedObject var model: ShareModel
    @Environment(\.colorScheme) var colorScheme

    var body: some View {
        NavigationView {
            ZStack(alignment: .bottom) {
                if model.isLoaded {
                    List(model.filteredChats) { chat in
                        HStack {
                            profileImage(
                                chatInfoId: chat.chatInfo.id,
                                systemFallback: chatIconName(chat.chatInfo),
                                size: 30
                            )
                            Text(chat.chatInfo.displayName)
                            Spacer()
                            radioButton(selected: chat == model.selected)
                        }
                        .contentShape(Rectangle())
                        .onTapGesture { model.selected = model.selected == chat ? nil : chat }
                        .tag(chat)
                    }
                } else {
                    ProgressView().frame(maxHeight: .infinity)
                }
            }
            .navigationTitle("Share")
            .safeAreaInset(edge: .bottom) {
                switch model.bottomBar {
                case .sendButton:
                    compose(isLoading: false)
                case .loadingSpinner:
                    compose(isLoading: true)
                case .loadingBar(let progress):
                    loadingBar(progress: progress)
                }
            }
        }
        .searchable(
            text: $model.search,
            placement: .navigationBarDrawer(displayMode: .always)
        )
        .alert($model.errorAlert) { alert in
            Button("Ok") { model.completion() }
        }
    }

    private func compose(isLoading: Bool) -> some View {
        VStack(spacing: .zero) {
            Divider()
            if let preview = model.preview {
                itemPreview(preview)
                Divider()
            }
            HStack {
                Group {
                    if #available(iOSApplicationExtension 16.0, *) {
                        TextField("Comment", text: $model.comment, axis: .vertical)
                    } else {
                        TextField("Comment", text: $model.comment)
                    }
                }
                .contentShape(Rectangle())
                .disabled(isLoading)
                .padding(.horizontal, 12)
                .padding(.vertical, 4)
                Group {
                    if isLoading {
                        ProgressView()
                    } else {
                        Button(action: model.send) {
                            Image(systemName: "arrow.up.circle.fill")
                                .resizable()
                        }
                        .disabled(model.isSendDisbled)
                    }
                }
                .frame(width: 28, height: 28)
                .padding(6)

            }
            .background(Color(.systemBackground))
            .clipShape(RoundedRectangle(cornerRadius: 20))
            .overlay(
                RoundedRectangle(cornerRadius: 20)
                    .strokeBorder(.secondary, lineWidth: 0.5).opacity(0.7)
            )
            .padding(8)
        }
        .background(.thinMaterial)
    }

    private func itemPreview(_ preview: SharedContent.Preview) -> some View {
        HStack(alignment: .center, spacing: 8) {
            switch preview {
            case let .image(uiImage):
                Image(uiImage: uiImage)
                    .resizable()
                    .scaledToFit()
                    .frame(minHeight: 40, maxHeight: 60)
            case let .fileName(string):
                Image(systemName: "doc.fill")
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .frame(width: 30, height: 30)
                    .foregroundColor(Color(uiColor: .tertiaryLabel))
                    .padding(.leading, 4)
                Text(string)
            }
            Spacer()
        }
        .padding(.vertical, 1)
        .frame(minHeight: 54)
        .background {
            switch colorScheme {
            case .light: LightColorPaletteApp.sentMessage
            case .dark: DarkColorPaletteApp.sentMessage
            @unknown default: Color(.tertiarySystemBackground)
            }
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

    private func profileImage(chatInfoId: ChatInfo.ID, systemFallback: String, size: Double) -> some View {
        Group {
            if let uiImage = model.profileImages[chatInfoId] {
                Image(uiImage: uiImage).resizable()
            } else {
                Image(systemName: systemFallback).resizable()
            }
        }
        .foregroundStyle(Color(.tertiaryLabel))
        .frame(width: size, height: size)
        .clipShape(RoundedRectangle(cornerRadius: size * 0.225, style: .continuous))
    }

    private func radioButton(selected: Bool) -> some View {
        Image(systemName: selected ? "checkmark.circle.fill" : "circle")
            .imageScale(.large)
            .foregroundStyle(selected ? Color.accentColor : Color(.tertiaryLabel))
    }
}
