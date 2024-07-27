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
            if let content = model.sharedContent  {
               itemPreview(content)
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

    @ViewBuilder private func itemPreview(_ content: SharedContent) -> some View {
        switch content {
        case let .image(preview, _): imagePreview(preview)
        case let .movie(preview, _, _): imagePreview(preview)
        case let .url(linkPreview): imagePreview(linkPreview.image)
        case let .data(cryptoFile):
            previewArea {
                Image(systemName: "doc.fill")
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .frame(width: 30, height: 30)
                    .foregroundColor(Color(uiColor: .tertiaryLabel))
                    .padding(.leading, 4)
                Text(cryptoFile.filePath)
            }
        case .text: EmptyView()
        }
    }

    private func imagePreview(_ img: String) -> some View {
        previewArea {
            Image(uiImage: UIImage(base64Encoded: img)!)
                .resizable()
                .scaledToFit()
                .frame(minHeight: 40, maxHeight: 60)
        }
    }

    @ViewBuilder private func previewArea<V: View>(@ViewBuilder content: @escaping () -> V) -> some View {
        HStack(alignment: .center, spacing: 8) {
            content()
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
        Divider()
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
