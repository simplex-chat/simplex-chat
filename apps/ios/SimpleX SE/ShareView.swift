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
    @State private var password = String()
    @AppStorage(GROUP_DEFAULT_PROFILE_IMAGE_CORNER_RADIUS, store: groupDefaults) private var radius = defaultProfileImageCorner

    var body: some View {
        NavigationView {
            ZStack(alignment: .bottom) {
                if model.isLoaded {
                    List(model.filteredChats) { chat in
                        let isProhibited = model.isProhibited(chat)
                        let isSelected = model.selected == chat
                        HStack {
                            profileImage(
                                chatInfoId: chat.chatInfo.id,
                                iconName: chatIconName(chat.chatInfo),
                                size: 30
                            )
                            Text(chat.chatInfo.displayName).foregroundStyle(
                                isProhibited ? .secondary : .primary
                            )
                            Spacer()
                            radioButton(selected: isSelected && !isProhibited)
                        }
                        .contentShape(Rectangle())
                        .onTapGesture {
                            if isProhibited {
                                model.errorAlert = ErrorAlert(
                                    title: "Cannot forward message",
                                    message: "Selected chat preferences prohibit this message."
                                ) { Button("Ok", role: .cancel) { } }
                            } else {
                                model.selected = isSelected ? nil : chat
                            }
                        }
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
            if model.alertRequiresPassword {
                SecureField("Passphrase", text: $password)
                Button("Ok") {
                    model.setup(with: password)
                    password = String()
                }
                Button("Cancel", role: .cancel) { model.completion() }
            } else {
                Button("Ok") { model.completion() }
            }
        }
        .onChange(of: model.comment) {
            model.hasSimplexLink = hasSimplexLink($0)
        }
    }

    private func compose(isLoading: Bool) -> some View {
        VStack(spacing: 0) {
            Divider()
            if let content = model.sharedContent  {
               itemPreview(content)
            }
            HStack {
                Group {
                    if #available(iOSApplicationExtension 16.0, *) {
                        TextField("Comment", text: $model.comment, axis: .vertical).lineLimit(6)
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
        case let .url(preview): linkPreview(preview)
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

    @ViewBuilder private func imagePreview(_ img: String) -> some View {
        if let img = UIImage(base64Encoded: img) {
            previewArea {
                Image(uiImage: img)
                    .resizable()
                    .scaledToFit()
                    .frame(minHeight: 40, maxHeight: 60)
            }
        } else {
            EmptyView()
        }
    }

    @ViewBuilder private func linkPreview(_ linkPreview: LinkPreview) -> some View {
        previewArea {
            HStack(alignment: .center, spacing: 8) {
                if let uiImage = UIImage(base64Encoded: linkPreview.image) {
                    Image(uiImage: uiImage)
                        .resizable()
                        .aspectRatio(contentMode: .fit)
                        .frame(maxWidth: 80, maxHeight: 60)
                }
                VStack(alignment: .center, spacing: 4) {
                    Text(linkPreview.title)
                        .lineLimit(1)
                    Text(linkPreview.uri.absoluteString)
                        .font(.caption)
                        .lineLimit(1)
                        .foregroundColor(.secondary)
                }
                .padding(.vertical, 5)
                .frame(maxWidth: .infinity)
            }
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
            Text("Sending message…")
            ProgressView(value: progress)
        }
        .padding()
        .background(Material.ultraThin)
    }

    @ViewBuilder private func profileImage(chatInfoId: ChatInfo.ID, iconName: String, size: Double) -> some View {
        if let uiImage = model.profileImages[chatInfoId] {
            clipProfileImage(Image(uiImage: uiImage), size: size, radius: radius)
        } else {
            Image(systemName: iconName)
                .resizable()
                .foregroundColor(Color(uiColor: .tertiaryLabel))
                .frame(width: size, height: size)
// add background when adding themes to SE
//                .background(Circle().fill(backgroundColor != nil ? backgroundColor! : .clear))
        }
    }

    private func radioButton(selected: Bool) -> some View {
        Image(systemName: selected ? "checkmark.circle.fill" : "circle")
            .imageScale(.large)
            .foregroundStyle(selected ? Color.accentColor : Color(.tertiaryLabel))
    }
}
