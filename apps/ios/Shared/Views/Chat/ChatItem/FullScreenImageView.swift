//
//  FullScreenImageView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 08/10/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct FullScreenImageView: View {
    @EnvironmentObject var m: ChatModel
    @State var chatItem: ChatItem
    @State var image: UIImage
    @Binding var showView: Bool
    @State var scrollProxy: ScrollViewProxy?

    var body: some View {
        ZStack {
            Color.black.edgesIgnoringSafeArea(.all)
            ZoomableScrollView {
                ZStack {
                    Color.black.edgesIgnoringSafeArea(.all)
                    Image(uiImage: image)
                        .resizable()
                        .scaledToFit()
                        .animation(.linear(duration: 0.15), value: image)
                }
            }
        }
        .onTapGesture { showView = false }
        .gesture(
            DragGesture(minimumDistance: 80)
            .onChanged { gesture in
                let t = gesture.translation
                if t.height > 60 && t.height > abs(t.width) * 2  {
                    showView = false
                    if let proxy = scrollProxy {
                        proxy.scrollTo(chatItem.viewId)
                    }
                }
            }
            .onEnded { gesture in
                let t = gesture.translation
                let w = abs(t.width)
                if w > 80 && w > abs(t.height) * 2 {
                    if let item = m.nextChatItemData(chatItem.id, previous: t.width > 0, map: chatItemImage) {
                        (chatItem, image) = item
                    }
                }
            }
        )
    }

    private func chatItemImage(_ ci: ChatItem) -> (ChatItem, UIImage)? {
        if case .image = ci.content.msgContent,
           let img = getLoadedImage(ci.file) {
            return (ci, img)
        }
        return nil
    }
}
