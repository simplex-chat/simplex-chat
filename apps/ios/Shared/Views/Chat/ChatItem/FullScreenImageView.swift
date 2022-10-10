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
    @State private var showNext = false
    @State private var nextImage: UIImage?
    @State private var scrolling = false
    @State private var offset: CGFloat = 0
    @State private var nextOffset: CGFloat = 0

    var body: some View {
        GeometryReader(content: imageScrollView)
    }

    func imageScrollView(_ g: GeometryProxy) -> some View {
        ZStack {
            Color.black.edgesIgnoringSafeArea(.all)
            if showNext, let nextImage = nextImage {
                imageView(image).offset(x: offset)
                imageView(nextImage).offset(x: offset + nextOffset)
            } else {
                ZoomableScrollView {
                    imageView(image)
                }
            }
        }
        .onTapGesture { showView = false }
        .gesture(
            DragGesture(minimumDistance: 80)
            .onChanged { gesture in
                let t = gesture.translation
                let w = abs(t.width)
                if t.height > 60 && t.height > w * 2  {
                    showView = false
                    if let proxy = scrollProxy {
                        proxy.scrollTo(chatItem.viewId)
                    }
                } else if w > 60 && w > abs(t.height) * 2 && !scrolling {
                    let previous = t.width > 0
                    scrolling = true
                    if let item = m.nextChatItemData(chatItem.id, previous: previous, map: chatItemImage) {
                        var img: UIImage
                        (chatItem, img) = item
                        nextImage = img
                        let s = g.size.width
                        var toOffset: CGFloat
                        (toOffset, nextOffset) = previous ? (s, -s) : (-s, s)
                        showNext = true
                        withAnimation(.easeIn(duration: 0.2)) {
                            offset = toOffset
                        }
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.2) {
                            image = img
                            showNext = false
                            offset = 0
                        }
                    }
                }
            }
            .onEnded { _ in scrolling = false }
        )
    }

    private func imageView(_ img: UIImage) -> some View {
        ZStack {
            Color.black
            Image(uiImage: img)
                .resizable()
                .scaledToFit()
        }
    }

    private func chatItemImage(_ ci: ChatItem) -> (ChatItem, UIImage)? {
        if case .image = ci.content.msgContent,
           let img = getLoadedImage(ci.file) {
            return (ci, img)
        }
        return nil
    }
}
