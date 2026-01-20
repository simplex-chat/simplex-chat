//
//  VideoThumbnailView.swift
//  SimpleX
//
//  Created by Suren Poghosyan on 20.01.26.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import AVKit

struct VideoThumbnailView: View {
   
    // MARK: - Properties
    let chatItem: ChatItem
    let file: CIFile
    let size: CGFloat
    
    // MARK: - Environment
    @EnvironmentObject var theme: AppTheme
    
    // MARK: - State
    @State private var thumbnail: UIImage?
    
    // MARK: - Body
    var body: some View {
        ZStack {
            thumbnailImage
            playIcon
        }
        .frame(width: size, height: size)
        .contentShape(Rectangle())
        .onAppear(perform: loadThumbnail)
    }
    
    // MARK: - Subviews
    @ViewBuilder
    private var thumbnailImage: some View {
        if let thumbnail {
            Image(uiImage: thumbnail)
                .resizable()
                .scaledToFill()
                .frame(width: size, height: size)
                .clipped()
        } else {
            placeholderView
        }
    }
    
    private var placeholderView: some View {
        Rectangle()
            .fill(Color(.systemGray5))
            .frame(width: size, height: size)
            .overlay {
                Image(systemName: "video.fill")
                    .font(.system(size: 32))
                    .foregroundColor(.gray)
            }
    }
    
    private var playIcon: some View {
        Image(systemName: "play.circle.fill")
            .font(.system(size: 40))
            .foregroundColor(.white)
            .shadow(radius: 3)
    }
    
    // MARK: - Helper Methods
    private func loadThumbnail() {
        guard thumbnail == nil,
              case let .video(_, image, _) = chatItem.content.msgContent else {
            return
        }
        thumbnail = imageFromBase64(image)
    }
}
