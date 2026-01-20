//
//  ImageThumbnailView.swift
//  SimpleX
//
//  Created by Suren Poghosyan on 20.01.26.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ImageThumbnailView: View {
    let chatItem: ChatItem
    let file: CIFile
    let size: CGFloat
    
    @Binding var selectedImage: ChatItem?
    @Binding var showFullScreen: Bool
    @EnvironmentObject var theme: AppTheme
    
    var body: some View {
        ZStack {
            if let image = getLoadedImage(file) {
                Image(uiImage: image)
                    .resizable()
                    .scaledToFill()
                    .frame(width: size, height: size)
                    .clipped()
            } else {
                Rectangle()
                    .fill(Color(.systemGray5))
                    .frame(width: size, height: size)
                    .overlay {
                        Image(systemName: "photo")
                            .font(.system(size: 32))
                            .foregroundColor(.gray)
                    }
            }
        }
        .frame(width: size, height: size)
        .contentShape(Rectangle())
        .onTapGesture {
            selectedImage = chatItem
            showFullScreen = true
        }
    }
}
